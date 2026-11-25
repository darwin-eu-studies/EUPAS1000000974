# Copyright 2026 European Medicines Agency
#
# This software is developed by the DARWIN EU Coordination Centre
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This study code contains unmodified open source dependencies.


tp <- tmpPrefix()
nm <- uniqueTableName(prefix = tp)

individuals <- cdm$individuals |>
  rename("person_id" = "subject_id")

minRecords <- individuals |>
  tally() |>
  pull() * 0.005

logMessage("Extract conditions")
conditions <- cdm$condition_occurrence |>
  select("person_id", "concept_id" = "condition_concept_id", "condition_start_date") |>
  inner_join(
    individuals |>
      select("person_id", "cohort_start_date"), 
    by = "person_id"
  ) |>
  filter(condition_start_date < cohort_start_date) |>
  mutate(days = date_count_between(start = cohort_start_date, end = condition_start_date)) |>
  select("person_id", "concept_id", "days") |>
  compute(name = nm)
conditions <- conditions |>
  inner_join(
    conditions |>
      group_by(concept_id) |>
      summarise(n = n_distinct(person_id)) |>
      filter(n > minRecords & concept_id != 0) |>
      select("concept_id"),
    by = "concept_id"
  ) |>
  compute(name = nm) |>
  mutate(window = case_when(
    days < -365 ~ "any_to_m366",
    days < -30 ~ "m365_to_m31",
    .default = "m30_to_m1"
  )) |>
  distinct(person_id, concept_id, window) |>
  collect()

logMessage("Extract drugs")
drugs <- cdm$drug_exposure |>
  select("person_id", "concept_id" = "drug_concept_id", "drug_exposure_start_date") |>
  inner_join(
    individuals |>
      select("person_id", "cohort_start_date"), 
    by = "person_id"
  ) |>
  filter(drug_exposure_start_date < cohort_start_date) |>
  mutate(days = date_count_between(start = cohort_start_date, end = drug_exposure_start_date)) |>
  select("person_id", "concept_id", "days") |>
  filter(days >= -365) |>
  compute(name = nm)
drugs <- drugs |>
  inner_join(
    drugs |>
      group_by(concept_id) |>
      summarise(n = n_distinct(person_id)) |>
      filter(n > minRecords & concept_id != 0) |>
      select("concept_id"),
    by = "concept_id"
  ) |>
  compute(name = nm) |>
  mutate(window = case_when(
    days < -30 ~ "m365_to_m31",
    .default = "m30_to_m1"
  )) |>
  distinct(person_id, concept_id, window) |>
  collect()

covariates <- drugs |>
  union_all(conditions) |>
  mutate(covariate = paste0("cov_", concept_id, "_win_", window))

rm(drugs, conditions)

logMessage("Covariates names")
covs <- covariates |>
  distinct(concept_id) |>
  pull()
covariatesNames <- covariates |>
  distinct(covariate, concept_id, window) |>
  inner_join(
    cdm$concept |>
      filter(concept_id %in% covs) |>
      select("concept_id", "concept_name") |>
      collect(),
    by = "concept_id"
  ) |>
  mutate(
    concept_name = paste0(concept_name, " (", concept_id, ")"),
    window = case_when(
      window == "any_to_m366" ~ "-Inf to -366",
      window == "m365_to_m31" ~ "-365 to -31",
      window == "m30_to_m1" ~ "-30 to -1"
    )
  ) |>
  select("covariate", "concept_name", "window")
  
covariates <- covariates |>
  select(!c("concept_id", "window")) |>
  distinct()

logMessage("Get stratifications")
x <- individuals |>
  collect()

strata <- list()
strata[["Overall"]] <- x |>
  pull("person_id")
strata[["Age: 18 to 39"]] <- x |>
  filter(age_group == "18 to 39") |>
  pull("person_id")
strata[["Age: 40 to 59"]] <- x |>
  filter(age_group == "40 to 59") |>
  pull("person_id")
strata[["Age: 60 to 70"]] <- x |>
  filter(age_group == "60 to 79") |>
  pull("person_id")
strata[["Age: 80 or above"]] <- x |>
  filter(age_group == "80 or above") |>
  pull("person_id")
strata[["Sex: Female"]] <- x |>
  filter(sex == "Female") |>
  pull("person_id")
strata[["Sex: Male"]] <- x |>
  filter(sex == "Male") |>
  pull("person_id")
strata[["CV history: Yes"]] <- x |>
  filter(history_of_cv == "Yes") |>
  pull("person_id")
strata[["CV history: No"]] <- x |>
  filter(history_of_cv == "No") |>
  pull("person_id")
strata[["Prior SSRI: Yes"]] <- x |>
  filter(previous_ssri_use == "Yes") |>
  pull("person_id")
strata[["Prior SSRI: No"]] <- x |>
  filter(previous_ssri_use == "No") |>
  pull("person_id")
strata[["Depression (w/ or w/o anxiety)"]] <- x |>
  filter(indication == "Depression (w/ or w/o anxiety)") |>
  pull("person_id")
strata[["Anxiety with no depression"]] <- x |>
  filter(indication == "Anxiety with no depression") |>
  pull("person_id")

logMessage("Identifying individuals")
nm <- uniqueTableName(tp)
ind <- individuals |>
  select(!"age_group") |>
  addDemographicsQuery(
    ageGroup = list(
      c(18, 24), c(25, 29), c(30, 34), c(35, 39), c(40, 44), c(45, 49), 
      c(50, 54), c(55, 59), c(60, 64), c(65, 69), c(70, 74), c(75, 79),
      c(80, 84), c(85, 89), c(90, Inf)
    )
  ) |>
  mutate(
    index_day = date_count_between(start = as.Date("2010-01-01"), end = cohort_start_date),
    drug_stop_90 = date_count_between(start = cohort_start_date, end = end_date_90),
    drug_stop_365 = date_count_between(start = cohort_start_date, end = end_date_365)
  ) |>
  compute(name = nm) |>
  addDeathDays(name = nm) |>
  addCohortIntersectDays(
    targetCohortTable = "index_medications",
    targetCohortId = c("venlafaxine_90", "mirtazapine_90"),
    window = c(0, Inf),
    nameStyle = "{cohort_name}",
    name = nm,
    order = "first"
  )

if (cdmName(cdm) == "BIFAP") {
  ind <- ind |>
    left_join(
      cdm$person |>
        select("person_id", "location_id"),
      by = "person_id"
    ) |>
    compute(name = nm)
}

ind <- ind |>
  mutate(next_drug = if_else(index_drug %in% c("mirtazapine"), venlafaxine_90, mirtazapine_90)) |>
  select(!c("cohort_start_date", "end_date_365", "end_date_90", "venlafaxine_90", "mirtazapine_90")) |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999),
    next_drug = coalesce(next_drug, 9999)
  )

logMessage("Collect subjects into memory")
subjects <- cdm$study_cohorts_obj1 |>
  addCohortName() |>
  select("cohort_name", "person_id" = "subject_id") |>
  collect() |>
  union_all(
    cdm$study_cohorts_obj2 |>
      addCohortName() |>
      select("cohort_name", "person_id" = "subject_id") |>
      collect()
  )

logMessage("Remove tables created during feature extraction")
dropSourceTable(cdm = cdm, name = starts_with(tp))
