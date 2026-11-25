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

logMessage(message = "Instantiate cohorts")

codelist_medications <- importCodelist(path = here("Codelist", "Medications"), type = "csv")

codelist_conditions <- importCodelist(path = here("Codelist", "Conditions"), type = "csv")

codelist_cv_diseases <- codelist_conditions[c(
  "ischemic_heart_disease_v1", "atrial_fibrillation_v1", "valve_disease_broad",
  "myocardial_infarction_v1", "ischaemic_stroke_broad_v3"
)] |>
  unionCodelists()
names(codelist_cv_diseases) <- "cv_diseases"

logMessage("Create index medications cohorts")

cdm$index_medications <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist_medications[c("mirtazapine", "venlafaxine")],
  name = "index_medications"
) |>
  copyCohorts2(name = "index_medications", n = 2) |>
  renameCohort(
    newCohortName = c("mirtazapine_90", "venlafaxine_90", "mirtazapine_365", "venlafaxine_365"),
    cohortId = c("mirtazapine", "venlafaxine", "mirtazapine_1", "venlafaxine_1")
  ) |>
  collapseCohorts(gap = 90L, cohortId = c("mirtazapine_90", "venlafaxine_90")) |>
  collapseCohorts(gap = 365L, cohortId = c("mirtazapine_365", "venlafaxine_365"))

logMessage("Create objective 3 cohorts")

cdm$study_cohorts_obj3 <- cdm$index_medications |>
  requireIsFirstEntry(name = "study_cohorts_obj3") |>
  requireInDateRange(dateRange = study_period) |>
  requireAge(ageRange = c(18, 150)) |>
  requireCohortIntersect(
    cohortId = starts_with("mirtazapine"),
    targetCohortTable = "index_medications",
    targetCohortId = "venlafaxine_90",
    window = c(-Inf, 0),
    intersections = 0
  ) |>
  requireCohortIntersect(
    cohortId = starts_with("venlafaxine"),
    targetCohortTable = "index_medications",
    targetCohortId = "mirtazapine_90",
    window = c(-Inf, 0),
    intersections = 0
  ) |>
  copyCohorts2(
    n = 2,
    name = "study_cohorts_obj3"
  ) |>
  renameCohort(
    newCohortName = c(
      "mirtazapine_90_2y", "mirtazapine_90_5y", "venlafaxine_90_2y", "venlafaxine_90_5y",
      "mirtazapine_365_2y", "mirtazapine_365_5y", "venlafaxine_365_2y", "venlafaxine_365_5y"
    ),
    cohortId = c(
      "mirtazapine_90", "mirtazapine_90_1", "venlafaxine_90", "venlafaxine_90_1",
      "mirtazapine_365", "mirtazapine_365_1", "venlafaxine_365", "venlafaxine_365_1"
    )
  ) |>
  requirePriorObservation(
    cohortId = c("mirtazapine_90_2y", "venlafaxine_90_2y", "mirtazapine_365_2y", "venlafaxine_365_2y"),
    minPriorObservation = 730L
  ) |>
  requirePriorObservation(
    cohortId = c("mirtazapine_90_5y", "venlafaxine_90_5y", "mirtazapine_365_5y", "venlafaxine_365_5y"),
    minPriorObservation = 1825L
  )

logMessage("Create anxiety, depression and insomnia cohorts")

cdm$conditions_of_interest <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist_conditions[c("anxiety_broad", "depression", "insomnia", "migraine", "menopausal_symtpoms", "neuropathic_pain")],
  name = "conditions_of_interest",
  subsetCohort = "study_cohorts_obj3"
) |>
  renameCohort(newCohortName = "anxiety", cohortId = "anxiety_broad")

logMessage("Create anxiety and depression episodes")

cdm$condition_episodes <- cdm$conditions_of_interest |>
  copyCohorts2(name = "condition_episodes", n = 1, cohortId = c("anxiety", "depression")) |>
  requireCohortIntersect(
    cohortId = "anxiety",
    targetCohortTable = "condition_episodes",
    targetCohortId = "anxiety",
    window = c(-365, -1),
    intersection = 0L
  ) |>
  requireCohortIntersect(
    cohortId = "depression",
    targetCohortTable = "condition_episodes",
    targetCohortId = "depression",
    window = c(-365, -1),
    intersection = 0L
  )

logMessage("Create CV cohorts")

cdm$cv_diseases <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist_cv_diseases,
  name = "cv_diseases",
  subsetCohort = "study_cohorts_obj3"
)

logMessage("Create comorbidities cohorts")

cdm$comorbidities <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist_conditions[c(
    "hf_exclusion", "atrial_fibrillation_v1", "cardiomyopathy_broad",
    "ischemic_heart_disease_v1", "valve_disease_broad",
    "myocardial_infarction_v1", "ischaemic_stroke_broad_v3", "hypertension_v1",
    "diabetes_mellitus_v1", "obesity_v1", "chronic_renal_disease_v1"
  )],
  useSourceFields = TRUE,
  name = "comorbidities",
  subsetCohort = "study_cohorts_obj3"
) |>
  renameCohort(
    cohortId = c(
      "hf_exclusion", "atrial_fibrillation_v1", "ischemic_heart_disease_v1", "valve_disease_broad", "myocardial_infarction_v1", "ischaemic_stroke_broad_v3", "hypertension_v1",
      "diabetes_mellitus_v1", "obesity_v1", "chronic_renal_disease_v1"
    ),
    newCohortName = c(
      "heart_failure", "atrial_fibrillation", "ischemic_heart_disease", "valve_disease", "myocardial_infarction", "ischaemic_stroke", "hypertension",
      "diabetes_mellitus", "obesity", "chronic_renal_disease"
    )
  )

logMessage("Create visits cohorts")

cdm$visits <- conceptCohort(
  cdm = cdm,
  conceptSet = c(list(ouptatient_gp_visit = 581477), list(inpatient_visit = 9201), list(outpatient_specialist_visit = 9202)),
  name = "visits",
  subsetCohort = "study_cohorts_obj3"
)

logMessage("Create medications cohorts")

cdm$medications <- conceptCohort(
  cdm = cdm,
  conceptSet = codelist_medications[c(
    "ssri", "other_antidepressants",
    "mao_inhibitors", "aldosterone_antagonists",
    "antiarrythmics", "betablockers", "ccb", "glucocorticoids", "high_ceiling_diuretics",
    "nsaids", "raas"
  )],
  name = "medications",
  subsetCohort = "study_cohorts_obj3"
)

logMessage("Create NCO cohorts")

ncoCodes <- importCodelist(path = here("Codelist"), type = "csv")
cdm$nco <- conceptCohort(
  cdm = cdm,
  conceptSet = ncoCodes,
  name = "nco",
  subsetCohort = "study_cohorts_obj3"
)

logMessage("Create objective 1 cohorts")

nms <- cdm$study_cohorts_obj3 |>
  settings() |>
  pull("cohort_name")
cdm$study_cohorts_obj1 <- cdm$study_cohorts_obj3 |>
  requireCohortIntersect(
    targetCohortTable = "medications",
    targetCohortId = "mao_inhibitors",
    window = c(-365, 0),
    intersections = 0L,
    name = "study_cohorts_obj1"
  ) |>
  requireCohortIntersect(
    targetCohortTable = "medications",
    targetCohortId = "other_antidepressants",
    window = c(-365, 0),
    intersections = 0L
  ) |>
  requireCohortIntersect(
    targetCohortTable = "comorbidities",
    targetCohortId = "heart_failure",
    window = c(-Inf, 0),
    intersections = 0L
  ) |>
  requireCohortIntersect(
    targetCohortTable = "comorbidities",
    targetCohortId = "cardiomyopathy_broad",
    window = c(-Inf, 0),
    intersections = 0L
  ) |>
  renameCohort(
    cohortId = nms,
    newCohortName = paste0(nms, "_no_cv")
  )

logMessage("Create objective 2 cohorts")

cdm$study_cohorts_obj2 <- cdm$study_cohorts_obj3 |>
  requireCohortIntersect(
    targetCohortTable = "medications",
    targetCohortId = "mao_inhibitors",
    window = c(-365, 0),
    intersections = 0L,
    name = "study_cohorts_obj2"
  ) |>
  requireCohortIntersect(
    targetCohortTable = "medications",
    targetCohortId = "other_antidepressants",
    window = c(-365, 0),
    intersections = 0L
  ) |>
  requireCohortIntersect(
    targetCohortTable = "comorbidities",
    targetCohortId = "heart_failure",
    window = c(-Inf, 0),
    intersections = c(1, Inf)
  ) |>
  renameCohort(
    cohortId = nms,
    newCohortName = paste0(nms, "_with_hf")
  )

logMessage("Add individuals strata")

cdm$individuals <- cdm$study_cohorts_obj3 |>
  addCohortName() |>
  select(index_drug = "cohort_name", "subject_id", "cohort_start_date", "cohort_end_date") |>
  mutate(
    end_name = if_else(
      index_drug %in% c("mirtazapine_365_2y", "mirtazapine_365_5y", "venlafaxine_365_2y", "venlafaxine_365_5y"),
      "end_date_365",
      "end_date_90"
    ),
    index_drug = if_else(
      index_drug %in% c("mirtazapine_365_2y", "mirtazapine_365_5y", "mirtazapine_90_2y", "mirtazapine_90_5y"),
      "mirtazapine",
      "venlafaxine"
    )
  ) |>
  distinct() |>
  pivot_wider(names_from = "end_name", values_from = "cohort_end_date") |>
  compute(name = "individuals") |>
  addDemographics(
    age = FALSE,
    sex = TRUE,
    ageGroup = age_groups,
    priorObservation = TRUE,
    futureObservation = TRUE,
    name = "individuals"
  ) |>
  addCohortIntersectFlag(
    targetCohortTable = "conditions_of_interest",
    targetCohortId = "anxiety",
    window = c(-365, 0),
    nameStyle = "anxiety",
    name = "individuals"
  ) |>
  addCohortIntersectFlag(
    targetCohortTable = "conditions_of_interest",
    targetCohortId = "depression",
    window = c(-365, 0),
    nameStyle = "depression",
    name = "individuals"
  ) |>
  mutate(indication = case_when(
    .data$depression == 1 ~ "Depression (w/ or w/o anxiety)",
    .data$anxiety == 1 ~ "Anxiety with no depression",
    .default = "No anxiety or depression"
  )) |>
  compute(name = "individuals") |>
  addCohortIntersectFlag(
    targetCohortTable = "cv_diseases",
    window = c(-Inf, -1),
    nameStyle = "any_cv",
    name = "individuals"
  ) |>
  mutate(history_of_cv = case_when(
    .data$any_cv == 1 ~ "Yes",
    .data$any_cv == 0 ~ "No",
    .default = NA_character_
  )) |>
  select(-"any_cv") |>
  compute(name = "individuals") |>
  addCohortIntersectFlag(
    targetCohortTable = "medications",
    targetCohortId = "ssri",
    window = c(-365, -1),
    nameStyle = "previous_ssri_use",
    name = "individuals"
  ) |>
  mutate(previous_ssri_use = case_when(
    .data$previous_ssri_use == 1 ~ "Yes",
    .data$previous_ssri_use == 0 ~ "No",
    .default = NA_character_
  )) |>
  compute(name = "individuals")

logMessage("Create outcome cohorts")
excluded_source_concepts <- codelist_conditions[["hf_outcome_exclude"]]
cdm2 <- cdm
cdm2$condition_occurrence <- cdm2$condition_occurrence |>
  filter(!(.data$condition_source_concept_id %in% excluded_source_concepts))
cdm$hf <- conceptCohort(cdm2,
  conceptSet = c(codelist_conditions["hf_outcome"], codelist_conditions["heart_failure_narrow"]),
  subsetCohort = "study_cohorts_obj3",
  useSourceFields = TRUE,
  name = "hf"
) |>
  renameCohort(cohortId = c("hf_outcome", "heart_failure_narrow"), newCohortName = c("hf_broad", "hf_narrow"))

# objective 1
altervative_causes <- unionCodelists(codelist_conditions[c("ischemic_heart_disease_v1", "valve_disease_broad", "pulmonary_heart_disease")])

cdm$hf_obj1 <- cdm$hf |>
  copyCohorts2(name = "hf_obj1", n = 2) |>
  renameCohort(
    newCohortName = c("inc_hf_broad", "inc_hf_broad_excl_alternatives", "inc_hf_narrow", "inc_hf_narrow_excl_alternatives"),
    cohortId = c("hf_broad", "hf_broad_1", "hf_narrow", "hf_narrow_1")
  ) |>
  requireIsFirstEntry() |>
  requireConceptIntersect(
    conceptSet = altervative_causes,
    cohortId = c("inc_hf_broad_excl_alternatives", "inc_hf_narrow_excl_alternatives"),
    window = c(-30, 0),
    intersections = 0L
  )

cdm$cardiomyopathy <- conceptCohort(cdm,
  conceptSet = codelist_conditions[c("cardiomyopathy_broad", "cardiomyopathy_primary")],
  subsetCohort = "study_cohorts_obj1",
  name = "cardiomyopathy"
) |>
  requireIsFirstEntry()
cdm <- bind(cdm$hf_obj1, cdm$cardiomyopathy, name = "outcomes_obj1")

# objective 2

cdm$acute_pulmonary_oedema <- conceptCohort(cdm,
  conceptSet = codelist_conditions["acute_pulmonary_oedema_broad"],
  subsetCohort = "study_cohorts_obj2",
  name = "acute_pulmonary_oedema"
)
cdm$recurrent_hf <- cdm$hf |>
  copyCohorts2(name = "recurrent_hf", n = 1, cohortId = "hf_broad") |>
  requireCohortIntersect(
    targetCohortTable = "hf",
    targetCohortId = "hf_broad",
    intersections = 0L,
    window = c(-30, -1)
  )

cdm <- bind(cdm$acute_pulmonary_oedema, cdm$recurrent_hf, name = "outcomes_obj2")

# hospitalisation
cdm2 <- cdm
if (cdm_name == "SIDIAP") {
  cdm2$condition_occurrence <- cdm2$condition_occurrence |>
    dplyr::filter(.data$condition_type_concept_id == 32829, !(.data$condition_source_concept_id %in% excluded_source_concepts))
  cdm2$observation <- cdm2$observation |>
    dplyr::filter(.data$observation_type_concept_id == 32829)

  cdm2$procedure_occurrence <- cdm2$procedure_occurrence |>
    dplyr::filter(.data$procedure_type_concept_id == 32829)
} else {
  cdm2$visit_occurrence <- cdm2$visit_occurrence |>
    dplyr::filter(.data$visit_concept_id == 9201)

  cdm2$condition_occurrence <- cdm2$condition_occurrence |>
    dplyr::semi_join(cdm2$visit_occurrence, by = "visit_occurrence_id") |>
    dplyr::filter(!(.data$condition_source_concept_id %in% excluded_source_concepts))
  cdm2$observation <- cdm2$observation |>
    dplyr::semi_join(cdm2$visit_occurrence, by = "visit_occurrence_id")
  cdm2$procedure_occurrence <- cdm2$procedure_occurrence |>
    dplyr::semi_join(cdm2$visit_occurrence, by = "visit_occurrence_id")
}

cdm$hf_hosp <- conceptCohort(
  cdm = cdm2,
  conceptSet = codelist_conditions["hf_outcome"],
  subsetCohort = "study_cohorts_obj3",
  useSourceFields = TRUE,
  name = "hf_hosp"
) |>
  renameCohort(newCohortName = "hf_hosp")
