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


logMessage("Calculate outcome dates")
out <- individuals |>
  select("person_id", "cohort_start_date") |>
  inner_join(
    cdm$outcomes_obj1 |>
      addCohortName() |>
      select("cohort_name", "person_id" = "subject_id", "out_date" = "cohort_start_date") |>
      mutate(outcome_type = "main", comparison = "obj1") |>
      union_all(
        cdm$outcomes_obj2 |>
          addCohortName() |>
          select("cohort_name", "person_id" = "subject_id", "out_date" = "cohort_start_date") |>
          mutate(outcome_type = "main", comparison = "obj2")
      ) |>
      dplyr::union_all(
        cdm$hf_hosp |>
          addCohortName() |>
          select("cohort_name", "person_id" = "subject_id", "out_date" = "cohort_start_date") |>
          mutate(outcome_type = "main", comparison = "both")
      ) |>
      dplyr::union_all(
        cdm$nco |>
          addCohortName() |>
          select("cohort_name", "person_id" = "subject_id", "out_date" = "cohort_start_date") |>
          mutate(outcome_type = "nco", comparison = "both")
      ),
    by = "person_id"
  ) |>
  collect() |>
  mutate(out_days = date_count_between(start = cohort_start_date, end = out_date, precision = "day")) |>
  rename("outcome" = "cohort_name") |>
  select(!c("cohort_start_date", "out_date")) |>
  filter(out_days >= 0) |>
  group_by(person_id, outcome) |>
  filter(out_days == min(out_days)) |>
  ungroup()

outcomes <- out |>
  distinct(outcome, outcome_type, comparison)

outcomeResults <- comparisons |>
  pmap(\(reference, comparator, comparison_id, comparison_name) {
    logMessage(paste0("Outcomes ID: ", comparison_id, " - ", comparison_name))
    
    x <- matched |>
      filter(.data$comparison_id == .env$comparison_id) |>
      mutate(cohort_name = factor(cohort_name, c(reference, comparator))) |>
      select(!c(starts_with("reason"), "comparison_id"))
    
    resOut <- outcomes |>
      pmap(\(outcome, outcome_type, comparison) {
        if (comparison != "both") {
          if (comparison =="obj1" & grepl(pattern = "with_hf", x = comparison_name)) {
            return(NULL)
          }
          if (comparison =="obj2" & grepl(pattern = "no_cv", x = comparison_name)) {
            return(NULL)
          }
        }
        logMessage(paste0("Outcome: ", outcome))
        xx <- x |>
          left_join(
            out |>
              filter(.data$outcome == .env$outcome) |>
              select("person_id", "out_days"),
            by = "person_id"
          ) |>
          mutate(out_days = coalesce(out_days, 9999))
        
        res <- c("pp", "pp_np", "pp_2y", "itt") |>
          map(\(fu) {
            xxx <- xx |>
              select(
                "cohort_name", "person_id", "cluster_id", 
                follow_up = all_of(paste0("follow_up.", fu)), "out_days"
              ) |>
              mutate(
                status = if_else(out_days <= follow_up, 1, 0),
                time = if_else(status == 1, out_days, follow_up)
              ) |>
              select(!c("follow_up", "out_days")) |>
              filter(time > 0)
            
            strata |>
              imap(\(id, st) {
                diag <- tibble(
                  error = character(),
                  fun = character()
                )
                
                xxxx <- xxx |>
                  filter(person_id %in% id)
                
                # incidence rates
                inc <- xxxx |>
                  group_by(cohort_name) |>
                  summarise(
                    event_count = sum(status),
                    person_years = sum(time) / 365.25
                  ) |>
                  mutate(incidence_10000 = event_count / person_years * 10000)
                
                # poisson
                pois <- tryCatch({
                  reg <- glm(status ~ cohort_name + offset(log(time)), family = poisson, data = xxxx)
                  tidy(reg) |>
                    filter(term != "(Intercept)") |>
                    select("irr_coef" = "estimate", "irr_se_coef" = "std.error")
                },
                error = function(e) as.character(e)
                )
                if (is.character(pois)) {
                  diag <- diag |>
                    union_all(tibble(error = pois, fun = "poisson"))
                  pois <- tibble(irr_coef = NA_real_, irr_se_coef = NA_real_)
                }
                
                # cox model
                cox <- tryCatch({
                  fit <- coxph(Surv(time, status) ~ cohort_name, data = xxxx, cluster = cluster_id)
                  tidy(fit) |>
                    select("coef" = "estimate", "se_coef" = "std.error", "robust_se_coef" = "robust.se")
                },
                error = function(e) as.character(e)
                )
                if (is.character(cox)) {
                  diag <- diag |>
                    union_all(tibble(error = cox, fun = "cox"))
                  cox <- tibble(coef = NA_real_, se_coef = NA_real_)
                }
                
                # survival probs
                if (outcome_type != "nco") {
                  probs <- tryCatch({
                    fit <- survfit(Surv(time, status) ~ cohort_name, data = xxxx)
                    summary_time <- sort(unique(c(0, xxxx$time)))
                    surv <- summary(fit, times = summary_time)
                    tibble(
                      time = surv$time,
                      survival = surv$surv,
                      lower_survival = surv$lower,
                      upper_survival = surv$upper,
                      cohort_name = str_replace(surv$strata, "^cohort_name=", "")
                    )
                  },
                  error = function(e) as.character(e)
                  )
                  if (is.character(probs)) {
                    diag <- diag |>
                      union_all(tibble(error = probs, fun = "survfit"))
                    probs <- tibble(
                      time = numeric(),
                      survival = numeric(),
                      lower_survival = numeric(),
                      upper_survival = numeric(),
                      cohort_name = character()
                    )
                  }
                } else {
                  probs = tibble(
                    time = numeric(),
                    survival = numeric(),
                    lower_survival = numeric(),
                    upper_survival = numeric(),
                    cohort_name = character()
                  )
                }
                
                list(inc = inc, pois = pois, cox = cox, probs = probs, diag = diag) |>
                  map(\(x) mutate(x, strata = st))
              }) |>
              purrr::compact() |>
              addCol(analysis = fu)
          }) |>
          addCol(outcome = outcome)
      }) |>
      addCol(
        reference = reference,
        comparator = comparator,
        cdm_name = cdmName(cdm),
        package_name = pkg_name,
        package_version = pkg_version
      )
  })

# format result
logMessage("Formatting results")

results[["inc"]] <- outcomeResults |>
  map("inc") |>
  bind_rows() |>
  mutate(
    result_type = "inc",
    variable_name = "Incidence rate",
    variable_level = NA_character_
  ) |>
  transformToSummarisedResult(
    group = c("reference", "comparator", "cohort_name"),
    strata = c("analysis", "outcome", "strata"),
    estimates = c("event_count", "person_years", "incidence_10000"),
    settings = c("result_type", "package_name", "package_version")
  )

results[["pois"]] <- outcomeResults |>
  map("pois") |>
  bind_rows() |>
  mutate(
    result_type = "pois",
    variable_name = "Poisson regression",
    variable_level = NA_character_
  ) |>
  transformToSummarisedResult(
    group = c("reference", "comparator"),
    strata = c("analysis", "outcome", "strata"),
    estimates = c("irr_coef", "irr_se_coef"),
    settings = c("result_type", "package_name", "package_version")
  )

results[["cox"]] <- outcomeResults |>
  map("cox") |>
  bind_rows() |>
  mutate(
    se_coef = if_else(is.na(coef), NA, se_coef),
    result_type = "cox",
    variable_name = "Cox regression",
    variable_level = NA_character_
  ) |>
  transformToSummarisedResult(
    group = c("reference", "comparator"),
    strata = c("analysis", "outcome", "strata"),
    estimates = c("coef", "se_coef", "robust_se_coef"),
    settings = c("result_type", "package_name", "package_version")
  )

results[["probs"]] <- outcomeResults |>
  map("probs") |>
  bind_rows() |>
  mutate(
    result_type = "survival_prob",
    variable_name = "Survival probability",
    variable_level = sprintf("%i", time)
  ) |>
  select(!"time") |>
  transformToSummarisedResult(
    group = c("reference", "comparator", "cohort_name"),
    strata = c("analysis", "outcome", "strata"),
    estimates = c("survival", "lower_survival", "upper_survival"),
    settings = c("result_type", "package_name", "package_version")
  )

results[["diag"]] <- outcomeResults |>
  map("diag") |>
  bind_rows() |>
  mutate(
    result_type = "model_errors",
    variable_name = "Error",
    variable_level = fun
  ) |>
  select(!"fun") |>
  transformToSummarisedResult(
    group = c("reference", "comparator"),
    strata = c("analysis", "outcome", "strata"),
    estimates = c("error"),
    settings = c("result_type", "package_name", "package_version")
  )
