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


logMessage("Define comparisons")
comparisons <- tribble(
  ~reference, ~comparator,
  "mirtazapine_90_2y_no_cv", "venlafaxine_90_2y_no_cv",
  "mirtazapine_90_5y_no_cv", "venlafaxine_90_5y_no_cv",
  "mirtazapine_365_2y_no_cv", "venlafaxine_365_2y_no_cv",
  "mirtazapine_365_5y_no_cv", "venlafaxine_365_5y_no_cv",
  "mirtazapine_90_2y_with_hf", "venlafaxine_90_2y_with_hf",
  "mirtazapine_90_5y_with_hf", "venlafaxine_90_5y_with_hf",
  "mirtazapine_365_2y_with_hf", "venlafaxine_365_2y_with_hf",
  "mirtazapine_365_5y_with_hf", "venlafaxine_365_5y_with_hf"
) |>
  mutate(
    comparison_id = row_number(),
    comparison_name = paste0(reference, " vs ", comparator)
  )

matchResult <- comparisons |>
  pmap(\(reference, comparator, comparison_id, comparison_name) {
    logMessage(paste0("Matching ID: ", comparison_id, " - ", comparison_name))
    
    set.seed(12345678)
    
    x <- subjects |>
      filter(cohort_name %in% c(reference, comparator)) |>
      mutate(
        cohort_name = if_else(cohort_name == reference, 0, 1)
      )
    
    if (length(unique(x$cohort_name)) != 2) {
      logMessage("No matches found")
      return(NULL)
    }
    
    x <- x |>
      left_join(
        covariates |>
          inner_join(
            x |>
              select("person_id"),
            by = "person_id"
          ) |>
          mutate(value = 1) |>
          pivot_wider(names_from = "covariate", values_from = "value"),
        by = "person_id"
      ) |>
      mutate(across(starts_with("cov_"), \(x) coalesce(x, 0)))
    
    # lasso to select variables
    lasso <- cv.glmnet(
      x = x |>
        select(starts_with("cov_")) |>
        as.matrix(), 
      y = x$cohort_name, 
      alpha = 1
    )
    coefs <- coef.glmnet(lasso, s = "lambda.1se")
    selectedLassoFeatures <- names(coefs[(coefs[,1]!=0),1])
    selectedLassoFeatures <- selectedLassoFeatures[selectedLassoFeatures != "(Intercept)"]
    
    x <- x |>
      select("cohort_name", "person_id", all_of(selectedLassoFeatures)) |>
      left_join(
        ind |>
          select("person_id", all_of(exactCov), "prior_observation", "index_day"), 
        by = "person_id"
      )
    
    # matching
    formula <- paste0(
      "cohort_name ~ ",
      paste0(selectedLassoFeatures, collapse = " + "),
      " + prior_observation"
    ) |>
      as.formula()
    match <- matchit(
      formula = formula,
      data = x,
      exact = exactCov,
      method  = "nearest",
      ratio   = 1,
      replace = FALSE,
      caliper = c(0.2, index_day = 365),
      std.caliper = c(TRUE, FALSE)
    )
    
    matched_data <- match.data(match) |>
      mutate(cohort_name = if_else(cohort_name == 0, reference, comparator)) |>
      select("cohort_name", "person_id", "cluster_id" = "subclass") |>
      mutate(comparison_id = comparison_id)
    
    coef <- tidy(match$model) |>
      select("term", "coef" = "estimate", "se_coef" = "std.error") |>
      mutate(comparison_id = comparison_id)
    
    list(matched_data = matched_data, coef = coef)
  })

logMessage("Prepare matched cohorts")
matched <- matchResult |>
  map("matched_data") |>
  bind_rows() |>
  mutate(cluster_id = as.numeric(cluster_id))

logMessage("Export PS coefficients")
results[["coefficients"]] <- matchResult |>
  map("coef") |>
  bind_rows() |>
  left_join(
    covariatesNames |>
      select("term" = "covariate", "concept_name", "window"),
    by = "term"
  ) |>
  inner_join(comparisons, by = "comparison_id") |>
  mutate(
    variable_name = coalesce(concept_name, term),
    variable_level = window,
    cdm_name = cdmName(cdm),
    result_type = "coefficients",
    package_name = pkg_name,
    package_version = pkg_version
  ) |>
  select(!c("term", "comparison_id", "concept_name", "window", "comparison_name")) |>
  transformToSummarisedResult(
    group = c("reference", "comparator"),
    estimates = c("coef", "se_coef"),
    settings = c("result_type", "package_name", "package_version")
  )

logMessage("Update objective 1 and 2 cohorts")

nm <- uniqueTableName()
matchedIndivdiuals <- matched |>
  select("cohort_name", "subject_id" = "person_id") |>
  distinct()
cdm <- insertTable(cdm = cdm, name = nm, table = matchedIndivdiuals)
cdm$study_cohorts_obj1 <- cdm$study_cohorts_obj1 |>
  addCohortName() |>
  inner_join(cdm[[nm]], by = c("cohort_name", "subject_id")) |>
  compute(name = "study_cohorts_obj1") |>
  recordCohortAttrition(reason = "Restrict to matched individuals")
cdm$study_cohorts_obj2 <- cdm$study_cohorts_obj2 |>
  addCohortName() |>
  inner_join(cdm[[nm]], by = c("cohort_name", "subject_id")) |>
  compute(name = "study_cohorts_obj2") |>
  recordCohortAttrition(reason = "Restrict to matched individuals")
cdm <- dropSourceTable(cdm = cdm, name = nm)
