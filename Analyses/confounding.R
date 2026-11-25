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


results[["confounding"]] <- comparisons |>
  pmap(\(reference, comparator, comparison_id, comparison_name) {
    logMessage(paste0("Confounding ID: ", comparison_id, " - ", comparison_name))
    
    x <- subjects |>
      filter(cohort_name %in% c(reference, comparator)) |>
      mutate(cohort_name = if_else(cohort_name == reference, 0, 1))
    
    if (length(unique(x$cohort_name)) != 2) {
      return(NULL)
    }
    
    x <- x |>
      inner_join(
        ind |>
          select(
            "person_id", "depression", "anxiety", "age", "age_group", "sex", 
            "prior_observation", "index_day", "previous_ssri_use", 
            "history_of_cv", any_of("location_id")
          ) |>
          mutate(across(c("previous_ssri_use", "history_of_cv"), \(x) if_else(x == "Yes", 1, 0))),
        by = "person_id"
      ) |>
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
    
    binary <- colnames(x) |>
      keep(\(x) startsWith(x, "cov_"))
    binary <- c(binary, "depression", "anxiety", "previous_ssri_use", "history_of_cv")
    categorical <- c("age_group", "sex")
    if (cdmName(cdm) == "BIFAP") {
      categorical <- c(categorical, "location_id")
    }
    numeric <- c("age", "prior_observation", "index_day")
    
    notAdjusted <- smdBinary(x, binary) |>
      bind_rows(smdCategorical(x, categorical)) |>
      bind_rows(smdNumeric(x, numeric)) |>
      mutate(type = "crude")
    
    x <- x |>
      inner_join(
        matched |>
          filter(.data$comparison_id == .env$comparison_id) |>
          select("person_id"),
        by = "person_id"
      )
    
    adjusted <- smdBinary(x, binary) |>
      bind_rows(smdCategorical(x, categorical)) |>
      bind_rows(smdNumeric(x, numeric)) |>
      mutate(type = "matched")
    
    notAdjusted |>
      union_all(adjusted) |>
      mutate(
        reference = reference,
        comparator = comparator,
        cdm_name = cdmName(cdm),
        result_type = "smd",
        package_name = pkg_name,
        package_version = pkg_version
      ) |>
      transformToSummarisedResult(
        group = c("reference", "comparator"),
        strata = "type",
        estimates = c("smd", "asmd", "percentage_0", "percentage_1", "mean_0", "mean_1", "var_0", "var_1"),
        settings = c("result_type", "package_name", "package_version")
      )
  }) |>
  bind()

results[["confounding"]] <- results[["confounding"]] |>
  left_join(
    covariatesNames |>
      rename(variable_name = "covariate"),
    by = "variable_name"
  ) |>
  mutate(
    variable_name = coalesce(concept_name, variable_name),
    variable_level = coalesce(window, variable_level)
  ) |>
  select(!c("concept_name", "window"))
