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

smdBinary <- function(x, variables) {
  x |>
    select("cohort_name", all_of(variables)) |>
    pivot_longer(!"cohort_name") |>
    group_by(cohort_name, name) |>
    summarise(n = mean(value), .groups = "drop") |>
    mutate(cohort_name = paste0("percentage_", cohort_name)) |>
    pivot_wider(names_from = "cohort_name", values_from = "n") |>
    mutate(
      smd = if_else(
        percentage_1 == percentage_0,
        0,
        (percentage_1 - percentage_0) / sqrt((percentage_1 * (1 - percentage_1) + (percentage_0 * (1 - percentage_0))) / 2)
      ),
      asmd = abs(smd),
      variable_level = NA_character_
    ) |>
    rename(variable_name = "name")
}
smdNumeric <- function(x, variables) {
  x |>
    select("cohort_name", all_of(variables)) |>
    pivot_longer(!"cohort_name") |>
    group_by(cohort_name, name) |>
    summarise(mean = mean(value), var = var(value), .groups = "drop") |>
    pivot_longer(!c("cohort_name", "name"), names_to = "func") |>
    pivot_wider(names_from = c("func", "cohort_name"), values_from = "value") |>
    mutate(
      smd = (mean_1 - mean_0) / sqrt(var_1 + var_0),
      asmd = abs(smd),
      variable_level = NA_character_
    ) |>
    rename(variable_name = "name")
}
smdCategorical <- function(x, variables) {
  variables |>
    map(\(xv) {
      x |>
        select("cohort_name", "person_id", name = all_of(xv)) |>
        mutate(value = 1) |>
        pivot_wider() |>
        mutate(across(!c("cohort_name", "person_id"), \(x) coalesce(x, 0))) |>
        smdBinary(sort(unique(x[[xv]]))) |>
        select(!"variable_level") |>
        mutate(
          variable_level = variable_name,
          variable_name = xv
        )
    }) |>
    bind_rows()
}
addCol <- function(res, ...) {
  inc <- map(res, "inc")
  pois <- map(res, "pois")
  cox <- map(res, "cox")
  probs <- map(res, "probs")
  diag <- map(res, "diag")
  list(inc = inc, pois = pois, cox = cox, probs = probs, diag = diag) |>
    map(\(x) {
      x |>
        bind_rows() |>
        mutate(...)
    })
}
getCharacterisation <- function(cohort) {
  
  res <- list()
  
  res[["characterisation"]] <- cohort |>
    summariseCharacteristics(
      strata = list("sex", "age_group", "indication", "history_of_cv", "previous_ssri_use"),
      demographics = TRUE,
      ageGroup = age_groups,
      cohortIntersectFlag = list(
        "Conditions of interest [-Inf, -366]" = list(
          targetCohortTable = "conditions_of_interest", window = c(-Inf, -366)
        ),
        "Conditions of interest [-365, 0]" = list(
          targetCohortTable = "conditions_of_interest", window = c(-365, 0)
        ),
        "Comorbidities [-Inf, -366]" = list(
          targetCohortTable = "comorbidities", window = list(c(-Inf, -366))
        ),
        "Comorbidities [-365, -1]" = list(
          targetCohortTable = "comorbidities", window = list(c(-365, -1))
        ),
        "Prior medications [-365, -1]" = list(
          targetCohortTable = "medications", window = c(-365, -1)
        ),
        "Healthcare visits [-365, -1]" = list(
          targetCohortTable = "visits", window = c(-365, -1)
        ),
        "Prior Anxiety/Depression episodes [-Inf, 0]" = list(
          targetCohortTable = "condition_episodes", window = c(-Inf, 0)
        )
      ),
      cohortIntersectDays  = list("Days from last condition of interest" = list(
        targetCohortTable = "conditions_of_interest",
        targetCohortId = c("anxiety", "depression"),
        order = "last",
        window = c(-Inf, 0)
      ))
    )
  
  logMessage(message = paste0("Summarise venlafaxine DUS for: ", tableName(cohort)))
  
  res[["drug_utilisation_venlafaxine"]] <- cohort |>
    summariseDrugUtilisation(
      strata = list("sex", "age_group", "indication", "history_of_cv", "previous_ssri_use"),
      cohortId = starts_with("venlafaxine"),
      gapEra = 90L,
      conceptSet = codelist_medications["venlafaxine"],
      ingredientConceptId = c(743670),
      numberExposures = TRUE,
      numberEras = FALSE,
      daysExposed = FALSE,
      daysPrescribed = FALSE,
      timeToExposure = FALSE,
      initialExposureDuration = FALSE,
      initialQuantity = FALSE,
      cumulativeQuantity = FALSE,
      initialDailyDose = TRUE,
      cumulativeDose = TRUE
    )
  
  logMessage(message = paste0("Summarise mirtazapine DUS for: ", tableName(cohort)))
  
  res[["drug_utilisation_mirtazapine"]] <- cohort |>
    summariseDrugUtilisation(
      strata = list("sex", "age_group", "indication", "history_of_cv", "previous_ssri_use"),
      cohortId =  starts_with("mirtazapine"),
      gapEra = 90L,
      conceptSet = codelist_medications["mirtazapine"],
      ingredientConceptId = c(725131),
      numberExposures = TRUE,
      numberEras = FALSE,
      daysExposed = FALSE,
      daysPrescribed = FALSE,
      timeToExposure = FALSE,
      initialExposureDuration = FALSE,
      initialQuantity = FALSE,
      cumulativeQuantity = FALSE,
      initialDailyDose = TRUE,
      cumulativeDose = TRUE
    )
  
  bind(res)
}
addStrataColumns <- function(cohort) {
  cdm <- cdmReference(cohort)
  nm <- tableName(cohort)
  cohort |>
    inner_join(
      cdm$individuals |>
        select(
          "subject_id", "sex", "age_group", "indication", "history_of_cv",
          "previous_ssri_use"
        ),
      by = "subject_id"
    ) |>
    compute(name = nm)
}
filterType <- function(cdm, typeConceptId) {
  tables <- c("visit_occurrence", "condition_occurrence",
              "procedure_occurrence", "device_exposure", "measurement",
              "observation")
  for (nm in tables) {
    if (nm %in% names(cdm)) {
      col <- omopgenerics::omopColumns(table = nm, field = "type_concept")
      cdm[[nm]] <- cdm[[nm]] |>
        dplyr::filter(.data[[col]] %in% .env$typeConceptId)
    }
  }
  return(cdm)
}
filterPrescriptions <- function(cdm, typeConceptId) {
  cdm$drug_exposure <- cdm$drug_exposure |>
        dplyr::filter(.data$drug_type_concept_id %in% .env$typeConceptId)
                      
  return(cdm)
}
copyCohorts2 <- function(cohort, n, name, cohortId = NULL) {
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort)
  set <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id", "cohort_name") |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  newSet <- list()
  for (k in seq_len(n)) {
    if (k > 1) {
      newSet[[k]] <- set |>
        dplyr::mutate(cohort_name = paste0(cohort_name, "_", k - 1))
    } else {
      newSet[[k]] <- set
    }
  }
  newSet <- dplyr::bind_rows(newSet) |>
    dplyr::mutate(new_cohort_definition_id = dplyr::row_number())
  joinSet <- newSet |>
    dplyr::select("cohort_definition_id", "new_cohort_definition_id")
  newAttrition <-  omopgenerics::attrition(cohort) |>
    dplyr::inner_join(joinSet, by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  newCodelist <- attr(cohort, "cohort_codelist") |>
    dplyr::collect() |>
    dplyr::inner_join(joinSet, by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id")
  cdm <- omopgenerics::cdmReference(cohort) |>
    omopgenerics::insertTable(name = "temp_set", table = joinSet)
  cohort <- cohort |>
    dplyr::inner_join(cdm$temp_set, by = "cohort_definition_id", relationship = "many-to-many") |>
    dplyr::select(!"cohort_definition_id") |>
    dplyr::rename("cohort_definition_id" = "new_cohort_definition_id") |>
    dplyr::compute(name = name)
  newSet <- newSet |>
    dplyr::rename(
      original_cohort_id = "cohort_definition_id",
      cohort_definition_id = "new_cohort_definition_id"
    )
  cdm[[name]] <- cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = newSet, 
      cohortAttritionRef = newAttrition,
      cohortCodelistRef = newCodelist
    )
  cdm[[name]]
}
