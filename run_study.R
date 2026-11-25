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


# Check code_to_run inputs ----
validateCdmArgument(cdm = cdm, requiredTables = c(
  "person", "observation_period", "condition_occurrence", "drug_exposure", 
  "concept"
))
assertNumeric(minCellCount, length = 1)
cdms <- c("CPRD GOLD", "HI-SPEED", "FinOMOP-THL", "SIDIAP", "BIFAP", "DK-DHR")
cdm_name <- cdmName(cdm)
if (!cdm_name %in% cdms) {
  cli::cli_abort(c(
    x = "CDM Name is {.pkg {cdm_name}} but must be a choice between {.var {cdms}}.",
    i = "You can change the CDM name when creating the cdm_reference object: {.code cdm <- cdmFromCon(..., cdmName = 'my_name')}"
  ))
}

# Create a log file ----
createLogFile(logFile = here("Results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# Define analysis settings -----
if(cdm_name == "FinOMOP-THL") {
  study_period <- c(as.Date("2016-01-01"), as.Date("2023-12-31"))
}else if (cdm_name == "HI-SPEED"){
  study_period <- c(as.Date("2015-01-01"), as.Date(NA))
} else {
  study_period <- c(as.Date("2010-01-01"), as.Date(NA))
}

if (cdm_name == "BIFAP") {
  exactCov <- c("age_group", "sex", "depression", "anxiety", "previous_ssri_use", "history_of_cv", "location_id")
} else {
  exactCov <- c("age_group", "sex", "depression", "anxiety", "previous_ssri_use", "history_of_cv")
}

age_groups <- list(c(18,39), c(40, 59), c(60,79), c(80, Inf))
pkg_version <- "1.0.2"
pkg_name <- "P4-C3-005"

# Source functions -----
source(here("Analyses", "functions.R"))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----
if (cdm_name == "BIFAP") {
  logMessage("Subset to type of interest")
  cdm <- filterType(cdm = cdm, typeConceptId = 32817)
}
if (cdm_name == "SIDIAP") {
  logMessage("Subset to prescripion drug records")
  cdm <- filterPrescriptions(cdm = cdm, typeConceptId = 32839)
}

# CDM summary -----
logMessage("Extract snapshot")
results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Summarise observation period")
results[["obs_period"]] <- summariseObservationPeriod(cdm)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
source(here("Cohorts", "instantiate_cohorts.R"))

# Run analyses ----
logMessage("START ANALYSES")

logMessage("Extract features")
source(here("Analyses", "feature_extraction.R"))

logMessage("Match cohorts")
source(here("Analyses", "matching.R"))

logMessage("Evaluate confounding")
source(here("Analyses", "confounding.R"))

logMessage("Analyse follow up")
source(here("Analyses", "follow_up.R"))

logMessage("Outcome model")
source(here("Analyses", "outcome.R"))

logMessage("Characterisation")
source(here("Analyses", "cohort_characteristics.R"))

logMessage("ANALYSES FINISHED")

# Finish ----
logMessage("Bind results")
results <- bind(results)

exportSummarisedResult(
  results,
  minCellCount = minCellCount,
  fileName = "results_{cdm_name}_{date}.csv",
  path = here("Results")
)

cli::cli_alert_success("Study finished")
