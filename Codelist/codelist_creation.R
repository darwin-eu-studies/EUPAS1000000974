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

# P4 C3 005 ----

file_path <- here::here("Codelist", "Source", "P4C3005_medicines_V2.xlsx")
x <- readxl::read_excel(file_path, sheet = "in")

## MAO ----
mao_concepts <- x |> dplyr::filter(.data$Name %in% c("other_AD_N06AA", "other_AD_N06AF","other_AD_N06AG")) |>
  dplyr::pull(.data$conceptIDs)
mao_inhibitors_cl <- list("mao_inhibitors" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = mao_concepts)$concept_id) |> omopgenerics::newCodelist()
omopgenerics::exportCodelist(mao_inhibitors_cl, path = here::here("Codelist", "Medications"), type = "csv")

## other_antidepressants ----

other_ad_concepts <- x |> dplyr::filter(.data$Name %in% c("other_AD_N06AX")) |>
  dplyr::pull(.data$conceptIDs)
other_ad_cl <- list("other_antidepressants" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = other_ad_concepts)$concept_id) |> omopgenerics::newCodelist()
omopgenerics::exportCodelist(other_ad_cl, path = here::here("Codelist", "Medications"), type = "csv")

## antiarrythmics ----

antiarrythmics_concepts <- x |> dplyr::filter(.data$Name %in% c("antiarrythmics_C01B")) |>
  dplyr::pull(.data$conceptIDs)
antiarrythmics_cl <- list("antiarrythmics" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = antiarrythmics_concepts)$concept_id) |> omopgenerics::newCodelist()
omopgenerics::exportCodelist(antiarrythmics_cl, path = here::here("Codelist", "Medications"), type = "csv")


## high_ceiling_diuretics ----

high_ceiling_diuretics_concepts <- x |> dplyr::filter(.data$Name %in% c("high_ceiling_diuretics_C03C")) |>
  dplyr::pull(.data$conceptIDs)
high_ceiling_diuretics_cl <- list("high_ceiling_diuretics" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = high_ceiling_diuretics_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(high_ceiling_diuretics_cl, path = here::here("Codelist", "Medications"), type = "csv")

## aldosterone_antagonists ----

aldosterone_antagonists_concepts <- x |> dplyr::filter(.data$Name %in% c("aldosterone_antagonists_C03DA")) |>
  dplyr::pull(.data$conceptIDs)
aldosterone_antagonists_cl <- list("aldosterone_antagonists" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = aldosterone_antagonists_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(aldosterone_antagonists_cl, path = here::here("Codelist", "Medications"), type = "csv")

## raas ----

raas_concepts <- x |> dplyr::filter(.data$Name %in% c("raas_C09A_C09C_C09XA")) |>
  dplyr::pull(.data$conceptIDs)
raas_cl <- list("raas" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = raas_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(raas_cl, path = here::here("Codelist", "Medications"), type = "csv")

## betablockers ----

betablockers_concepts <- x |> dplyr::filter(.data$Name %in% c("betablockers_C07A")) |>
  dplyr::pull(.data$conceptIDs)
betablockers_cl <- list("betablockers" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = betablockers_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(betablockers_cl, path = here::here("Codelist", "Medications"), type = "csv")

## ccb ----

ccb_concepts <- x |> dplyr::filter(.data$Name %in% c("ccb_C08C_C08D_C08E")) |>
  dplyr::pull(.data$conceptIDs)
ccb_cl <- list("ccb" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = ccb_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(ccb_cl, path = here::here("Codelist", "Medications"), type = "csv")

## nsaids ----

nsaids_concepts <- x |> dplyr::filter(.data$Name %in% c("nsaids_M01A")) |>
  dplyr::pull(.data$conceptIDs)
nsaids_cl <- list("nsaids" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = nsaids_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(nsaids_cl, path = here::here("Codelist", "Medications"), type = "csv")

## glucocorticoids ----

glucocorticoids_concepts <- x |> dplyr::filter(.data$Name %in% c("glucocorticoids")) |>
  dplyr::pull(.data$conceptIDs)
glucocorticoids_cl <- list("glucocorticoids" = CodelistGenerator::getDescendants(cdm = cdm, conceptId = glucocorticoids_concepts)$concept_id) |> 
  omopgenerics::newCodelist()
omopgenerics::exportCodelist(glucocorticoids_cl, path = here::here("Codelist", "Medications"), type = "csv")


# haert failure narrow ----

file_path <- here::here("Codelist", "Source", "venlafaxine_HF_narrow.xlsx")
x <- readxl::read_excel(file_path) |>
  dplyr::mutate(concept_id = as.numeric(.data$concept_id))
heart_failure_narrow_cl <- list( "heart_failure_narrow" = x |>
  dplyr::filter(.data$Narrow_ale == "T") |>
  dplyr::pull(.data$concept_id)) |> 
  omopgenerics::newCodelist()

omopgenerics::exportCodelist(heart_failure_narrow_cl, path = here::here("Codelist", "Conditions"), type = "csv")
