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


# itt: Intention to treat
# pp: per protocol
# pp_2y: per protocol + 2 years
# pp_np: per protocol no pair censoring

logMessage("Calculate follow-up")
matched <- matched |>
  inner_join(ind, by = "person_id") |>
  mutate(
    drug_stop = if_else(
      str_detect(.data$cohort_name, "_90_"),
      .data$drug_stop_90,
      .data$drug_stop_365
    )
  ) |>
  select(!c("drug_stop_90", "drug_stop_365"))
matched <- matched |>
  mutate(
    reason.pp_2y = case_when(
      days_to_death <= future_observation & days_to_death <= next_drug & days_to_death <= drug_stop & days_to_death <= 730 ~ "death",
      drug_stop <= future_observation & drug_stop <= next_drug & drug_stop <= 730 ~ "stop treatment",
      next_drug <= future_observation & next_drug <= 730 ~ "start competing drug",
      future_observation <= 730 ~ "end of observation",
      .default = "2 years"
    ),
    follow_up.pp_2y = case_when(
      reason.pp_2y == "death" ~ days_to_death,
      reason.pp_2y == "start competing drug" ~ next_drug,
      reason.pp_2y == "stop treatment" ~ drug_stop,
      reason.pp_2y == "2 years" ~ 730,
      reason.pp_2y == "end of observation" ~ future_observation
    ),
    reason.pp = case_when(
      days_to_death <= future_observation & days_to_death <= next_drug & days_to_death <= drug_stop ~ "death",
      drug_stop <= future_observation & drug_stop <= next_drug ~ "stop treatment",
      next_drug <= future_observation ~ "start competing drug",
      .default = "end of observation",
    ),
    follow_up.pp = case_when(
      reason.pp == "death" ~ days_to_death,
      reason.pp == "start competing drug" ~ next_drug,
      reason.pp == "stop treatment" ~ drug_stop,
      reason.pp == "end of observation" ~ future_observation
    ),
    reason.pp_np = reason.pp,
    follow_up.pp_np = follow_up.pp,
    reason.itt = case_when(
      days_to_death <= future_observation & days_to_death <= 730  ~ "death",
      future_observation <= 730 ~ "end of observation",
      .default = "2 years"
    ),
    follow_up.itt = case_when(
      reason.itt == "death" ~ days_to_death,
      reason.itt == "2 years" ~ 730,
      reason.itt == "end of observation" ~ future_observation
    )
  ) |>
  select(
    "cohort_name", "person_id", "cluster_id", "comparison_id",
    starts_with("reason"), starts_with("follow_up")
  )

# pair censoring
pairCensor <- matched |>
  select("cluster_id", "comparison_id", reason = "reason.pp", follow_up = "follow_up.pp") |>
  filter(reason %in% c("stop treatment", "start competing drug")) |>
  group_by(comparison_id, cluster_id) |>
  summarise(pair_censor = min(follow_up), .groups = "drop")
matched <- matched |>
  left_join(pairCensor, by = c("comparison_id", "cluster_id")) |>
  mutate(
    pair_censor = coalesce(pair_censor, 9999),
    reason.pp = if_else(pair_censor < follow_up.pp, "pair censoring", reason.pp),
    follow_up.pp = if_else(reason.pp == "pair censoring", pair_censor, follow_up.pp),
    reason.pp_2y = if_else(pair_censor < follow_up.pp_2y, "pair censoring", reason.pp_2y),
    follow_up.pp_2y = if_else(reason.pp_2y == "pair censoring", pair_censor, follow_up.pp_2y),
  ) |>
  select(!"pair_censor")
  
logMessage("Summarise follow-up")
cols <- c("reason.pp_2y", "reason.pp", "reason.pp_np", "reason.itt", "follow_up.pp_2y", "follow_up.pp", "follow_up.pp_np", "follow_up.itt")
results[["follow_up"]] <- matched |>
  inner_join(comparisons, by = "comparison_id") |>
  select("reference", "comparator", "cohort_name", all_of(cols)) |>
  pivot_longer(
    cols = all_of(cols),
    names_to  = c(".value", "analysis"),
    names_sep = "\\."
  ) |>
  summariseResult(
    includeOverallGroup = FALSE,
    group = list(c("reference", "comparator", "cohort_name")),
    includeOverallStrata = FALSE,
    strata = "analysis",
    variables = list("follow_up", "reason"),
    estimates = list(c("min", "q25","median", "q75", "max", "mean", "sd"), c("count", "percentage"))
  ) |>
  suppressMessages() |>
  tidy() |>
  mutate(
    cdm_name = cdmName(cdm),
    result_type = "follow_up",
    package_name = pkg_name,
    package_version = pkg_version
  ) |>
  transformToSummarisedResult(
    group = c("reference", "comparator", "cohort_name"),
    strata = c("analysis"),
    estimates = c("min", "q25","median", "q75", "max", "mean", "sd", "count", "percentage"),
    settings = c("result_type", "package_name", "package_version")
  )
