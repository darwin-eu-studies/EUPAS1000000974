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


for (cohort_name in c("study_cohorts_obj1", "study_cohorts_obj2", "study_cohorts_obj3")) {
  
  logMessage(message = paste("Summarise attrition for: ", cohort_name))
  
  results[[paste0("attrition_", cohort_name)]] <- summariseCohortAttrition(cdm[[cohort_name]])
  
  logMessage(message = paste0("Add stratifications variables for: ", cohort_name))
  
  cdm[[cohort_name]] <- addStrataColumns(cdm[[cohort_name]])
  
  logMessage(message = paste0("Summarise characterisation for: ", cohort_name))
  
  results[[paste0("characterisation_", cohort_name)]] <- getCharacterisation(cdm[[cohort_name]])
  
}
