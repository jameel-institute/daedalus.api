model_run <- function(parameters, model_version) {
  # Returning sample response for now
  from_file <- read_local_json("sample_scenario_results_response.json")
  jsonlite::toJSON(from_file, auto_unbox = TRUE)
}
