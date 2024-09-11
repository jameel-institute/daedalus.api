model_run <- function(parameters, model_version) {
  # Returning sample response for now
  to_json(read_local_json("sample_scenario_results_response.json"), auto_unbox = TRUE)
}
