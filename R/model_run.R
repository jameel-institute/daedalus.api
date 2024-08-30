model_run <- function(parameters, model_version) {
  print("doing model run")
  # Returning sample response for now
  json_verbatim(read_local_json("sample_scenario_results_response.json"))
}