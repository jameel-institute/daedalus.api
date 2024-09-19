model_run <- function(parameters, model_version) {
  country <- parameters$country
  pathogen <- parameters$pathogen
  response <- parameters$response
  vaccine <- parameters$vaccine # TODO: include vaccine in params to daedalus
  hospital_capacity <- parameters$hospital_capacity # TODO: include hospital capacity in params to daedalus

  results <- daedalus::daedalus(country, pathogen, response_strategy = response)
  time_series <- dplyr::group_by(results$model_data, time, compartment) |>
    dplyr::summarise(value = sum(value)) |>
    tidyr::pivot_wider(id_cols = "time", values_from = "value", names_from = "compartment")
  time_series <- time_series[,c("infect_asymp", "infect_symp", "hospitalised", "recovered", "dead")]
  # read all sample data, and replace time series with daedalus results, and parameters with real params
  results <- read_local_json("sample_scenario_results_response.json")
  results$parameters <- list(
    country = country,
    pathogen = pathogen,
    response = response,
    vaccine = vaccine,
    hospital_capacity = hospital_capacity
  )
  results$time_series <- time_series
  results
}
