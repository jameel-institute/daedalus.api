model_run <- function(parameters, model_version) {
  country <- parameters$country
  pathogen <- parameters$pathogen
  response <- parameters$response
  # TODO: include vaccine in params to daedalus
  vaccine <- parameters$vaccine
  # TODO: include hospital capacity in params to daedalus
  hospital_capacity <- parameters$hospital_capacity

  results <- daedalus::daedalus(
    country,
    pathogen,
    response_strategy = response)
  time_series <- dplyr::group_by(results$model_data, time, compartment)
  time_series <- dplyr::summarise(time_series, value = sum(value))
  time_series <- tidyr::pivot_wider(
    time_series,
    id_cols = "time", values_from = "value", names_from = "compartment"
  )
  time_series$infect <- time_series$infect_asymp + time_series$infect_symp
  time_series <- time_series[, c("infect",
                                "hospitalised",
                                "dead")]
  # read sample data, replace time series and parameters with real values
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
