model_run <- function(parameters, model_version) {
  country <- parameters$country
  pathogen <- parameters$pathogen
  response <- parameters$response
  vaccine <- parameters$vaccine
  hospital_capacity <- parameters$hospital_capacity
  hospital_capacity_num <- as.numeric(hospital_capacity)

  model_results <- daedalus::daedalus(
    country,
    pathogen,
    response_strategy = response,
    response_threshold = hospital_capacity_num,
    vaccine_investment = vaccine
  )
  time_series <- dplyr::group_by(model_results$model_data, time, compartment)
  time_series <- dplyr::summarise(time_series, value = sum(value))
  time_series <- tidyr::pivot_wider(
    time_series,
    id_cols = "time", values_from = "value", names_from = "compartment"
  )
  time_series$prevalence <-
    time_series$infect_asymp +
    time_series$infect_symp +
    time_series$hospitalised

  time_series <- time_series[, c("prevalence",
                                "hospitalised",
                                "dead")]

  raw_costs <- daedalus::get_costs(model_results)
  costs <- get_nested_costs(raw_costs)

  interventions <- list()
  if (response != "none") {
    closure_info <- model_results$response_data$closure_info
    level <- model_results$response_data$implementation_level
    closure <- list(
      id = "response",
      level = level,
      start = closure_info$closure_time_start,
      end = closure_info$closure_time_end
    )
    interventions <- list(closure)
  }

  results <- list(
  )
  results$parameters <- list(
    country = country,
    pathogen = pathogen,
    response = response,
    vaccine = vaccine,
    hospital_capacity = hospital_capacity
  )
  results$costs <- costs
  results$time_series <- time_series
  results$interventions <- interventions
  results$capacities <- list(
    list(
      id = "hospital_capacity",
      value = hospital_capacity_num
    )
  )
  results
}
