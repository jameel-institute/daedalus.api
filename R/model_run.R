model_run <- function(parameters, model_version) {
  # TODO: use real parameters
  results <- daedalus::daedalus("Canada", "influenza_1918")
  d <- dplyr::group_by(results$model_data, time, compartment) |> dplyr::summarise(value = sum(value)) |> tidyr::pivot_wider(id_cols = "time", values_from = "value", names_from = "compartment")
  d <- d[,c("hospitalised", "dead")]
  names(d)[[2]] <- "deaths"
  # read all sample data, and replace time series with daedalus results
  #from_file <- read_local_json("sample_scenario_results_response.json")
  #from_file$time_series <- d
  #from_file
  list(time_series = d)
}
