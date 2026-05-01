model_run <- function(parameters, model_version) {
  country <- parameters$country
  pathogen <- parameters$pathogen
  response <- parameters$response
  vaccine <- parameters$vaccine
  hospital_capacity <- parameters$hospital_capacity
  hospital_capacity_num <- as.numeric(hospital_capacity)

  # `behaviour` is passed from the UI as a string that is converted to a
  # `daedalus_new_behaviour()` with some specific parameters in `R/behaviour.R`
  behav_choice_string <- parameters$behaviour

  country_obj <- daedalus::daedalus_country(country)

  # get default hosp cap
  hosp_cap_default <- daedalus::get_data(country_obj, "hospital_capacity")
  behaviour <- daedalus.api::process_behaviour_choice(
    behav_choice_string,
    hosp_cap_default
  )

  stopifnot(
    "Hospital capacity must be > 0 but it is not!" = hospital_capacity_num > 0.0
  )
  # manually assign hospital capacity to `country`
  country_obj$hospital_capacity <- hospital_capacity_num

  # SG launch: set infection to have no immunity waning
  pathogen_obj <- daedalus::daedalus_infection(
    pathogen,
    rho = 0.0
  )

  # SG launch: create a timed NPI with openness coefs given by `response`
  start_time <- 50
  end_time <- 200
  openness <- daedalus.data::closure_strategy_data[[response]]
  response_obj <- daedalus::daedalus_timed_npi(
    start_time,
    end_time,
    list(openness),
    country_obj
  )

  # SG launch: set vax immunity waning to near zero
  waning_period_infinite <- 1e8
  vaccine_obj <- daedalus::daedalus_vaccination(
    vaccine,
    country_obj,
    waning_period = waning_period_infinite
  )

  model_results <- daedalus::daedalus(
    country_obj,
    pathogen_obj,
    response_strategy = response_obj,
    vaccine_investment = vaccine_obj,
    behaviour = behaviour
  )

  model_data <- daedalus::get_data(model_results)

  time_series <- dplyr::summarise(
    model_data,
    value = sum(value),
    .by = c("time", "compartment")
  )
  time_series <- tidyr::pivot_wider(
    time_series,
    id_cols = "time",
    values_from = "value",
    names_from = "compartment"
  )

  # manually sum hospitalised prevalence to simplify output
  # separate hospitalisation cols are dropped later during column subsetting
  time_series <- dplyr::mutate(
    time_series,
    hospitalised = .data$hospitalised_recov + .data$hospitalised_death,
    prevalence = .data$infect_asymp + .data$infect_symp + .data$hospitalised
  )

  time_series <- time_series[, c("prevalence", "hospitalised", "dead")]

  # get total vaccinations time series
  vax_time_series <- dplyr::filter(
    model_data,
    .data$vaccine_group == "vaccinated"
  )
  vax_time_series <- dplyr::summarise(
    vax_time_series,
    vaccinated = sum(value),
    .by = "time"
  )
  time_series$vaccinated <- vax_time_series$vaccinated

  # get incidence time series
  incidences <- daedalus::get_incidence(model_results)
  incidences <- tidyr::pivot_wider(
    incidences,
    id_cols = "time",
    names_from = "measure"
  )
  time_series$new_infected <- incidences$daily_infections
  time_series$new_hospitalised <- incidences$daily_hospitalisations
  time_series$new_dead <- incidences$daily_deaths

  time_series$new_vaccinated <-
    daedalus::get_new_vaccinations(model_results)$new_vaccinations

  raw_costs <- daedalus::get_costs(model_results)
  costs <- get_nested_costs(raw_costs)

  interventions <- list()
  if (response != "none") {
    npi_info <- model_results$response_data$npi_info
    n_closures <- length(npi_info$npi_durations)
    closure <- Map(
      npi_info$npi_times_start,
      npi_info$npi_times_end,
      f = function(x, y) {
        list(
          id = "response",
          start = x,
          end = y
        )
      }
    )
    interventions <- closure
  }

  # get country information
  gdp <- get_annual_gdp(country)
  age_vsl <- get_age_vsl(country)
  average_vsl <- get_average_vsl(country)

  results <- list()
  results$parameters <- list(
    country = country,
    pathogen = pathogen,
    response = response,
    vaccine = vaccine,
    hospital_capacity = hospital_capacity,
    behaviour = behav_choice_string
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
  results$gdp <- gdp
  results$vsl <- list(
    average = average_vsl,
    pre_school = age_vsl[1],
    school_age = age_vsl[2],
    working_age = age_vsl[3],
    retirement_age = age_vsl[4]
  )

  results
}
