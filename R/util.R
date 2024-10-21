scalar <- function(x) {
  jsonlite::unbox(x)
}

package_version_string <- function(name) {
  as.character(utils::packageVersion(name))
}

# Overwrite base function with more informative error
system_file <- function(...) {
  tryCatch({
    system.file(..., mustWork = TRUE, package = "daedalus.api")
  }, error = function(e) {
    stop(sprintf("Failed to locate file from args\n%s",
                 paste(list(...), collapse = " ")))
  })
}

read_local_json <- function(filename) {
  jsonlite::fromJSON(system_file("json", filename), simplifyVector = FALSE)
}

to_json <- function(data, auto_unbox = FALSE, ...) {
  jsonlite::toJSON(data, auto_unbox = auto_unbox, null = "null", ...)
}

get_hospital_capacity_range <- function(default_capacity, step) {
  # Given a default hospital capacity from daedalus return a range from
  # which the user can choose, with min, default and max, all rounded to
  # the step value.
  # Range values are:
  # min: 90% of default
  # max: 130% of default
  round_value <- function(value, step) {
    round(value / step) * step
  }
  list(
    min = round_value(default_capacity * 0.9, step),
    default = round_value(default_capacity, step),
    max = round_value(default_capacity * 1.3, step)
  )
}

get_vaccine_option_description <- function(vaccine_option) {
  # get vaccination data from the package for a given global vaccine
  # investment scenario, and generate description (help text) from that
  vax_data <- daedalus::vaccination_scenario_data[[vaccine_option]]
  stringr::str_glue(
    "An investment level corresponding to: ",
    "vaccine rollout commencing {start} days after the outbreak starts, ",
    "a vaccine administration rate of {rate}% of population per day, ",
    "and an upper limit of vaccine coverage of {coverage}% of the ",
    "general population",
    start = vax_data$vax_start_time,
    rate = signif(vax_data$nu, 2),
    coverage = vax_data$vax_uptake_limit
  )
}

validate_parameters <- function(parameters, metadata) {
  # Expect parameters in model run request to include all and only values
  # specified in metadata
  parameter_names <- names(parameters)
  required <- lapply(metadata$parameters, function(param) {
    param$id
  })
  pass <- identical(sort(parameter_names), sort(unlist(required)))
  if (!pass) {
    stop("The parameters provided do not match required parameters: ",
         toString(required))
  }
}

get_nested_costs <- function(raw_costs) {
  # Reshape raw costs from the package into a nested structure
  # or display in the web app
  total <- raw_costs$total_cost

  gdp <- raw_costs$economic_costs$economic_cost_total
  gdp_closures <- raw_costs$economic_costs$economic_cost_closures
  gdp_absences <- raw_costs$economic_costs$economic_cost_absences

  education <- raw_costs$education_costs$education_cost_total
  education_closures <- raw_costs$education_costs$education_cost_closures
  education_absences <- raw_costs$education_costs$education_cost_absences

  life_years <- raw_costs$life_years_lost$life_years_lost_total
  life_years_age <- raw_costs$life_years_lost$life_years_lost_age

  cost_item <- function(id, value, children = NULL) {
    item <- list(id = id, value = value)
    if (!is.null(children)) {
      item$children <- children
    }
    item
  }

  list(
    cost_item("total", total, list(
      cost_item("gdp", gdp, list(
        cost_item("gdp_closures", gdp_closures),
        cost_item("gdp_absences", gdp_absences)
      )),
      cost_item("education", education, list(
        cost_item("education_closures", education_closures),
        cost_item("education_absences", education_absences)
      )),
      cost_item("life_years", life_years, list(
        cost_item("life_years_pre_school", life_years_age[["0-4"]]),
        cost_item("life_years_school_age", life_years_age[["5-19"]]),
        cost_item("life_years_working_age", life_years_age[["20-65"]]),
        cost_item("life_years_retirement_age", life_years_age[["65+"]])
      ))
    ))
  )
}

#' Validate Country
#'
#' This function checks if the provided country name is
#' valid by ensuring it is a character string
#' and exists within the `daedalus::country_names` list.
#'
#' @param country A character string representing
#'  the name of the country to validate.
#'
#' @return This function does not return a value.
#'  It will stop execution if the validation fails.
#'
#' @examples
#' \dontrun{
#' validate_country("United States")
#' validate_country("InvalidCountry") # This will cause an error
#' }
#' @keywords internal
validate_country <- function(country) {
  stopifnot(
    "`country` must be a country name from `daedalus::country_names" =
      is.character(country) && country %in% daedalus::country_names
  )
}

#' Get annual GDP from DAEDALUS country data
#'
#' @description Convert daily GVA values to annual GDP values.
#'
#' @param country A string giving a country name
#' from among `daedalus::country_names`.
#'
#' @return A single number value for the annual GDP of a country in terms of
#' million dollars. Values are in 2018 terms.
#' @keywords internal
get_annual_gdp <- function(country) {
  validate_country(country)

  num_days_year <- 365

  country_data <- daedalus::daedalus_country(country)
  gva <- daedalus::get_data(country_data, "gva")

  sum(gva * num_days_year)
}

#' Get Average Value of Statistical Life (VSL) for a Country
#'
#' @description This function calculates the average
#' Value of Statistical Life (VSL) for a specified country.
#' It computes the weighted mean of VSL using the demography data as weights.
#'
#' @param country A character string representing the name of the country.
#'
#' @return A numeric value representing the average
#' VSL for the specified country.
#'
#' @examples
#' \dontrun{
#'   avg_vsl <- get_average_vsl("USA")
#'   print(avg_vsl)
#' }
#'
#' @keywords internal
get_average_vsl <- function(country) {
  validate_country(country)

   country_data <- daedalus::daedalus_country(country)
   vsl <- daedalus::get_data(country_data, "vsl")
   demography <- daedalus::get_data(country_data, "demography")

   weighted.mean(vsl, demography)
}
