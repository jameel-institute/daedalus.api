scalar <- function(x) {
  jsonlite::unbox(x)
}

package_version_string <- function(name) {
  as.character(utils::packageVersion(name))
}

# Overwrite base function with more informative error
system_file <- function(...) {
  tryCatch(
    {
      system.file(..., mustWork = TRUE, package = "daedalus.api")
    },
    error = function(e) {
      stop(
        sprintf(
          "Failed to locate file from args\n%s",
          paste(list(...), collapse = " ")
        ),
        call. = FALSE
      )
    }
  )
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
  vax_data <- daedalus.data::vaccination_scenario_data[[vaccine_option]]
  glue::glue(
    "An investment level corresponding to: vaccine rollout commencing {start} \\
    days after the outbreak starts, a vaccine administration rate of {rate}% \\
    of population per day, and an upper limit of vaccine coverage of \\
    {coverage}% of the general population",
    start = vax_data$start_time,
    rate = signif(vax_data$rate, 2),
    coverage = vax_data$uptake_limit
  )
}

get_pathogen_description <- function(pathogen_id) {
  # get pathogen information from the package and generate
  # description including R0 and IFR range (across all countries)
  # for the pathogen

  infection <- daedalus::daedalus_infection(pathogen_id)
  country_ifrs <- vapply(
    daedalus.data::country_data,
    function(country) {
      stats::weighted.mean(infection$ifr, country$demography)
    },
    1.0
  )
  ifr_range <- range(country_ifrs)
  glue::glue(
    "A disease with R0 of {r0} and infection fatality ratio \\
    between {ifr_min_pc}% and {ifr_max_pc}% depending on country",
    ifr_min_pc = signif(ifr_range[[1]] * 100, 2),
    ifr_max_pc = signif(ifr_range[[2]] * 100, 2),
    r0 = signif(infection$r0, 2)
  )
}

get_behaviour_description <- function(behaviour_option) {
  # get vaccination data from the package for a given global vaccine
  # investment scenario, and generate description (help text) from that
  if (behaviour_option == "none") {
    "No population-level behavioural change is modelled."
  } else {
    optimism <- switch(behaviour_option, low = 0.25, medium = 0.5, high = 0.75)
    glue::glue(
      "Modelling population behaviour with a baseline \\
      level of optimism about the outbreak of {optimism}, along a scale of \\
      [0.0, 1.0]. Higher levels of optimism result in fewer individuals \\
      adopting protective behaviour."
    )
  }
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
    stop(
      "The parameters provided do not match required parameters: ",
      toString(required),
      call. = FALSE
    )
  }
}

#' Prepare Daedalus costs output for display
#'
#' @description
#' `get_nested_costs()` and `get_nested_natural_costs()` prepare daedalus costs
#' outputs for display. `get_natural_costs()` currently only prepares life-years
#' lost but may include other costs in their natural units in future.
#'
#' @name costs_to_display
#' @rdname costs_to_display
#'
#' @param raw_costs A list resulting from a call to `daedalus::get_costs()` on
#' a `<daedalus_output>` class object.
#'
#' @return A recursive nested list with the elements `"id"` and `"value"`
#' with a string and numeric value respectively.
#' Additionally, a `"children"` list element may be present containing another
#' list with the same recursive structure.
#' The lowest level list within `"children"` has no `"children"` element.
#'
#' @keywords internal
get_nested_costs <- function(raw_costs) {
  # Reshape raw costs from a call to `daedalus::get_costs()` into a nested
  # structure for display in the web app
  total <- raw_costs$total_cost

  gdp <- raw_costs$economic_costs$economic_cost_total
  gdp_closures <- raw_costs$economic_costs$economic_cost_closures
  gdp_absences <- raw_costs$economic_costs$economic_cost_absences

  education <- raw_costs$education_costs$education_cost_total
  education_closures <- raw_costs$education_costs$education_cost_closures
  education_absences <- raw_costs$education_costs$education_cost_absences

  # NOTE: daedalus returns life years and values separately;
  # accessing value here but retaining 'years' as var name
  life_value_total <- raw_costs$life_value_lost$life_value_lost_total
  life_value_age <- raw_costs$life_value_lost$life_value_lost_age

  life_years_data <- get_life_years_lost(raw_costs)
  life_years_total <- life_years_data$life_years_lost_total
  life_years_age <- life_years_data$life_years_age

  # NOTE: the direct output of this function does not validate against
  # inst/scenarioCosts.json; this fn returns a list rather than a
  # cost_item() output to leave open the option of multiple top-level costs
  list(
    cost_item(
      "total",
      list(list(metric = "usd_millions", value = total)),
      list(
        cost_item(
          "gdp",
          list(list(metric = "usd_millions", value = gdp)),
          list(
            cost_item(
              "gdp_closures",
              list(list(metric = "usd_millions", value = gdp_closures))
            ),
            cost_item(
              "gdp_absences",
              list(list(metric = "usd_millions", value = gdp_absences))
            )
          )
        ),
        cost_item(
          "education",
          list(list(metric = "usd_millions", value = education)),
          list(
            cost_item(
              "education_closures",
              list(list(metric = "usd_millions", value = education_closures))
            ),
            cost_item(
              "education_absences",
              list(list(metric = "usd_millions", value = education_absences))
            )
          )
        ),
        cost_item(
          "life_years",
          list(
            list(metric = "usd_millions", value = life_value_total),
            list(metric = "life_years", value = life_years_total)
          ),
          list(
            cost_item(
              "life_years_pre_school",
              list(
                list(metric = "usd_millions", value = life_value_age[["0-4"]]),
                list(metric = "life_years", value = life_years_age[["0-4"]])
              )
            ),
            cost_item(
              "life_years_school_age",
              list(
                list(metric = "usd_millions", value = life_value_age[["5-19"]]),
                list(metric = "life_years", value = life_years_age[["5-19"]])
              )
            ),
            cost_item(
              "life_years_working_age",
              list(
                list(
                  metric = "usd_millions",
                  value = life_value_age[["20-64"]]
                ),
                list(metric = "life_years", value = life_years_age[["20-64"]])
              )
            ),
            cost_item(
              "life_years_retirement_age",
              list(
                list(metric = "usd_millions", value = life_value_age[["65+"]]),
                list(metric = "life_years", value = life_years_age[["65+"]])
              )
            )
          )
        )
      )
    )
  )
}

#' @name costs_to_display
#'
#' @keywords internal
get_life_years_lost <- function(raw_costs) {
  life_years_age <- raw_costs$life_years_lost$life_years_lost_age
  total <- sum(life_years_age)

  list(
    life_years_lost_total = total,
    life_years_age = life_years_age
  )
}

#' Get annual GDP from DAEDALUS country data
#'
#' @description Convert daily GVA values to annual GDP values.
#'
#' @param country A string giving a country name
#' from among `daedalus.data::country_names` or
#' an ISO2 code from among `daedalus.data::country_codes_iso2c` or an ISO3 code
#' from among `daedalus.data::country_codes_iso3c`.
#'
#' @return A single number value for the annual GDP of a country in terms of
#' million dollars. Values are in 2018 terms.
#' @keywords internal
get_annual_gdp <- function(country) {
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
#' @param country A string giving a country name
#' from among `daedalus.data::country_names` or
#' an ISO2 code from among `daedalus.data::country_codes_iso2c` or an ISO3 code
#' from among `daedalus.data::country_codes_iso3c`.
#'
#' @return A numeric value representing the average
#' VSL for the specified country.
#'
#' @examples
#' \dontrun{
#' avg_vsl <- get_average_vsl("USA")
#' print(avg_vsl)
#' }
#'
#' @keywords internal
get_average_vsl <- function(country) {
  country_data <- daedalus::daedalus_country(country)
  age_vsl <- daedalus::get_data(country_data, "vsl")
  demography <- daedalus::get_data(country_data, "demography")

  stats::weighted.mean(age_vsl, demography)
}

#' Get age-specific VSL
#'
#' @keywords internal
get_age_vsl <- function(country) {
  country_data <- daedalus::daedalus_country(country)

  daedalus::get_data(country_data, "vsl")
}

#' @name costs_to_display
#'
#' @description
#' `cost_item()` is a helper function that prepares list elements in the format
#' `"id"`, `"value"`, `"children"`.
#'
#' @param id String description of list name.
#'
#' @param values List contents.
#'
#' @param children Nested lists contained within the top-level list, if any.
#' Defaults to `NULL`.
#'
#' @keywords internal
cost_item <- function(id, values, children = NULL) {
  item <- list(id = id, values = values)
  if (!is.null(children)) {
    item$children <- children
  }
  item
}
