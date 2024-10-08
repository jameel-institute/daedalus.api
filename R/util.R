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
        cost_item("life_years_infants", life_years_age[["0-4"]]),
        cost_item("life_years_adolescents", life_years_age[["5-19"]]),
        cost_item("life_years_working_age", life_years_age[["20-65"]]),
        cost_item("life_years_retirement_age", life_years_age[["65+"]])
      ))
    ))
  )
}
