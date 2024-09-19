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

get_hospital_capacity_for_pop <- function(population, step) {
  # This is very likely to change but for now we set hospital capacity values
  # for a country as:
  # min: 30 per 100K population
  # default: 45 per 100K population
  # max: 130 per 100K population
  value_per_100k_pop_as_absolute <- function(value, population, step) {
    unrounded <- (population / 100000) * value
    round(unrounded / step) * step
  }
  list(
    min = value_per_100k_pop_as_absolute(30, population, step),
    default = value_per_100k_pop_as_absolute(45, population, step),
    max = value_per_100k_pop_as_absolute(130, population, step)
  )
}

aggregate_daedalus_output <- function(daedalus_output) {
  # Aggregate daedalus output for age group and economic sector, and return values
  # as a vector ordered by time value
  aggregate(daedalus_output, list(daedalus_output$time, daedalus_output$compartment), FUN=sum)
}

validate_parameters <- function(parameters, metadata) {
  # Expect parameters in model run request to include all and only values specified in metadata
  parameter_names <- names(parameters)
  required <- lapply(metadata$parameters, function(param) {
    param$id
  })
  pass <- identical(sort(parameter_names), sort(unlist(required)))
  if (!pass) {
    required_string <- paste(required, collapse = ", ")
    stop(paste0("The parameters provided do not match required parameters: ", required_string))
  }
}
