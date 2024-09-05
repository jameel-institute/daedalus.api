##' Create an daedalus.api server, a porcelain object
##'
##' @title Create daedalus.api
##'
##' @param validate Logical, indicating if validation should be done
##'   on responses.  This should be `FALSE` in production
##'   environments.  See [porcelain::porcelain] for details
##'
##' @param log_level Logging level to use. Sensible options are "off",
##'   "info" and "all".
##'
##' @return A [porcelain::porcelain] object. Notably this does *not*
##'   start the server
##'
##' @export
api <- function(validate = NULL, log_level = "info") {
  logger <- make_logger(log_level)
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$include_package_endpoints()
  api
}

##' @porcelain GET / => json(root)
root <- function() {
  versions <- c(
    list(
      daedalus = utils::packageVersion("daedalus"),
      daedalus.api = utils::packageVersion("daedalus.api")
    )
  )
  lapply(versions, function(v) scalar(as.character(v)))
}

##' @porcelain GET /metadata => json(metadata)
metadata <- function() {
  # JIDEA-62: we will use relevant model version - from qs if specified, else
  # from latest available metadata file. For now get model version from package.
  model_version <- scalar(as.character(packageVersion("daedalus")))
  # JIDEA-62: we will read in correct metadata version according to requested
  # model_version
  metadata_version <- "0.1.0"
  metadata_file <- sprintf("metadata_%s.json", metadata_version)
  response <- read_local_json(metadata_file)

  response$modelVersion <- model_version
  # Helper for the options which don't come from the json
  get_option <- function(id, label) {
    list(id = scalar(id), label = scalar(label))
  }

  param_ids <- lapply(response$parameters, function(param){
    param$id
  })

  # Set available countries from daedalus package
  # JIDEA-62: use the right version of daedalus/model
  # we will get ISO ids from daedalus when available
  country_names <- daedalus::country_names

  country_options <- lapply(country_names, function(country) {
    country_string <- as.character(country)
    get_option(country_string, country_string)
  })
  country_idx <- match("country", param_ids)
  response$parameters[[country_idx]]$options <- country_options

  # JIDEA-61: get pathogen information from daedalus, when available
  pathogen_options <- list(
    get_option("sars-cov-1", "SARS-CoV-1"),
    get_option("sars-cov-2-pre-alpha", "SARS-CoV-2 pre-alpha (wildtype)"),
    get_option("sars-cov-2-omicron", "SARS-CoV-2 omicron"),
    get_option("sars-cov-2-delta", "SARS-CoV-2 delta"),
    get_option("influenza-2009", "Influenza 2009 (Swine flu)"),
    get_option("influenza-1957", "Influenza 1957"),
    get_option("influenza-1918", "Influenza 1918 (Spanish flu)")
  )
  pathogen_idx <- match("pathogen", param_ids)
  response$parameters[[pathogen_idx]]$options <- pathogen_options

  # setNames to get json object not array
  hospital_capacity_idx <- match("hospital_capacity", param_ids)
  step <- response$parameters[[hospital_capacity_idx]]$step
  hospital_capacities <- lapply(setNames(country_names, country_names), function(country) {
    demography <- daedalus::country_data[[country]]$demography
    population <- sum(unlist(demography))
    get_hospital_capacity_for_population(population, step)
  })
  response$parameters[[hospital_capacity_idx]]$updateNumericFrom <- list(
    parameterId = "country",
    values = hospital_capacities
  )

  json_verbatim(response)
}

#' @porcelain
#'   POST /scenario/run => json(scenarioRunResponse)
#'   body data :: json(scenarioRunRequest)
scenario_run <- function(data) {
  # Returning sample response for now
  json_verbatim(read_local_json("sample_scenario_run_response.json"))
}

#' @porcelain
#'   GET /scenario/status => json(scenarioStatus)
#'   query run_id :: string
scenario_status <-  function(run_id) {
  # Returning sample response for now
  json_verbatim(read_local_json("sample_scenario_status_response.json"))
}

#' @porcelain
#'   GET /scenario/results => json(scenarioResults)
#'   query run_id :: string
scenario_results <- function(run_id) {
  # Returning sample response for now
  json_verbatim(read_local_json("sample_scenario_results_response.json"))
}
