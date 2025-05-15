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
  queue <- Queue$new()
  api$include_package_endpoints(state = list(queue = queue))
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

read_metadata_file <- function(metadata_version = "0.1.0") {
  metadata_file <- sprintf("metadata_%s.json", metadata_version)
  read_local_json(metadata_file)
}

##' @porcelain GET /metadata => json(metadata)
metadata <- function() {
  # JIDEA-62: we will use relevant model version - from qs if specified, else
  # from latest available metadata file. For now get model version from package.
  model_version <- scalar(as.character(packageVersion("daedalus")))
  # JIDEA-62: we will read in correct metadata version according to requested
  # model_version
  response <- read_metadata_file()

  response$modelVersion <- model_version
  # Helper for the options which don't come from the json
  get_option <- function(id, label) {
    list(id = scalar(id), label = scalar(label))
  }

  param_ids <- lapply(response$parameters, function(param) {
    param$id
  })

  # Set available countries from daedalus package
  # JIDEA-62: use the right version of daedalus/model
  country_names <- daedalus.data::country_names
  country_codes <- daedalus.data::country_codes_iso3c

  country_options <- lapply(seq_along(country_names), function(idx) {
    get_option(country_codes[[idx]], country_names[[idx]])
  })
  country_idx <- match("country", param_ids)
  response$parameters[[country_idx]]$options <- country_options

  # JIDEA-61: get pathogen information from daedalus, when available
  pathogen_options <- list(
    get_option("sars_cov_1", "SARS 2004"),
    get_option("sars_cov_2_pre_alpha", "Covid-19 wild-type"),
    get_option("sars_cov_2_omicron", "Covid-19 Omicron"),
    get_option("sars_cov_2_delta", "Covid-19 Delta"),
    get_option("influenza_2009", "Influenza 2009 (Swine flu)"),
    get_option("influenza_1957", "Influenza 1957"),
    get_option("influenza_1918", "Influenza 1918 (Spanish flu)")
  )
  pathogen_options <- lapply(pathogen_options, function(option) {
    option$description <- get_pathogen_description(option$id)
    option
  })
  pathogen_idx <- match("pathogen", param_ids)
  response$parameters[[pathogen_idx]]$options <- pathogen_options

  hospital_capacity_idx <- match("hospital_capacity", param_ids)
  step <- response$parameters[[hospital_capacity_idx]]$step
  # setNames to get json object not array
  hospital_capacities <- lapply(
    setNames(country_names, country_codes),
    function(country_name) {
      default <- daedalus::daedalus_country(country_name)$hospital_capacity

      get_hospital_capacity_range(default, step)
    }
  )

  response$parameters[[hospital_capacity_idx]]$updateNumericFrom <- list(
    parameterId = "country",
    values = hospital_capacities
  )

  vaccine_idx <- match("vaccine", param_ids)
  response$parameters[[vaccine_idx]]$options <- lapply(
    response$parameters[[vaccine_idx]]$options,
    function(vaccine_option) {
      vaccine_option$description <-
        get_vaccine_option_description(vaccine_option$id)
      vaccine_option
    }
  )

  to_json(response, auto_unbox = TRUE)
}

#' @porcelain
#'   POST /scenario/run => json(scenarioRunResponse)
#'   state queue :: queue
#'   body data :: json(scenarioRunRequest)
scenario_run <- function(queue, data) {
  data <- jsonlite::parse_json(data)
  parameters <- data$parameters
  validate_parameters(parameters, read_metadata_file())
  run_id <- queue$queue_model_run(
    parameters,
    model_version = data$modelVersion
  )
  list(runId = scalar(run_id))
}

#' @porcelain
#'   GET /scenario/status/<run_id:string> => json(scenarioStatus)
#'   state queue :: queue
scenario_status <- function(queue, run_id) {
  lapply(queue$get_run_status(run_id), scalar)
}

#' @porcelain
#'   GET /scenario/results/<run_id:string> => json(scenarioResults)
#'   state queue :: queue
scenario_results <- function(queue, run_id) {
  results <- queue$get_run_results(run_id)
  results$runId <- run_id
  to_json(results, auto_unbox = TRUE, dataframe = "columns")
}
