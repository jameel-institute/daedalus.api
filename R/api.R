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

# TODO: specify schema and rerun roxygen2::roxygenize()!!
##' @porcelain GET /metadata => json(metadata)
metadata <- function() {
  # TODO: Use relevant model version - from qs if specified, else from latest available metadata file (jidea-62)
  model_version <- scalar("0.1.0")
  # TODO: read in correct metadata version according to model_version (jidea-62)
  metadata_file <- sprintf("metadata_%s.json", model_version)
  response <- read_json(metadata_file)
  response$modelVersion <- model_version
  
  # Helper for the options which don't come from the json
  get_option <- function(id, label) {
    list(id = scalar(id), label = scalar(label))
  }
  
  # Set available countries from daedalus package
  # TODO: use the right version of daedalus/model
  # TODO: get ISO ids from daedalus when available
  country_options <- lapply(daedalus::country_names, function(country) {
    country_string <- as.character(country)
    get_option(country_string, country_string)
  })
  country_idx <- match("country", response$parameters$id)
  response$parameters$options[[country_idx]] <- country_options
  
  # TODO: get pathogen information from daedalus, when available (JIDEA-61)
  pathogen_options <- list(
    get_option("sars-cov-1", "SARS-CoV-1"),
    get_option("sars-cov-2-pre-alpha", "SARS-CoV-2 pre-alpha (wildtype)"),
    get_option("sars-cov-2-omicron", "SARS-CoV-2 omicron"),
    get_option("sars-cov-2-delta", "SARS-CoV-2 delta"),
    get_option("influenza-2009", "Influenza 2009 (Swine flu)"),
    get_option("influenza-1957", "Influenza 1957"),
    get_option("influenza-1918", "Influenza 1918 (Spanish flu)")
  )
  pathogen_idx <- match("pathogen", response$parameters$id)
  response$parameters$options[[pathogen_idx]] <- pathogen_options
  
  response
}
