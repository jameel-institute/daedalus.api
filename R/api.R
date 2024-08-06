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
##' @porcelain GET /metadata => json
metadata <- function() {
  # TODO: Use relevant model version
  model_version <- "0.1.0"
  metadata_file <- sprintf("metadata_%s.json", model_version)
  response <- read_json(metadata_file)
  
  response$modelVersion <- model_version
  response
}
