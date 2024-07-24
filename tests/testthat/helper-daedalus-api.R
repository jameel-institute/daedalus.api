daedalus_api_endpoint <- function(..., validate = TRUE) {
  porcelain::porcelain_package_endpoint(
    "daedalus.api", ...,
    validate = validate
  )
}
