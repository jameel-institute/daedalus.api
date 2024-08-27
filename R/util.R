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
  jsonlite::fromJSON(system_file("json", filename))
}

# (Probably) temporary method to return verbatim sample json
# - return nulls as nulls and scalars as scalars!
json_verbatim <- function(json) {
  jsonlite::toJSON(json, auto_unbox = TRUE, null = "null")
}
