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

to_json <- function(data, auto_unbox = FALSE) {
  jsonlite::toJSON(data, auto_unbox = auto_unbox, null = "null")
}
