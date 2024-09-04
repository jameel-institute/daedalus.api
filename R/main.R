parse_main <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
daedalus.api [options]

Options:
  --log-level=LEVEL  Log-level (off, info, all) [default: info]
  --validate         Enable json schema validation
  --port=PORT        Port to run api on [default: 8001]"
  dat <- docopt::docopt(usage, args)
  list(
    log_level = dat$log_level,
    validate = dat$validate,
    port = as.integer(dat$port)
  )
}


main <- function(args = commandArgs(TRUE)) {
  dat <- parse_main(args)
  api <- api(dat$validate, dat$log_level, configure_queue = TRUE)
  api$run("0.0.0.0", port = dat$port)
}


main_worker <- function() {
  queue <- Queue$new(configure_queue = FALSE)

  worker <- rrq::rrq_worker$new(
    queue$controller$queue_id,
    con = queue$controller$con
  )
  worker$loop()
}
