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
  api <- api(dat$validate, dat$log_level)
  api$run("0.0.0.0", port = dat$port)
}


main_worker <- function() {
  worker <- rrq::rrq_worker$new(
    get_queue_id(),
    con = get_redis_connection()
  )
  worker$loop()
}

main_configure_queue <- function() {
  queue_id <- get_queue_id()
  rrq::rrq_configure(queue_id,
                     store_max_size = 1000L,
                     offload_path = get_results_dir(),
                     con = get_redis_connection())
}
