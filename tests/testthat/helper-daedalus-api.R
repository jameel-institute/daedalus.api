daedalus_api_endpoint <- function(..., validate = TRUE) {
  porcelain::porcelain_package_endpoint(
    "daedalus.api", ...,
    validate = validate
  )
}

check_for_redis <- function() {
  available <- redux::redis_available()
  expect_true(available)
}

start_test_queue_with_worker <- function() {
  tempDir <- tempdir()
  Sys.setenv(DAEDALUS_QUEUE_ID = "daedalus.api.tests.queue", REDIS_CONTAINER_NAME = "localhost")
  Sys.setenv(DAEDALUS_LOGS_DIR = tempDir, DAEDALUS_RESULTS_DIR = tempDir)
  queue <- Queue$new(configure_queue = FALSE)
  rrq::rrq_worker_spawn(1, controller = queue$controller)
  queue
}

wait_for_task_complete <- function(run_id, controller, n_tries) {
  rrq::rrq_task_wait(
    run_id, controller = controller, timeout = n_tries
  )
}