daedalus_api_endpoint <- function(..., validate = TRUE) {
  porcelain::porcelain_package_endpoint(
    "daedalus.api", ...,
    validate = validate
  )
}

skip_if_no_redis <- function() {
  # Allows skipping redis/rrq e2e tests on mac os
  available <- redux::redis_available()
  if (!available) {
    testthat::skip("Skipping test as redis is not available")
  }
}

start_test_queue_with_worker <- function() {
  queue <- Queue$new(configure_queue = FALSE) # nolint
  rrq::rrq_worker_spawn(1L, controller = queue$controller)
  queue
}

wait_for_task_complete <- function(run_id, controller, n_tries) {
  rrq::rrq_task_wait(
    run_id, controller = controller, timeout = n_tries
  )
}
