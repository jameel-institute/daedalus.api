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
  queue <- Queue$new() # nolint
  rrq::rrq_worker_spawn(1L, controller = queue$controller)
  queue
}

wait_for_task_complete <- function(run_id, controller, n_tries) {
  rrq::rrq_task_wait(
    run_id, controller = controller, timeout = n_tries
  )
}
