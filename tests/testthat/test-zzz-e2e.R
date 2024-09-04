skip_if_no_redis()
tempDir <- tempdir()
# Env vars required by the queue
withr::local_envvar(
  .new = list(
    DAEDALUS_QUEUE_ID = "daedalus.api.tests.queue",
    REDIS_CONTAINER_NAME = "localhost",
    DAEDALUS_LOGS_DIR = tempDir,
    DAEDALUS_RESULTS_DIR = tempDir
  )
)
queue <- start_test_queue_with_worker()
bg <- porcelain::porcelain_background$new(
  api,
  # Force error response if data does not validate against schema
  list(validate = TRUE)
)
bg$start()
on.exit(rrq::rrq_worker_stop(controller = queue$controller))
on.exit(bg$stop(), add = TRUE)

test_that("can run server", {
  r <- bg$request("GET", "/")
  expect_identical(httr::status_code(r), 200L)

  dat <- httr::content(r)
  expect_identical(dat$status, "success")
  expect_null(dat$errors)
  expect_identical(dat$data$daedalus, package_version_string("daedalus"))
  expect_identical(
    dat$data$daedalus.api, package_version_string("daedalus.api")
  )
})

test_that("can run model, get status and results", {
  # 1. Run model
  data <- list(
    modelVersion = "0.0.1",
    parameters = list(
      param1 = "param1"
    )
  )
  body <- json_verbatim(data)
  run_response <- bg$request(
    "POST", "/scenario/run",
    body = body,
    encode = "raw",
    httr::content_type("application/json")
  )
  body <- httr::content(run_response)
  expect_identical(httr::status_code(run_response), 200L)

  run_id <- body$data$runId
  expect_identical(nchar(run_id), 32L)

  # 2. Wait for run to complete successfully
  is_task_successful <- wait_for_task_complete(run_id, queue$controller, 10L)
  expect_true(is_task_successful)

  # 3. Test can get expected statu response
  status_url <- paste0("/scenario/status/", run_id) # nolint
  status_response <- bg$request("GET", status_url)
  status_body <- httr::content(status_response)
  expect_identical(httr::status_code(status_response), 200L)

  expect_identical(status_body$data$runStatus, "complete")
  expect_true(status_body$data$runSuccess)
  expect_true(status_body$data$done)
  expect_identical(status_body$data$runId, run_id)

  # 4. Test can get results
  results_url <- paste0("/scenario/results/", run_id) # nolint
  results_response <- bg$request("GET", results_url)
  expect_identical(httr::status_code(results_response), 200L)
  results_body <- httr::content(results_response)
  results_data <- results_body$data
  expect_gt(length(results_data$costs), 0L)
  expect_gt(length(results_data$capacities), 0L)
  expect_gt(length(results_data$interventions), 0L)
  expect_gt(length(results_data$time_series), 0L)
})
