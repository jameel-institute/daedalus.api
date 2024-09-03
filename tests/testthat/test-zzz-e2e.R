check_for_redis();
queue <- start_test_queue_with_worker()
bg <- porcelain::porcelain_background$new(
  api,
  list(validate = TRUE) # Force error response if data does not validate against schema
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
  expect_equal(httr::status_code(run_response), 200)

  run_id <- body$data$runId
  expect_equal(nchar(run_id), 32)

  # 2. Wait for run to complete successfully
  is_task_successful <- wait_for_task_complete(run_id, queue$controller, 10)
  expect_true(is_task_successful)

  # 3. Test can get expected statu response
  status_url <- paste0("/scenario/status/", run_id)
  status_response <- bg$request("GET", status_url)
  status_body <- httr::content(status_response)
  expect_equal(httr::status_code(status_response), 200)

  expect_equal(status_body$data$runStatus, "complete")
  expect_true(status_body$data$runSuccess)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$runId, run_id)

  # 4. Test can get results
  results_url <- paste0("/scenario/results/", run_id)
  results_response <- bg$request("GET", results_url)
  expect_equal(httr::status_code(results_response), 200)
  results_body <- httr::content(results_response)
  results_data <- results_body$data
  expect_gt(length(results_data$costs), 0)
  expect_gt(length(results_data$capacities), 0)
  expect_gt(length(results_data$interventions), 0)
  expect_gt(length(results_data$time_series), 0)
})
