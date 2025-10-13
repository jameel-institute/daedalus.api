# NOTE: Tests run on the INSTALLED version of the package;
# If tests consistently fail, especially on JSON validation, install the local
# (changed) version of the package before running these tests.

check_for_redis()
temp_dir <- tempdir()
# Env vars required by the queue
qid <- test_queue_id()
withr::local_envvar(
  .new = list(
    DAEDALUS_QUEUE_ID = qid,
    REDIS_CONTAINER_NAME = "localhost",
    DAEDALUS_LOGS_DIR = temp_dir,
    DAEDALUS_RESULTS_DIR = temp_dir
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
    dat$data$daedalus.api,
    package_version_string("daedalus.api")
  )
})

test_that("can run model, get status and results", {
  # 1. Run model
  data <- list(
    modelVersion = "0.0.2",
    parameters = list(
      country = "GBR",
      pathogen = "sars_cov_1",
      response = "economic_closures",
      vaccine = "low",
      hospital_capacity = "4500"
    )
  )
  body <- jsonlite::toJSON(data, auto_unbox = TRUE)
  run_response <- bg$request(
    "POST",
    "/scenario/run",
    body = body,
    encode = "raw",
    httr::content_type("application/json")
  )
  body <- httr::content(run_response)
  expect_identical(httr::status_code(run_response), 200L)

  run_id <- body$data$runId
  expect_identical(nchar(run_id), 32L)

  # 2. Wait for run to complete successfully
  is_task_successful <- wait_for_task_complete(run_id, queue$controller, 100)
  expect_true(is_task_successful)

  # 3. Test can get expected status response
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
  results_body <- httr::content(results_response)

  expect_identical(httr::status_code(results_response), 200L)
  results_data <- results_body$data
  expect_gt(length(results_data$costs), 0)
  expect_gt(length(results_data$capacities), 0)
  expect_gt(length(results_data$interventions), 0)
  expect_gt(length(results_data$time_series), 0)

  time_series_length <- length(results_data$time_series$vaccinated)
  expect_gt(time_series_length, 0)
  expect_length(results_data$time_series$prevalence, time_series_length)
  expect_length(results_data$time_series$hospitalised, time_series_length)
  expect_length(results_data$time_series$dead, time_series_length)
  expect_length(results_data$time_series$new_infected, time_series_length)
  expect_length(
    results_data$time_series$new_hospitalised,
    time_series_length
  )
  expect_length(results_data$time_series$new_dead, time_series_length)
  expect_length(results_data$time_series$new_vaccinated, time_series_length)

  expect_gt(results_data$gdp, 0)
  expect_true(
    all(
      vapply(results_data$vsl, `>`, logical(1), 0)
    )
  )

  # 5. Test nested costs - values should add up
  tolerance <- testthat_tolerance()
  costs_total <- results_data$costs[[1]]

  expect_identical(costs_total$id, "total")
  expect_nested_names(
    costs_total,
    c("gdp", "education", "life_years")
  )
  expect_nested_value_sum(costs_total)

  gdp_total <- costs_total$children[[1]]
  expect_nested_names(
    gdp_total,
    c("gdp_closures", "gdp_absences")
  )
  expect_nested_value_sum(gdp_total)

  education_total <- costs_total$children[[2]]
  expect_nested_names(
    education_total,
    c("education_closures", "education_absences")
  )
  expect_nested_value_sum(education_total)

  life_years_total <- costs_total$children[[3]]
  expect_nested_value_sum(life_years_total, 1)
  expect_nested_value_sum(life_years_total, 2) # check sum in life years
  expect_nested_names(
    life_years_total,
    c(
      "life_years_pre_school",
      "life_years_school_age",
      "life_years_working_age",
      "life_years_retirement_age"
    )
  )
})
