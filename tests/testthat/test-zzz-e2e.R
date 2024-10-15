check_for_redis()
temp_dir <- tempdir()
# Env vars required by the queue
qid <- paste0("daedalus.api.tests.queue-", uuid::UUIDgenerate())
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
    dat$data$daedalus.api, package_version_string("daedalus.api")
  )
})

test_that("can run model, get status and results", {
  # 1. Run model
  data <- list(
    modelVersion = "0.0.2",
    parameters = list(
      country = "United Kingdom",
      pathogen = "sars_cov_1",
      response = "economic_closures",
      vaccine = "low",
      hospital_capacity = "4500"
    )
  )
  body <- jsonlite::toJSON(data, auto_unbox = TRUE)
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

  expect_gt(results_data$gdp, 0)

  # 5. Test nested costs - values should add up
  tolerance <- testthat_tolerance()
  costs_total <- results_data$costs[[1]]
  expect_identical(costs_total$id, "total")
  gdp_total <- costs_total$children[[1]]
  expect_identical(gdp_total$id, "gdp")
  education_total <- costs_total$children[[2]]
  expect_identical(education_total$id, "education")
  life_years_total <- costs_total$children[[3]]
  expect_identical(life_years_total$id, "life_years")
  expect_equal(costs_total$value,
               sum(gdp_total$value,
                   education_total$value,
                   life_years_total$value),
               tolerance = tolerance)
  gdp_closures <- gdp_total$children[[1]]
  expect_identical(gdp_closures$id, "gdp_closures")
  gdp_absences <- gdp_total$children[[2]]
  expect_identical(gdp_absences$id, "gdp_absences")
  expect_equal(gdp_total$value,
               sum(gdp_closures$value,
                   gdp_absences$value),
               tolerance = tolerance)
  education_closures <- education_total$children[[1]]
  expect_identical(education_closures$id, "education_closures")
  education_absences <- education_total$children[[2]]
  expect_identical(education_absences$id, "education_absences")
  expect_equal(education_total$value,
               sum(education_closures$value,
                   education_absences$value),
               tolerance = tolerance)
  lifeyears_infants <- life_years_total$children[[1]]
  expect_identical(lifeyears_infants$id, "life_years_infants")
  lifeyears_adolescents <- life_years_total$children[[2]]
  expect_identical(lifeyears_adolescents$id, "life_years_adolescents")
  lifeyears_working_age <- life_years_total$children[[3]]
  expect_identical(lifeyears_working_age$id, "life_years_working_age")
  lifeyears_retirement_age <- life_years_total$children[[4]]
  expect_identical(lifeyears_retirement_age$id, "life_years_retirement_age")
  expect_equal(life_years_total$value,
               sum(lifeyears_infants$value,
                   lifeyears_adolescents$value,
                   lifeyears_working_age$value,
                   lifeyears_retirement_age$value),
               tolerance = tolerance)
})
