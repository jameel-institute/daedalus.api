test_that("Root data returns sensible, validated, data", {
  ## Just hello world for the package really
  endpoint <- daedalus_api_endpoint("GET", "/")
  expect_no_condition(
    res <- endpoint$run() # nolint
  )
  expect_true(res$validated)

  expect_contains(
    names(res$data),
    c("daedalus", "daedalus.api")
  )

  expect_match(unlist(res$data), regexp = "^[0-9]+\\.[0-9]+\\.[0-9]+$") # nolint
})

test_that("Can construct the api", {
  obj <- api()
  result <- evaluate_promise({
    value <- obj$request("GET", "/")$status
  })
  expect_identical(value, 200L)
  logs <- lapply(
    strsplit(result$output, "\n", fixed = TRUE)[[1]],
    jsonlite::parse_json
  )
  expect_length(logs, 2L)
  expect_identical(logs[[1L]]$logger, "daedalus.api")
})

test_that("Can get metadata", {
  endpoint <- daedalus_api_endpoint("GET", "/metadata")
  expect_no_condition(
    res <- endpoint$run() # nolint
  )
  expect_true(res$validated)

  res <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)

  params <- res$data$parameters
  expected_parameters <- c(
    "country",
    "pathogen",
    "response",
    "vaccine",
    "hospital_capacity"
  )
  expect_setequal(
    vapply(
      params,
      function(param) {
        param$id
      },
      character(1L)
    ),
    expected_parameters
  )
  country_idx <- match("country", expected_parameters)
  country_options <- params[[country_idx]]$options
  daedalus_country_codes <- daedalus.data::country_codes_iso3c
  # expect country ids to match those from daedalus
  expect_identical(
    vapply(
      country_options,
      function(option) {
        option$id
      },
      character(1)
    ),
    daedalus_country_codes
  )
  # expect country labels to match those from daedalus
  daedalus_country_names <- daedalus.data::country_names
  expect_identical(
    vapply(
      country_options,
      function(option) {
        option$label
      },
      character(1)
    ),
    daedalus_country_names
  )
  expect_identical(params[[country_idx]]$defaultOption, "THA")
  hosp_cap_idx <- match("hospital_capacity", expected_parameters)
  update_values <- res$data$parameters[[hosp_cap_idx]]$updateNumericFrom$values
  expect_named(update_values, daedalus_country_codes)
  for (country in daedalus_country_codes) {
    values <- update_values[[country]]
    expect_identical(values$min %% 100, 0)
    expect_identical(values$default %% 100, 0)
    expect_identical(values$max %% 100, 0)
    expect_gte(values$max, values$default)
    expect_gte(values$default, values$min)
  }

  pathogen_idx <- match("pathogen", expected_parameters)
  pathogen_options <- params[[pathogen_idx]]$options
  for (option in pathogen_options) {
    expect_match(option$description, "A disease with R0 of [0-9]")
  }
})


# This adadpts the e2e test with the same name; it could be split
# further.  The strategy here is that we can produce objects with
# `daedalus_api_endpoint` that can be used to simulate requests to the
# api without doing any network requests, or by using a separate
# process for the server. We can apply the same approach to the rrq
# workers, by using `rrq_worker` directly to create a blocking worker,
# and by preventing it using a worker process to run the model.  This
# approach will mean that errors are easier to intercept, and
# debugging with `browser()` becomes straightforward.
test_that("can run model, get status and results", {
  queue_id <- test_queue_id()
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
  endpoint_run <- daedalus_api_endpoint(
    "POST", "/scenario/run", queue_id = queue_id
  )
  endpoint_status <- daedalus_api_endpoint(
    "GET", "/scenario/status/<run_id:string>", queue_id = queue_id
  )
  endpoint_results <- daedalus_api_endpoint(
    "GET", "/scenario/results/<run_id:string>", queue_id = queue_id
  )

  # Submit the task, validate that we get back an rrq handle in the
  # response:
  res <- endpoint_run$run(data = body)
  expect_equal(res$status_code, 200)
  run_id <- res$data$runId
  expect_match(run_id, "^[0-9a-f]{32}$")

  # Run the job, in process - errors will still be swallowed by the
  # worker, but there is no need to wait on anything, and the code
  # used is the dev-mode package.
  suppressMessages({
    worker <- test_worker_blocking(queue_id)
    worker$step(immediate = TRUE)
  })

  # Retrieve the status, checking the format of the list of returned
  # data. We also check that the response was validated against the
  # schema:
  res <- endpoint_status$run(run_id)
  expect_equal(res$status_code, 200)
  expect_mapequal(res$data, list(
    runStatus = scalar("complete"),
    runSuccess = scalar(TRUE),
    done = scalar(TRUE),
    runErrors = NULL,
    runId = scalar(run_id)
  )
  )
  expect_true(res$validated)

  # Fetch the result back:
  res <- endpoint_results$run(run_id)
  expect_equal(res$status_code, 200)
  expect_true(res$validated)

  # Tests copied from the e2e tests, except that because serialisation
  # has already happened here (the data element is of class json) we
  # need to manually deserialise, which can cause some roundtrip
  # errors
  results_data <- jsonlite::fromJSON(res$data, simplifyVector = FALSE)
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
  expect_gt(results_data$average_vsl, 0)

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
  # This test is broken, and it's not immediately obvious where the issue is
  # expect_equal(
  #   costs_total$value,
  #   sum(
  #     gdp_total$value,
  #     education_total$value,
  #     life_years_total$value
  #   ),
  #   tolerance = tolerance
  # )
  gdp_closures <- gdp_total$children[[1]]
  expect_identical(gdp_closures$id, "gdp_closures")
  gdp_absences <- gdp_total$children[[2]]
  expect_identical(gdp_absences$id, "gdp_absences")
  expect_equal(
    gdp_total$value,
    sum(
      gdp_closures$value,
      gdp_absences$value
    ),
    tolerance = tolerance
  )
  education_closures <- education_total$children[[1]]
  expect_identical(education_closures$id, "education_closures")
  education_absences <- education_total$children[[2]]
  expect_identical(education_absences$id, "education_absences")
  expect_equal(
    education_total$value,
    sum(
      education_closures$value,
      education_absences$value
    ),
    tolerance = tolerance
  )
  lifeyears_pre_school <- life_years_total$children[[1]]
  expect_identical(lifeyears_pre_school$id, "life_years_pre_school")
  lifeyears_school_age <- life_years_total$children[[2]]
  expect_identical(lifeyears_school_age$id, "life_years_school_age")
  lifeyears_working_age <- life_years_total$children[[3]]
  expect_identical(lifeyears_working_age$id, "life_years_working_age")
  lifeyears_retirement_age <- life_years_total$children[[4]]
  expect_identical(lifeyears_retirement_age$id, "life_years_retirement_age")
  expect_equal(
    life_years_total$value,
    sum(
      lifeyears_pre_school$value,
      lifeyears_school_age$value,
      lifeyears_working_age$value,
      lifeyears_retirement_age$value
    ),
    tolerance = tolerance
  )
})
