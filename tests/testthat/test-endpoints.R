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
    vapply(params, function(param) {
      param$id
    }, character(1L)),
    expected_parameters
  )
  country_idx <- match("country", expected_parameters)
  country_options <- params[[country_idx]]$options
  daedalus_country_codes <- daedalus::country_codes_iso3c
  # expect country ids to match those from daedalus
  expect_identical(
    vapply(country_options, function(option) {
        option$id
    }, character(1)),
    daedalus_country_codes
  )
  # expect country labels to match those from daedalus
  daedalus_country_names <- daedalus::country_names
  expect_identical(
    vapply(country_options, function(option) {
      option$label
    }, character(1)),
    daedalus_country_names
  )
  expect_identical(params[[country_idx]]$defaultOption, "Thailand")
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
})
