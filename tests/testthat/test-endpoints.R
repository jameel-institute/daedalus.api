# NB endpoint which require interaction with rrq and redis are tested in test-zzz-e2e

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
  obj <- api(configure_queue = FALSE)
  result <- evaluate_promise({
    value <- obj$request("GET", "/")$status
  })
  expect_identical(value, 200L)
  logs <- lapply(
    strsplit(result$output, "\n", fixed = TRUE)[[1L]],
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
  expected_parameters <- c(
    "country",
    "pathogen",
    "response",
    "vaccine"
  )
  expect_setequal(
    res$data$parameters$id,
    expected_parameters
  )
  country_idx <- match("country", res$data$parameters$id)
  country_options <- res$data$parameters$options[[country_idx]]
  daedalus_countries <- daedalus::country_names
  # expect country ids to match those from daedalus
  expect_identical(
    vapply(country_options, function(option) {
        option$id
    }, character(1L)),
    daedalus_countries
  )
  # expect country labels to match those from daedalus
  expect_identical(
    vapply(country_options, function(option) {
      option$label
    }, character(1L)),
    daedalus_countries
  )
})
