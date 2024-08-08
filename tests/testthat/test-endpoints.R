test_that("Root data returns sensible, validated, data", {
  ## Just hello world for the package really
  endpoint <- daedalus_api_endpoint("GET", "/")
  res <- endpoint$run()
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
    strsplit(result$output, "\n", fixed = TRUE)[[1L]],
    jsonlite::parse_json
  )
  expect_length(logs, 2L)
  expect_identical(logs[[1L]]$logger, "daedalus.api")
})

test_that("Can get metadata", {
  endpoint <- daedalus_api_endpoint("GET", "/metadata")
  res <- endpoint$run()
  expect_true(res$validated)
  
  lapply(res$data$parameters$id, function(id){
    expect_true(id %in% expected_parameters)
  })
  
  # expect country ids to match those from daedalus
  country_idx <- match("country", res$data$parameters$id)
  country_options <- res$data$parameters$options[[country_idx]]
  daedalus_countries = daedalus::country_names
  expect_equal(length(country_options), length(daedalus_countries))
  lapply(seq_along(country_options), function(idx) {
    expect_equal(country_options[[idx]]$id, scalar(daedalus_countries[[idx]]))
    expect_equal(country_options[[idx]]$label, scalar(daedalus_countries[[idx]]))
  })
})
