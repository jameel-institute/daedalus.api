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
