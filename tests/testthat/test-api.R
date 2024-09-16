skip()
test_that("can run scenario", {
  data <- list(
    modelVersion = "0.0.1",
    parameters = list()
  )
  queue <- Queue$new()
  result <- scenario_run(queue, jsonlite::toJSON(data))
  expect_type(result$runId, "character")
})
