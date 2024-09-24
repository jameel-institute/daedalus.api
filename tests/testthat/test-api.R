test_that("can run scenario", {
  data <- list(
    modelVersion = "0.0.1",
    parameters = list(
      country = "Canada",
      pathogen = "influenza_1918",
      response = "none",
      vaccine = "high",
      hospital_capacity = "9000"
    )
  )
  queue <- Queue$new()
  result <- scenario_run(queue, jsonlite::toJSON(data))
  expect_type(result$runId, "character")
})
