test_that("can run model and return results", {
  res <- model_run(list(), "0.0.1")
  expect_named(res, c(
    "runId",
    "parameters",
    "costs",
    "capacities",
    "interventions",
    "time_series"
  ))
})
