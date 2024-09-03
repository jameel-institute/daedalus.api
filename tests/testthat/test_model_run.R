test_that("can run model and return results", {
  res_json <- model_run(list(), "0.0.1")
  res <- jsonlite::fromJSON(res_json)
  expect_equal(names(res), c(
    "runId",
    "parameters",
    "costs",
    "capacities",
    "interventions",
    "time_series"
  ))
})