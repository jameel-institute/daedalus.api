test_that("can run model and return results", {
  mock_model_data <- read.csv("mock_model_data.csv")
  mock_results <- list(model_data = mock_model_data)
  mock_daedalus <- mockery::mock(mock_results)
  mockery::stub(model_run, "daedalus::daedalus", mock_daedalus)

  parameters <- list(
    country="Canada",
    pathogen="influenza_1918",
    response="none",
    vaccine="high",
    hospital_capacity="4500"
  )
  res <- model_run(parameters, "0.0.1")

  expect_identical(
    mockery::mock_args(mock_daedalus)[[1]],
    list("Canada", "influenza_1918", response_strategy="none")
  )

  expect_named(res, c(
    "runId",
    "parameters",
    "costs",
    "capacities",
    "interventions",
    "time_series"
  ))
  expect_named(res$time_series, c("infect_asymp", "infect_symp", "hospitalised", "recovered", "dead"))
  expect_identical(res$time_series$infect_asymp, c(7L, 27L))
  expect_identical(res$time_series$infect_symp, c(9L, 29L))
  expect_identical(res$time_series$hospitalised, c(11L, 31L))
  expect_identical(res$time_series$recovered, c(13L, 33L))
  expect_identical(res$time_series$dead, c(15L, 35L))
  expect_identical(res$parameters, parameters)
})
