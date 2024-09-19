test_that("can run model and return results", {
  mock_model_data <- data.frame(
    time = c(1, 1, 1, 1, 2, 2, 2, 2),
    age_group = c("0-4", "0-4", "4-19", "4-19", "0-4", "0-4", "4-19", "4-19"),
    compartment = c("hospitalised", "dead", "hospitalised", "dead", "hospitalised", "dead", "hospitalised", "dead"),
    econ_sector = c("sector1", "sector1", "sector1", "sector1", "sector1", "sector1", "sector1", "sector1"),
    value = c(10, 1, 20, 2, 30, 3, 40, 4)
  )
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
  expect_named(res$time_series, c("hospitalised", "dead"))
  expect_identical(res$time_series$hospitalised, c(30, 70))
  expect_identical(res$time_series$dead, c(3, 7))
  expect_identical(res$parameters, parameters)
})
