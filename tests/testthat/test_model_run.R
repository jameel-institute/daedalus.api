test_that("can run model and return results", {
  mock_model_data <- read.csv("mock_model_data.csv")
  mock_results <- list(
    model_data = mock_model_data,
    response_data = list(
      closure_info = list(
        closure_time_start = 11,
        closure_time_end = 79
      )
    )
  )
  mock_daedalus <- mockery::mock(mock_results)
  mockery::stub(model_run, "daedalus::daedalus", mock_daedalus)

  mockery::stub(model_run, "daedalus::get_data", mock_model_data)

  mock_costs_data <- daedalus_mock_costs()
  mock_get_costs <- mockery::mock(mock_costs_data)
  mockery::stub(model_run, "daedalus::get_costs", mock_get_costs)
  mock_get_gdp <- mockery::mock(9999)
  mockery::stub(model_run, "get_annual_gdp", mock_get_gdp)

  parameters <- list(
    country = "CAN",
    pathogen = "influenza_1918",
    response = "elimination",
    vaccine = "high",
    hospital_capacity = "4500"
  )
  res <- model_run(parameters, "0.0.1")

  expect_identical(
    mockery::mock_args(mock_daedalus)[[1]],
    list("CAN",
         "influenza_1918",
         response_strategy = "elimination",
         response_threshold = 4500,
         vaccine_investment = "high"
    )
  )

  expect_named(res, c(
    "parameters",
    "costs",
    "time_series",
    "interventions",
    "capacities",
    "gdp"
  ))
  expect_named(res$time_series, c("prevalence",
                                  "hospitalised",
                                  "dead",
                                  "vaccinated"))
  expect_identical(res$time_series$prevalence, c(28L, 87L))
  expect_identical(res$time_series$hospitalised, c(11L, 31L))
  expect_identical(res$time_series$dead, c(15L, 37L))
  expect_identical(res$time_series$vaccinated, c(1L, 5L))
  expect_identical(res$parameters, parameters)
  expect_nested_mock_costs(res$costs)
  expect_identical(res$interventions, list(
    list(
      id = "response",
      start = 11,
      end = 79
    )
  ))
  expect_length(res$capacities, 1L)
  expect_identical(res$capacities[[1]]$id, "hospital_capacity")
  expect_identical(res$capacities[[1]]$value, 4500)
  expect_identical(res$gdp, 9999)
})
