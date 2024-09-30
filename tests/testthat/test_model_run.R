test_that("can run model and return results", {
  mock_model_data <- read.csv("mock_model_data.csv")
  mock_results <- list(
    model_data = mock_model_data,
    response_data = list(
      implementation_level = "heavy",
      closure_info = list(
        closure_time_start = 11,
        closure_time_end = 79
      )
    )
  )
  mock_daedalus <- mockery::mock(mock_results)
  mockery::stub(model_run, "daedalus::daedalus", mock_daedalus)

  mock_costs_data <- daedalus_mock_costs()
  mock_get_costs <- mockery::mock(mock_costs_data)
  mockery::stub(model_run, "daedalus::get_costs", mock_get_costs)

  parameters <- list(
    country = "Canada",
    pathogen = "influenza_1918",
    response = "elimination",
    vaccine = "high",
    hospital_capacity = "4500"
  )
  res <- model_run(parameters, "0.0.1")

  expect_identical(
    mockery::mock_args(mock_daedalus)[[1]],
    list("Canada",
         "influenza_1918",
         response_strategy = "elimination",
         response_threshold = 4500
    )
  )

  expect_named(res, c(
    "runId",
    "parameters",
    "costs",
    "capacities",
    "interventions",
    "time_series"
  ))
  expect_named(res$time_series, c("infect",
                                  "hospitalised",
                                  "dead"))
  expect_identical(res$time_series$infect, c(16L, 56L))
  expect_identical(res$time_series$hospitalised, c(11L, 31L))
  expect_identical(res$time_series$dead, c(15L, 35L))
  expect_identical(res$parameters, parameters)
  expect_nested_mock_costs(res$costs)
  expect_identical(res$interventions, list(
    list(
      id = "response",
      level = "heavy",
      start = 11,
      end = 79
    )
  ))
})
