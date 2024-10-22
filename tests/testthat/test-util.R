test_that("can get hospital capacity range", {
  result <- get_hospital_capacity_range(4513, 100)
  expect_identical(result$min, 4100)
  expect_identical(result$default, 4500)
  expect_identical(result$max, 5900)
})

test_that("can validate valid parameters", {
  parameters <- list(p2 = "v2", p1 = "v1")
  metadata <- list(
    parameters = list(
      list(id = "p1", label = "Param 1"),
      list(id = "p2", label = "Param 2")
    )
  )
  # should be no error
  expect_no_condition(validate_parameters(parameters, metadata))
})

test_that("can validate invalid parameters", {
  parameters <- list(rp2 = "v2", rp1 = "v1")
  metadata <- list(
    parameters = list(
      list(id = "mp1", label = "Param 1"),
      list(id = "mp2", label = "Param 2")
    )
  )
  expect_error(
    validate_parameters(parameters, metadata),
    "The parameters provided do not match required parameters: mp1, mp2"
  )
})

test_that("can get nested costs", {
  raw_costs <- daedalus_mock_costs()
  costs <- get_nested_costs(raw_costs)
  expect_nested_mock_costs(costs)
})

test_that("can get vaccine option description", {
  res <- get_vaccine_option_description("medium")
  expected <- stringr::str_glue(
    "An investment level corresponding to: ",
    "vaccine rollout commencing 200 days after the outbreak starts, ",
    "a vaccine administration rate of 0.43% of population per day, ",
    "and an upper limit of vaccine coverage of 60% of the general population"
  )
  expect_identical(res, expected)
})

test_that("Get annual GDP", {
  expect_no_condition(
    lapply(daedalus::country_names, get_annual_gdp)
  )
  expect_gt(
    min(vapply(daedalus::country_names, get_annual_gdp, numeric(1L))), 0
  )
})

test_that("can get average vsl for countries", {
  expect_no_condition(
    lapply(daedalus::country_names, get_average_vsl)
  )
  expect_gt(
    min(vapply(daedalus::country_names, get_average_vsl, numeric(1L))), 0
  )
})

test_that("calculates correct value of weighted mean of vsl", {
  mock_country_data <- list(
    vsl = c(1000, 2000, 3000, 4000),
    demography = c(0.1, 0.2, 0.3, 0.4)
  )
  mock_get_country_data <- mockery::mock(mock_country_data)
  mockery::stub(get_average_vsl, "daedalus::daedalus_country", mock_get_country_data)
  
  res <- get_average_vsl("CAN")

  expect_equal(res, weighted.mean(mock_country_data$vsl, mock_country_data$demography))
})
