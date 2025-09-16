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
    lapply(daedalus.data::country_names, get_annual_gdp)
  )
  expect_gt(
    min(vapply(daedalus.data::country_names, get_annual_gdp, numeric(1L))),
    0
  )
})

test_that("can get average vsl for countries", {
  expect_no_condition(
    lapply(daedalus.data::country_names, get_average_vsl)
  )
  expect_gt(
    min(vapply(daedalus.data::country_names, get_average_vsl, numeric(1L))),
    0
  )
})

test_that("calculates correct value of weighted mean of vsl", {
  mock_country_data <- list(
    vsl = c(1000, 2000, 3000, 4000),
    demography = c(0.1, 0.2, 0.3, 0.4)
  )
  mock_get_country_data <- mockery::mock(mock_country_data)
  mockery::stub(
    get_average_vsl,
    "daedalus::daedalus_country",
    mock_get_country_data
  )

  res <- get_average_vsl("CAN")

  expect_identical(
    res,
    stats::weighted.mean(mock_country_data$vsl, mock_country_data$demography)
  )
})

test_that("generates expected pathogen description", {
  ifr <- c(0.1, 0.2, 0.3, 0.4)
  mock_daedalus_infection <- mockery::mock(list(
    ifr = ifr,
    r0 = 1.72165
  ))
  mockery::stub(
    get_pathogen_description,
    "daedalus::daedalus_infection",
    mock_daedalus_infection
  )

  res <- get_pathogen_description("sars_cov_1")
  mockery::expect_args(mock_daedalus_infection, 1, "sars_cov_1")
  expected <- stringr::str_glue(
    "A disease with R0 of 1.7 and infection fatality ratio between 24% ",
    "and 31% depending on country"
  )
  expect_identical(res, expected)
})

test_that("can get VSL by age sector", {
  mock_country_data <- list(
    vsl = c(8000, 9000, 12000, 6000),
    demography = c(0.1, 0.2, 0.3, 0.4)
  )
  mock_get_country_data <- mockery::mock(mock_country_data)
  mockery::stub(
    get_vsl_by_age_sector,
    "daedalus::daedalus_country",
    mock_get_country_data
  )

  res <- get_vsl_by_age_sector("CAN")
  
  expected <- c("0-4" = 8000, "5-19" = 9000, "20-64" = 12000, "65+" = 6000)
  expect_identical(res, expected)
})

test_that("can get life years lost in natural units", {
  mock_life_years_data <- list(value = 150.5)
  mock_get_life_years_lost <- mockery::mock(mock_life_years_data)
  mockery::stub(
    get_life_years_lost_natural,
    "daedalus::get_life_years_lost",
    mock_get_life_years_lost
  )
  
  mock_model_results <- list()
  res <- get_life_years_lost_natural(mock_model_results)
  
  expect_identical(res, 150.5)
  mockery::expect_args(mock_get_life_years_lost, 1, mock_model_results, "none")
})

test_that("can get education lost in natural units with closures", {
  mock_model_results <- list(
    response_data = list(
      closure_info = list(
        closure_durations = c(10, 20, 15)
      ),
      openness = rep(0.2, 45)  # 20% openness across all sectors during closures
    ),
    country_parameters = list(
      demography = c(100000, 200000, 500000, 150000)
    )
  )
  
  res <- get_education_lost_natural(mock_model_results)
  
  # Expected calculation:
  # n_students = 200000
  # total_closure_days = 45
  # education_openness = 0.2 (for education sector, index 41)
  # edu_effectiveness_remote = 0.33
  # education_loss_factor = (1 - 0.2) * (1 - 0.33) = 0.8 * 0.67 = 0.536
  # total_education_days_lost = 200000 * 45 * 0.536 = 4,824,000
  expected <- 200000 * 45 * 0.536
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("can get education lost in natural units with openness data unavailable", {
  mock_model_results <- list(
    response_data = list(
      closure_info = list(
        closure_durations = c(10, 20, 15)
      ),
      openness = rep(0.2, 10)  # Only 10 sectors, not enough for education sector (index 41)
    ),
    country_parameters = list(
      demography = c(100000, 200000, 500000, 150000)
    )
  )
  
  res <- get_education_lost_natural(mock_model_results)
  
  # Expected calculation (fallback when openness data insufficient):
  # n_students = 200000
  # total_closure_days = 45  
  # education_loss_factor = (1 - 0.33) = 0.67 (full closure assumption)
  # total_education_days_lost = 200000 * 45 * 0.67 = 6,030,000
  expected <- 200000 * 45 * 0.67
  expect_equal(res, expected, tolerance = 1e-6)
})

test_that("can get education lost in natural units with insufficient demographics", {
  mock_model_results <- list(
    response_data = list(
      closure_info = list(
        closure_durations = c(10)
      ),
      openness = rep(0.5, 45)
    ),
    country_parameters = list(
      demography = c(100000)  # Only one age group, insufficient
    )
  )
  
  # Expect a warning and result of 0
  expect_warning(
    result <- get_education_lost_natural(mock_model_results),
    "Insufficient demographic data"
  )
  expect_identical(result, 0)
})

test_that("can get education lost in natural units with no closures", {
  mock_model_results <- list(
    response_data = list(
      closure_info = list(
        closure_durations = NA
      )
    )
  )
  
  res <- get_education_lost_natural(mock_model_results)
  expect_identical(res, 0)
})
