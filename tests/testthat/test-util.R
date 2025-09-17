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

test_that("can get vsl by age sector for countries", {
  expect_no_condition(
    lapply(daedalus.data::country_names, get_vsl_by_age_sector)
  )
  
  # Test that all countries return positive VSL values for all age sectors
  vsl_results <- lapply(daedalus.data::country_names, get_vsl_by_age_sector)
  for (vsl_by_age in vsl_results) {
    expect_setequal(names(vsl_by_age), c("vsl_pre_school", "vsl_school_age", "vsl_working_age", "vsl_retirement_age"))
    expect_true(all(vsl_by_age > 0))
  }
})

test_that("vsl by age sector returns correct structure", {
  mock_country_data <- list(
    vsl = c(1000, 2000, 3000, 4000),
    demography = c(0.1, 0.2, 0.3, 0.4)
  )
  mock_get_country_data <- mockery::mock(mock_country_data)
  mockery::stub(
    get_vsl_by_age_sector,
    "daedalus::daedalus_country",
    mock_get_country_data
  )

  res <- get_vsl_by_age_sector("CAN")

  expected <- stats::setNames(
    c(1000, 2000, 3000, 4000),
    c("vsl_pre_school", "vsl_school_age", "vsl_working_age", "vsl_retirement_age")
  )
  expect_identical(res, expected)
})

test_that("get_nested_natural_costs returns correct structure", {
  mock_life_years_lost <- list(
    life_years_lost = c(100, 200, 300, 400)
  )
  mock_get_life_years_lost <- mockery::mock(mock_life_years_lost)
  mockery::stub(
    get_nested_natural_costs,
    "daedalus::get_life_years_lost",
    mock_get_life_years_lost
  )

  mock_model_results <- list()  # Not used in the mocked function
  res <- get_nested_natural_costs(mock_model_results)

  # Should return an array with one item (total)
  expect_length(res, 1L)
  
  total_item <- res[[1]]
  expect_identical(total_item$id, "total")
  expect_identical(total_item$value, 1000)  # 100 + 200 + 300 + 400
  expect_length(total_item$children, 1L)
  
  life_years_item <- total_item$children[[1]]
  expect_identical(life_years_item$id, "life_years")
  expect_identical(life_years_item$value, 1000)
  expect_length(life_years_item$children, 4L)
  
  # Check age group items
  age_group_ids <- vapply(life_years_item$children, function(x) x$id, character(1))
  expected_ids <- c("life_years_natural_pre_school", "life_years_natural_school_age", 
                    "life_years_natural_working_age", "life_years_natural_retirement_age")
  expect_setequal(age_group_ids, expected_ids)
  
  age_group_values <- vapply(life_years_item$children, function(x) x$value, numeric(1))
  expect_equal(age_group_values, c(100, 200, 300, 400))
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
