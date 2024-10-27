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
  mockery::stub(
    get_average_vsl,
    "daedalus::daedalus_country", mock_get_country_data
  )

  res <- get_average_vsl("CAN")

  expect_identical(
    res,
    weighted.mean(mock_country_data$vsl, mock_country_data$demography)
  )
})

test_that("generates expected pathogen description", {
  mock_daedalus_infection <- mockery::mock(list(
    ifr = c(0.1, 0.2, 0.3, 0.4),
    r0 = 1.72165
  ))
  mockery::stub(get_pathogen_description,
                "daedalus::daedalus_infection",
                mock_daedalus_infection)

  mockery::stub(get_pathogen_description,
                "daedalus::country_names",
                c("Country1", "Country2"))


  mock_daedalus_country <- mockery::mock(
    list(
      name = "Country1",
      demography = c(
        "0-4" = 0.1,
        "5-19" = 0.2,
        "20-64" = 0.5,
        "65+" = 0.2
      )
    ),
    list(
      name = "Country2",
      demography = c(
        "0-4" = 0.2,
        "5-19" = 0.3,
        "20-64" = 0.4,
        "65+" = 0.1
      )
    )
  )
  mockery::stub(get_pathogen_description,
                "daedalus::daedalus_country",
                mock_daedalus_country)

  res <- get_pathogen_description("sars_cov_1")
  mockery::expect_args(mock_daedalus_infection, 1, "sars_cov_1")
  expected <- stringr::str_glue(
    "A disease with R0 of 1.7 and infection fatality ratio between 24% ",
    "and 28% depending on country"
  )
  expect_identical(res, expected)
})
