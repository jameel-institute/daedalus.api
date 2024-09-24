test_that("can get hospital capacity for population", {
  result <- get_hospital_capacity_for_pop(1263182, 10)
  expect_identical(result$min, 380)
  expect_identical(result$default, 570)
  expect_identical(result$max, 1640)
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
  life_years_lost_age <- setNames(c(5, 10, 15, 20), c("0-4", "5-19", "20-65", "65+"))
  raw_costs <- list(
    total_cost = 100,
    economic_costs = list(
      economic_cost_total = 20,
      economic_cost_closures = 5,
      economic_cost_absences = 15
    ),
    education_costs = list(
      education_cost_total = 30,
      education_cost_closures = 10,
      education_cost_absences = 20
    ),
    life_years_lost = list(
      life_years_lost_total = 50,
      life_years_lost_age = life_years_lost_age
    )
  )

  costs <- get_nested_costs(raw_costs)
  expect_identical(length(costs), 1L)
  total <- costs[[1]]
  expect_identical(total$id, "total")
  expect_identical(total$value, 100)
  expect_identical(length(total$children), 3L)

  gdp <- total$children[[1]]
  expect_identical(gdp$id, "gdp")
  expect_identical(gdp$value, 20)
  expect_identical(length(gdp$children), 2L)

  gdp_closures <- gdp$children[[1]]
  expect_identical(gdp_closures$id, "gdp_closures")
  expect_identical(gdp_closures$value, 5)
  expect_false("children" %in% names(gdp_closures))

  gdp_absences <- gdp$children[[2]]
  expect_identical(gdp_absences$id, "gdp_absences")
  expect_identical(gdp_absences$value, 15)
  expect_false("children" %in% names(gdp_absences))

  education <- total$children[[2]]
  expect_identical(education$id, "education")
  expect_identical(education$value, 30)
  expect_identical(length(education$children), 2L)

  education_closures <- education$children[[1]]
  expect_identical(education_closures$id, "education_closures")
  expect_identical(education_closures$value, 10)
  expect_false("children" %in% names(education_closures))

  education_absences <- education$children[[2]]
  expect_identical(education_absences$id, "education_absences")
  expect_identical(education_absences$value, 20)
  expect_false("children" %in% names(education_absences))

  life_years <- total$children[[3]]
  expect_identical(life_years$id, "life_years")
  expect_identical(life_years$value, 50)
  expect_identical(length(life_years$children), 4L)

  life_years_infants <- life_years$children[[1]]
  expect_identical(life_years_infants$id, "life_years_infants")
  expect_identical(life_years_infants$value, 5)
  expect_false("children" %in% names(life_years_infants))

  life_years_adolescents <- life_years$children[[2]]
  expect_identical(life_years_adolescents$id, "life_years_adolescents")
  expect_identical(life_years_adolescents$value, 10)
  expect_false("children" %in% names(life_years_adolescents))

  life_years_working_age <- life_years$children[[3]]
  expect_identical(life_years_working_age$id, "life_years_working_age")
  expect_identical(life_years_working_age$value, 15)
  expect_false("children" %in% names(life_years_working_age))

  life_years_retirement_age <- life_years$children[[4]]
  expect_identical(life_years_retirement_age$id, "life_years_retirement_age")
  expect_identical(life_years_retirement_age$value, 20)
  expect_false("children" %in% names(life_years_retirement_age))
})
