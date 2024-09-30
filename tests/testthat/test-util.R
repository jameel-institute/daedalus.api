test_that("can get hospital capacity range", {
  result <- get_hospital_capacity_for_pop(4513, 100)
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
