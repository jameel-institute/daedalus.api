skip()
test_that("can get hospital capacity for population", {
  result <- get_hospital_capacity_for_pop(1263182, 10)
  expect_identical(result$min, 380)
  expect_identical(result$default, 570)
  expect_identical(result$max, 1640)
})
