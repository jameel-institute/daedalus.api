test_that("Custom tests for nested lists work", {
  nested_list <- list(
    values = list(
      list(value = 100)
    ),
    children = list(
      list(
        id = "child_01",
        values = list(
          list(metric = "dummy", value = 80)
        )
      ),
      list(
        id = "child_02",
        values = list(
          list(metric = "dummy", value = 20)
        )
      )
    )
  )

  expect_success(
    expect_nested_value_sum(nested_list)
  )
  expect_success(
    expect_nested_names(nested_list, c("child_01", "child_02"))
  )
})
