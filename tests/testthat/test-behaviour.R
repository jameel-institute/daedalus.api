test_that("Behaviour option conversion works", {
  behaviour_options <- c("none", "low", "medium", "high")
  hospital_capacity <- 1e4
  invisible(
    lapply(behaviour_options, function(x) {
      expect_no_condition(
        process_behaviour_choice(x, hospital_capacity)
      )
    })
  )

  expect_null(
    process_behaviour_choice("none")
  )

  invisible(
    lapply(behaviour_options[-1], function(x) {
      expect_s3_class(
        process_behaviour_choice(x, hospital_capacity),
        "daedalus_behaviour"
      )
    })
  )
})

test_that("Behaviour option descriptions are correct", {
  behaviour_options <- c("low", "medium", "high")
  desc_option <- rev(behaviour_options)
  expected_kwords <- glue::glue("{desc_option} level of optimism")

  invisible(
    Map(behaviour_options, expected_kwords, f = function(x, y) {
      expect_true(
        grepl(y, get_behaviour_description(x), fixed = TRUE)
      )
    })
  )

  expect_true(
    grepl("does not adopt", get_behaviour_description("none"), fixed = TRUE)
  )
})
