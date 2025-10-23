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
