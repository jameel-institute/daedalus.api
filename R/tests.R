#' Expect values for nested costs output
#'
#' @name custom_tests
#' @rdname custom_tests
#'
#' @description
#' Helper expectations functions to check nested costs output.
#' `expect_nested_value_sum()` checks that the level cost under
#' `x$values[[1]]$value` is the sum of the costs associated with its children.
#'
#' `expect_nested_names()` checks that `x$children` have expected names.
#'
#' @param x A nested list of costs, which is expected to have a top level cost
#' under `x$values[[i]]$value`, and nested costs under `x$children`.
#'
#' @param i_unit An index, defaulting to 1, for which index of `x$values` is
#' summed.
#'
#' @keywords internal
expect_nested_value_sum <- function(x, i_unit = 1L) {
  top_level_sum <- x$values[[i_unit]]$value

  list_values <- vapply(
    x$children,
    function(z) {
      z$values[[i_unit]]$value
    },
    numeric(1L)
  )
  list_values_sum <- sum(list_values)

  testthat::expect_identical(
    list_values_sum,
    top_level_sum,
    tolerance = 1e-6 # rather than testthat_tolerance()
  )
}

#' @keywords internal
expect_nested_names <- function(x, names) {
  names_list <- vapply(x$children, `[[`, "id", FUN.VALUE = character(1L))

  testthat::expect_identical(
    names_list,
    names
  )
}
