# Expect values for nested costs output

Helper expectations functions to check nested costs output.
`expect_nested_value_sum()` checks that the level cost under
`x$values[[1]]$value` is the sum of the costs associated with its
children.

`expect_nested_names()` checks that `x$children` have expected names.

## Usage

``` r
expect_nested_value_sum(x, i_unit = 1L)
```

## Arguments

- x:

  A nested list of costs, which is expected to have a top level cost
  under `x$values[[i]]$value`, and nested costs under `x$children`.

- i_unit:

  An index, defaulting to 1, for which index of `x$values` is summed.
