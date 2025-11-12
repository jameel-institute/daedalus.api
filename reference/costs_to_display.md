# Prepare Daedalus costs output for display

`get_nested_costs()` and `get_nested_natural_costs()` prepare daedalus
costs outputs for display. `get_natural_costs()` currently only prepares
life-years lost but may include other costs in their natural units in
future.

`cost_item()` is a helper function that prepares list elements in the
format `"id"`, `"value"`, `"children"`.

## Usage

``` r
get_nested_costs(raw_costs)

get_life_years_lost(raw_costs)

cost_item(id, values, children = NULL)
```

## Arguments

- raw_costs:

  A list resulting from a call to
  [`daedalus::get_costs()`](https://jameel-institute.github.io/daedalus/reference/get_costs.html)
  on a `<daedalus_output>` class object.

- id:

  String description of list name.

- values:

  List contents.

- children:

  Nested lists contained within the top-level list, if any. Defaults to
  `NULL`.

## Value

A recursive nested list with the elements `"id"` and `"value"` with a
string and numeric value respectively. Additionally, a `"children"` list
element may be present containing another list with the same recursive
structure. The lowest level list within `"children"` has no `"children"`
element.
