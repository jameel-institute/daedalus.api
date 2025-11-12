# Translate behaviour choice to daedalus input

Note that the mapping from the dashboard options to the optimism
parameter is reversed. For a dashboard option of 'low', optimism is
high. This allows the dashboard option to present as 'change in public
behaviour'.

## Usage

``` r
process_behaviour_choice(x, hospital_capacity)
```

## Arguments

- x:

  A single string for the behavioural choice.

- hospital_capacity:

  The country hospital capacity.

## Value

Either `NULL`, or a `<daedalus_behaviour>` with 'new' behavioural
parameters, suitable for passing to the `behaviour` argument of
[`daedalus::daedalus()`](https://jameel-institute.github.io/daedalus/reference/daedalus.html).
