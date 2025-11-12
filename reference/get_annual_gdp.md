# Get annual GDP from DAEDALUS country data

Convert daily GVA values to annual GDP values.

## Usage

``` r
get_annual_gdp(country)
```

## Arguments

- country:

  A string giving a country name from among
  [`daedalus.data::country_names`](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  or an ISO2 code from among
  [`daedalus.data::country_codes_iso2c`](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html)
  or an ISO3 code from among
  [`daedalus.data::country_codes_iso3c`](https://jameel-institute.github.io/daedalus.data/reference/country_names_codes.html).

## Value

A single number value for the annual GDP of a country in terms of
million dollars. Values are in 2018 terms.
