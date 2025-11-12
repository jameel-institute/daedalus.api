# Get Average Value of Statistical Life (VSL) for a Country

This function calculates the average Value of Statistical Life (VSL) for
a specified country. It computes the weighted mean of VSL using the
demography data as weights.

## Usage

``` r
get_average_vsl(country)
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

A numeric value representing the average VSL for the specified country.

## Examples

``` r
if (FALSE) { # \dontrun{
avg_vsl <- get_average_vsl("USA")
print(avg_vsl)
} # }
```
