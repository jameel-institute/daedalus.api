# Create daedalus.api

Create an daedalus.api server, a porcelain object

## Usage

``` r
api(validate = NULL, log_level = "info")
```

## Arguments

- validate:

  Logical, indicating if validation should be done on responses. This
  should be `FALSE` in production environments. See
  [porcelain::porcelain](https://rdrr.io/pkg/porcelain/man/porcelain.html)
  for details

- log_level:

  Logging level to use. Sensible options are "off", "info" and "all".

## Value

A
[porcelain::porcelain](https://rdrr.io/pkg/porcelain/man/porcelain.html)
object. Notably this does *not* start the server
