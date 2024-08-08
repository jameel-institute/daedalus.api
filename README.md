# daedalus.api

<!-- badges: start -->

[![Project Status: Concept -- Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept) [![CRAN status](https://www.r-pkg.org/badges/version/daedalus.api)](https://CRAN.R-project.org/package=daedalus.api) [![Codecov test coverage](https://codecov.io/gh/jameel-institute/daedalus.api/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jameel-institute/daedalus.api?branch=main) [![R-CMD-check](https://github.com/jameel-institute/daedalus.api/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jameel-institute/daedalus.api/actions/workflows/R-CMD-check.yaml) [![Build status](https://badge.buildkite.com/2fe5d34f1b4c4681b4e0e8d464f4fdaf44358fc48325b92580.svg)](https://buildkite.com/mrc-ide/daedalus-dot-api)

<!-- badges: end -->

_daedalus.api_ is an API package for the [_daedalus_ package](https://github.com/jameel-institute/daedalus) and is primarily intended for internal use.

## Installation

You can install the development version of daedalus.api from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jameel-institute/daedalus.api")
```

## Quick start

```sh
# the image will be assigned the tag 'latest'
docker pull mrcide/daedalus.api:latest

# run the container
# see docker run --help for options
docker run -d --name daedalus-api --rm -p 8001:8001 mrcide/daedalus.api:latest

# check root endpoint `GET/`
curl -s http://localhost:8001 | jq

# stop the service
docker stop daedalus-api
```

## Development

To add an endpoint, implement a method in `api.R` with `@porcelain` comment, then run `roxygen2::roxygenize()` to generate the porcelain code
in `porcelain.R`. See the [porcelain docs](https://reside-ic.github.io/porcelain/articles/roxygen.html) for more details.

## Model versions

The API should be backwards compatible and support running older versions of the model. 
Some endpoints support providing `modelVersion` as a query string parameter, e.g. to run or get metadata for a particular version of the model. 

Metadata is stored in the `inst/json` folder, in files named `metadata_[VERSION].json` where `[VERSION]` is the first model version where
that metadata applied. Requesting metadata for a model version will return the metadata which applies to that version, (which may have been 
first introduced in an earlier version). The metadata response includes a `modelVersion` property - this value will be the `modelVersion`
requested in the query string, if provided. If `modelVersion` was not provided in the query string, the returned 
model version will be the most recent metadata's `[VERSION]`. 

## Related projects

See the [_daedalus_ package](https://github.com/jameel-institute/daedalus) which implements the DAEDALUS integrated model of economic, social, and health costs of a pandemic.
