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
# run the api container with queue worker
# This pulls and runs image for current SHA. If none is pushed to registry you can build and push
# locally using ./docker/build
./docker/run_containers

# check root endpoint `GET/`
curl -s http://localhost:8001 | jq

# stop the containers
./docker/clear_containers
```

## Development

To add an endpoint, implement a method in `api.R` with `@porcelain` comment, then run `roxygen2::roxygenize()` to generate the porcelain code
in `porcelain.R`. See the [porcelain docs](https://reside-ic.github.io/porcelain/articles/roxygen.html) for more details.

## Testing

 - Redis needs to be running for the e2e tests to pass. Use `./scripts/redis start`, and tear down with `./scripts/redis kill.`

 - Some tests run using the _installed_ version of the package. If e2e tests consistently fail, especially on JSON validation, install the local (changed or development) version of the package and try running the tests again.

### Testing integration with the dashboard

To test the integration of this package within the entire system including the [dashboard](https://github.com/jameel-institute/daedalus-web-app/), use the [daedalus-deploy](https://github.com/jameel-institute/daedalus-deploy/) tool to install and run all images locally, following the README. You should alter the common config file (`daedalus.yml`) so that the API image tag value points to the feature branch of this package that you want to test, e.g.:

```yml
# daedalus.yml
api:
  image:
    repo: mrcide
    name: daedalus.api
    tag: jidea-297
```

Ensure that your start-up command uses the `--pull` option so that the deploy tool requests the latest versions of the images. Then visit `https://localhost/` in your browser to do your manual testing (ignoring warnings from your browser that the site is not secure).

## Model versions

The API should be backwards compatible and support running older versions of the model. 
Some endpoints support providing `modelVersion` as part of the body, e.g. to run or get metadata for a particular version of the model. 

Metadata is stored in the `inst/json` folder, in files named `metadata_[VERSION].json` where `[VERSION]` is the first model version where
that metadata applied. Requesting metadata for a model version will return the metadata which applies to that version, (which may have been 
first introduced in an earlier version). The metadata response includes a `modelVersion` property - this value will be the `modelVersion`
requested in the query string, if provided. If `modelVersion` was not provided in the query string, the returned 
model version will be the most recent metadata's `[VERSION]`. 

## Related projects

See the [_daedalus_ package](https://github.com/jameel-institute/daedalus) which implements the DAEDALUS integrated model of economic, social, and health costs of a pandemic.
