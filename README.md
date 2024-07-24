
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daedalus.api

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![CRAN
status](https://www.r-pkg.org/badges/version/daedalus.api)](https://CRAN.R-project.org/package=daedalus.api)
[![Codecov test
coverage](https://codecov.io/gh/j-idea/daedalus.api/branch/main/graph/badge.svg)](https://app.codecov.io/gh/j-idea/daedalus.api?branch=main)
[![R-CMD-check](https://github.com/j-idea/daedalus.api/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/j-idea/daedalus.api/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*daedalus.api* is an API package for the [*daedalus*
package](https://github.com/jameel-institute/daedalus) and is primarily
intended for internal use.

## Installation

You can install the development version of daedalus.api from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jameel-institute/daedalus.api")
```

## Quick start

Clone this repository and run the following command from the repository
directory to build the Docker image described by `docker/Dockerfile`.
You will need [Docker Engine](https://docs.docker.com/engine/) for this.
You may also need `sudo` permissions.

``` sh
# the image will be assigned the tag 'latest'
docker build . -f docker/Dockerfile -t daedalus.api:latest

# run the container
# see docker run --help for options
docker run -d --rm -p 8001:8001 daedalus.api:latest

# check root endpoint `GET/`
curl -s http://localhost:8001 | jq
```

Instructions to build directly from this repository will be added soon.

## Related projects

See the [*daedalus*
package](https://github.com/jameel-institute/daedalus) which implements
the DAEDALUS integrated model of economic, social, and health costs of a
pandemic.
