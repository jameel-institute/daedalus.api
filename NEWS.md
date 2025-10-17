# daedalus.api 0.1.7

- Updated the default option for the country parameter from Thailand to United Kingdom.

# daedalus.api 0.1.6

This patch version updates cost and results outputs (PR #47).

- Updates to function `get_nested_costs()` and JSON schema `scenarioCosts.json` to return costs as an array of metric-value pairs; this allows costs to be expressed in different units.

- Updates to results schema `scenarioResults.json` to return the age-specific VSL for countries.

- Updates to metadata schema to return metric and value for costs with multiple units.

- Added results list names and time-series names as package constants.

- Moved `cost_item()` function from inline helper to internal package function.

- Updates to tests helper functions; added helper tests for nested list outputs in `R/tests.R`.

# daedalus.api 0.1.5

- Update `model_run()` daedalus model runner to error if a country hospital capacity <= 0 is passed as this reportedly causes run errors;

- Update `model_run()` to return separate lists for each model NPI;

- Update package to work with _daedalus_ `main` > 0.2.25;

- Removing spelling checks from tests.

# daedalus.api 0.1.4

- Update metadata for policy responses parameter to denote that this parameter's options are not to be considered as ordered.

# daedalus.api 0.1.3

The changelog for this package begins here.

- Function `model_run()` uses new version of `daedalus()` with hospital capacity parameter moved to `daedalus_country` class;

- Internal costs function `get_nested_costs()` accesses `<daedalus_output>` member `life_value` for health costs.

- Importing new package _daedalus.data_ for some data.

- Updates to formatting (using `air`) and infrastructure to fix lints and make docs easier to find online.
