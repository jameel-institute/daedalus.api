# daedalus.api 0.1.5

- Update `model_run()` daedalus model runner to error if a country hospital capacity <= 0 is passed as this reportedly causes run errors;

- Update `model_run()` to return separate lists for each model NPI;

- Update package to work with _daedalus_ `main` > 0.2.25.

# daedalus.api 0.1.4

- Update metadata for policy responses parameter to denote that this parameter's options are not to be considered as ordered.

# daedalus.api 0.1.3

The changelog for this package begins here.

- Function `model_run()` uses new version of `daedalus()` with hospital capacity parameter moved to `daedalus_country` class;

- Internal costs function `get_nested_costs()` accesses `<daedalus_output>` member `life_value` for health costs.

- Importing new package _daedalus.data_ for some data.

- Updates to formatting (using `air`) and infrastructure to fix lints and make docs easier to find online.
