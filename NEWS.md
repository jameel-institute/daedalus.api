# daedalus.api 0.1.13

This patch version changes the default model options for a dashboard run, and is intended to be used for the READI-TF workshop and launch event.

- Closures are timed and not reactive, and active from 50 -- 200 days;

- All infections have no infection-derived immunity waning, with $\rho$ set to 0.0;

- All vaccination options enforce essentially no immunity waning.

# daedalus.api 0.1.12

This patch version enforces use of _daedalus_ > v0.3.6 (not yet released; see [this PR](https://github.com/jameel-institute/daedalus/pull/154)), which updated community contacts scaling.
This PR also updates for _daedalus_ > 0.3.5 and adds a call to `daedalus::get_data()` on the model output in the run script `R/model_run.R` to account for changes to the `<daedalus_output>` class in daedalus PR #156.

# daedalus.api 0.1.11

This patch version migrates the building and storing of Docker images from Buildkite to GitHub Container Registry.

# daedalus.api 0.1.10

This patch version enforces use of _daedalus_ >= v0.3.2 with corrected contact matrix scaling.

# daedalus.api 0.1.9

This patch version prevents the behavioural mechanism from being sensitive to the user-provided hospital capacity. The country default hospital capacity is used instead.

# daedalus.api 0.1.8

This patch version introduces a choice of behavioural model under the user-facing label 'Change in public behaviour' (PR #49).

- Updated `model_run.R` to pass a user-specified behavioural response choice to `daedalus::daedalus()`; this uses the 'new' behavioural model via `daedalus::daedalus_new_behaviour()`;

- Adds a small exported function to help process the behavioural response choice; note that the mapping from users' behaviour choice to the parameter $\bar B$ is reversed ("low": high optimism, less protective behaviour; "high": low optimism, more protective behaviour). The default behavioural effectiveness $\delta$ = 0.2, with responsiveness parameter $k_2$ = 0.01.

- Updates the API to return behaviour choice descriptions (added via internal function), and updates the metadata display file to show options;

- Updates mock data, some tests and `model_run.R` for changes in _daedalus_ v0.2.36 and above.

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
