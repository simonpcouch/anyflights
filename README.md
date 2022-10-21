
# anyflights <a href='https://anyflights.netlify.app/'><img src='https://raw.githubusercontent.com/simonpcouch/anyflights/main/figs/logo.png' align="right" height="300" /></a>

[![CRAN
status](https://www.r-pkg.org/badges/version/anyflights)](https://cran.r-project.org/package=anyflights)
[![Build 
status](https://github.com/simonpcouch/anyflights/workflows/R-CMD-check/badge.svg)](https://github.com/simonpcouch/anyflights/actions)
[![Codecov test
coverage](https://codecov.io/gh/simonpcouch/anyflights/branch/main/graph/badge.svg)](https://app.codecov.io/gh/simonpcouch/anyflights?branch=main)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/da4987b7-5dce-4f76-bd5d-a40b0eaeca5a/deploy-status)](https://anyflights.netlify.app/)

The `anyflights` package supplies a set of functions to generate air
travel data (and data packages\!) similar to
[`nycflights13`](https://github.com/tidyverse/nycflights13). With a
user-defined year and airport, the `anyflights` function will grab data
on:

  - `flights`: all flights that departed a given airport in a given year
    and month
  - `weather`: hourly meterological data for a given airport in a given
    year and month
  - `airports`: airport names, FAA codes, and locations
  - `airlines`: translation between two letter carrier (airline) codes
    and names
  - `planes`: construction information about each plane found in
    `flights`

The package also exports a set of functions to query each of the above
datasets individually. Each of the functions are named `get_*` followed
with the names of the datasets (shown above in `code`), and require a
varying combination of the `station` (character vector of FAA LID
airport codes), `year` (as an integer), and `month` (integer vector)
arguments. Optionally, the user can also supply a `dir` argument
(filepath to a folder) to save the resulting datasets.

These functions require a *strong* connection to run; expect several
minutes of runtime, especially for the `anyflights` and `get_flights`
functions.

The FAA usually posts all of the data for a given year during February
or March of the following year. All airports with an FAA LID code are
supported, to my knowledge. Data queries earlier than 2013 are
significantly less thoroughly tested than those 2013 and on.

### Installation

-----

`anyflights` is now on CRAN\! Install with the following code:

``` r
install.packages("anyflights")
```

To install the developmental version, use the following code:

``` r
#install.packages("remotes")
remotes::install_github("simonpcouch/anyflights")
```

### Basic Usage

-----

To grab a named list of 5 dataframes giving `nycflights13`-like air
travel data for the Portland International Airport during June 2019, you
could call:

``` r
pdxflights19 <- anyflights("PDX", 2019, 6)
```

To grab the original `nycflights13` datasets, try:

``` r
nycflights13_ <- anyflights(c("JFK", "LGA", "EWR"), 2013)
```

The package also provides an `as_flights_package` function to create a
data-only package from the data outputted by `anyflights`. To create the
package from the data, just supply a name:

``` r
pdxflights19 %>%
  as_flights_package("pdxflights")
```

and check out the data-only package in the `pdxflights` directory\! 🐛

If you have used `anyflights` to create a data package, please link me
to the repository and I might feature it here\! Please do not submit the
outputted package to CRAN or similar repositories as original packages.

## Contributing

-----

Contributions to `anyflights`, whether in the form of bug fixes, issue
reports, new code or documentation improvements are encouraged and
welcome. Please see
[CONTRIBUTING.md](https://github.com/simonpcouch/anyflights/blob/main/CONTRIBUTING.md)
if you’re interested in contributing\!
