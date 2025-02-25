# anyflights (development version)

# anyflights 0.3.5

* Include `tz = "GMT"` argument to `ISOdatetime()` so that weather output isn't 
  affected by the user's local timezone (#25, @ismayc).
* Fill in missing values of `temp`, `dewp`, `humid`, `precip`, and `pressure` 
  since they are only recorded once an hour in the source data (#25, @ismayc).

# anyflights 0.3.4

* Fix typo in documentation about changing timeout in R session options when
`utils::download.file()` fails (#20 by `@patrickvossler18`)

* Resolve download issues with planes data.

# anyflights 0.3.3

* Fix HTML5 NOTEs on R devel.

# anyflights 0.3.2

* Add information about R session timeout option in the error message when
`utils::download.file()` fails (#13 by `@patrickvossler18`)
* Transition continuous integration from Travis to GitHub Actions
* Fix broken URLs for `get_airlines()` data (#14, #15 by `@leoohyama` and `@alex-gable`)

# anyflights 0.3.1

* Fix bug in `as_flights_package()` when `nycflights13` is not installed (#11)
* Add a default `name` argument to `as_flights_package()`

# anyflights 0.3.0

* Add progress updates to `anyflights()` and `get_flights()` (#4)
* Clarify documentation on best practices for downloading data on many
stations and years (#6)
* Performance improvements to `get_weather()` (#8)
* Data packages generated with `as_flights_package()` now pass R CMD check! (#9)

# anyflights 0.2.0

* Significant improvements to stability and performance
* Add `as_flights_package()` function to convert `anyflights()` data
objects to data-only packages
* Add `month` argument to `get_flights()` and `get_weather()`
* Allow users to return data objects without saving to file
* Documentation improvements, bug fixes, and increases in unit testing
coverage


# anyflights 0.1.0

* Original release!
