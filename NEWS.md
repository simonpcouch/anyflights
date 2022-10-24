## v 0.3.3.9000 (developmental version)

To be released as v0.3.4.

* Fix typo in documentation about changing timeout in R session options when
`utils::download.file()` fails (#20 by `@patrickvossler18`)

## v 0.3.3

* Fix HTML5 NOTEs on R devel.

## v 0.3.2

* Add information about R session timeout option in the error message when
`utils::download.file()` fails (#13 by `@patrickvossler18`)
* Transition continuous integration from Travis to GitHub Actions
* Fix broken URLs for `get_airlines()` data (#14, #15 by `@leoohyama` and `@alex-gable`)

## v 0.3.1

----

* Fix bug in `as_flights_package()` when `nycflights13` is not installed (#11)
* Add a default `name` argument to `as_flights_package()`

## v 0.3.0

----

* Add progress updates to `anyflights()` and `get_flights()` (#4)
* Clarify documentation on best practices for downloading data on many
stations and years (#6)
* Performance improvements to `get_weather()` (#8)
* Data packages generated with `as_flights_package()` now pass R CMD check! (#9)

## v 0.2.0

----

* Significant improvements to stability and performance
* Add `as_flights_package()` function to convert `anyflights()` data
objects to data-only packages
* Add `month` argument to `get_flights()` and `get_weather()`
* Allow users to return data objects without saving to file
* Documentation improvements, bug fixes, and increases in unit testing
coverage


## v 0.1.0

----

* Original release!