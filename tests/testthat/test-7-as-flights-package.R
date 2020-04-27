context("as_flights_package")

test_that("as_flights_package works", {

  skip_on_cran()
  skip_on_ci()
  
  toy_data <- list(flights = dplyr::sample_n(nycflights13::flights, 30),
                   weather = dplyr::sample_n(nycflights13::weather, 30),
                   airports = dplyr::sample_n(nycflights13::airports, 30),
                   planes = dplyr::sample_n(nycflights13::planes, 30),
                   airlines = nycflights13::airlines)
  
  as_flights_package(toy_data, "toyflights13")
  
  expect_true(file.exists("toyflights13/R/toyflights13-package.R"))
  expect_true(file.exists("toyflights13/man/flights.Rd"))
  expect_true(file.exists("toyflights13/data/flights.rda"))
  expect_true(file.exists("toyflights13/toyflights13.Rproj"))
  
  unlink("toyflights13", recursive = TRUE)

})
