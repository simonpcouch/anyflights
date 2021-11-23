context("as_flights_package")

test_that("as_flights_package works", {
  
  skip_on_cran()
  skip_on_ci()
  
  test_data <- list(flights = dplyr::sample_n(nycflights13::flights, 30),
                    weather = dplyr::sample_n(nycflights13::weather, 30),
                    airports = dplyr::sample_n(nycflights13::airports, 30),
                    planes = dplyr::sample_n(nycflights13::planes, 30),
                    airlines = nycflights13::airlines)
  
  as_flights_package(test_data, "testflights13")
  
  expect_true(file.exists("testflights13/R/flights.R"))
  expect_true(file.exists("testflights13/man/flights.Rd"))
  expect_true(file.exists("testflights13/data/flights.rda"))
  expect_true(file.exists("testflights13/testflights13.Rproj"))
  
  unlink("testflights13", recursive = TRUE)
  
})
