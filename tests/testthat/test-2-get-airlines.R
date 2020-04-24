context("get airlines")

test_that("standard get_airlines", {
  skip_if(skip_conditions())
  
  airlines_ <- get_airlines()
})

test_that("get_airlines joined to nycflights13", {
  skip_if(skip_conditions())
  
  # grab the airlines data and join it with the nycflights13 flights data
  airlines_ <- get_airlines(flights_data = nycflights13::flights)
  
  # grab the original nycflights13 airlines data
  airlines_orig <- nycflights13::airlines
  
  # expect same nrow, ncol, column names, and column types
  expect_equal(nrow(airlines_), nrow(airlines_orig))
  expect_equal(ncol(airlines_), ncol(airlines_orig))
  expect_equal(colnames(airlines_), colnames(airlines_orig))
  expect_equal(purrr::map(airlines_, class) %>% unlist(),
               purrr::map(airlines_orig, class) %>% unlist())
})