context("get flights")

test_that("standard get_flights (PDX, June 2018)", {
  skip_on_cran()
  
  flights_ <- get_flights("PDX", 2018, 6)
})

test_that("standard get_flights (NYC, February 2013)", {
  skip_on_cran()
  
  flights_2 <- get_flights(c("JFK", "LGA", "EWR"), 2013, 2)
  
  flights_orig <- nycflights13::flights %>% dplyr::filter(month == 2)
  
  expect_equal(nrow(flights_2), nrow(flights_orig))
  expect_equal(ncol(flights_2), ncol(flights_orig))
  expect_equal(colnames(flights_2), colnames(flights_orig))
  expect_equal(purrr::map(flights_2, class) %>% unlist(),
               purrr::map(flights_orig, class) %>% unlist())
})