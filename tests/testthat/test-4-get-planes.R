context("get planes")

test_that("standard get_planes", {
  skip_on_cran()
  
  planes_ <- get_planes(2018)
})

test_that("get_planes joined to nycflights13", {
  skip_on_cran()

  # grab the planes data and join it with the nycflights13 flights data
  planes_ <- get_planes(2013, flights_data = nycflights13::flights)

  # grab the original nycflights13 planes data
  planes_orig <- nycflights13::planes

  # expect same nrow, ncol, column names, and column types
  expect_equal(ncol(planes_), ncol(planes_orig))
  expect_equal(colnames(planes_), colnames(planes_orig))
  expect_equal(purrr::map(planes_, class) %>% unlist(),
               purrr::map(planes_orig, class) %>% unlist())
})
