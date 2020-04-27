context("get airlines")

test_that("standard get_airports", {
  skip_on_cran()
  
  airports_ <- get_airports()
  airports_orig <- nycflights13::airports

  expect_equal(ncol(airports_),
               ncol(airports_orig))
  expect_equal(colnames(airports_),
               colnames(airports_orig))
  expect_equal(purrr::map(airports_, class) %>% unlist(),
               purrr::map(airports_orig, class) %>% unlist())
  
})