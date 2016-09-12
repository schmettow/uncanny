
library(testthat)
library(uncanny)

context("General tests")
C_ <- list() # coefficient tales

test_that("basic workflow",{
  expect_silent(uv_process(c(2, 400), full_table = T))
  p = matrix(c(1, 2, 200, 600), 2)
  expect_silent(D <- uv_response(p))
  expect_is(D, "tbl_df")
  expect_is(plot_dashboard(D), "gtable")
  expect_is(plot_response(D), "gg")
  expect_is(plot_process_mix(D), "gg")
})

