foo <- function() "something complicted"

library(testthat)


test_that("foo works", {
  local_edition(3)
  out <- foo()
  expect_type(out, "character")
  expect_snapshot_output(out)

})
testthat::test_file()

packageDescription("testthat")
