context("cbs")

test_that("cbs bulk gets current players", {

  r <- cbs_bulk()
  expect_equal(class(r), 'data.frame')
})
