context("cbs")

test_that("cbs bulk gets current players", {

  r <- cbs_bulk()
  expect_equal(class(r), 'data.frame')
})


test_that("cbs scrape returns one player correctly", {

  p <- cbs(1894641)
  expect_equal(class(p), 'data.frame')

  p <- cbs(1894642)
  expect_equal(class(p), 'data.frame')

})


test_that("cbs scrape errors on known bad player", {
  expect_error(cbs(1894653))
})
