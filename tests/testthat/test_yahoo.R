context("yahoo")

test_that("yahoo response returns valid player data", {

  y <- yahoo(8861)

  expect_is(y, 'data.frame')
  expect_equal(y$name, 'Mike Trout')
})


test_that("yahoo response raises error for known problematic id", {

  expect_error(yahoo(1899))

})
