context('mlb')

test_that('mlb returns valid data for known mlbid', {
  m <- mlb(545361)

  expect_is(m, 'data.frame')
  expect_equal(m$name, 'Mike Trout')
})

test_that('mlb throws an error for known bad value', {
  expect_error(mlb(0))
})
