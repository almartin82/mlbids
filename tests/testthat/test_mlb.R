context('mlb')

test_that('mlb returns valid data for known mlbid', {
  m <- mlb(545361)

  expect_is(m, 'data.frame')
  expect_equal(m$name, 'Mike Trout')
})


test_that('mlb returns valid data for known mlbid, no nickname', {
  m <- mlb(545362)

  expect_is(m, 'data.frame')
  expect_equal(m$name, 'Josh Turley')
  expect_equal(m$dob, '8/26/1990 in Texarkana, TX')
})


test_that('mlb throws an error for known bad value', {
  expect_error(mlb(0))
})


test_that('evan longoria test (prononciation field)', {
  m <- mlb(446334)
  expect_equal(m$nickname, 'Longo')
  expect_equal(m$dob, '10/7/1985 in Downey, CA')
})
