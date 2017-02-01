library(leos.opportunity.estimator)
context('Test epidemiological week operations')

test_that('Create epiweekyear data from date string', {
  expect_equal(generate.columns.from.date('2010-01-01')$epiweek, 52)
  expect_equal(generate.columns.from.date('2010-01-01')$epiyear, 2009)
  expect_equal(generate.columns.from.date('2010-01-01')$epiyearweek, '2009W52')
})

test_that('Create epiweekyear data from Date', {
  expect_equal(generate.columns.from.date(as.Date('2010-01-01 22:00:00'))$epiweek, 52)
  expect_equal(generate.columns.from.date('2010-01-01 22:00:00')$epiyear, 2009)
  expect_equal(generate.columns.from.date('2010-01-01 22:00:00')$epiyearweek, '2009W52')
})

test_that('Create epiweek and year data from epiweekyear', {
  expect_equal(generate.columns.from.epiyearweek('2009W52')$epiweek, 52)
  expect_equal(generate.columns.from.epiyearweek('2009W52')$epiyear, 2009)
})

test_that('Check episem operation', {
  expect_equal(episem('2009-01-01'), '2008W53')
})

test_that('Check previous epiweek operation', {
  expect_equal(previous.epiyearweek.from.date('2009-01-07'), '2008W53')
})
