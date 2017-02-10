library(leos.opportunity.estimator)
context('Test epidemiological week operations')

test_that('Create epiweekyear data from date string', {
  df <- data.frame(list(date='2010-01-01'))
  expect_equal(generate.columns.from.date(df, 'date')$epiweek, 52)
  expect_equal(generate.columns.from.date(df, 'date')$epiyear, 2009)
  expect_equal(generate.columns.from.date(df, 'date')$epiyearweek, '2009W52')
})

test_that('Create epiweekyear data from Date', {
  df <- data.frame(list(date=as.Date('2010-01-01')))
  expect_equal(generate.columns.from.date(df, 'date')$epiweek, 52)
  expect_equal(generate.columns.from.date(df, 'date')$epiyear, 2009)
  expect_equal(generate.columns.from.date(df, 'date')$epiyearweek, '2009W52')
})

test_that('Create epiweek and year data from epiweekyear', {
  df <- data.frame(list(epiyearweek='2009W52'))
  expect_equal(generate.columns.from.epiyearweek(df, 'epiyearweek')$epiweek, 52)
  expect_equal(generate.columns.from.epiyearweek(df, 'epiyearweek')$epiyear, 2009)
})

test_that('Check episem operation', {
  expect_equal(episem('2009-01-01'), '2008W53')
})

test_that('Check previous epiweek operation', {
  expect_equal(previous.epiyearweek.from.date('2009-01-07'), '2008W53')
})
