library(leos.opportunity.estimator)
context('Test notifications aggregate')

test_that('Create weekly notification from single case registry', {
  cases.vec <-c(34, 25, 26, 25, 24, 9)
  epiweek.test <- 52
  epiyear.test <- 2014
  d2 <- cbind(opportunity.example.data,
              epiyearweek=(sapply(opportunity.example.data$DT_NOTIFIC, FUN=episem)))[,c('ID_MUNICIP', 'epiyearweek')]
  expect_true(all(tail(aggregateby.notified.cases(d2, current.epiweek=epiweek.test,
                                           current.epiyear=epiyear.test))$notified_cases == cases.vec))
})
