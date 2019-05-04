library(testthat)


context("Test for summary measures")

test_that("aux_mean works as expected", {

  expect_equal(aux_mean(trials = 5, prob = 0.5), 2.5)
  expect_equal(aux_mean(trials = 10, prob = 0.3), 3)
  expect_equal(aux_mean(trials = 10, prob = 0.4), 4)
})
test_that("aux_variance works as expected", {

  expect_equal(aux_variance(trials = 5, prob = 0.5), 1.25)
  expect_equal(aux_variance(trials = 10, prob = 0.3), 2.1)
  expect_equal(aux_variance(trials = 10, prob = 0.4), 2.4)
})
test_that("aux_mode works as expected", {

  expect_equal(aux_mode(trials = 5, prob = 0.5), c(3, 2))
  expect_equal(aux_mode(trials = 10, prob = 0.3), 3)
  expect_equal(aux_mode(trials = 10, prob = 0.4), 4)
})
test_that("aux_skewness works as expected", {

  expect_equal(aux_skewness(trials = 5, prob = 0.5), 0)
  expect_equal(aux_skewness(trials = 10, prob = 0.5), 0)
  expect_equal(aux_skewness(trials = 10, prob = 1), -Inf)
})
test_that("aux_kurtosis works as expected", {

  expect_equal(aux_kurtosis(trials = 5, prob = 0.5), -0.4)
  expect_equal(aux_kurtosis(trials = 10, prob = 0.5), -0.2)
  expect_equal(aux_kurtosis(trials = 10, prob = 1), Inf)
})
