library(testthat)


context("Test for checkers ")

test_that("check_prob works as expected", {

  expect_true(check_prob(0.5))
  expect_error(check_prob(-1))
  expect_error(check_prob("a"))
})


test_that("check_trials works as expected", {

  expect_true(check_trials(1))
  expect_error(check_trials(-1))
  expect_error(check_trials(1.5))
})

test_that("check_success works as expected", {

  expect_true(check_success(trials = 5, success = 4))
  expect_true(check_success(trials = 5, success = 1:3))
  expect_error(check_success(trial = 5, success = 6))
  expect_error(check_success(trial = 5.5, success = 5))
  expect_error(check_success(trial = 5, success = 5.5))
})
