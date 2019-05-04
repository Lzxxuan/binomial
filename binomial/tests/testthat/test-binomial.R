library(testthat)


context("Test for binomial")

test_that("bin_choose works as expected", {
  expect_equal(bin_choose(n = 5, k = 2), 10)
               expect_equal(bin_choose(5, 0), 1)
               expect_error(bin_choose(5, 6), "k cannot be greater than n")
               expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
})

  test_that("bin_probability works as expected", {
    expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
    expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
    expect_equal(bin_probability(success = 55, trials = 100, prob = 0.45), 0.01075277)
    expect_error(bin_probability(success = 2, trials = 5, prob = 4), "invalid prob value: p has to be a number betwen 0 and 1")
  })

  test_that("bin_distribution works as expected", {
    trials <- 5
    prob <- 0.5
    test_dat <- data.frame("success" = 0: trials, "probability" = 0: trials)
    for (i in 1: nrow(test_dat))
      test_dat[i, 2] <- bin_probability(success = i - 1, trials = trials, prob = prob)
    class(test_dat) <- c("bindis", "data.frame")

    expect_identical(bin_distribution(trials = trials, prob = prob), test_dat)
    expect_is(bin_distribution(trials = trials, prob = prob), c("bindis", "data.frame"))
    expect_identical(dim(bin_distribution(trials = trials, prob = prob)), dim(test_dat))
  })

  test_that("bin_cumulative works as expected", {
    trials <- 5
    prob <- 0.5
    test_dat2 <- bin_distribution(trials = trials, prob = prob)
    test_dat2$cumulative <- 0: trials
    test_dat2[1, 3] <- test_dat2[1,2]
    for (i in 2: nrow(test_dat2))
      test_dat2[i, 3] <- test_dat2[i, 2] + test_dat2[i - 1,3]
    class(test_dat2) <- c("bincum", "data.frame")

    expect_identical(bin_cumulative(trials = trials, prob = prob), test_dat2)
    expect_is(bin_cumulative(trials = trials, prob = prob), c("bincum", "data.frame"))
    expect_identical(dim(bin_cumulative(trials = trials, prob = prob)), dim(test_dat2))
  })
