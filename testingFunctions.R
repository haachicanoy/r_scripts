# Unit testing
# H. Achicanoy, 2016

options(warn = -1)
options(scipen = 999)
suppressMessages(library(testthat))

test_that("Test sqrt_newton: positive numeric",{
  eps <- 0.001
  expected <- 2
  actual <- sqrt_newton(4, 1, eps = eps)
  expect_lt(abs(expected - actual), eps)
})

test_that("Test sqrt_newton: negative numeric",{
  expect_error(sqrt_newton(-4, 1))
})

test_that("Test sqrt_newton: not enough iterations",{
  expect_error(sqrt_newton(4, 1E100, 1E-100, iter = 100))
})

# Introducing an error condition
test_that("Test sqrt_newton: wrong on purpose",{
  eps <- 0.001
  expected <- 12
  actual <- sqrt_newton(4, 1, eps = eps)
  expect_lt(abs(expected - actual), eps)
})