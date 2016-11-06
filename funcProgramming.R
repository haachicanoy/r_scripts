# Functional programming using Map() and Reduce()
# Source: http://www.brodrigues.co/functional_programming_and_unit_testing_for_data_munging/
# H. Achicanoy, 2016

## To take in account
# 1. Make your functions referentially transparent.
# 2. Avoid side effects (if possible).
# 3. Make your functions do one thing (if possible).
# 4. A function that takes another function as an argument is called an higher-order function.
# You can write your own higher-order functions and this is a way of having short and easily testable functions.
# Making these functions then work together is trivial and is what makes functional programming very powerful.

# Create a nice function (here using a loop)
sqrt_newton <- function(a, init, eps = 0.01){
  while(abs(init**2 - a) > eps){
    init <- 1/2 *(init + a/init)
  }
  return(init)
}

sqrt_newton(a = 16, init = 2)

## Map() uses

# Apply this function to a vector of numbers
# Option 1: returns a vector with the answers but also warning messages
numbers <- c(16, 25, 36, 49, 64, 81)
res <- sqrt_newton(numbers, init = rep(1, 6), eps = rep(0.001, 6))

# Option 2: returns a list with the answers without errors
res <- Map(sqrt_newton, numbers, init = 1)
# Map() applies a function to every element of a list and returns a list

# Option 3
sqrt_newton_vec <- function(numbers, init, eps = 0.01){
  return(Map(sqrt_newton, numbers, init, eps))
}
res <- sqrt_newton_vec(numbers, 1)

# Option 4
res <- lapply(numbers, sqrt_newton, init = 1)

# Option 5
res <- sapply(numbers, sqrt_newton, init = 1)

# Option 6
sqrt_newton_vec <- function(numbers, init, eps = 0.01){
  return(sapply(numbers, sqrt_newton, init, eps))
}

res <- sqrt_newton_vec(numbers, 1)

# Option 7
inits <- c(100, 20, 3212, 487, 5, 9888)
res <- mapply(sqrt_newton, numbers, init = inits)

## Reduce() uses

Reduce(`+`, numbers, init = 0)

# Example
my_min <- function(a, b){
  if(a < b){
    return(a)
  } else {
    return(b)
  }
}

Reduce(my_min, numbers)
min(numbers)

# Unit testing
# Source: http://www.brodrigues.co/functional_programming_and_unit_testing_for_data_munging/
# H. Achicanoy, 2016

# Classical way
sqrt_newton(4, 1)

# Using testthat package
options(warn = -1)
options(scipen = 999)
suppressMessages(library(testthat))

# Main steps
# 1. Write a file containing your tests
# 2. Run the tests

# Test did not passes
test_that("Test sqrt_newton: positive numeric",{
  expected <- 2 # Define the result we expect
  actual <- sqrt_newton(4, 1) # Code to test
  expect_equal(expected, actual) # Compare
})

# Test passes
test_that("Test sqrt_newton: positive numeric",{
  eps <- 0.001
  expected <- 2
  actual <- sqrt_newton(4, 1, eps = eps)
  expect_lt(abs(expected - actual), eps)
})

# Finding errors
test_that("Test sqrt_newton: negative numeric",{
  expect_error(sqrt_newton(-4, 1))
})
# We found it doesn't deal with negative numbers
# We found the while loop may run forever if the condition is never fulfilled (for example if eps is too small

# Rewrite our function
sqrt_newton <- function(a, init, eps = 0.01){
  stopifnot(a >= 0)
  while(abs(init**2 - a) > eps){
    init <- 1/2 *(init + a/init)
  }
  return(init)
}

# Check errors again
test_that("Test sqrt_newton: negative numeric",{
  expect_error(sqrt_newton(-4, 1))
})

# Now looking for errors in loop step
sqrt_newton(49, 1E100000, 1E-100000)

# Rewrite function with a limited number of iterations
sqrt_newton <- function(a, init, eps = 0.01){
  stopifnot(a >= 0)
  i <- 1
  while(abs(init**2 - a) > eps){
    init <- 1/2 *(init + a/init)
    i <- i + 1
    if(i > 100) stop("Maximum number of iterations reached")
  }
  return(init)
}

# Now if we try to run it
sqrt_newton(49, 1E100, 1E-100)

# Rewrite function allowing user change number of iterations
sqrt_newton <- function(a, init, eps = 0.01, iter = 100){
  stopifnot(a >= 0)
  i <- 1
  while(abs(init**2 - a) > eps){
    init <- 1/2 *(init + a/init)
    i <- i + 1
    if(i > iter) stop("Maximum number of iterations reached")
  }
  return(init)
}

# Testing code again
test_that("Test sqrt_newton: not enough iterations",{
  expect_error(sqrt_newton(4, 1E100, 1E-100, iter = 100))
})

# Testing code using a script for that
test_file('GitHub/r_scripts/testingFunctions.R')

# It is possible to prove a testing scripts in a folder
test_dir("tests") # Here tests is a folder where all scripts are

# Using bash script

# !/bin/sh
# Rscript -e "testthat::test_that('/whole/path/to/your/tests')"











