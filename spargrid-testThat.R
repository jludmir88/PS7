# Creating unit tests
library("testthat")

context("Correct Integration")
# Tests that improper inputs will have errors
test_that("Proper Inputs", {
  ex_1 <- function(x){
    return(x^2)
  }
# If the any lower value is greater than an upper value, there should be an error.
expect_error(spargrid_int_dim_par(ex_1, lower = 2, upper = 1, dim = 1),
  "The lower values must be smaller than the upper values")  
# If the dimension does not equal the number of lower and the number of upper values,
# there should be an error
expect_error(spargrid_int_dim_par(ex_1, lower = 2, upper = 1, dim = 2),
  "The number of dimensions must equal the number of lower and upper values")
# if the lengths of the lower values and upper values are not equal, there should be an error
expect_error(spargrid_int_dim_par(ex_1, lower = c(2,3), upper = 1, dim = 1),
  "The length of lower and upper must be equal")  
})

test_that("Proper Output", {
  ex_1 <- function(x){
    return(x^2)
  }
# Should return correct integrated solution
num <- as.vector(spargrid_int_dim_par(ex_1, lower=c(4), upper=c(11), dim=1))
expect_equal(num, 422.33, tolerance=.01)
  })

