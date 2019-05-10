context("test-sat_pca")

data("ga_sourcemedium_wide")
input <- ga_sourcemedium_wide
x <- sat_pca(x = input, min_nonzero = 1, min_val = 0.01)

test_that("output is a list", {
  expect_true(is.list(x))
})

test_that("output is a list", {
  expect_equal(length(x), 4)
})

test_that("1st element is a data frame", {
  expect_true(is.data.frame(x[[1]]))
})

test_that("1st element has same num rows as input", {
  expect_equal(nrow(x[[1]]), nrow(input))
})

test_that("1st element has one fewed col than input", {
  expect_equal(ncol(x[[1]]), ncol(input)-1)
})

test_that("2nd element is numeric", {
  expect_true(is.numeric(x[[2]]))
})

test_that("2nd element dimensionality correct", {
  expect_equal(dim(x[[2]]), dim(input) -c(0,1))
})

test_that("no warnings are returned", {
  expect_silent(
    sat_pca(x =  ga_sourcemedium_wide)
  )
})

