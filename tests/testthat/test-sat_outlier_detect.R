context("test-sat_outlier_detect")

data("google_analytics_data")

input <- google_analytics_data

fit <- sat_outlier_detect(x =  input$conversion_rate,
                          wt = input$sessions,
                          index = input$ds,
                          tr = TRUE)


test_that("returns a data frame", {
  expect_true(is.data.frame(fit))
})

test_that("output has 6 cols", {
  expect_equal(ncol(fit), 6)
})

test_that("output has same number of rows as input", {
  expect_equal(nrow(input), nrow(fit))
})

test_that("output has correct colnames", {
  expect_equal(colnames(fit), c("index", "data", "residuals", "resid_lower", "resid_upper", "outlier"))
})

test_that("outlier col is factor", {
  expect_true(is.factor(fit$outlier))
})

test_that("no warnings are returned", {
  expect_silent(
    sat_outlier_detect(x =  google_analytics_data$conversion_rate)
    )
})





