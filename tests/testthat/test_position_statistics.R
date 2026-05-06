test_that("quantile measure works unweighted", {
  x <- c(1, 2, 3, 4, 5)
  q50 <- position_statistic(x, measure = "quantile", probs = 0.5)
  expect_equal(q50, median(x))

  q25 <- position_statistic(x, measure = "quantile", probs = 0.25)
  expect_equal(q25, quantile(x, 0.25)[[1]])
})

test_that("quantile measure works weighted", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(10, 20, 30, 20, 10)

  q50_weighted <- position_statistic(x, w = w, measure = "quantile", probs = 0.5)
  q50_median <- position_statistic(x, w = w, measure = "median")
  expect_equal(q50_weighted, q50_median)
})

test_that("quantile at 0.5 equals median for both weighted and unweighted", {
  x <- c(3.2, 1.5, 4.8, 2.1, 5.0, 3.7)

  # unweighted
  expect_equal(
    position_statistic(x, measure = "quantile", probs = 0.5),
    position_statistic(x, measure = "median")
  )

  # weighted
  w <- c(5, 10, 15, 20, 25, 30)
  expect_equal(
    position_statistic(x, w = w, measure = "quantile", probs = 0.5),
    position_statistic(x, w = w, measure = "median")
  )
})

test_that("quantile errors when probs is missing", {
  x <- c(1, 2, 3)
  expect_error(position_statistic(x, measure = "quantile"),
               "probs")
})

test_that("quantile handles NA values", {
  x <- c(1, NA, 3, 4, 5)
  result <- position_statistic(x, measure = "quantile", probs = 0.5, na.rm = TRUE)
  expect_equal(result, median(c(1, 3, 4, 5)))
})

test_that("quantile handles proportional weights (sum <= 1)", {
  x <- c(1, 2, 3, 4)
  w <- c(0.1, 0.2, 0.3, 0.4)  # sum = 1.0, triggers normwt = TRUE
  result <- position_statistic(x, w = w, measure = "quantile", probs = 0.75)
  expect_true(is.numeric(result))
  expect_true(result >= min(x) && result <= max(x))
})
