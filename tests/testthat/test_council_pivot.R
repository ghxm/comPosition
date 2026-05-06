test_that("council_pivot returns a position between min and max", {
  # 5 large EU countries with known positions
  country_ids <- c(54, 43, 26, 27, 74)  # DEU, FRA, ITA, ESP, POL
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= min(positions) && result <= max(positions))
})

test_that("council_pivot midpoint is symmetric under position negation", {
  # If we negate all positions, the midpoint should also negate
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result_pos <- council_pivot(positions, country_ids, "2022-01-01")
  result_neg <- council_pivot(-positions, country_ids, "2022-01-01")
  expect_equal(result_pos, -result_neg, tolerance = 1e-10)
})

test_that("council_pivot midpoint is between left and right (or equal)", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  midpoint <- council_pivot(positions, country_ids, "2022-01-01", return = "midpoint")
  interval <- council_pivot(positions, country_ids, "2022-01-01", return = "interval")

  # Midpoint is always the average of left and right, regardless of ordering
  expect_equal(midpoint, unname((interval["left"] + interval["right"]) / 2))
})

test_that("council_pivot core can be empty (left > right) with asymmetric weights", {
  # With highly asymmetric population weights (e.g., DEU), the left pivot
  # may exceed the right pivot, indicating an empty QMV core
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  interval <- council_pivot(positions, country_ids, "2022-01-01", return = "interval")
  # Just verify we get two numeric values
  expect_length(interval, 2)
  expect_true(all(is.numeric(interval)))
})

test_that("council_pivot handles NA positions", {
  country_ids <- c(54, 43, 26)
  positions <- c(NA, NA, NA)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.na(result))
})

test_that("council_pivot handles partial NA positions", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, NA, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= -1.0 && result <= 1.0)
})

test_that("council_pivot return parameter works", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  expect_true(is.numeric(council_pivot(positions, country_ids, "2022-01-01", return = "left")))
  expect_true(is.numeric(council_pivot(positions, country_ids, "2022-01-01", return = "right")))
  expect_length(council_pivot(positions, country_ids, "2022-01-01", return = "interval"), 2)
  expect_error(council_pivot(positions, country_ids, "2022-01-01", return = "invalid"))
})
