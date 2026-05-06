test_that("council_pivot returns a position between min and max", {
  # Use a post-Lisbon date with known EU27 composition
  # We need real country_ids that exist in the voting weight table
  # DEU=54, FRA=43, ITA=26, ESP=27, POL=74 (5 large countries)
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= min(positions) && result <= max(positions))
})

test_that("council_pivot respects population threshold", {
  # With 5 small countries, population weights are unequal enough
  # that the population threshold (65%) binds later than the states threshold (55%)
  # LUX=7, MLT=72, CYP=51, EST=75, SVN=60
  country_ids <- c(7, 72, 51, 75, 60)
  positions <- c(1, 2, 3, 4, 5)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  # States threshold met at country 3 (cum=0.6), but pop threshold not until
  # country 5 (SVN has the largest weight). Pivot should be 5.
  expect_equal(result, 5)
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
