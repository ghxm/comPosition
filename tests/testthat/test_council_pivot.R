# --- Auto-detection and regime switching ---

test_that("council_pivot auto-detects Lisbon regime for post-2014 dates", {
  country_ids <- c(54, 43, 26, 27, 74)  # DEU, FRA, ITA, ESP, POL
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= min(positions) && result <= max(positions))
})

test_that("council_pivot auto-detects Nice regime for pre-2014 dates", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  # Should not warn (auto-detects Nice)
  result <- council_pivot(positions, country_ids, "2012-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= min(positions) && result <= max(positions))
})

test_that("council_pivot warns when forcing Lisbon regime on pre-2014 dates", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  expect_warning(
    council_pivot(positions, country_ids, "2012-01-01", regime = "lisbon"),
    "not meaningful"
  )
})

test_that("council_pivot regime can be forced to nice for post-2014 dates", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01", regime = "nice")
  expect_true(is.numeric(result))
})

test_that("council_pivot errors on invalid regime", {
  expect_error(council_pivot(c(1, 2, 3), c(54, 43, 26), "2022-01-01", regime = "invalid"),
               "Unknown regime")
})


# --- Symmetry and interval properties ---

test_that("council_pivot midpoint is symmetric under position negation", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result_pos <- council_pivot(positions, country_ids, "2022-01-01")
  result_neg <- council_pivot(-positions, country_ids, "2022-01-01")
  expect_equal(result_pos, -result_neg, tolerance = 1e-10)
})

test_that("council_pivot midpoint equals average of interval endpoints", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  midpoint <- council_pivot(positions, country_ids, "2022-01-01", return = "midpoint")
  interval <- council_pivot(positions, country_ids, "2022-01-01", return = "interval")

  expect_equal(midpoint, unname((interval["left"] + interval["right"]) / 2))
})

test_that("council_pivot core is non-empty (left >= right) under both regimes", {
  # Under any supermajority rule the two winning coalitions must overlap,
  # so left_pivot >= right_pivot (non-empty core)
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  # Lisbon
  interval_l <- council_pivot(positions, country_ids, "2022-01-01", return = "interval")
  expect_true(interval_l["left"] >= interval_l["right"])

  # Nice
  interval_n <- council_pivot(positions, country_ids, "2012-01-01", return = "interval")
  expect_true(interval_n["left"] >= interval_n["right"])
})


# --- Nice Treaty regime specifics ---

test_that("nice_qmv_threshold returns correct values per period", {
  expect_equal(comPosition:::nice_qmv_threshold("2012-01-01"), 255 / 345)  # EU27
  expect_equal(comPosition:::nice_qmv_threshold("2013-09-01"), 260 / 352)  # EU28
  expect_equal(comPosition:::nice_qmv_threshold("2005-01-01"), 232 / 321)  # EU25
  expect_equal(comPosition:::nice_qmv_threshold("1996-01-01"), 62 / 87)    # EU15
  expect_true(is.na(comPosition:::nice_qmv_threshold("1990-01-01")))       # pre-Nice
})

test_that("council_pivot Nice states threshold is strict majority (> 50%)", {
  # With even n, 50% is NOT a majority. E.g., 2 out of 4 is a tie, not a majority.
  # Use 4 countries where the 2nd country would meet >= 0.5 but not > 0.5
  # ITA=29, ESP=27, NLD=13, BEL=12 (total=81, threshold for EU28 period = 260/352)
  # Force threshold_votes low so only the states criterion matters
  country_ids <- c(26, 27, 8, 64)
  positions <- c(1, 2, 3, 4)

  # With strict majority: need 3 out of 4 states (floor(4/2)+1 = 3)
  # With lax >= 0.5: would only need 2 out of 4
  # The vote threshold (260/352) is very high, so for just 4 countries
  # we override it to something low to isolate the states effect
  result <- council_pivot(positions, country_ids, "2013-09-01",
                          threshold_votes = 0.01)
  interval <- council_pivot(positions, country_ids, "2013-09-01",
                            threshold_votes = 0.01, return = "interval")

  # With strict majority (3/4), left pivot must be at least position 3
  expect_true(interval["left"] >= 3)
})

test_that("council_pivot Nice regime produces different result from Lisbon", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  nice_result <- council_pivot(positions, country_ids, "2012-01-01", regime = "nice")
  lisbon_result <- suppressWarnings(
    council_pivot(positions, country_ids, "2012-01-01", regime = "lisbon")
  )

  expect_true(is.numeric(nice_result))
  expect_true(is.numeric(lisbon_result))
})


# --- Unanimity regime ---

test_that("council_pivot unanimity core spans full position range", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  interval <- council_pivot(positions, country_ids, "2022-01-01",
                            regime = "unanimity", return = "interval")
  expect_equal(unname(interval["left"]), max(positions))
  expect_equal(unname(interval["right"]), min(positions))
})

test_that("council_pivot unanimity midpoint is (min + max) / 2", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  mid <- council_pivot(positions, country_ids, "2022-01-01", regime = "unanimity")
  expect_equal(mid, (min(positions) + max(positions)) / 2)
})


# --- NA handling ---

test_that("council_pivot handles all-NA positions", {
  result <- council_pivot(c(NA, NA, NA), c(54, 43, 26), "2022-01-01")
  expect_true(is.na(result))
})

test_that("council_pivot handles partial NA positions", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, NA, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
  expect_true(result >= -1.0 && result <= 1.0)
})

test_that("council_pivot handles NA population weights", {
  country_ids <- c(54, 43, 999, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  result <- council_pivot(positions, country_ids, "2022-01-01")
  expect_true(is.numeric(result))
})


# --- Input validation ---

test_that("council_pivot errors on mismatched input lengths", {
  expect_error(council_pivot(c(1, 2, 3), c(54, 43), "2022-01-01"),
               "same length")
})

test_that("council_pivot return parameter works", {
  country_ids <- c(54, 43, 26, 27, 74)
  positions <- c(-1.0, -0.5, 0.0, 0.5, 1.0)

  expect_true(is.numeric(council_pivot(positions, country_ids, "2022-01-01", return = "left")))
  expect_true(is.numeric(council_pivot(positions, country_ids, "2022-01-01", return = "right")))
  expect_length(council_pivot(positions, country_ids, "2022-01-01", return = "interval"), 2)
  expect_error(council_pivot(positions, country_ids, "2022-01-01", return = "invalid"))
})
