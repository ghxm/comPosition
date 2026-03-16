# Tests for composition and position calculation correctness
# These test the bugs fixed in commit 4ee89e8 to prevent regressions.

# --- subset_by_date ---

test_that("subset_by_date keeps rows where date falls within [start, end]", {
  df <- data.frame(
    id = c("A", "B", "C"),
    start = as.Date(c("2000-01-01", "2010-01-01", "2020-01-01")),
    end = as.Date(c("2005-12-31", "2015-12-31", NA)),
    stringsAsFactors = FALSE
  )

  result <- subset_by_date(df, date = "2012-06-15",
                           date_start_var = "start", date_end_var = "end")

  # B is active (2010-2015), C is active (2020-NA, but started after query date so excluded)
  expect_true("B" %in% result$id)
  # A ended in 2005, must NOT appear
  expect_false("A" %in% result$id)
  # C hasn't started yet
  expect_false("C" %in% result$id)
})

test_that("subset_by_date excludes rows whose period ended before the query date", {
  df <- data.frame(
    id = c("expired", "current"),
    start = as.Date(c("2000-01-01", "2000-01-01")),
    end = as.Date(c("2010-01-01", "2025-01-01")),
    stringsAsFactors = FALSE
  )

  result <- subset_by_date(df, date = "2020-06-15",
                           date_start_var = "start", date_end_var = "end")

  expect_equal(result$id, "current")
})

test_that("subset_by_date includes rows with NA end date (still active)", {
  df <- data.frame(
    id = "ongoing",
    start = as.Date("2000-01-01"),
    end = as.Date(NA),
    stringsAsFactors = FALSE
  )

  result <- subset_by_date(df, date = "2020-01-01",
                           date_start_var = "start", date_end_var = "end")

  expect_equal(NROW(result), 1)
})


# --- subset_to_eu: semantic membership tests ---

test_that("subset_to_eu excludes Croatia before July 2013", {
  # Croatia joined 2013-07-01
  dummy <- dataset(
    data.frame(country_id = eu_accession_dates$country_id,
               value = seq_len(NROW(eu_accession_dates)),
               stringsAsFactors = FALSE),
    type = "parlgov_test"
  )

  result <- subset_to_eu(dummy, date = "2013-01-15")
  hrv_id <- eu_accession_dates$country_id[eu_accession_dates$country_name_short == "HRV"]
  expect_false(hrv_id %in% result$country_id)
})

test_that("subset_to_eu includes Croatia after July 2013", {
  dummy <- dataset(
    data.frame(country_id = eu_accession_dates$country_id,
               value = seq_len(NROW(eu_accession_dates)),
               stringsAsFactors = FALSE),
    type = "parlgov_test"
  )

  result <- subset_to_eu(dummy, date = "2014-01-15")
  hrv_id <- eu_accession_dates$country_id[eu_accession_dates$country_name_short == "HRV"]
  expect_true(hrv_id %in% result$country_id)
})

test_that("subset_to_eu excludes UK after Brexit (2020-01-31)", {
  dummy <- dataset(
    data.frame(country_id = eu_accession_dates$country_id,
               value = seq_len(NROW(eu_accession_dates)),
               stringsAsFactors = FALSE),
    type = "parlgov_test"
  )

  result <- subset_to_eu(dummy, date = "2020-06-15")
  gbr_id <- eu_accession_dates$country_id[eu_accession_dates$country_name_short == "GBR"]
  expect_false(gbr_id %in% result$country_id)
})

test_that("subset_to_eu includes UK before Brexit", {
  dummy <- dataset(
    data.frame(country_id = eu_accession_dates$country_id,
               value = seq_len(NROW(eu_accession_dates)),
               stringsAsFactors = FALSE),
    type = "parlgov_test"
  )

  result <- subset_to_eu(dummy, date = "2019-01-15")
  gbr_id <- eu_accession_dates$country_id[eu_accession_dates$country_name_short == "GBR"]
  expect_true(gbr_id %in% result$country_id)
})

test_that("subset_to_eu respects the date argument, not a hardcoded date", {
  dummy <- dataset(
    data.frame(country_id = eu_accession_dates$country_id,
               value = seq_len(NROW(eu_accession_dates)),
               stringsAsFactors = FALSE),
    type = "parlgov_test"
  )

  # Two dates that should produce different EU membership sets
  result_pre_hrv <- subset_to_eu(dummy, date = "2012-01-01")
  result_post_hrv <- subset_to_eu(dummy, date = "2014-01-01")

  expect_false(identical(sort(result_pre_hrv$country_id),
                         sort(result_post_hrv$country_id)))
})


# --- convert_id_table: year filtering ---

test_that("convert_id_table year filter keeps parties active in the query year", {
  lt <- data.frame(
    from_id = c(1, 2, 3),
    to_id = c(10, 20, 30),
    year_first = c(1990, 2005, 2015),
    year_last = c(2000, NA, 2020),
    stringsAsFactors = FALSE
  )

  # Query year 2010: only party 2 is active (started 2005, no end)
  result <- convert_id_table(1, date = "2010-06-15", from = "from_id",
                             to = "to_id", linktable = lt)
  # Party 1 ended in 2000, should not match
  # Party 3 hasn't started (2015), should not match
  expect_true(is.na(result))

  result2 <- convert_id_table(2, date = "2010-06-15", from = "from_id",
                              to = "to_id", linktable = lt)
  expect_equal(result2, 20)
})

test_that("convert_id_table does not return parties that ended before the query year", {
  lt <- data.frame(
    from_id = c(1, 1),
    to_id = c(10, 11),
    year_first = c(1990, 2005),
    year_last = c(2000, NA),
    stringsAsFactors = FALSE
  )

  result <- convert_id_table(1, date = "2010-06-15", from = "from_id",
                             to = "to_id", linktable = lt)
  # Only the second entry (year_first=2005, year_last=NA) should match
  expect_equal(result, 11)
})


# --- convert_ids: parameter name fix ---

test_that("convert_ids accepts fuzzy parameter without error", {
  lt <- data.frame(
    from_id = c("alpha", "beta"),
    to_id = c(1, 2),
    stringsAsFactors = FALSE
  )

  # Should not error about unknown 'fuzzy' parameter
  expect_no_error(
    convert_ids(c("alpha"), from = "from_id", to = "to_id",
                linktable = lt, fuzzy = FALSE)
  )
})


# --- calculate_manifesto_positions ---

test_that("Lowe log-ratio formula is correct", {
  # Single observation: L=10%, R=20%, N=100 sentences
  L <- data.frame(var1 = 10)
  R <- data.frame(var1 = 20)
  N <- data.frame(total = 100)

  result <- calculate_manifesto_positions(L, R, N, method = 'lowe')

  L_count <- 10
  R_count <- 20
  expected <- log((R_count + 0.5) / (L_count + 0.5))
  expect_equal(as.numeric(result), expected)
})

test_that("Budge formula produces percentage-point scale, not 0-1 scale", {
  # L=10%, R=30%, N=200 -> L_count=20, R_count=60
  # Budge = (60-20)/200 = 0.2 (which is 20 percentage points / 100)
  L <- data.frame(var1 = 10)
  R <- data.frame(var1 = 30)
  N <- data.frame(total = 200)

  result <- calculate_manifesto_positions(L, R, N, method = 'budge')

  # (R_count - L_count) / N = (60-20)/200 = 0.2
  expect_equal(as.numeric(result), 0.2)
})

test_that("Kim-Fording formula is correct", {
  L <- data.frame(var1 = 10)
  R <- data.frame(var1 = 30)
  N <- data.frame(total = 100)

  result <- calculate_manifesto_positions(L, R, N, method = 'kimfording')

  L_count <- 10
  R_count <- 30
  expected <- (R_count - L_count) / (R_count + L_count)
  expect_equal(as.numeric(result), expected)
})


# --- determine_match_date: tie-breaking ---

test_that("determine_match_date returns single date when equidistant dates exist", {
  df <- data.frame(
    id = c(1, 1),
    date_var = c("2018-01-01", "2022-01-01"),
    value = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- determine_match_date(1, date = "2020-01-01", data = df,
                                 tolerance_lower = 365 * 3, tolerance_upper = 365 * 3,
                                 id_var = "id", date_var = "date_var")

  # Both dates are exactly 2 years away; should return the older one
  expect_length(result, 1)
  expect_equal(result, "2018-01-01")
})


# --- manifesto_country_positions: pov_pos/gov_pos fix ---

test_that("manifesto_country_positions works with weighted=FALSE", {
  x <- data.frame(var1 = c(5, 10, 8, 12))
  country_seats <- data.frame(country_id = c(1, 1, 2, 2), seats = c(50, 30, 40, 60))

  # Should not error (previously assigned to pov_pos but referenced gov_pos)
  expect_no_error(
    manifesto_country_positions(x, L_vars = "var1", R_vars = "var1",
                                N_vars = c("var1"),
                                country_seats = country_seats,
                                weighted = FALSE, method = 'lowe')
  )
})
