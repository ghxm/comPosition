#' Find the QMV pivotal position in the Council
#'
#' Identifies the pivotal position under the EU Council's Qualified Majority
#' Voting rules by computing both the left and right pivots and returning their
#' midpoint. The left pivot is found by sweeping positions from low to high; the
#' right pivot by sweeping from high to low. The interval between the two pivots
#' defines the QMV core (the set of positions that cannot be overturned by a
#' qualified majority). The midpoint of this interval is the returned estimate.
#'
#' Under post-Lisbon rules (from 2014-11-01), QMV requires both 55\% of member
#' states and 65\% of the EU population.
#'
#' @param positions numeric vector of country policy positions
#' @param country_id integer vector of ParlGov country IDs (same length as
#'   \code{positions})
#' @param date date (character or Date) for looking up population weights
#' @param threshold_states numeric; fraction of member states required
#'   (default 0.55 for post-Lisbon QMV)
#' @param threshold_pop numeric; fraction of population required
#'   (default 0.65 for post-Lisbon QMV)
#' @param return character; what to return. \code{"midpoint"} (default) returns
#'   the midpoint of the pivot interval. \code{"left"} and \code{"right"} return
#'   the respective endpoint. \code{"interval"} returns a named numeric vector
#'   with both endpoints.
#' @return A single numeric value (or a length-2 named vector if
#'   \code{return = "interval"}). Returns \code{NA} if the pivot cannot be
#'   determined.
#' @export
council_pivot <- function(positions, country_id, date,
                          threshold_states = 0.55, threshold_pop = 0.65,
                          return = "midpoint") {

    if (length(positions) != length(country_id)) {
        stop("positions and country_id must have the same length")
    }

    # Warn if date is before the post-Lisbon dual-threshold regime
    date_parsed <- if (is.character(date)) {
        lubridate::parse_date_time(date, orders = c('ymd', 'dmy'))
    } else {
        date
    }
    if (date_parsed < as.Date("2014-11-01")) {
        warning("council_pivot uses post-Lisbon dual-threshold QMV rules ",
                "(55% states + 65% population). Results for dates before ",
                "2014-11-01 are not meaningful because the proportional ",
                "weights for earlier periods are treaty vote counts, not ",
                "population shares.")
    }

    # Remove NAs pairwise
    valid <- !is.na(positions)
    if (sum(valid) == 0) return(NA)

    positions <- positions[valid]
    country_id <- country_id[valid]

    # Population weights from comPosition's existing data
    pop_weights <- council_voting_weights(country_id, date, type = "proportional")

    if (all(is.na(pop_weights))) return(NA)

    # Remove countries with NA population weights
    valid_pop <- !is.na(pop_weights)
    if (sum(valid_pop) == 0) return(NA)
    positions <- positions[valid_pop]
    country_id <- country_id[valid_pop]
    pop_weights <- pop_weights[valid_pop]

    n <- length(positions)
    state_weights <- rep(1 / n, n)  # uniform for states criterion
    pop_weights <- pop_weights / sum(pop_weights)  # normalize

    # Left pivot: sweep low-to-high
    ord_l <- order(positions)
    cum_states_l <- cumsum(state_weights[ord_l])
    cum_pop_l <- cumsum(pop_weights[ord_l])
    pivot_l_idx <- which(cum_states_l >= threshold_states & cum_pop_l >= threshold_pop)[1]

    # Right pivot: sweep high-to-low
    ord_r <- order(positions, decreasing = TRUE)
    cum_states_r <- cumsum(state_weights[ord_r])
    cum_pop_r <- cumsum(pop_weights[ord_r])
    pivot_r_idx <- which(cum_states_r >= threshold_states & cum_pop_r >= threshold_pop)[1]

    if (is.na(pivot_l_idx) || is.na(pivot_r_idx)) return(NA)

    left_pivot  <- positions[ord_l[pivot_l_idx]]
    right_pivot <- positions[ord_r[pivot_r_idx]]

    switch(return,
           midpoint = (left_pivot + right_pivot) / 2,
           left     = left_pivot,
           right    = right_pivot,
           interval = c(left = left_pivot, right = right_pivot),
           stop('Unknown return type: "', return, '". Use "midpoint", "left", "right", or "interval".')
    )
}
