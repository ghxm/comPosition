#' Nice Treaty QMV vote thresholds by period
#'
#' Returns the QMV qualified majority threshold (as a proportion of total
#' treaty votes) for a given date under the Nice Treaty voting rules.
#'
#' @param date date (character or Date)
#' @return Numeric threshold (proportion), or \code{NA} if the date is before
#'   the Nice Treaty voting rules (pre-2004-11-01).
#' @keywords internal
nice_qmv_threshold <- function(date) {
    date <- as.Date(date)
    if (date >= as.Date("2013-07-01")) return(260 / 352)  # EU28
    if (date >= as.Date("2007-01-01")) return(255 / 345)  # EU27
    if (date >= as.Date("2004-11-01")) return(232 / 321)  # EU25
    if (date >= as.Date("1995-01-01")) return(62 / 87)    # EU15
    NA
}


#' Find the QMV pivotal position in the Council
#'
#' Identifies the pivotal position under the EU Council's Qualified Majority
#' Voting rules by computing both the left and right pivots and returning their
#' midpoint. The left pivot is found by sweeping positions from low to high; the
#' right pivot by sweeping from high to low. The interval between the two pivots
#' defines the QMV core (the set of positions that cannot be overturned by a
#' qualified majority). The midpoint of this interval is the returned estimate.
#'
#' The function auto-detects the voting regime based on the date:
#' \itemize{
#'   \item \strong{Post-Lisbon} (from 2014-11-01): dual threshold requiring
#'     55\% of member states and 65\% of the EU population.
#'   \item \strong{Pre-Lisbon / Nice Treaty} (2004-11-01 to 2014-10-31):
#'     threshold based on treaty vote weights (e.g., 255/345 for EU27) plus a
#'     simple majority of member states. Note: the Nice Treaty formally also
#'     required 62\% of total EU population (verifiable on request by any member
#'     state), but this criterion was rarely binding in practice and is not
#'     implemented here.
#' }
#'
#' The \code{regime} parameter can be used to force a specific calculation mode
#' regardless of the date. \code{"unanimity"} is also available: it requires
#' 100\% of member states, making every country pivotal. Under unanimity the
#' core spans the full range of positions and the midpoint is
#' \code{(min + max) / 2}. Weights are ignored. The same result can be achieved
#' manually with any regime by setting \code{threshold_states = 1.0} (and
#' \code{threshold_pop = 1.0} for Lisbon).
#'
#' Note that forcing \code{"lisbon"} on pre-2014 dates is not meaningful
#' because the proportional weights for earlier periods are treaty vote counts,
#' not population shares.
#'
#' @param positions numeric vector of country policy positions
#' @param country_id integer vector of ParlGov country IDs (same length as
#'   \code{positions})
#' @param date date (character or Date) for looking up voting weights and
#'   auto-detecting the regime
#' @param regime character; force a specific voting regime. \code{"auto"}
#'   (default) detects from the date. \code{"lisbon"} forces the post-Lisbon
#'   dual threshold. \code{"nice"} forces the Nice Treaty single threshold.
#'   \code{"unanimity"} requires all member states (weights are irrelevant).
#' @param threshold_states numeric; fraction of member states required.
#'   Defaults to 0.55 for Lisbon, (floor(n/2)+1)/n for Nice (strict majority).
#'   Only used when not auto-detected or when overriding.
#' @param threshold_pop numeric; fraction of population required (Lisbon only,
#'   default 0.65).
#' @param threshold_votes numeric; fraction of treaty votes required (Nice only).
#'   If \code{NULL} (default), looked up automatically from the date.
#' @param return character; what to return. \code{"midpoint"} (default) returns
#'   the midpoint of the pivot interval. \code{"left"} and \code{"right"} return
#'   the respective endpoint. \code{"interval"} returns a named numeric vector
#'   with both endpoints.
#' @return A single numeric value (or a length-2 named vector if
#'   \code{return = "interval"}). Returns \code{NA} if the pivot cannot be
#'   determined.
#' @export
council_pivot <- function(positions, country_id, date,
                          regime = "auto",
                          threshold_states = NULL,
                          threshold_pop = 0.65,
                          threshold_votes = NULL,
                          return = "midpoint") {

    if (length(positions) != length(country_id)) {
        stop("positions and country_id must have the same length")
    }

    # Parse date
    date_parsed <- if (is.character(date)) {
        lubridate::parse_date_time(date, orders = c('ymd', 'dmy'))
    } else {
        date
    }

    # Auto-detect regime
    if (regime == "auto") {
        regime <- if (date_parsed >= as.Date("2014-11-01")) "lisbon" else "nice"
    } else if (regime == "lisbon" && date_parsed < as.Date("2014-11-01")) {
        warning("Forcing Lisbon regime on a pre-2014 date. The proportional ",
                "weights for this period are treaty vote counts, not population ",
                "shares. Results are not meaningful.")
    }

    # Remove NAs pairwise
    valid <- !is.na(positions)
    if (sum(valid) == 0) return(NA)
    positions <- positions[valid]
    country_id <- country_id[valid]

    # Get weights
    weights <- council_voting_weights(country_id, date, type = "proportional")
    if (all(is.na(weights))) return(NA)

    # Remove countries with NA weights
    valid_w <- !is.na(weights)
    if (sum(valid_w) == 0) return(NA)
    positions <- positions[valid_w]
    country_id <- country_id[valid_w]
    weights <- weights[valid_w]

    n <- length(positions)
    state_weights <- rep(1 / n, n)

    if (regime == "lisbon") {
        # Post-Lisbon: dual threshold (states + population)
        if (is.null(threshold_states)) threshold_states <- 0.55
        pop_weights <- weights / sum(weights)

        pivot_func <- function(ord) {
            cs <- cumsum(state_weights[ord])
            cp <- cumsum(pop_weights[ord])
            which(cs >= threshold_states & cp >= threshold_pop)[1]
        }

    } else if (regime == "nice") {
        # Pre-Lisbon: Nice Treaty vote weights + majority of states (> 50%)
        if (is.null(threshold_states)) threshold_states <- (floor(n / 2) + 1) / n
        if (is.null(threshold_votes)) {
            threshold_votes <- nice_qmv_threshold(date)
            if (is.na(threshold_votes)) {
                warning("No Nice Treaty QMV threshold available for date ", date,
                        ". Returning NA.")
                return(NA)
            }
        }
        vote_weights <- weights / sum(weights)

        pivot_func <- function(ord) {
            cs <- cumsum(state_weights[ord])
            cv <- cumsum(vote_weights[ord])
            which(cs >= threshold_states & cv >= threshold_votes)[1]
        }

    } else if (regime == "unanimity") {
        # Every member state must agree; weights are irrelevant
        pivot_func <- function(ord) {
            cs <- cumsum(state_weights[ord])
            which(cs >= 1.0)[1]
        }

    } else {
        stop('Unknown regime: "', regime, '". Use "auto", "lisbon", "nice", or "unanimity".')
    }

    # Left pivot: sweep low-to-high
    ord_l <- order(positions)
    pivot_l_idx <- pivot_func(ord_l)

    # Right pivot: sweep high-to-low
    ord_r <- order(positions, decreasing = TRUE)
    pivot_r_idx <- pivot_func(ord_r)

    if (is.na(pivot_l_idx) || is.na(pivot_r_idx)) return(NA)

    left_pivot  <- positions[ord_l[pivot_l_idx]]
    right_pivot <- positions[ord_r[pivot_r_idx]]

    switch(return,
           midpoint = (left_pivot + right_pivot) / 2,
           left     = left_pivot,
           right    = right_pivot,
           interval = c(left = left_pivot, right = right_pivot),
           stop('Unknown return type: "', return,
                '". Use "midpoint", "left", "right", or "interval".')
    )
}
