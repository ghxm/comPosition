#' Find the QMV pivotal position in the Council
#'
#' Identifies the pivotal voter position under the EU Council's Qualified
#' Majority Voting rules. Countries are ordered by their policy position and
#' the function finds the position at which both the member-state threshold
#' and the population threshold are simultaneously met.
#'
#' Under post-Lisbon rules (from 2014-11-01), QMV requires both 55\% of member
#' states and 65\% of the EU population. The pivotal voter is the country whose
#' inclusion in the coalition (ordered by position) first satisfies both criteria.
#'
#' @param positions numeric vector of country policy positions
#' @param country_id integer vector of ParlGov country IDs (same length as
#'   \code{positions})
#' @param date date (character or Date) for looking up population weights
#' @param threshold_states numeric; fraction of member states required
#'   (default 0.55 for post-Lisbon QMV)
#' @param threshold_pop numeric; fraction of population required
#'   (default 0.65 for post-Lisbon QMV)
#' @return A single numeric value: the policy position of the pivotal voter.
#'   Returns \code{NA} if the pivot cannot be determined (e.g., all positions
#'   are \code{NA}).
#' @export
council_pivot <- function(positions, country_id, date,
                          threshold_states = 0.55, threshold_pop = 0.65) {

    # Remove NAs pairwise
    valid <- !is.na(positions)
    if (sum(valid) == 0) return(NA)

    positions <- positions[valid]
    country_id <- country_id[valid]

    # Population weights from comPosition's existing data
    pop_weights <- council_voting_weights(country_id, date, type = "proportional")

    if (all(is.na(pop_weights))) return(NA)

    n <- length(positions)
    state_weights <- rep(1 / n, n)  # uniform for states criterion
    pop_weights <- pop_weights / sum(pop_weights, na.rm = TRUE)  # normalize

    # Order countries by position
    ord <- order(positions)
    cum_states <- cumsum(state_weights[ord])
    cum_pop <- cumsum(pop_weights[ord])

    # Pivot = first country where BOTH thresholds are met
    pivot_idx <- which(cum_states >= threshold_states & cum_pop >= threshold_pop)[1]

    if (is.na(pivot_idx)) return(NA)

    positions[ord[pivot_idx]]
}
