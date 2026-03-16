#' comPosition: EU institutional compositions and positions across time
#'
#' R package for creating EU institutional composition and position datasets
#' across time using ParlGov, Manifesto Project, and Party Facts data.
#'
#' @section Data sources:
#' The package downloads data from:
#' \itemize{
#'   \item ParlGov (elections, cabinets, commissioners): \url{https://parlgov.org}
#'   \item Manifesto Project (party positions): \url{https://manifesto-project.wzb.eu} (API key required)
#'   \item Party Facts (ID linking): \url{https://partyfacts.herokuapp.com}
#' }
#'
#' @section Data cutoffs:
#' All dataset objects carry a \code{data_cutoff} attribute indicating the most recent
#' observation date. The composition functions (\code{\link{ep}}, \code{\link{council}},
#' \code{\link{commission}}) warn when a requested date exceeds this cutoff.
#' Access it via \code{attr(my_dataset, "data_cutoff")}.
#'
#' @section Key functions:
#' \itemize{
#'   \item Compositions: \code{\link{ep}}, \code{\link{council}}, \code{\link{commission}}
#'   \item Positions: \code{\link{manifesto_positions}}, \code{\link{calculate_manifesto_positions}}, \code{\link{manifesto_country_positions}}
#'   \item Data loading: \code{\link{manifesto_dataset}}, \code{\link{parlgov_dataset}}, \code{\link{parlgov_commission_dataset}}
#'   \item ID conversion: \code{\link{convert_id_table}}, \code{\link{partyfacts_linktable}}
#' }
#'
"_PACKAGE"


# position object
# hold information on input dataset for which the posiitons are to be calculated
# hold information on ids in position dataset (links basically)
# variables etc
# dimensions

# composition object
# hold information on timeframe, type, varibales

# Function to create a position object holding raw dimension/CMP variable values

# -> can then be passed to a calculation function

# Function to create a composition object holding party_ids


# Position object
# has an linktable
# has a position value datset
# when given a list of varnames and ids, reutrns raw values, claculated positions etc
