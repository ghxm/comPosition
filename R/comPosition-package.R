#' @importFrom stats aggregate ave median na.omit quantile sd weighted.mean
#' @importFrom utils read.csv read.delim type.convert
NULL

# Suppress R CMD check NOTEs for variables used in non-standard evaluation
# or due to parameter name mismatches in unimplemented code paths
utils::globalVariables(c("committee_name", "group_by_at", "mutate",
                         "row_number"))
