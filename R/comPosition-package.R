#' @importFrom stats aggregate ave median na.omit quantile sd weighted.mean
#' @importFrom utils read.csv read.delim type.convert
NULL

# Suppress R CMD check NOTEs for variables used in non-standard evaluation
utils::globalVariables(c("group_by_at", "mutate", "row_number"))
