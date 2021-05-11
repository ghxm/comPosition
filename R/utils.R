
#' Keep attributes of an object
#'
#' @param x the new object the attributes shall be applied to
#' @param y the original reference object with the attributes that shall be retained
#' @param force_class class to overwrite values from y
#' @param force_attrs attributes to overwrite values from y
keep_attributes <- function(x, y, force_class = NA, force_attrs = NA){

    if (is.na(force_attrs)){
        # store attribute names not in x
        force_attrs <- attributes(y)[!names(attributes(y)) %in% names(attributes(x))]
    }

    attrs_names <- names(force_attrs)

    if (is.na(force_class)){
        force_class <- class(y)
    }

    class(x) <- force_class

    attributes(x)[attrs_names] <- force_attrs

    x

}
