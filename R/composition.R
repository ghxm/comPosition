
# composition object
# that has time,
# type
# linktable
# ids



# @TODO: create composition constructur
# https://adv-r.hadley.nz/s3.html
new_composition <- function(){

}


# if multiple linked ids for a given input ID (electoral alliance e.g.), average
# to facilitate this, assign identifiers and average over each identifier
# -> do this in a composition object?
# then pass the composition object the positions function whch will ahndle things appropriately

#' Create a composition object
#'
#' @param x The dataframe containing the units of the composition
#' @param from name of input ID origin data
#' @param type character string vector(?) containing the type of compositon (e.g. 'commission', 'ep_pleanry', 'ep_committee')
#' @param name character string containing a name for the composition, used for better overview only
#' @param date the date of the composition
#' @param linktable a dataframe containing linked ID information to convert between different datasets
#' @return a composition S3 object containing the main dataframe in the form of a composition id and a number > 0 of partyfacts_ids per composition id(?) as well as other metadata attributes
composition <- function(x, from, name = attr(x, 'name'), type = attr(x, 'name'), weight_var=attr(x, 'weight_var'), date = NA, linktable = NA, ...){

    # example case:
    # parlgov data is passed in

    if (!('dataset' %in% class(x))){
        stop('Please supply a dataset object')
    }else{
        # get attributes and store
        input_attrs <- attributes(x)
    }

    out <- x

    class(out) <- c('data.frame', 'composition', 'dataset')

    # type of entity
    #attr(out, 'data') <- x

    attr(out, 'from') <- from
    attr(out, 'name') <- name
    attr(out, 'type') <- type
    attr(out, 'date') <- date
    attr(out, 'linktable') <- linktable


    attrs <- list(...)

    if (length(attrs)>0){
        for (i in 1:length(attrs)){
            attr(out, names(attrs[i])) <- attrs[[i]]
        }
    }

    out$composition_id <- 1:NROW(out)

    out
}

# add_unit method ----

#' Add an observation to a composition object
add_unit.composition <- function(x, composition_id = NA, party_id = NA, other){

    # @TODO keep attributes
    keep_attributes(dplyr::bind_rows(x, c(composition_id, party_id, other)), x)

}


# add_id_col method ------

add_id_col <- function(x, ...) UseMethod("add_id_col")

#' Add party_id columns of various types to a dataset
#'
#' @param x a dataset to add party id_columns to
#' @param from party_id variable to use for matching
#' @param add character vector containing the dataset names of the party_ids to add
add_id_col.composition <- function(x, date = NA, to = c(), from = attr(x, 'from'), party_id_var = attr(x, 'party_id_var'), linktable = attr(x, 'linktable')){

    # convert ids, create table
    id_table <- convert_id(x[,party_id_var], date = date, from=from, to=c(from, to), linktable=linktable)

    out <- merge(x, id_table, by.x = party_id_var, by.y=from, all.x=TRUE)

    # return composition object with additional col
    keep_attributes(structure(out), x)

    # @TODO: test! @HIERWEITER


}




#' Subset a dataset by date
#'
#' @param x the dataset to subset
#' @param y optional dataset containing information on a unit's start and end dates, must be set to NA if date information is contained in x
#' @param date date to subset for in character 'YYYY-MM-DD' format
#' @param id_var, id_var.x, id_var.y
#' @param date_start_var composition start date variable name (must be part of either x or y)
#' @return the input dataset subset to to all units for which date_start_var => date <= date_end_var
subset_by_date <- function(x, y=NULL, date, id_var = intersect(names(x), names(y)),
                           id_var.x = id_var, id_var.y = id_var, date_start_var = NA, date_end_var = NA){

    if (is.character(date)){
        date <- lubridate::as_date(date)
    }

    if (!is.null(y)){
        # merge date information to x
        x_y <- merge(x,y[,c(names(y)[!names(y) %in% names(x)], id_var.y)], all.x=TRUE, by = id_var, by.x = id_var.x, by.y=id_var.y)
    } else {
        x_y <- x
    }

    x_y[which(
        date >= x_y[,date_start_var] &
            (date>= x_y[,date_end_var] | is.na(x_y[, date_end_var]))
    ),]

}

