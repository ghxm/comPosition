

values <- function(x, ...) UseMethod('values')

#' Retrieve raw positional values in a dataset for given ID(s)
#'
#' @param input vector of IDs matching the ids in the target data
#' @param date date for the values to retrieve when there are repeated measurements
#' @param data target dataset dataframe
#' @param tolerance_lower Lower tolerance for dates in the target data (see \code{\link{determine_match_date}})
#' @param tolerance_upper Upper tolerance for dates in the target data (see \code{\link{determine_match_date}})
#' @param id_var name of the ID variable in the target data
#' @param value_vars names of the value variables to retrieve form the target data
#' @param date_var name of the date variable in the target data
#' @param return_id_var whether to include the id variable in the return
#' @param additional_vars additional variables from the values dataset to include in return
#'
#' @return a dataframe of the values for the values_vars from the target data for the given input
values.default <- function(input, date = NULL, data, value_vars, tolerance_lower = 365*6, tolerance_upper = 365*2, id_var = attr(data, 'party_id_var'), date_var = attr(data, 'date_var'), return_id_var=FALSE, additional_vars=c()){


    if (return_id_var){
        additional_vars <- c(id_var, additional_vars)
    }

    unique_values <- as.data.frame(data[data[, id_var] %in% unique(input), c(id_var, value_vars, date_var, additional_vars)])

    # handle cases where a date has been set
    if (!is.null(date)){


        # if data date var is not Date, convert in order to prepare for date subsetting
        if (!lubridate::is.Date(data[,date_var])){
            data[, date_var] <- lubridate::parse_date_time(data[,date_var], orders=c('ymd', 'dmy'))
        }

        # subset data to input ids
        unique_values <- as.data.frame(data[data[, id_var] %in% unique(input), c(id_var, value_vars, date_var, additional_vars)])

        # select a corresponding data match date for the date argument
        unique_values$match_date <- sapply(unique_values[,id_var], function(x) determine_match_date(x, date=date, data=unique_values, tolerance_lower=tolerance_lower, tolerance_upper=tolerance_upper, date_var=date_var))

        unique_values$match_date <- lubridate::as_date(unique_values$match_date)

        unique_values <- unique_values[unique_values[,date_var] == unique_values$match_date,]
        unique_values <- unique_values[!is.na(unique_values$match_date),]


    }

    df_ids <- as.data.frame(input)
    names(df_ids) <- id_var
    df_ids$o <- 1:NROW(df_ids)


    # merge input with values and return that (in order to make the output match input vector)
    out <- merge(df_ids, unique_values, by = id_var, all.x=TRUE)

    # make sure out has the same ordering as input
    out[order(out$o),c(additional_vars, value_vars)]


}


values.composition <- function(x, date=format(attr(x, 'date'), "%Y-%m-%d"), data, value_vars, tolerance_lower = 365*6, tolerance_upper = 365*2, id_var = attr(data, 'party_id_var'), date_var = attr(data, 'date_var'), return_id_var=FALSE, additional_vars=c()){

    # @TODO select appropriate id column based on data type
    to <- attr(data, 'type')

    # @TODO if not avaialble, call add_id


    # return values
    values.default(x[,to], date=date, data=data, id_var = id_var, value_vars=value_vars, tolerance_lower = tolerance_lower, tolerance_upper = tolerance_upper, return_id_var=return_id_var, additional_vars=additional_vars)


}


#' Determine a match date for the values to be retrieved from a target dataset
#'
#' @param id ID value for the target datset
#' @param date date to be used for determining a matching date in the target dataset
#' @param data target dataset
#' @param tolerance_lower number of days to look for a matching target date in the past
#' @param tolerance_upper number of days to look for a matching target date in the future
#' @param id_var name of the ID variable in the target data
#' @param date_var name of the date variable in the target data
#'
#' @return The closest matching date value in the target data for the given input date
determine_match_date <- function(id, date, data, tolerance_lower=0, tolerance_upper=0, id_var, date_var){

    date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))

    if(is.na(date)){
        stop(paste('No valid date supplied'))
    }

    dates <- as.character(data[data[,id_var] == id, date_var])
    dates <- unique(dates)
    dates <- na.omit(dates)

    # check for within tolerance
    dates_tolerance <- dates[which(
        (lubridate::as_date(dates)<lubridate::as_date(date) & lubridate::as_date(dates)-lubridate::as_date(date)>=-tolerance_lower) |
            (lubridate::as_date(dates)>=lubridate::as_date(date) & lubridate::as_date(dates)-lubridate::as_date(date)<=tolerance_upper)
            )]

    # return date if within, otherwise returns NA
    if (NROW(dates_tolerance)>0){

        dates_abs_diff <- abs(lubridate::as_date(dates_tolerance)-lubridate::as_date(date))
        min_diff <- dates_tolerance[which(dates_abs_diff == min(dates_abs_diff))]

        # if there are multiple dates with the same (min) distance, select the older over the newer
        min_diff <- min_diff[which(min(min_diff)==min(min_diff))]

        return(min_diff)
    }else{
        return(NA)
    }

}
