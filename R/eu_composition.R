

#' Return all EU observations for a particular date
#'
#' @param x the dataset to subset
#' @param date specified date for EU composition
#' @param country_id_var name of the country id variable in x
#' @section TODO:
#' - implement linktable country id handling (incl. varying id_vars for x and linktable?)
#' - add option to subset to eu when only supplying party_id (link and then use parlgov to obtain country_id and call recursive?)
#' @export
subset_to_eu <- function(x, date = NA, country_id_var = attr(x, 'country_id_var'), country_id_linktable = NA){

    if (is.character(date)){
        date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))
    }

    # if attr == parlgov
    # use automated counry id var
    # a(nd linktable
    if (grepl('parlgov', attr(x, 'type'))){
        country_id_var <- 'country_id'
        country_id_linktable <- parlgov_country_linktable
    }

    keep_attributes(subset_by_date(x, eu_accession_dates, id_var = country_id_var, date = '2021-01-01', date_start_var = 'eu_accession_date', date_end_var='eu_exit_date'),x)

}








# EP ----

#' Deprecated
determine_ep_election_date <- function(date, election_dates){

    if (typeof(election_dates)=='character'){
        election_dates <- lubridate::parse_date_time(election_dates, orders=c('ymd', 'dmy'))
    }

    if (is.character(date)){
        date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))
    }

    election_dates <- election_dates[order(election_dates)]
    date >= election_dates


    df_election_dates <- as.data.frame(election_dates)
    df_election_dates$year <- lubridate::year(df_election_dates$election_dates)
    df_election_dates$month <- lubridate::month(df_election_dates$election_dates)
    df_election_dates$day <- lubridate::day(df_election_dates$election_dates)


}

#' Create an EP composition
#' @export
ep <- function(date, data, linktable, type='plenary'){

    if (is.character(date)){
        date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))
    }

    if(type == 'plenary'){

        if(missing(data)){
            parlgov_election <- parlgov_dataset(type='election')
        }else {
            parlgov_election <- data
        }

        if(missing(linktable)){
            linktable <- linktable()
        }


        parlgov_election_ep <- keep_attributes(subset_to_eu(parlgov_election, date=date), parlgov_election)



        # subset by date
        parlgov_election_ep <- keep_attributes(parlgov_election_ep[which(parlgov_election_ep$election_type=='ep' &
                                                          lubridate::parse_date_time(parlgov_election_ep$election_date, orders=c('ymd', 'dmy')) - date <= 3 &
                                                          lubridate::parse_date_time(parlgov_election_ep$election_date, orders=c('ymd', 'dmy')) - date >= -365*5+3),], parlgov_election_ep)

        # return composititon object
        composition(parlgov_election_ep, from = 'parlgov', 'EP plenary', type='parlgov_election', date=date, linktable=linktable)

    }

}



# Council ----

#' Create a council composition from cabinet parlgov data
#'
#' @param x a dataset for parties
#'
#' @return the input dataset x subset to contain only parties present in the council for a given date
#' @export
council <- function(date, data, linktable){

    if(is.character(date)){
        date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))
    }

    # subset to EU
    data_eu <- subset_to_eu(data, date)

    data_eu_gov <- keep_attributes(data_eu[which(data_eu$cabinet_party==1 & lubridate::parse_date_time(data_eu$start_date, orders=c('ymd', 'dmy'))<=date),], data_eu)

    # determine cabinet ids by grouping by country, sorting by election data and selecting the newest one
    cabinet_ids <- unique(data_eu_gov[lubridate::parse_date_time(data_eu_gov$start_date, orders=c('ymd', 'dmy')) == ave(lubridate::parse_date_time(data_eu_gov$start_date, orders=c('ymd', 'dmy')), data_eu_gov[,attr(data_eu_gov, 'country_id_var')], FUN=max), 'cabinet_id'])

    data_council <- keep_attributes(data_eu_gov[which(data_eu_gov$cabinet_id %in% cabinet_ids),], data_eu_gov)

    composition(data_council, from='parlgov', name = 'Council', type='parlogv_cabinet', date=date, linktable=linktable)

}
