

#' Convert party ids between datasets
#'
#' @param id vector of party IDs
#' @param date date for the party IDs (if not set, the function will return all matching ids regardless of recorded party years)
#' @param from the (data) source of the input IDs, e.g. 'parlgov' to convert ParlGov IDs into another ID format
#' @param to the desired output data for the party IDs, e.g. 'manifesto' to obtain the CMP IDs
#' @return A vector of matching IDs in the 'to' data
convert_id <- function(id, date=NA, from, to, linktable = NULL){

    if (is.null(linktable)){
        linktable <- partyfacts_linktable(ignore_years = TRUE)
    }

    # @TODO match by name with fuzzy search

    # make sure linktable is a dataframe for subsetting to work correctly
    linktable <- as.data.frame(linktable)

    id_match_subset <- which(linktable[,from] %in% id)


    # subset the linktable
    if (is.na(date)){
        out <- linktable[id_match_subset,to]
    }else{

        if(!('year_first' %in% names(id_match_subset))){
            stop('linktable does not contain date variables!')
        }

        year <- lubridate::year(lubridate::as_date(date))
        out <- linktable[id_match_subset&linktable$year_first>=year & (linktable$year_last<=year|is.na(linktable$year_last<=year)),to]
    }

    # if no matching ids found
    if (length(out)==0){
        return (NA)
    }else{
        return(out)
    }

}


#' Create a combined linktable of partyfacts and parlgov linktables
#'
linktable <- function(){
    # @TODO
    # use partyfacts
    # fill in with parlgov where partyfacts NA

    partyfacts_linktable(ignore_years=TRUE)


}

#' Create a lookup table from parlgov data
#'
parlgov_linktable <- function(data_url = 'https://partyfacts.herokuapp.com/download/external-parties-csv/'){
    # return dataframe with partyfacts_id data_id_a ...

    parlgov <- parlgov_dataset(type='party')
    parlgov$parlgov <- parlgov$party_id
    parlgov$manifesto <- parlgov$cmp
    parlgov$ches <- parlgov$chess

    parlgov[,c('parlgov', 'manifesto', 'ches', 'ees')]

}


#' Create a partyfacts lookup table
#'
partyfacts_linktable <- function(data_url = 'https://partyfacts.herokuapp.com/download/external-parties-csv/', ignore_years = FALSE){
    # return dataframe with partyfacts_id data_id_a ...
    pf_raw <- download_partyfacts(data_url)

    pf_raw <- pf_raw[!is.na(pf_raw$partyfacts_id),]

    if(ignore_years){
        id_cols <- c('partyfacts_id')
    }else{
        id_cols <- c('partyfacts_id', 'year_first', 'year_last')

    }


    # create IDs for duplicate entries
    pf_raw <- pf_raw %>%
                        group_by_at(c(id_cols, 'dataset_key')) %>%
                        mutate(duplicate_id = row_number())

    pf_wide <- tidyr::pivot_wider(pf_raw, c(id_cols, 'duplicate_id'), names_from = 'dataset_key', values_from = 'dataset_party_id')
    pf_wide <- type.convert(pf_wide, as.is=TRUE)

    # correction for CDU/CSU
    pf_wide[which(pf_wide$partyfacts_id %in% c(1375, 1731)),'manifesto'] <- 41521

    as.data.frame(pf_wide)


}


#' Download the latest partyfacts dataset
#'
#' @param url URL to download the dataset from
#' @return A dataframe of the partyfacts dataset
download_partyfacts <- function(data_url = 'https://partyfacts.herokuapp.com/download/external-parties-csv/'){
    read.csv(url(data_url), as.is=TRUE)
}
