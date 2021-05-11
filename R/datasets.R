
#' Create a dataset S3 object
#'
#' @param x datsaset, e.g. as a dtaframe
#' @param type character string containing the type of dataset (source), e.g. 'parlgov_party'
#' @param ... additional named attributes to be assigned to the dataset object
#' @return A dataset S3 object containing x and additional attributes
dataset <- function(x, type, ...){

    # @TODO set type

    attrs <- list(...)
    class(x) <- c("data.frame", 'dataset')

    attr(x, 'type') <- type


    # manifesto attributes
    if(type == 'manifesto'){
        attr(x, 'party_id_var') <- 'party'
        attr(x, 'party_name_var') <- 'partyname'
        attr(x, 'country_id_var') <- 'country'
        attr(x, 'country_name_var') <- 'countryname'
        attr(x, 'date_var') <- 'edate'

    }

    # parlgov attributes ----

    if(grepl("parlgov", type)){
        attr(x, 'party_id_var') <- 'party_id'
        attr(x, 'party_name_var') <- 'party_name_english'
        attr(x, 'country_id_var') <- 'country_id'
        attr(x, 'country_name_var') <- 'country_name'
        attr(x, 'weight_var') <- NA

    }

    if (type %in% c('parlgov_election', 'parlgov_cabinet')){
        attr(x, 'date_var') <- 'election_date'
        attr(x, 'date_start_var') <- 'start_date'
        attr(x, 'date_end_var') <- 'end_date'
        attr(x, 'weight_var') <- 'seats'

    }







    if(length(attrs)>0){
        for (i in 1:length(attrs)){
            attr(x, names(attrs[i])) <- attrs[[i]]
        }
    }


    x
}




# MANIFESTO (CMP) ----

manifesto_dataset <- function(base_url = 'https://manifesto-project.wzb.eu/down/data/2020b/datasets/MPDataset_MPDS2020b.csv'){

    raw <- download_manifesto(base_url)

    out <- dataset(raw, type='manifesto')


}

download_manifesto <- function(base_url = 'https://manifesto-project.wzb.eu/down/data/2020b/datasets/MPDataset_MPDS2020b.csv'){

    read.csv(base_url, as.is=TRUE)


}


# PARLGOV ----

parlgov_dataset <- function(base_url = 'http://www.parlgov.org/static/data/development-cp1252/', type){

    raw <- download_parlgov(base_url = base_url, type = type)
    out <- dataset(raw, type=paste0('parlgov_', type))

    out

}



download_parlgov <- function(base_url = 'http://www.parlgov.org/static/data/development-cp1252/', type){

    read.csv(paste0(base_url, 'view_', type, '.csv'), as.is=TRUE)

}
