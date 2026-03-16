
#' Create a dataset S3 object
#'
#' @param x datsaset, e.g. as a dtaframe
#' @param type character string containing the type of dataset (source), e.g. 'parlgov_party'
#' @param ... additional named attributes to be assigned to the dataset object
#' @return A dataset S3 object containing x and additional attributes
#' @export
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

    if (type == 'parlgov_commission'){
        attr(x, 'date_start_var') <- 'start_date'
        attr(x, 'date_end_var') <- 'end_date'
    }

    # @TODO MEP attributes_roles: may need to be transformed to account for multiple memberships...
    if (type %in% c('mep_roles')){
        attr(x, 'id_var') <- 'id'
        attr(x, 'date_start_var') <- 'date_start'
        attr(x, 'date_end_var') <- 'date_end'

    }







    if(length(attrs)>0){
        for (i in 1:length(attrs)){
            attr(x, names(attrs[i])) <- attrs[[i]]
        }
    }

    # Auto-detect data cutoff from the most recent date in the data
    attr(x, 'data_cutoff') <- detect_data_cutoff(x)

    x
}


#' Detect the most recent date in a dataset to use as a data cutoff
#'
#' @param x a dataset object
#' @return A Date or NA if no date column can be found
detect_data_cutoff <- function(x){

    # Try date columns in order of preference
    date_cols <- c(attr(x, 'date_start_var'), attr(x, 'date_var'),
                   attr(x, 'date_end_var'))
    date_cols <- date_cols[!is.na(date_cols) & date_cols %in% names(x)]

    if (length(date_cols) == 0) return(NA)

    # Use the first available date column as the primary indicator
    col <- date_cols[1]
    dates <- x[, col]
    dates <- dates[!is.na(dates) & dates != ""]

    if (length(dates) == 0) return(NA)

    # Try multiple date parsing strategies
    parsed <- suppressWarnings(
        lubridate::parse_date_time(dates, orders = c('ymd', 'dmy', 'Ymd', 'dmY'))
    )

    if (all(is.na(parsed))) return(NA)

    lubridate::as_date(max(parsed, na.rm = TRUE))
}


#' Check whether a requested date exceeds a dataset's data cutoff
#'
#' Issues a warning if the date is beyond the most recent observation in the data.
#'
#' @param date the requested composition date
#' @param data a dataset object
#' @param institution name of the institution (for the warning message)
check_data_cutoff <- function(date, data, institution = ""){

    cutoff <- attr(data, 'data_cutoff')
    if (is.null(cutoff) || is.na(cutoff)) return(invisible(NULL))

    if (is.character(date)){
        date <- lubridate::parse_date_time(date, orders = c('ymd', 'dmy'))
    }

    if (date > cutoff) {
        warning(
            if (institution != "") paste0(institution, ": ") else "",
            "Requested date (", format(date, "%Y-%m-%d"), ") is beyond the data cutoff (",
            format(cutoff, "%Y-%m-%d"), "). ",
            "The underlying data may not cover this period, leading to incomplete or incorrect compositions.",
            call. = FALSE
        )
    }

    invisible(NULL)
}




# MANIFESTO (CMP) ----

#' Create a manifesto dataset object
#'
#' Downloads the Manifesto Project Dataset (MPDS) via the official API and
#' wraps it as a dataset S3 object. Requires a free API key from
#' \url{https://manifesto-project.wzb.eu/signup}.
#'
#' @param api_key Manifesto Project API key. If NULL, reads from the
#'   MANIFESTO_API_KEY environment variable.
#' @param version Dataset version to download (e.g. "MPDS2025a"). Use
#'   "latest" (default) to automatically fetch the most recent version,
#'   or a path/URL to a local CSV file.
#' @return A dataset S3 object of type 'manifesto'
#' @export
manifesto_dataset <- function(api_key = NULL, version = 'latest'){

    raw <- download_manifesto(api_key = api_key, version = version)
    dataset(raw, type='manifesto')

}

#' Download the Manifesto Project Dataset
#'
#' @param api_key Manifesto Project API key. If NULL, reads from the
#'   MANIFESTO_API_KEY environment variable.
#' @param version Dataset version (e.g. "MPDS2025a"), "latest", or a
#'   path/URL to a local CSV file.
#' @return A data.frame of the manifesto dataset
#' @export
download_manifesto <- function(api_key = NULL, version = 'latest'){

    # If version is a file path or URL, read directly
    if (file.exists(version) || grepl('^https?://', version)) {
        return(read.csv(version, as.is = TRUE))
    }

    # Resolve API key
    if (is.null(api_key)) {
        api_key <- Sys.getenv('MANIFESTO_API_KEY', unset = '')
    }
    if (api_key == '') {
        stop('No API key provided. Set the MANIFESTO_API_KEY environment variable or pass api_key directly.\n',
             'Get a free key at https://manifesto-project.wzb.eu/signup')
    }

    # Resolve version
    if (version == 'latest') {
        version <- manifesto_latest_version()
    }

    # Download via API
    api_url <- paste0('https://manifesto-project.wzb.eu/api/v1/get_core?',
                      'key=', version,
                      '&api_key=', api_key,
                      '&kind=csv&raw=true')

    tmpfile <- tempfile(fileext = '.csv')
    on.exit(unlink(tmpfile))

    resp <- tryCatch(
        utils::download.file(api_url, tmpfile, quiet = TRUE, mode = 'w'),
        error = function(e) stop('Failed to download manifesto data: ', conditionMessage(e))
    )

    if (resp != 0 || file.size(tmpfile) < 100) {
        stop('Download failed. Check your API key and internet connection.')
    }

    read.csv(tmpfile, as.is = TRUE)

}

#' List available Manifesto Project Dataset versions
#'
#' @return A data.frame with columns 'id' and 'name'
#' @export
manifesto_versions <- function(){
    url <- 'https://manifesto-project.wzb.eu/api/v1/list_core_versions'
    raw <- readLines(url, warn = FALSE)
    parsed <- jsonlite::fromJSON(raw)
    parsed$datasets
}

#' Get the latest Manifesto Project Dataset version identifier
#'
#' @return A character string (e.g. "MPDS2025a")
manifesto_latest_version <- function(){
    versions <- manifesto_versions()
    versions$id[NROW(versions)]
}


# PARLGOV ----

#' Create a ParlGov dataset object
#'
#' @param base_url base URL for ParlGov CSV files
#' @param type ParlGov view type: 'election', 'cabinet', or 'party'
#' @return A dataset S3 object
#' @export
parlgov_dataset <- function(base_url = 'https://parlgov.org/data/parlgov-development_csv-utf-8/', type){

    raw <- download_parlgov(base_url = base_url, type = type)
    out <- dataset(raw, type=paste0('parlgov_', type))

    out

}


#' Download a ParlGov CSV file
#'
#' @param base_url base URL for ParlGov CSV files
#' @param type ParlGov view type: 'election', 'cabinet', or 'party'
#' @return A data.frame
#' @export
download_parlgov <- function(base_url = 'https://parlgov.org/data/parlgov-development_csv-utf-8/', type){

    read.csv(paste0(base_url, 'view_', type, '.csv'), as.is=TRUE)

}


#' Create a commissioner dataset from the Doering/ParlGov data
#'
#' @param url URL or file path to the commissioner CSV
#' @param update If TRUE (default), applies known corrections and additions
#'   to the Doering dataset (e.g. VDL I commission personnel changes).
#' @return A dataset S3 object of type 'parlgov_commission'
#' @export
parlgov_commission_dataset <- function(url="https://www.parlgov.org/data/parlgov-development_csv-utf-8/external_commissioner_doering.csv", update=TRUE){

    raw <- download_parlgov_commission(url=url)

    if (update) {
        raw <- apply_commissioner_updates(raw)
    }

    out <- dataset(raw, type='parlgov_commission')

    out

}

#' Download the Doering commissioner dataset
#'
#' @param url URL or file path to the commissioner CSV
#' @return A data.frame
#' @export
download_parlgov_commission <- function(url="https://www.parlgov.org/data/parlgov-development_csv-utf-8/external_commissioner_doering.csv"){

    read.csv(url, as.is=TRUE)

}


