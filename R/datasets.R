
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

    # Auto-detect data coverage range from dates in the data
    cutoffs <- detect_data_cutoffs(x)
    attr(x, 'data_cutoff_start') <- cutoffs$start
    attr(x, 'data_cutoff') <- cutoffs$end

    x
}


#' Detect the earliest and most recent dates in a dataset
#'
#' Uses \code{date_start_var} (or \code{date_var}) for the start cutoff and
#' \code{date_end_var} (or fallback) for the end cutoff, so datasets with
#' separate start/end columns get accurate coverage boundaries.
#'
#' @param x a dataset object
#' @return A list with \code{start} and \code{end} (Date or NA)
detect_data_cutoffs <- function(x){

    parse_dates <- function(col) {
        if (is.null(col) || !(col %in% names(x))) return(NULL)
        vals <- x[, col]
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) return(NULL)
        parsed <- suppressWarnings(
            lubridate::parse_date_time(vals, orders = c('ymd', 'dmy', 'Ymd', 'dmY'))
        )
        parsed[!is.na(parsed)]
    }

    # For start cutoff: prefer date_start_var, fall back to date_var
    start_cols <- c(attr(x, 'date_start_var'), attr(x, 'date_var'))
    start_dates <- NULL
    for (col in start_cols) {
        start_dates <- parse_dates(col)
        if (!is.null(start_dates)) break
    }

    # For end cutoff: prefer date_end_var, fall back to date_start_var, then date_var
    end_cols <- c(attr(x, 'date_end_var'), attr(x, 'date_start_var'), attr(x, 'date_var'))
    end_dates <- NULL
    for (col in end_cols) {
        end_dates <- parse_dates(col)
        if (!is.null(end_dates)) break
    }

    list(
        start = if (!is.null(start_dates)) lubridate::as_date(min(start_dates)) else NA,
        end = if (!is.null(end_dates)) lubridate::as_date(max(end_dates)) else NA
    )
}


#' Check whether a requested date is outside a dataset's coverage range
#'
#' Issues a warning if the date is before the earliest or after the most
#' recent observation in the data.
#'
#' @param date the requested composition date
#' @param data a dataset object
#' @param institution name of the institution (for the warning message)
check_data_cutoff <- function(date, data, institution = ""){

    if (is.character(date)){
        date <- lubridate::parse_date_time(date, orders = c('ymd', 'dmy'))
    }

    prefix <- if (institution != "") paste0(institution, ": ") else ""

    cutoff_start <- attr(data, 'data_cutoff_start')
    cutoff_end <- attr(data, 'data_cutoff')

    if (!is.null(cutoff_start) && !is.na(cutoff_start) && date < cutoff_start) {
        warning(
            prefix,
            "Requested date (", format(date, "%Y-%m-%d"), ") is before the data starts (",
            format(cutoff_start, "%Y-%m-%d"), "). ",
            "The underlying data may not cover this period, leading to incomplete or incorrect compositions.",
            call. = FALSE
        )
    }

    if (!is.null(cutoff_end) && !is.na(cutoff_end) && date > cutoff_end) {
        warning(
            prefix,
            "Requested date (", format(date, "%Y-%m-%d"), ") is beyond the data cutoff (",
            format(cutoff_end, "%Y-%m-%d"), "). ",
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

#' Resolve a file download URL from Harvard Dataverse
#'
#' Queries the Dataverse API for the latest version of a dataset and returns
#' the access URL for the specified file.
#'
#' @param filename the file name to look up (e.g. "view_election.tab")
#' @param doi persistent identifier of the dataset (default: ParlGov 2024)
#' @param base_url Dataverse instance base URL
#' @return A download URL string
dataverse_file_url <- function(filename,
                               doi = 'doi:10.7910/DVN/2VZ5ZC',
                               base_url = 'https://dataverse.harvard.edu') {

    api_url <- paste0(base_url, '/api/datasets/:persistentId?persistentId=', doi)

    metadata <- tryCatch(
        jsonlite::fromJSON(api_url),
        error = function(e) stop('Failed to reach Dataverse API: ', conditionMessage(e))
    )

    files <- metadata$data$latestVersion$files
    match_idx <- which(files$dataFile$filename == filename)

    if (length(match_idx) == 0) {
        stop('File "', filename, '" not found in Dataverse dataset ', doi)
    }

    file_id <- files$dataFile$id[match_idx[1]]
    paste0(base_url, '/api/access/datafile/', file_id)
}


#' Create a ParlGov dataset object
#'
#' Downloads ParlGov data from Harvard Dataverse (stable release) and wraps
#' it as a dataset S3 object.
#'
#' @param type ParlGov view type: 'election', 'cabinet', or 'party'
#' @param doi Dataverse persistent identifier for the ParlGov dataset
#' @return A dataset S3 object
#' @export
parlgov_dataset <- function(type, doi = 'doi:10.7910/DVN/2VZ5ZC'){

    raw <- download_parlgov(type = type, doi = doi)
    out <- dataset(raw, type=paste0('parlgov_', type))

    out

}


#' Download a ParlGov data file from Harvard Dataverse
#'
#' @param type ParlGov view type: 'election', 'cabinet', or 'party'
#' @param doi Dataverse persistent identifier for the ParlGov dataset
#' @return A data.frame
#' @export
download_parlgov <- function(type, doi = 'doi:10.7910/DVN/2VZ5ZC'){

    filename <- paste0('view_', type, '.tab')
    url <- dataverse_file_url(filename, doi = doi)

    tmpfile <- tempfile(fileext = '.tab')
    on.exit(unlink(tmpfile))

    resp <- tryCatch(
        utils::download.file(url, tmpfile, quiet = TRUE, mode = 'wb'),
        error = function(e) stop('Failed to download ParlGov data: ', conditionMessage(e))
    )

    if (resp != 0 || file.size(tmpfile) < 100) {
        stop('Download failed. Check your internet connection.')
    }

    read.delim(tmpfile, as.is = TRUE)
}


#' Create a commissioner dataset from the Doering/ParlGov data
#'
#' @param url URL or file path to the commissioner CSV. If the URL is
#'   unreachable, falls back to a bundled snapshot shipped with the package.
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
#' Attempts to download from the given URL. If the download fails, falls back
#' to a snapshot bundled with the package.
#'
#' @param url URL or file path to the commissioner CSV
#' @return A data.frame
#' @export
download_parlgov_commission <- function(url="https://www.parlgov.org/data/parlgov-development_csv-utf-8/external_commissioner_doering.csv"){

    # Try downloading from URL
    result <- tryCatch(
        read.csv(url, as.is=TRUE),
        error = function(e) NULL
    )

    if (!is.null(result)) return(result)

    # Fall back to bundled snapshot
    bundled <- system.file('extdata', 'commissioner_doering.csv', package = 'comPosition')
    if (bundled == '') {
        stop('Could not download commissioner data from URL and no bundled fallback found.')
    }

    message('URL unreachable, using bundled commissioner data snapshot.')
    read.csv(bundled, as.is = TRUE)
}


