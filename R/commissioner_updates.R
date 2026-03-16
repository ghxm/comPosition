
#' Apply known updates to the commissioner dataset
#'
#' Adds end_dates to commissioners who left office and appends replacement
#' commissioners that are not in the original Doering dataset. Currently
#' covers the full Von der Leyen I commission term (2019-12-01 to 2024-11-30).
#'
#' @param x a commissioner data.frame (as returned by download_parlgov_commission)
#' @return the updated data.frame with corrections applied
apply_commissioner_updates <- function(x){

    # VDL I commission ended 2024-11-30
    vdl_end <- '2024-11-30'

    # --- Set end_dates for VDL commissioners who served the full term ---
    vdl_no_end <- which(x$commission_name == 'Von der Leyen' & (x$end_date == '' | is.na(x$end_date)))
    x[vdl_no_end, 'end_date'] <- vdl_end

    # --- Set end_dates for commissioners who left early ---
    # Phil Hogan (id=296): resigned 2020-08-26
    x[x$id == 296, 'end_date'] <- '2020-08-26'

    # Mariya Gabriel (id=297): resigned 2023-05-15
    x[x$id == 297, 'end_date'] <- '2023-05-15'

    # Thierry Breton (id=301): resigned 2024-09-16
    x[x$id == 301, 'end_date'] <- '2024-09-16'

    # Adina Valean (id=308): resigned 2024-07-15
    x[x$id == 308, 'end_date'] <- '2024-07-15'

    # Virginijus Sinkevicius (id=312): resigned 2024-07-16
    x[x$id == 312, 'end_date'] <- '2024-07-16'

    # Dombrovskis (id=289): update portfolio to reflect Trade takeover from 2020-10-07,
    # set end_date on original entry and add new entry for Trade role
    x[x$id == 289, 'end_date'] <- '2020-10-06'

    # --- Append replacement commissioners ---
    next_id <- max(x$id, na.rm = TRUE) + 1L

    new_rows <- data.frame(
        comment = '',
        commission_year = 2019L,
        elected = c(1L, 1L, 1L),
        portfolio = c(
            'Trade',
            'Financial Services, Financial Stability and Capital Markets Union',
            'Innovation, Research, Culture, Education and Youth'
        ),
        id = next_id:(next_id + 2L),
        one_seat = c(1L, 1L, 1L),
        country_id = c(
            55L,  # Dombrovskis (Latvia) - Trade
            37L,  # McGuinness (Ireland)
            10L   # Ivanova (Bulgaria)
        ),
        commission_name = 'Von der Leyen',
        incumbent = c(1L, 0L, 0L),
        person_id = c(
            11889L,  # Dombrovskis
            NA_integer_,  # McGuinness (not in original data)
            NA_integer_   # Ivanova (not in original data)
        ),
        person_id_source = c(
            'Valdis Dombrovskis',
            'Mairead McGuinness',
            'Iliana Ivanova'
        ),
        start_date = c(
            '2020-10-07',  # Dombrovskis Trade
            '2020-10-07',  # McGuinness
            '2023-09-19'   # Ivanova
        ),
        end_date = c(
            vdl_end,  # Dombrovskis Trade
            vdl_end,  # McGuinness
            vdl_end   # Ivanova
        ),
        score_position = c(
            2.14,  # Dombrovskis (same as before)
            NA,    # McGuinness
            NA     # Ivanova
        ),
        party_id = c(
            1666L,  # Dombrovskis: Vienotiba / Unity
            1393L,  # McGuinness: Fine Gael (same as Hogan)
            1541L   # Ivanova: GERB (same as Gabriel)
        ),
        president = 0L,
        highest_position = c(
            'prime minister',
            'MEP, European Parliament Vice-President',
            'member of the European Court of Auditors'
        ),
        data_source = 'comPosition_update',
        government_party = c(10L, 10L, 10L),
        previous_cabinet_id = c(
            1592L,  # Dombrovskis (same as VDL entry)
            NA_integer_,
            NA_integer_
        ),
        stringsAsFactors = FALSE
    )

    # Ensure column order matches
    new_rows <- new_rows[, names(x)]

    rbind(x, new_rows)
}
