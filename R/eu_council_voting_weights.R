# adjusted version of Holger Doering's code

CouncilVotingData <- function (country_id, date.var, method = "") {
    # Create a data frame with Council voting weights for all EU members
    #
    # Args:
    #   NULL
    #   - method used for weights (empty for normal proportional, "bi_normal", "ssi")
    #
    # Returns:
    #   Data frame.
    #     country_name_short: ISO 3166-1 ALPHA-3 -- character
    #     country_id: ParlGov country id -- integer
    #     'dates': dates of voting weight changes -- character (YYYY-MM-DD)

    #vw.columns <- 13

    # Data (edited for Croatia and Population Voting Weights from 2014-11-01 on)
    proportional <- "country_name_short,country_id,1958-01-01,1973-01-01,1981-01-01,1986-01-01,1995-01-01,2004-05-01,2004-11-01,2007-01-01,2013-07-01,2014-11-01,2020-02-01
              SVK,1,,,,,,3,7,7,7,1.06,1.21
              LUX,7,1,2,2,2,2,2,4,4,4,0.12,0.14
              NLD,8,2,5,5,5,5,5,13,13,13,3.36,3.96
              BGR,10,,,,,,,,10,10,1.39,1.53
              LTU,15,,,,,,3,7,7,7,0.56,0.63
              DNK,21,,3,3,3,3,3,7,7,7,1.12,1.31
              ROU,23,,,,,,,,14,14,3.83,4.25
              ITA,26,4,10,10,10,10,10,29,29,29,11.95,13.32
              ESP,27,,,,8,8,8,27,27,27,9.08,10.6
              SWE,35,,,,,4,4,10,10,10,1.97,2.33
              IRL,37,,3,3,3,3,3,7,7,7,0.93,1.13
              HUN,39,,,,,,5,12,12,12,1.91,2.17
              GRC,41,,,5,5,5,5,12,12,12,2.1,2.37
              FRA,43,4,10,10,10,10,10,29,29,29,13.09,15.16
              GBR,44,,10,10,10,10,10,29,29,29,12.85,
              CYP,51,,,,,,2,4,4,4,0.17,0.2
              DEU,54,4,10,10,10,10,10,29,29,29,16.10,18.59
              LVA,55,,,,,,3,4,4,4,0.38,0.42
              AUT,59,,,,,4,4,10,10,10,1.71,2.0
              SVN,60,,,,,,3,4,4,4,0.4,0.47
              PRT,63,,,,5,5,5,12,12,12,2.01,2.31
              BEL,64,2,5,5,5,5,5,12,12,12,2.22,2.6
              FIN,67,,,,,3,3,7,7,7,1.07,1.24
              CZE,68,,,,,,5,12,12,12,2.04,2.36
              MLT,72,,,,,,2,3,3,3,0.09,0.12
              POL,74,,,,,,8,27,27,27,7.41,8.41
              EST,75,,,,,,3,4,4,4,0.26,0.3
              HRV,62,,,,,,,,,7,0.81,0.86"

    bi_normal <- "country_name_short,country_id,2007-01-01,2013-07-01,2014-11-01
          AUT,59,0.031,0.03,0.024
          BEL,64,0.037,0.036,0.027
          BGR,10,0.031,0.03,0.023
          CYP,51,0.013,0.012,0.014
          CZE,68,0.037,0.036,0.027
          DEU,54,0.078,0.076,0.116
          DNK,21,0.022,0.021,0.02
          ESP,27,0.074,0.072,0.068
          EST,75,0.013,0.012,0.015
          FIN,67,0.022,0.021,0.02
          FRA,43,0.078,0.076,0.095
          GBR,44,0.078,0.076,0.092
          GRC,41,0.037,0.036,0.028
          HRV,62,,0.021,0.019
          HUN,39,0.037,0.036,0.026
          IRL,37,0.022,0.021,0.019
          ITA,26,0.078,0.076,0.089
          LTU,15,0.022,0.021,0.017
          LUX,7,0.013,0.012,0.014
          LVA,55,0.013,0.012,0.016
          MLT,72,0.009,0.009,0.014
          NLD,8,0.04,0.039,0.034
          POL,74,0.074,0.072,0.056
          PRT,63,0.037,0.036,0.027
          ROU,23,0.043,0.042,0.04
          SVK,1,0.022,0.021,0.02
          SVN,60,0.013,0.012,0.016
          SWE,35,0.031,0.03,0.025"


    ssi <- "country_name_short,country_id,2007-01-01,2013-07-01,2014-11-01
          AUT,59,0.028,0.028,0.019
          BEL,64,0.034,0.033,0.023
          BGR,10,0.028,0.028,0.017
          CYP,51,0.011,0.011,0.007
          CZE,68,0.034,0.033,0.022
          DEU,54,0.087,0.085,0.149
          DNK,21,0.02,0.019,0.015
          ESP,27,0.08,0.079,0.08
          EST,75,0.011,0.011,0.008
          FIN,67,0.02,0.019,0.014
          FRA,43,0.087,0.085,0.117
          GBR,44,0.087,0.085,0.112
          GRC,41,0.034,0.033,0.023
          HRV,62,,0.019,0.013
          HUN,39,0.034,0.033,0.021
          IRL,37,0.02,0.019,0.013
          ITA,26,0.087,0.085,0.108
          LTU,15,0.02,0.019,0.011
          LUX,7,0.011,0.011,0.007
          LVA,55,0.011,0.011,0.009
          MLT,72,0.008,0.008,0.007
          NLD,8,0.037,0.036,0.032
          POL,74,0.08,0.079,0.067
          PRT,63,0.034,0.033,0.022
          ROU,23,0.04,0.039,0.039
          SVK,1,0.02,0.019,0.014
          SVN,60,0.011,0.011,0.009
          SWE,35,0.028,0.028,0.021"

    # depending on weighting method selected, select weighting data
    if(method == "bi_normal"){
        vw.data <- bi_normal
        vw.columns <- 5
    }else if (method == "ssi"){
        vw.data <- ssi
        vw.columns <- 5
    } else if (method == "" | method == "proportional") {
        vw.data <- proportional
        vw.columns <- 13
    }

    vw.data <- unlist(strsplit(vw.data, ',|\n'))

    # remove leading and trailing whitespace
    vw.data <- gsub('^\\s+|\\s+$', '', vw.data, perl=TRUE)

    vw <- matrix(vw.data[-c(1:vw.columns)], ncol=vw.columns, byrow=TRUE)
    vw <- data.frame(vw, stringsAsFactors=FALSE)
    names(vw) <- vw.data[1:vw.columns]
    vw[ , -1] <- apply(vw[ , -1], c(1, 2), as.numeric)

    return(vw)
}


#' Calculate voting weight in the Council for a specific date
#'
#' @param country_id vector of ParlGov country ids
#' @param date a date in character format
#' @export
council_voting_weights <- function (country_id, date, type = "bi_normal") {
    # Calculate voting weight in the Council for a specific date
    #
    # Args:
    #   country_id: vector of ParlGov country ids -- integer
    #   date: a date in character
    #
    # Returns:
    #   voting weights -- integer

    if(is.character(date)){
        date <- lubridate::parse_date_time(date, orders=c('ymd', 'dmy'))
    }


    vw <- CouncilVotingData(method = type)
    vw.dates <- grep('\\d{4}-\\d{2}-\\d{2}', colnames(vw), perl=TRUE, value=TRUE)

    # ignore dates before 1958 or after today
    if(date < vw.dates[1] | Sys.Date() < date) {
        warning('No weight information for this date')
        return(NA)
    }

    w <- NULL

    w <- sapply(country_id, function(x) {
        if(x %in% vw$country_id)
            vw[vw$country_id==x, max(vw.dates[date >= vw.dates])]
        else{
            NA
        }
    })

    w
}
