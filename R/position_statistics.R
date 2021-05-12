

# Standard deviation etc...

#@TODO: hier weiter

#' Calculate (weighted) positions statistics
#'
#' @param x a vector of positions
#' @param w a vector of weights the same length as x
#' @param measure the statistics to compute, 'mean', 'median', 'sd'
#' @param na.rm whether to exclude `NA` values from the calculation
#' @export
position_statistic <- function(x, w, measure, na.rm = TRUE){

    if(length(x)==0 | all(is.na(x))){
        return(NA)
    }

    if (missing(measure)){
        stop('Please specify a measure to compute')
    }

    if(missing(w)){
        unweighted <- TRUE
    } else if(all(w==1, na.rm = TRUE)) {
        unweighted <- TRUE
    } else {
        unweighted <- FALSE
    }

    if(unweighted){

        if(measure == "mean"){
            return(mean(x, na.rm = na.rm))
        } else if(measure == "median"){
            return(median(x, na.rm = na.rm))
        } else if(measure == "sd"){
            return(sd(x, na.rm = na.rm))
        }

    }else{ #weighted

        w[is.na(w)] <- 0


        if (sum (w, na.rm=TRUE) <= 1){
            scaling <- 100
        }else{
            scaling <- 1
        }


        if(measure == "mean"){
            return(weighted.mean(x, w*scaling, na.rm = na.rm))
        } else if(measure == "median"){
            return(Hmisc::wtd.quantile(x, w*scaling, probs = c(0.5), na.rm = na.rm)[[1]])
        } else if(measure == "sd"){
            return(sqrt(Hmisc::wtd.var(x, w*scaling, na.rm = na.rm)))
        }

    }


}






