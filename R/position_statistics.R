

# Standard deviation etc...

#' Calculate (weighted) positions statistics
#'
#' @param x a vector of positions
#' @param w a vector of weights the same length as x
#' @param measure the statistics to compute: \code{"mean"}, \code{"median"},
#'   \code{"sd"}, or \code{"quantile"}
#' @param probs numeric probability in \code{[0, 1]} for the quantile to compute.
#'   Required when \code{measure = "quantile"}.
#' @param na.rm whether to exclude \code{NA} values from the calculation
#' @return A single numeric value.
#' @export
position_statistic <- function(x, w, measure, probs, na.rm = TRUE){

    if(length(x)==0 | all(is.na(x))){
        return(NA)
    }

    if (missing(measure)){
        stop('Please specify a measure to compute')
    }

    if (measure == "quantile" && missing(probs)){
        stop('Please specify probs for quantile measure')
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
        } else if(measure == "quantile"){
            return(quantile(x, probs = probs, na.rm = na.rm)[[1]])
        }

    }else{ #weighted

        w[is.na(w)] <- 0

        # Use normwt when weights are proportions (summing to ~1) rather than
        # counts (like seats), so Hmisc normalizes to sample size
        if (sum(w, na.rm = TRUE) <= 1.5){
            normwt = TRUE
        }else{
            normwt = FALSE
        }

        if(measure == "mean"){
            return(weighted.mean(x, w, na.rm = na.rm))
        } else if(measure == "median"){
            return(Hmisc::wtd.quantile(x, w, probs = c(0.5), na.rm = na.rm, normwt = normwt)[[1]])
        } else if(measure == "sd"){
            return(sqrt(Hmisc::wtd.var(x, w, na.rm = na.rm, normwt=normwt)))
        } else if(measure == "quantile"){
            return(Hmisc::wtd.quantile(x, w, probs = probs, na.rm = na.rm, normwt = normwt)[[1]])
        }

    }

    stop('Unknown measure: "', measure, '". Use "mean", "median", "sd", or "quantile".')


}




