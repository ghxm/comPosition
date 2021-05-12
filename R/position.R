
# MANIFESTO ----

# if multiple ids for a given input ID (electoral allaince e.g.), average
# to facilitate this, assign identifiers and average over each identifier
# -> do this in a composition object?
# then pass the composition object the positions function whch will ahndle things appropriately



#' @param x a dataframe containing an ID variable and the value manifesto variables L and R
#' @param L a vector of variable names in x representing the L pole
#' @param R a vector of variable names in x representing the R pole
#' @param N a string containing a variable name in x representing N (total number of coded sentences)
#' @export
manifesto_positions <- function(x, L_vars, R_vars, N_vars = c('total'), method = 'lowe'){

    calculate_manifesto_positions(x[, L_vars], x[, R_vars], x[N_vars], method=method)


}


#' Manifesto positions aggregated at the country level
#'
#' @param x a dataframe containing the values manifesto variables L and R
#' @param L a vector of variable names in x representing the L pole
#' @param R a vector of variable names in x representing the R pole
#' @param N a string containing a variable name in x representing N (total number of coded sentences)
#' @param country_seats a dataframe specifying a country_id variable for each value in x as well as a 'seats' variable
#' @export
manifesto_country_positions <- function(x, L_vars, R_vars, N_vars = c('total'), country_seats, weighted=TRUE, method = 'lowe', na.rm=TRUE){

    country_party_postitions <- calculate_manifesto_positions(x[, L_vars], x[, R_vars], x[N_vars], method=method)

    country_party_postitions <- cbind(country_seats, country_party_postitions)
    names(country_party_postitions)[3] <- 'position'

    country_gov_postions <- sapply(unique(country_party_postitions$country_id), function(y) {

        country_gov_parties <- country_party_postitions[which(country_party_postitions$country_id==y),]

        if (weighted) {
            gov_pos <- position_statistic(country_gov_parties[,'position'], w=country_gov_parties[,'seats'], measure='mean', na.rm=na.rm)
        } else {
            pov_pos <- position_statistic(country_gov_parties[,'position'], measure='mean', na.rm=na.rm)
        }

        c('country_id' = y, 'position' = gov_pos)
    })

    t(country_gov_postions)

}

#' @export
manifesto_issue_dimension_position <- function(x, ...) UseMethod('manifesto_issue_dimension_position')

#' Generate position on dimensions consisting of issues
#'
#' @param x a composition object
#' @param issues a list() of issues, each containg a list containing maninfesto categories for L and R poles
#' @export
manifesto_issue_dimension_position.composition <- function(x, date, data, issues = list(), method='lowe'){

    issues_positions <- data.frame()


   # for each issue
    for (l in 1:NROW(issues)){

        issue <- issues[[l]]

        # get values
        vals <- values(x, date=date, data=data, value_vars = c(issue[['L']], issue[['R']]), additional_vars = c('party','total'))

        pos <- calculate_manifesto_positions(vals[, issue[['L']]], vals[, issue[['R']]], vals['total'], method=method)

        # store composition_id, position
        issues_positions <- rbind(issues_positions, cbind(x['composition_id'], pos))

    }

   # calculate the mean per composition_id
    dimension_positions <- aggregate(issues_positions[,2], list(issues_positions[,'composition_id']), mean)

    # make sure the positions are in the order of the input composition
    dimension_positions[x$composition_id, 2]

}


# @TODO manifesto_country_issue_dimension_position


#' @param L a vector of variable names
#' @param R a vector of variable names
#' @param method
#' @export
calculate_manifesto_positions <- function(L, R, N, method='lowe'){

    L <- (rowSums(data.frame(L))/100)*N
    R <- (rowSums(data.frame(R))/100)*N

    if (method=='lowe'){
        log((R+0.5)/(L+0.5))
    }else if (method=='budge'){
        (R-L)/N
    }else if (method=='kimfording'){
        (R-L)/(R+L)
    }


}


