# comPosition #

[![Build Status](https://travis-ci.org/ghxm/comPosition.svg?branch=master)](https://travis-ci.org/ghxm/comPosition)
[![codecov.io](https://codecov.io/github/ghxm/comPosition/coverage.svg?branch=master)](https://codecov.io/github/ghxm/comPosition?branch=master)


*WARNING: Development version - use at your own risk. <a href="mailto:maximilian.haag@gsi.uni-muenchen.de?subject=comPosition R package">Contact the author</a> for further information on the package status. For bug reports, questions and features, please refer to the [GitHub Issues](https://github.com/ghxm/comPosition/issues).*


## Description

R package to create EU institutional composition and position datasets across time using various data sources.

## Installation

At this point in time, the development version of the package can be installed directly from GitHub, e.g. by running

```devtools::install_github('ghxm/comPosition', force = TRUE, upgrade  = "never")```


## Basic usage and concept

The `comPostion` package, as suggested by the name, can be used to retrieve compositions of entities, e.g. institutions, as well as statistics on associated data of the members of the. In principle, the package is agnostic to data sources and can extended to work with arbitrary data (see below).

Here's an example to illustrate the above:

Consider a situation where we are interested in the composition of the European Parliament at 14 April 2012, i.e. we want to know which national parties were represented in the a European Parliament at a specific point in time and how many seats each of them held at that time.

For this, we are going to use the ParlGov election dataset, which holds data on various elections including elections to the European Parliament. The current version of the ParlGov dataset can be directly retreived via an in-built package function. In order to be able to connect our parliamentary election data to other datasets later on, we are also going to use the Partyfacts dataset as a "linktable".

```

library(comPositison)

parlgov_party <- parlgov_dataset(type='party')

lt <- partyfacts_linktable(ignore_years = TRUE)

```

With this data, we can now create a composition object. For the European Parliament, the package comes with a built-in class that already defines some of the necessary properties for European Parliament composition object subsetting and includes some special functions, e.g. to find the corresponding election date.

To avoid confusion the `comPosition` package uses the `YYYY-MM-DD` date format. Other formats may work but not necessarily. Therefore, it's best to stick to `YYYY-MM-DD`.

```
plenary <- ep("2012-04-14", data=parlgov_election, linktable = lt, type='plenary')

```

Once we created our composition object, we can already retreive some statistics from the ParlGov dataset we used as input, e.g. the total number of seats in the plenary:

```
# @TODO
```

## Examples: EU institutional positions

To calculate measures at scale, it's best to define a function that can then be called in a loop or apply function, e.g. to calculate measures for a variety of dates. First we'll load up the necessary data:


```

lt <- partyfacts_linktable(ignore_years = TRUE)
#parlgov_election <- parlgov_dataset(type='election')
#parlgov_cabinet <- parlgov_dataset(type='cabinet')
parlgov_party <- parlgov_dataset(type='party')
manifesto_data <- manifesto_dataset()

parlgov_commission <- parlgov_commission_dataset()


```


We can then use the functions below to calculate the aggregated measures. In this case 'positions' derived from party manifesto data.


### National government positions function

```

calculate_gov_pos <- function(date, dimension = 'eu', method = 'lowe'){

    cou <- council(date, data=parlgov_cabinet, linktable = lt)
    cou <- add_id_col(cou, to = 'manifesto')

    cou_values <- comPosition::values(cou, date = date, value_vars = c(get_pole(dimension, 'L'), get_pole(dimension, 'R')), data=manifesto_data, additional_vars=c('total', 'countryname', 'country'))

    cou_country_pos <- manifesto_country_positions(cou_values, L_vars = get_pole(dimension, 'L'), R_vars = get_pole(dimension, 'R'), country_seats = cou[,c('country_id', 'seats')], method=method)

    merge(cou_country_pos, parglov_country_lt, by.x='country_id', by.y='country_id', all.x = TRUE, all.y=FALSE) |>
      mutate(date = date, 'pos_{dimension}_{method}':= position) |>
      select(date, country_id, country_name_short, glue('pos_{dimension}_{method}'))

}

```

### EP plenary position function

```

calculate_ep_pos <- function(date, dimension, method, dim_var_name = FALSE){
    plenary <- ep(date, data=parlgov_election,linktable = lt, type='plenary')
    plenary <- add_id_col(plenary, to = 'manifesto')

    # 'normal dimensions'
    if (!is.list(dimension)){
        plenary_values <- comPosition::values(plenary, date = date, value_vars = c(get_pole(dimension, 'L'), get_pole(dimension, 'R')), data=manifesto_data, additional_vars=c('total'))


        plenary_pos <- manifesto_positions(plenary_values, L_vars = get_pole(dimension, 'L'), R_vars = get_pole(dimension, 'R'), method=method)


    }else { # eurovoc issue positions
        dimension <- dimension[1]
        dim_var_name <- FALSE
        plenary_pos <- as.data.frame(manifesto_issue_dimension_position(plenary, date, data=manifesto_data, issues=dimension, method=method))

    }

    pos <- c(position_statistic(plenary_pos[,1], w = plenary$seats, measure = 'mean', na.rm=TRUE),
             position_statistic(plenary_pos[,1], w = plenary$seats, measure = 'median', na.rm=TRUE),
             position_statistic(plenary_pos[,1], w = plenary$seats, measure = 'sd', na.rm=TRUE))

    # set dimension name for variable naming below
    if (dim_var_name){
        dimension_name <- paste0(dimension,'_')
    } else {
        dimension_name <- ''
    }

    names(pos) <- c(paste0('pos_ep_', dimension_name,'mean_', substr(method,1,1)),
                    paste0('pos_ep_', dimension_name, 'median_', substr(method,1,1)),
                    paste0('pos_ep_', dimension_name, 'sd_', substr(method,1,1)))


    pos

}

```

### Council position function

```

calculate_council_pos <- function(date, dimension, method, dim_var_name = FALSE){
    cou <- council(date, data=parlgov_cabinet, linktable = lt)
    cou <- add_id_col(cou, to = 'manifesto')

    cou_values <- comPosition::values(cou, date = date, value_vars = c(get_pole(dimension, 'L'), get_pole(dimension, 'R')), data=manifesto_data, additional_vars=c('total'))

    cou_country_pos <- manifesto_country_positions(cou_values, L_vars = get_pole(dimension, 'L'), R_vars = get_pole(dimension, 'R'), country_seats = cou[,c('country_id', 'seats')], method = method)

    w <- comPosition::council_voting_weights(cou_country_pos[,'country_id'], date=date, type='bi_normal')

    pos <- c(position_statistic(cou_country_pos[,2], w = w, measure = 'mean', na.rm=TRUE),
             position_statistic(cou_country_pos[,2], w = w, measure = 'median', na.rm=TRUE),
             position_statistic(cou_country_pos[,2], w = w, measure = 'sd', na.rm=TRUE)

    )

    # set dimension name for variable naming below
    if (dim_var_name){
        dimension_name <- paste0(dimension,'_')
    } else {
        dimension_name <- ''
    }

    names(pos) <- c(paste0('pos_cou_bi_normal_', dimension_name,'mean_', substr(method,1,1)),
                    paste0('pos_cou_bi_normal_', dimension_name, 'median_', substr(method,1,1)),
                    paste0('pos_cou_bi_normal_', dimension_name, 'sd_', substr(method,1,1)))


    pos

}


```

### Commission position function

```

calculate_commission_pos <- function(date, dimension, method, dim_var_name = TRUE, debug=FALSE){

    if (debug){
        print(date)
    }

    # set dimension name for variable naming below
    if (dim_var_name){
        dimension_name <- paste0(dimension,'_')
    } else {
        dimension_name <- ''
    }

    com <- commission(date=date, data=parlgov_commission, linktable=lt)

    com <- add_id_col(com, to='manifesto')

    if(NROW(com)==0){
        pos <- c(NA, NA, NA)

        names(pos) <- c(paste0('pos_com_', dimension_name,'mean_', substr(method,1,1)),
                        paste0('pos_com_', dimension_name, 'median_', substr(method,1,1)),
                        paste0('pos_com_', dimension_name, 'sd_', substr(method,1,1)))


        return(pos)

    }

    if (dimension=='rile' & method=='budge'){
        com_pos <- values(com, date=date, value_vars = c(), data=manifesto_data, additional_vars=c('rile', 'total'))
    }else {
        if (dimension=='rile'){
            com_values <- values(com, date=date, value_vars = c(get_pole('rile', 'L'), get_pole('rile', 'R')), data=manifesto_data, additional_vars=c('total'))
            com_pos <- manifesto_positions(com_values, L_vars = get_pole('rile', 'L'), R_vars=get_pole('rile', 'R'), method=method)

        }

    }


    pos <- c(position_statistic(com_pos[,1], measure = 'mean', na.rm=TRUE),
             position_statistic(com_pos[,1], measure = 'median', na.rm=TRUE),
             position_statistic(com_pos[,1], measure = 'sd', na.rm=TRUE))

    names(pos) <- c(paste0('pos_com_', dimension_name,'mean_', substr(method,1,1)),
                     paste0('pos_com_', dimension_name, 'median_', substr(method,1,1)),
                     paste0('pos_com_', dimension_name, 'sd_', substr(method,1,1)))

    pos

}

```


Once this is done, we can start retrieving the measures.


## Advanced topics

### Adding entity classes


### Working with other data sources



