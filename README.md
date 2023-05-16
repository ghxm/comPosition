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

Once we our composition object, we can already retreive some statistics from the ParlGov dataset we used as input, e.g. the total number of seats in the plenary.


```

# @TODO 

# number of seats

sum(plenary$seats)


# number of national parties


# number of political groups




```





<!---
@TODO: introduce entity classes and functionality

--->


## Advanced topics

### Adding entity classes


### Working with other data sources



