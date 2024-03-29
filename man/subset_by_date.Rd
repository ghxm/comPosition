% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/composition.R
\name{subset_by_date}
\alias{subset_by_date}
\title{Subset a dataset by date}
\usage{
subset_by_date(
  x,
  y = NULL,
  date,
  id_var = intersect(names(x), names(y)),
  id_var.x = id_var,
  id_var.y = id_var,
  date_start_var = NA,
  date_end_var = NA
)
}
\arguments{
\item{x}{the dataset to subset}

\item{y}{optional dataset containing information on a unit's start and end dates, must be set to NA if date information is contained in x}

\item{date}{date to subset for in character 'YYYY-MM-DD' format}

\item{id_var, }{id_var.x, id_var.y}

\item{date_start_var}{composition start date variable name (must be part of either x or y)}
}
\value{
the input dataset subset to to all units for which date_start_var => date <= date_end_var
}
\description{
Subset a dataset by date
}
