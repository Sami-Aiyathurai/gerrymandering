% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_cleaning_uncontested.R
\name{check_wards}
\alias{check_wards}
\title{Takes in the filtered statewide information and creates a character vector with all of the unique wards within
the specified district. Each district is composed of specified wards, and by taking the unique wards of a
district we can identify across redistricting cycles how voters behaved.
This is called in district_func}
\usage{
check_wards(x)
}
\arguments{
\item{x}{data frame of wards in a given state assembly district}
}
\description{
Takes in the filtered statewide information and creates a character vector with all of the unique wards within
the specified district. Each district is composed of specified wards, and by taking the unique wards of a
district we can identify across redistricting cycles how voters behaved.
This is called in district_func
}
