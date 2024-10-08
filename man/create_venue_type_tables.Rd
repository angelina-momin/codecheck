% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_render_table_venues.R
\name{create_venue_type_tables}
\alias{create_venue_type_tables}
\title{Create Venue Type-Specific Tables}
\usage{
create_venue_type_tables(register_table)
}
\arguments{
\item{register_table}{The data frame containing the original register data.}
}
\value{
A list of tables, one for each venue type.
}
\description{
Generates tables for each venue type by filtering the register data. 
It adds columns for the venue slug and the number of codechecks for each venue type.
}
