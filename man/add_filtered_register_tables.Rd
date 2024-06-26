% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_filter_register_table.R
\name{add_filtered_register_tables}
\alias{add_filtered_register_tables}
\title{Function for adding filtered register tables to a list based on specified filters.
Each entry in the resulting list is a filtered register table by the filter type provided.}
\usage{
add_filtered_register_tables(list_register_tables, register_table, filter_by)
}
\arguments{
\item{list_register_tables}{A list to store the filtered tables}

\item{register_table}{The register table to filter}

\item{filter_by}{A vector of strings specifying the filter types}
}
\value{
A list of filtered register tables
}
\description{
Function for adding filtered register tables to a list based on specified filters.
Each entry in the resulting list is a filtered register table by the filter type provided.
}
