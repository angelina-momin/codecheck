% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register.R
\name{register_render}
\alias{register_render}
\title{Function for rendering the register into different view}
\usage{
register_render(
  register = read.csv("register.csv", as.is = TRUE),
  filter_by = c("venues", "codecheckers"),
  outputs = c("html", "md", "json")
)
}
\arguments{
\item{register}{A `data.frame` with all required information for the register's view}

\item{outputs}{The output formats to create}
}
\value{
A `data.frame` of the register enriched with information from the configuration files of respective CODECHECKs from the online repositories
}
\description{
NOTE: You should put a GitHub API token inth the environment variable `GITHUB_PAT` to fix rate limits. Acquire one at see https://github.com/settings/tokens.
}
\details{
- `.html`
- `.md``
}
\author{
Daniel Nüst
}
