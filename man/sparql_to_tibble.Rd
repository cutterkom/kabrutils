% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wikibase-sparql.R
\name{sparql_to_tibble}
\alias{sparql_to_tibble}
\title{Fetch results of a SPARQL query to a tibble}
\usage{
sparql_to_tibble(query, endpoint, useragent)
}
\arguments{
\item{query}{SPARQL query as a string}

\item{endpoint}{SPARQL endpoint of a Wikibase instance}

\item{useragent}{default: paste("Wikibase", R.version.string)}
}
\description{
Fetch results of a SPARQL query to a tibble
}
\examples{
\dontrun{
addresses_in_munich <- 'SELECT ...'
endpoint <- "https://database.factgrid.de/sparql"
sparql_tibble <- sparql_to_tibble(query = addresses_in_munich, endpoint = endpoint)
}
}
