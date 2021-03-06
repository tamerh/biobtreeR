% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lists.R
\name{bbListAttrs}
\alias{bbListAttrs}
\title{Retrieve attributes of dataset}
\usage{
bbListAttrs(dataset)
}
\arguments{
\item{dataset}{Dataset identifier}
}
\value{
attributes names
}
\description{
Provides list of available attributes for a dataset to use in search and mapping queries.
}
\examples{

bbListAttrs("hgnc")
bbListAttrs("ensembl")

}
