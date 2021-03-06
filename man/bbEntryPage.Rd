% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entry.R
\name{bbEntryPage}
\alias{bbEntryPage}
\title{Retrieve entry result page}
\usage{
bbEntryPage(identifer, source, page, totalPage)
}
\arguments{
\item{identifer}{Identifer for the entry.}

\item{source}{Dataset identifier}

\item{page}{Page index it starts from 0}

\item{totalPage}{Total number of page for the entry. This value needs to calculate by user via using total number of entries which is available at the root result for the entry
and divide it to the paging sizeb of 200}
}
\value{
returns biobtree json object
}
\description{
If an entry contains large set of mapping entries it is paginated by biobtree with confiGured paging size.
This function retrieve these paging for an entry. Biobtree paging size for each entry is 200.
}
\examples{

bbStart() # if not already started
bbEntryPage("ENSG00000141956","ensembl",0,0)


}
\author{
Tamer Gur
}
