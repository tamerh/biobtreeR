#' @title Retrieve entry
#'
#' @description Returns entry for an identifier and dataset. Entry contains all the data raw data for and entry such as mappings, attiributes and paging info if exists.
#'
#' @param identifer Identifer for the entry. Note that keywords are not accepted. For instance insted of "vav_human" keyword "p15498" identifier must be passed
#' @param source Dataset identifier
#'
#' @return returns biobtree json object
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbStart()
#' bbEntry("HGNC:12009","hgnc")
#'

bbEntry <- function(identifer,source){

  searchurl <- paste0(getConfig()@endpoint,"/ws/entry/?i=",encodeURIComponent(identifer),"&s=",source)
  res <- fromJSON(searchurl,simplifyVector = FALSE,encoding = "UTF-8")
  return(res)
}

#' @title Retrieve entry with filtered dataset
#'
#' @description Similar with entry retrieval but filtered mapping entries with given datasets.
#'
#'
#' @param identifer Identifer for the entry.
#' @param source Dataset identifier
#' @param filters Comma seperated dataset identifer to retrieve
#' @param page Page index if results is more than default biobtree paging size.
#'
#' @return returns biobtree json object
#'
#' @author Tamer Gur
#'
#' @examples
#'
#'
#' bbStart()
#' bbEntryFilter("HGNC:12009","hgnc","uniprot,ensembl")
#'
#'


bbEntryFilter <-function(identifer,source,filters,page=NULL) {

  searchurl = paste0(getConfig()@endpoint,"/ws/filter/?i=",encodeURIComponent(identifer),'&s=', source , '&f=' ,filters)

  if (length(page) > 0) {
    searchurl =paste0(searchurl,"&p=" , page)
  }

  res <- fromJSON(searchurl,simplifyVector = FALSE,encoding = "UTF-8")

  return(res)

}

#' @title Retrieve entry result page
#'
#' @description If an entry contains large set of mapping entries it is paginated by biobtree with confiGured paging size. This function retrieve these paging for an entry
#'
#' @param identifer Identifer for the entry.
#' @param source Dataset identifier
#' @param page Page index it starts from 0
#' @param totalPage Total number of page for the entry. This value needs to calculate by user via using total number of entries which is available at the root result for the entry
#' and divide it confiGured biobtree paging size which has default value of 200
#'
#' @return returns biobtree json object
#'
#' @author Tamer Gur
#'
#' bbStart()
#' bbEntryPage("HGNC:12009","hgnc",0,0)
#'
#'

bbEntryPage <- function (identifer, source, page, totalPage) {

  searchurl = paste0(getConfig()@endpoint,"/ws/page/?i=" ,identifer , '&s=' , source , '&p=' , page , '&t=' , totalPage)

  res <- fromJSON(searchurl,simplifyVector = FALSE,encoding = "UTF-8")

  return(res)

}

