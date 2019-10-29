#' @title Search identifiers or special keywords
#'
#' @description Search identifiers or special keywords terms uniformly and resolve their actual unique identifiers and datasets. Keywords
#' can be several things for instance for uniprot an accession like "vav_human" can be a keyword which points to its original
#' identifier "P15498". Or gene name can be also a keyword like "tpi1" which could points multiple dataset like ensembl and hgnc.
#'
#' @param terms Comma seperated identifers or keywords
#' @param source Optional dataset identifiers to search only within this dataset.
#' @param filter Filter expression useful to filter out results when a keyword point several results. For instance if the biobtree index with multiple organism
#' a same gene search could hit several results for different species to filter only a specific species a filter can apply to search function.
#' @param page By default no need to pass this parameter since it returns all the results. It can be used with limit parameter for very large results to process them in paginated manner.
#' About paging every long search or mapping result paginated in biobtree and for paginated results every response contains a key to get the next page results. So if this
#' parameter is set with this key specified next page results returned for the given search term.
#' @param lite By default it is TRUE and allow function return quickly with data.frame containing most important fields. If set to TRUE function return raw results converted from json.
#' @param limit  Limits the number of search results. By default without any limit all the results returned.
#'
#' @return returns search results in data.frame by default if lite set it true returns json object
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbStart()
#' bbSearch("P15498,tpi1,shh")
#'
#' \dontrun{
#' # run this examplee with building the default dataset with bbBuildData()
#' bbSearch("tpi1","ensembl",filter='ensembl.genome=="homo_sapiens"')
#' }
#'

bbSearch <- function(terms,source=NULL,filter=NULL, page=NULL,limit=1000,showURL=FALSE,lite=TRUE){

  wsurl <- function(terms,source,filter,page,lite,showURL){

      searchurl <- paste0(getConfig()@endpoint,"/ws/?i=",encodeURIComponent(terms))

      if (length(page) > 0) {
        searchurl <-paste0(searchurl,"&p=" , page)
      }
      if (length(filter) > 0) {
        searchurl <- paste0(searchurl,"&f=" , filter)
      }
      if (length(source) > 0) {
        searchurl <-paste0(searchurl,"&s=" ,source)
      }
      if (!lite) {
        searchurl <-paste0(searchurl,"&d=d")
      }
      if (showURL){
        searchurl <- paste0(searchurl,"&u=u")
      }
      return(searchurl)
  }


  urlstr <- wsurl(terms,source,filter,page,lite,showURL)

  res <- fromJSON(urlstr,simplifyVector = FALSE,encoding = "UTF-8")

  if (length(res$Err)>0){
    return((res))
  }

  results<-res$results

  if(length(res$nextpage)>0 && length(results)< limit ){
        lastpagekey=res$nextpage
        while(length(lastpagekey)>0 && length(results)<limit){
          urlstr<- wsurl(terms,source,filter,lastpagekey,lite,showURL)
          r <- fromJSON(urlstr,simplifyVector = FALSE,encoding = "UTF-8")
          if (length(r$Err)>0){
            return(r)
          }
          results<-append(results,r$results)
          lastpagekey<-r$nextpage
        }
  }


  if (lite){
    input<-c()
    id<-c()
    source<-c()
    if(showURL){
      urls<-c()
    }
    i=1
    for(r in results){
      if (length(r$keyword)>0){
        input[i]<-r$keyword
      }else{
        input[i]<-r$identifier
      }
      id[i]<-r$identifier
      source[i]<-getConfig()@datasetMetaByNum[[paste(r$dataset)]]$id
      if(showURL){
        urls[i]<-r$url
      }
      i=i+1
    }

    df<-data.frame(input=input,identifier=id,dataset=source)
    if(showURL){
      df["url"]<-urls
    }
    return(df)
  }
  return(res)
}
