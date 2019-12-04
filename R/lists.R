#' @title List available datasets
#'
#' @description Lists the available source and target datasets with their numeric identifiers.
#'
#' @return returns datasets
#'
#' @examples
#'
#' bbListDatasets()
#'
bbListDatasets <- function(){

  conf<-getConfig()

  if(length(conf@datasetIDs)>0){
    return(conf@datasetIDs)
  }

  if(!file.exists(file.path(conf@bbDir,"conf","source.dataset.json"))){
    bbBuildCustomDB(rawArgs="install")
  }else if (length(conf@datasetIDs)==0){
    setConfig()
  }

  return(getConfig()@datasetIDs)

}

#' @title Retrieve attributes of dataset
#'
#' @description Provides list of available attributes for a dataset to use in search and mapping queries.
#'
#' @param dataset Dataset identifier
#'
#' @return attributes names
#'
#'
#' @examples
#'
#' bbListAttrs("hgnc")
#' bbListAttrs("ensembl")
#'

bbListAttrs <- function(dataset){


  attrList<-function(){

    conf<-getConfig()

    if (!(dataset %in% names(conf@datasetMeta))){
      stop("Invalid dataset")
    }

    attrsStr<-conf@datasetMeta[[dataset]]$attrs

    if (!is.null(attrsStr)){
      return(unlist(strsplit(attrsStr,",")))
    }else{
      return(c())
    }
  }

  conf<-getConfig()

  if(length(conf@datasetMeta)>0){
    return(attrList())
  }

  if(!file.exists(file.path(conf@bbDir,"conf","source.dataset.json"))){
    bbBuildCustomDB(rawArgs="install")
  }else if (length(conf@datasetMeta)==0){
    setConfig()
  }

  return(attrList())

}
