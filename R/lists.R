#' @title List Genomes
#'
#' @description This function list the ensembl genome names. These names can be used in bbBuildData function. If biobtree web server
#' is not runnint this function triggers to run it. And also if no data built with bbBuildData before it create a example data using
#' hgnc just to show the list the genomes since biobtree web server require at least one built data to properly start.
#'
#' @param ensemblType These param can be one of the following "ensembl", "ensembl_bacteria", "ensembl_fungi", "ensembl_metazoa", "ensembl_plants", "ensembl_protists"
#'
#' @return returns list of genome names
#'
#' @examples
#'
#' bbListGenomes("ensembl")
#' #For ensembl genomes
#' bbListGenomes("ensembl_bacteria")
#' bbListGenomes("ensembl_fungi")
#' bbListGenomes("ensembl_metazoa")
#' bbListGenomes("ensembl_plants")
#' bbListGenomes("ensembl_protists")
#'
bbListGenomes <- function(ensemblType){

  if (!(ensemblType =="ensembl" || ensemblType =="ensembl_bacteria" || ensemblType =="ensembl_fungi"
      || ensemblType =="ensembl_metazoa" || ensemblType =="ensembl_plants" || ensemblType =="ensembl_protists")){

    stop(p("Invalid dataset type for genome listing",ensemblType))

  }

  if(isbbRunning()){

    geneomeJsonUrl <- paste0(getConfig()@endpoint,"/genomes/",ensemblType,".paths.json")

    res<-content(GET(geneomeJsonUrl),as = "text",encoding = "UTF-8")

    resjson <- fromJSON(res,simplifyVector = FALSE,encoding = "UTF-8")

    return(names(resjson$jsons))

  }else{

    conf<-getConfig()

    if(!file.exists(file.path(conf@bbDir,"ensembl","ensembl.paths.json"))){
      bbBuildData(rawArgs="install")
    }

    genomeFile<-file.path(paste0(conf@bbDir,"/ensembl/",ensemblType,".paths.json"))
    genomeString<-readChar(genomeFile,file.info(genomeFile)$size)
    genomePaths <- fromJSON(genomeString,simplifyVector = FALSE,encoding = "UTF-8")

    return(names(genomePaths$jsons))

  }


}

#' @title List available datasets
#'
#' @description This function list the available source and target datasets with their numeric identifiers. If biobtree web server
#' is not runnint this function triggers to run it. And also if no data built with bbBuildData before it create a example data using
#' hgnc just to show the available datasets since biobtree web server require at least one built data to properly start.
#'
#' @param outDir If outDir specified during bbBuiltData specified same one here. If not specified tempdir is used.
#'
#' @param biobtreeURL Use this this parameter if you running biobtree tool seperately such as in a remote server.
#' Then set this parameter with the endpoint of this running biobtree url with its ip address and port http://${IP}:${PORT} .
#' Note that default port of biobtree is 8888.
#' @return returns list of datasets
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
    bbBuildData(rawArgs="install")
  }else if (length(conf@datasetIDs)==0){
    setConfig()
  }

  return(getConfig()@datasetIDs)

}

#' @title Retrieve attributes of dataset
#'
#' @description Provides list of available attributes for a dataset to use in search and mapping queries.
#'
#' @param source Dataset identifier
#'
#' @return returns attributes names
#'
#'
#' @examples
#'
#' bbBuildData(datasets="hgnc")
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
    bbBuildData(rawArgs="install")
  }else if (length(conf@datasetMeta)==0){
    setConfig()
  }

  return(attrList())

}
