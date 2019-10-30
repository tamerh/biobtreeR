#' @title List Genomes
#'
#' @description This function list the ensembl genome names. These names can be used in bbBuildData function.
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

    stop(paste0("Invalid dataset type for genome listing",ensemblType))

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
    bbBuildData(rawArgs="install")
  }else if (length(conf@datasetMeta)==0){
    setConfig()
  }

  return(attrList())

}
