biobtreeREnv <- new.env(parent = emptyenv())
bbConfig<-NULL

#' @title Class for biobtreeR config
#'
#' @description This class holds the datasets meta data and web service endpoints and used while executing the search/mapping queries.
#' Instance of this class with name bbConfig is globally set by bbStart function. About dataset meta data, this class instance
#' holds all the datasets unqiue identifers, entry url templates etc. In biobtree each dataset has unique charachter
#' and numeric identifier. For instance Uniprot's charachter identifier is "uniprot" and numeric identifier is 1.
#' When performing queries the dataset charachter identifier is used for convinience but in actual database it is saved numerically.
#'
setClass("bbConfig", representation(bbDir="character",remote="logical",endpoint="character",metaEndpoint="character",datasetIDs="data.frame", datasetMeta="list", datasetMetaByNum="list" ))


getConfig <- function(){

  if (exists("bbConfig", envir=biobtreeREnv)){
    return(get("bbConfig", envir=biobtreeREnv))
  }

  localEndpoint="http://localhost:8888"

  conf<-new("bbConfig",
            bbDir=tempdir(),
            endpoint=localEndpoint,
            metaEndpoint=paste0(localEndpoint,"/ws/meta"),
            remote=FALSE)

  assign("bbConfig",conf, envir = biobtreeREnv)

  return(conf)

}

setConfig<-function(){

  conf<-getConfig()

  datasetIDs <- list()
  datasetMeta <- list()
  datasetMetaByNum <- list()

  if (conf@remote){

    rawmeta <- fromJSON(paste0(conf@endpoint,"/ws/meta"),simplifyVector = FALSE,encoding = "UTF-8")

    #first sort by numeric id
    datasetNumericIds <- sort(as.numeric(names(rawmeta)))

    for (v in datasetNumericIds) {
      datasetMetaByNum[v] <- rawmeta[paste(v)]
    }

    for (v in datasetNumericIds) {
      datasetMeta[datasetMetaByNum[[v]]$id] <- rawmeta[paste(v)]
      datasetMeta[[datasetMetaByNum[[v]]$id]]$id <- v
    }

  }else{

    conf@endpoint="http://localhost:8888"
    conf@metaEndpoint=paste0(conf@endpoint,"/ws/meta")

    srcFile<-file.path(paste0(conf@bbDir,"/conf/","source.dataset.json"))
    srcString<-readChar(srcFile,file.info(srcFile)$size)
    srcList <- fromJSON(srcString,simplifyVector = FALSE,encoding = "UTF-8")

    defFile<-file.path(paste0(conf@bbDir,"/conf/","default.dataset.json"))
    defString<-readChar(defFile,file.info(defFile)$size)
    defList <- fromJSON(defString,simplifyVector = FALSE,encoding = "UTF-8")

    optFile<-file.path(paste0(conf@bbDir,"/conf/","optional.dataset.json"))
    optString<-readChar(optFile,file.info(optFile)$size)
    optList <- fromJSON(optString,simplifyVector = FALSE,encoding = "UTF-8")

    datasetMeta <- appendList(appendList(srcList,defList),optList)

    for (v in names(datasetMeta)){
      idnum<-datasetMeta[[v]]$id
      datasetMetaByNum[idnum] <- datasetMeta[v]
      datasetMetaByNum[[idnum]]$id <- v
    }

    datasetNumericIds <- sort(as.numeric(names(datasetMetaByNum)))

  }

  if(length(datasetMeta)==0 || length(datasetMetaByNum)==0){
    stop("Error while retrieving datasets information.")
  }

  strID<-vector(length =length(datasetNumericIds) )
  numID<-c()
  types<-c()
  i=1
  for(v in datasetNumericIds){
    strID[[i]]<-datasetMetaByNum[[paste(v)]]$id
    numID[[i]]<-paste(v)
    if(i<30){
      types[i]<-"data source&target"
    }else{
      types[i]<-"target"
    }
    i=i+1
  }

  datasetIDs<-data.frame(id=strID,numeric_id=numID,type=types)

  conf@datasetIDs=datasetIDs
  conf@datasetMeta=datasetMeta
  conf@datasetMetaByNum=datasetMetaByNum
  assign("bbConfig",conf, envir = biobtreeREnv)

}

#' @title Use remote server
#'
#' @description Use this function if you running biobtree tool seperately in a remote server
#'
#' @param remoteEndpoint remote endpoint url with ip address and port in the form of http://${IP}:${PORT} .
#' Note that default port of biobtree is 8888.
#'
#' @return empty
#'
bbUseRemoteServer<-function(remoteEndpoint){

      bbStop()
      conf<-getConfig()
      conf@remote=TRUE
      conf@endpoint=remoteEndpoint
      conf@metaEndpoint=paste0(remoteEndpoint,"/ws/meta")
      assign("bbConfig",conf, envir = biobtreeREnv)

}

setOutDir<-function(outDir){

  conf<-getConfig()
  conf@bbDir=outDir
  assign("bbConfig",conf, envir = biobtreeREnv)
  return(0)

}

appendList <- function (x, val)
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}

bbExeFile<- function(){

  conf<-getConfig()

  if (Sys.info()['sysname'] == "Windows") {

    if(!file.exists("biobtree.exe")){

      latestUrl<-paste0("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_Windows_64bit.zip")
      download.file(latestUrl,"biobtree_Windows_64bit.zip",mode="wb")
      unzip("biobtree_Windows_64bit.zip")
      file.remove("biobtree_Windows_64bit.zip")

    }
    bbExe<-file.path(conf@bbDir,"biobtree.exe")
    return(bbExe)

  }else if (Sys.info()['sysname'] == "Darwin"){

    if(!file.exists("biobtree")){

      latestUrl<-paste0("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_MacOS_64bit.tar.gz")
      download.file(latestUrl,"biobtree_MacOS_64bit.tar.gz",mode="wb")
      system2("tar",args=" -xzvf biobtree_MacOS_64bit.tar.gz")
      file.remove("biobtree_MacOS_64bit.tar.gz")

    }
    bbExe<-file.path(conf@bbDir,"biobtree")
    return(bbExe)

  }else if (Sys.info()['sysname'] == "Linux"){

    if(!file.exists("biobtree")){

      latestUrl<-paste0("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_Linux_64bit.tar.gz")
      download.file(latestUrl,"biobtree_Linux_64bit.tar.gz",mode="wb")
      system2("tar",args=" -xzvf biobtree_Linux_64bit.tar.gz")
      file.remove("biobtree_Linux_64bit.tar.gz")

    }
    bbExe<-file.path(conf@bbDir,"biobtree")
    return(bbExe)

  }

  stop(paste0("Unsupported OS -> ",Sys.info()['sysname']))

}

latestbbVersion<- function(){

  relurl<-GET("https://github.com/tamerh/biobtree/releases/latest")
  if (relurl$status_code!=200){
    stop("Error while connecting github for retrieving file")
  }
  splittedurl<-unlist(strsplit(relurl$url,"/"))
  return(splittedurl[length(splittedurl)])

}
