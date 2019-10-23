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
setClass("bbConfig", representation(endpoint="character",datasetIDs="data.frame", datasetMeta="list", datasetMetaByNum="list" ))


#' @title Build your data
#'
#' @description This function allows to build your selected datasets locally for making search/mapping queries via biobtree executable. Available datasets
#' are uniprot,ensembl,ensembl genomes,hgnc,taxonomy,chembl,hmdb,go,eco,efo,chebi,interpro,literature_mappings,uniparc,uniref50,uniref90,uniref100,uniprot_unreviewed.
#' These datasets are called source datasets other dataset' identifers and mappings retrieved from these source datasets. Note that any new call
#' of this function overrides previously built data.
#'
#' @param genome Comma seperated list of ensembl genomes. To list all the genome names \code{bbListGenomes} function can be used.
#'
#' @param datasets Comma seperated list of datasets for build. Default datasets are taxonomy ensembl(homo_sapiens) uniprot(reviewed) hgnc go eco efo chebi interpro
#'
#' @param targetDatasets Comma seperated target datasets. This params allows to buld data with only given datasets. It is useful if interested only certain dataset mappings
#' It speeds up the process and save disc space.
#'
#' @param genomePattern Alternative to the genome paramter to build set of genomes conveniently
#'
#' @param outDir Output directory for biobtree data. If not specified temp directory is used.
#'
#' @param rawArgs Directly run biobtree with its all available arguments. If this paramter used all other parameters are ignored
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbStop() # stop first if running
#' bbBuildData(datasets="hgnc")
#'
#' bbBuildData(datasets="hgnc",targetDatasets="uniprot,ensembl")
#' \dontrun{
#'
#'   # build all the default dataset
#'   bbBuildData()
#'
#'   # build both mouse and human genomes in ensembl insted of default human
#'   bbBuildData(genome="homo_sapiens,mus_musculus")
#'
#'   # build default datasets with all the ensembl genomes with names contains
#'   # "mus" which are "mus_caroli", "mus_musculus", "mus_musculus_129s1svimj" ...
#'   bbBuildData(genomePattern="mus")
#'
#'   # for ensembl genomes needs to specify seperately in datasets
#'   # + means default dataset plus check all the genomes with bbListGenomes()
#'   bbBuildData(datasets="+ensembl_metazoa",genomes="caenorhabditis_elegans,drosophila_melanogaster")
#'   bbBuildData(datasets="+ensembl_fungi",genomes="saccharomyces_cerevisiae")
#'   bbBuildData(datasets="+ensembl_plants,ensembl_protists", \
#'   genomes="arabidopsis_thaliana,phytophthora_parasitica")
#'   bbBuildData(datasets="+ensembl_bacteria",genomes="salmonella_enterica")
#'   # bacteria genomes with pattern
#'   bbBuildData(datasets="+ensembl_bacteria",genomePattern="serovar_infantis,serovar_virchow")
#'
#'   #build only certain datasets
#'   bbBuildData(datasets="uniprot,hgnc")
#'
#'
#' }
bbBuildData<-function(genome=NULL,datasets=NULL,targetDatasets=NULL,genomePattern=NULL,outDir=NULL,rawArgs=NULL) {

  if(isbbRunning()){
    stop("There is a running biobtree stop with bbStop()")
  }

  rootDir<-getwd()

  if(length(outDir)>0){
    if(!file.exists(file.path(outDir))){
      stop("Specified outDir is not exist")
    }
    bbDir<-file.path(outDir)
  }else{
    bbDir<-tempdir()
  }

  #dir.create(bbDir, showWarnings = FALSE)
  setwd(bbDir)

  execFile <- bbExeFile()

  if(length(rawArgs)>0){

    system2(execFile,args=rawArgs)

  }else if (length(datasets)>0 && datasets=="sample_data"){

    args<-" -d go,hgnc,uniprot,ensembl,interpro"
    args<- p(args," --uniprot.file ",system.file("exdata/uniprot_sample.xml.gz",package="biobtreeR"))
    args<- p(args," --interpro.file ",system.file("exdata/interpro_sample.xml.gz",package="biobtreeR"))
    untar(system.file("exdata/ensembl_sample.json.tar.gz",package="biobtreeR"))
    args<- p(args," --ensembl.file ","ensembl_sample.json")
    untar(system.file("exdata/go_sample.tar.gz",package="biobtreeR"))
    args<- p(args," --go.file ","go_sample.owl")
    args<-p(args," build")

    system2(execFile,args=args)

  }else{

    args<-""

    if(length(genome)>0){
      args<-p(args," -s ",genome)
    }else if (length(genomePattern)>0){
      args<-p(args," -sp ",genomePattern)
    }

    if(length(datasets)>0){
      args<-p(args," -d ",datasets)
    }

    if(length(targetDatasets)>0){
      args<-p(args," -t ",targetDatasets)
    }

    args<-p(args," build")

    system2(execFile,args=args)

  }

  setwd(rootDir)
}

#' @title Start biobtreeR
#'
#' @description Once target datasets is built with \code{bbBuildData} this function used to start biobtree server in the background for performing search/mapping queries.
#' This function also sets the bbConfig variable
#'
#' @param outDir If outDir specified during bbBuiltData specified same one here. If not specified tempdir is used.
#'
#' @param biobtreeURL Use this this parameter if you running biobtree tool seperately such as in a remote server.
#' Then set this parameter with the endpoint of this running biobtree url with its ip address and port http://${IP}:${PORT} .
#' Note that default port of biobtree is 8888.
#'
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#' bbStart()
#' bbStop()
bbStart<-function(outDir=NULL,biobtreeURL=NULL) {

  if (length(biobtreeURL) > 0) {

    getConfig(biobtreeURL)
    return(p("biobtreeR configured wth url ",biobtreeURL))

  }else{

    rootDir<-getwd()

    if(length(outDir)>0){
      if(!file.exists(file.path(outDir))){
        stop("Specified outDir is not exist")
      }
      bbDir<-file.path(outDir)
    }else{
      bbDir<-tempdir()
    }

    setwd(bbDir)

    execFile <- bbExeFile()

    if(!isbbRunning()){ #First check for running biobtree if found use it

      system2(execFile,args = "web",wait = FALSE)

      # wait here until biobtree data process complete
      print("Starting biobtree...")
      elapsed<-0
      timeoutDuration<-50
      while(TRUE){

        if (elapsed > timeoutDuration){
          bbStop()
          stop("biobtree could not started. Please check that you have built data correctly")
        }

        Sys.sleep(1)

        if(!isbbRunning()){
          elapsed<-elapsed+1
          next
        }

        getConfig()
        setwd(rootDir)
        return("biobtreeR started")

      }
    }else{

      getConfig()
      setwd(rootDir)
      return("biobtreeR started before.Config refreshed")

    }

 }

}

getConfig <- function(endpoint=NULL){

  if (exists("bbConfig", envir=biobtreeREnv)){
    return(get("bbConfig", envir=biobtreeREnv))
  }

  datasetIDs <- list()
  datasetMeta <- list()
  datasetMetaByNum <- list()

  if(length(endpoint)<=0){
    endpoint <- "http://localhost:8888"
  }

  rawmeta <- fromJSON(p(endpoint,"/ws/meta"),simplifyVector = FALSE,encoding = "UTF-8")

  #first sort by numeric id
  datasetNumericIds <- sort(as.numeric(names(rawmeta)))

  for (v in datasetNumericIds) {
    datasetMetaByNum[v] <- rawmeta[paste(v)]
  }

  for (v in datasetNumericIds) {
    datasetMeta[datasetMetaByNum[[v]]$id] <- rawmeta[paste(v)]
    datasetMeta[[datasetMetaByNum[[v]]$id]]$id <- v
  }

  if(length(datasetMeta)==0 || length(datasetMetaByNum)==0){
    stop("Error while retrieving meta information. Check the biobtree server")
  }

  strID<-c()
  numID<-c()
  types<-c()
  i=1
  for(v in datasetNumericIds){
    strID[i]<-datasetMetaByNum[[v]]$id
    numID[i]<-v
    if(i<30){
      types[i]<-"source&target"
    }else{
      types[i]<-"target"
    }
    i=i+1
  }
  datasetIDs<-data.frame(id=strID,numeric_id=numID,type=types)

  conf<-new("bbConfig",
            endpoint=endpoint,
            datasetIDs=datasetIDs,
            datasetMeta=datasetMeta,
            datasetMetaByNum=datasetMetaByNum)

  assign("bbConfig",conf, envir = biobtreeREnv)

  return(conf)

}

#
#Checks for running biobtree if found use it
#
isbbRunning <- function(biobtreeURL=NULL){

  if (length(biobtreeURL)>0){
    metaEndpoint<-p(biobtreeURL,"/ws/meta")
  }else{
    metaEndpoint<-"http://localhost:8888/ws/meta"
  }

  response <- tryCatch(
    HEAD(metaEndpoint),
    error=function(e) e
  )

  return(!inherits(response, "error"))

}

bbExeFile<- function(){

  if (Sys.info()['sysname'] == "Windows") {

  if(!file.exists("biobtree.exe")){

    latestUrl<-p("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_Windows_64bit.zip")
    download.file(latestUrl,"biobtree_Windows_64bit.zip",mode="wb")
    unzip("biobtree_Windows_64bit.zip")
    file.remove("biobtree_Windows_64bit.zip")

  }
    rootDir<-getwd()
    bbExe<-file.path(rootDir,"biobtree.exe")
    return(bbExe)

  }else if (Sys.info()['sysname'] == "Darwin"){

    if(!file.exists("biobtree")){

      latestUrl<-p("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_MacOS_64bit.tar.gz")
      download.file(latestUrl,"biobtree_MacOS_64bit.tar.gz",mode="wb")
      system2("tar",args=" -xzvf biobtree_MacOS_64bit.tar.gz")
      file.remove("biobtree_MacOS_64bit.tar.gz")

    }
    rootDir<-getwd()
    bbExe<-file.path(rootDir,"biobtree")
    return(bbExe)

  }else if (Sys.info()['sysname'] == "Linux"){

    if(!file.exists("biobtree")){

      latestUrl<-p("https://github.com/tamerh/biobtree/releases/download/",latestbbVersion(),"/biobtree_Linux_64bit.tar.gz")
      download.file(latestUrl,"biobtree_Linux_64bit.tar.gz",mode="wb")
      system2("tar",args=" -xzvf biobtree_Linux_64bit.tar.gz")
      file.remove("biobtree_Linux_64bit.tar.gz")

    }
    rootDir<-getwd()
    bbExe<-file.path(rootDir,"biobtree")
    return(bbExe)

  }

  stop(p("Unsupported OS -> ",Sys.info()['sysname']))

}

latestbbVersion<- function(){

  relurl<-GET("https://github.com/tamerh/biobtree/releases/latest")
  if (relurl$status_code!=200){
    stop("Error while connecting github for retrieving file")
  }
  splittedurl<-unlist(strsplit(relurl$url,"/"))
  return(splittedurl[length(splittedurl)])

}

#' @title List Genomes
#'
#' @description This function list the ensembl genome names. These names can be used in bbBuildData function. If biobtree web server
#' is not runnint this function triggers to run it. And also if no data built with bbBuildData before it create a example data using
#' hgnc just to show the list the genomes since biobtree web server require at least one built data to properly start.
#'
#' @param ensemblType These param can be one of the following "ensembl", "ensembl_bacteria", "ensembl_fungi", "ensembl_metazoa", "ensembl_plants", "ensembl_protists"
#'
#' @param outDir If outDir specified during bbBuiltData specified same one here. If not specified tempdir is used.
#'
#' @param biobtreeURL Use this this parameter if you running biobtree tool seperately such as in a remote server.
#' Then set this parameter with the endpoint of this running biobtree url with its ip address and port http://${IP}:${PORT} .
#' Note that default port of biobtree is 8888.
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
bbListGenomes <- function(ensemblType,outDir=NULL,biobtreeURL=NULL){


  if (length(biobtreeURL) > 0) {

    if(!isbbRunning(biobtreeURL)){
      stop("biobtree with given url is not running")
    }

  }else{

    if(!isbbRunning()){

      if(length(outDir)>0){
        if(!file.exists(file.path(outDir))){
          stop("Specified outDir is not exist")
        }
        bbDir<-file.path(outDir)
      }else{
        bbDir<-tempdir()
      }

      if(!file.exists(file.path(bbDir,"out","db","db.meta.json"))){
        bbBuildData(outDir = bbDir,datasets = "hgnc") # this just to allow user discover genomes without calling built data
      }
      bbStart(outDir = bbDir,biobtreeURL = biobtreeURL)
    }

  }


  if (ensemblType =="ensembl" || ensemblType =="ensembl_bacteria" || ensemblType =="ensembl_fungi"
      || ensemblType =="ensembl_metazoa" || ensemblType =="ensembl_plants" || ensemblType =="ensembl_protists"){

    geneomeJsonUrl <- p(getConfig(endpoint=biobtreeURL)@endpoint,"/genomes/",ensemblType,".paths.json")

    res<-content(GET(geneomeJsonUrl),as = "text")

    resjson <- fromJSON(res)

    return(names(resjson$jsons))

  }else{
    stop(p("Invalid ensembl type ",ensemblType))
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
bbListDatasets <- function(outDir=NULL,biobtreeURL=NULL){

  if (length(biobtreeURL) > 0) {

    if(!isbbRunning(biobtreeURL)){
      stop("biobtree with given url is not running")
    }
    return(getConfig(biobtreeURL)@datasetIDs)

  }else{

    if(!isbbRunning()){

      if(length(outDir)>0){
        if(!file.exists(file.path(outDir))){
          stop("Specified outDir is not exist")
        }
        bbDir<-file.path(outDir)
      }else{
        bbDir<-tempdir()
      }

      if(!file.exists(file.path(bbDir,"out","db","db.meta.json"))){
        bbBuildData(outDir = bbDir,datasets = "hgnc") # this just to allow user discover genomes without calling built data
      }
      bbStart(outDir = bbDir,biobtreeURL = biobtreeURL)
    }
    return(getConfig()@datasetIDs)

  }

}

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

bbSearch <- function(terms,source=NULL,filter=NULL, page=NULL,lite=TRUE,limit=1000){

  wsurl <- function(terms,source,filter,page){

      searchurl <- p(getConfig()@endpoint,"/ws/?i=",encodeURIComponent(terms))

      if (length(page) > 0) {
        searchurl <-p(searchurl,"&p=" , page)
      }
      if (length(filter) > 0) {
        searchurl <- p(searchurl,"&f=" , filter)
      }
      if (length(source) > 0) {
        searchurl <-p(searchurl,"&s=" ,source)
      }
      return(searchurl)
  }


  urlstr <- wsurl(terms,source,filter,page)

  res <- fromJSON(urlstr,simplifyVector = FALSE,encoding = "UTF-8")

  if (length(res$Err)>0){
    return((res))
  }

  results<-res$results

  if(length(res$nextpage)>0 && length(results)< limit ){
        lastpagekey=res$nextpage
        while(length(lastpagekey)>0 && length(results)<limit){
          urlstr<- wsurl(terms,source,filter,lastpagekey)
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
    i=1
    for(r in results){
      if (length(r$keyword)>0){
        input[i]<-r$keyword
      }else{
        input[i]<-r$identifier
      }
      id[i]<-r$identifier
      source[i]<-getConfig()@datasetMetaByNum[[r$dataset]]$id
      i=i+1
    }
    df<-data.frame(input=input,identifier=id,dataset=source)
    return(df)
  }
  return(res)
}


#' @title Chain mapping and filtering
#'
#' @description Chain mapping identifiers or keywords with filtering and retrieving attributes if available.
#'
#' @param terms Input terms for the mapping. Same with search functionality they can be comma seperated identifers or keywords
#' @param mapfilter Mapping query which consist of map and optional filter functions in the form of map(dataset).filter(Boolean query expression)
#' The boolean expressions are based on datasets attributes and dataset attributes can be list with bbAttr function.
#' Dataset attributes which used in the filters starts with their dataset name. In biobtree boolean expressions
#' feature has been implemented via Google common expression language so its full capability can be checked in its documentation.
#' @param source Optional dataset identifiers for searching input terms within the given dataset.
#' @param page Optional parameter works similar with bbSearch page paramter.
#' @param lite By default it is TRUE and allow function return quickly with data.frame with mapping identifiers and attributes. If set to TRUE function return raw results converted from json.
#' @param limit Limits the number of mapping results. By default without any limit all the results returned.
#' @param inattrs Optional comma seperated attribute names for input identifiers and if available their values includes in result data.frame
#' @param attrs Optional comma seperated attribute names for mapping identifiers and if available their values includes in result data.frame
#' @param showInputColumn Optional logical parameter to show the input identifers in the result data.frame
#'
#' @return returns mapping results in data.frame by default if lite set it true returns json object
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbStart()
#' bbMapping("tpi1",'map(uniprot)')
#' bbMapping("shh",'map(ensembl)')
#'
#' \dontrun{
#' # run these examples with building the default dataset with bbBuildData()
#' #Map protein to its go terms and retrieve go term types
#' bbMapping("AT5G3_HUMAN",'map(go)',attrs = "type")
#'
#' #Map protein to its go terms with filter by its type and retrieve their types
#' bbMapping("AT5G3_HUMAN",'map(go).filter(go.type=="biological_process")',attrs = "type")
#'
#' #Map gene names to exon identifiers and retrieve the region
#' bbMapping("ATP5MC3,TP53",'map(transcript).map(exon)',attrs = "seq_region_name")
#'
#' #Map Affymetrix identifiers to Ensembl identifiers and gene names
#' bbMapping("202763_at,213596_at,209310_s_at",source ="affy_hg_u133_plus_2"
#' ,'map(transcript).map(ensembl)',attrs = "name")
#'
#'}
bbMapping <- function(terms, mapfilter, page=NULL, source=NULL,lite=TRUE,limit=1000,inattrs=NULL,attrs=NULL,showInputColumn=FALSE){

  wsurl <- function(terms,mapfilter,source,page){

    mfurl <- p(getConfig()@endpoint,"/ws/map/?i=",encodeURIComponent(terms),"&m=",encodeURIComponent(mapfilter))

    if (length(page) > 0) {
      mfurl <-p(mfurl,"&p=" , page)
    }
    if (length(source) > 0) {
      mfurl <-p(mfurl,"&s=" ,source)
    }

    return(mfurl)

  }

  totalMapping=0
  urlstr <- wsurl(terms,mapfilter,source,page)
  res <- fromJSON(urlstr,simplifyVector = FALSE,encoding = "UTF-8")
  if (length(res$Err)>0){
      return(res)
  }

  results <- res$results

  for (r in results){
    totalMapping=totalMapping+length(r$targets)
  }

  lastpagekey<-res$nextpage

  if(length(lastpagekey)>0 && totalMapping< limit ){

    while(length(lastpagekey)>0 && totalMapping<limit){

      urlstr<- wsurl(terms,mapfilter,source,lastpagekey)
      newres <- fromJSON(urlstr,simplifyVector = FALSE,encoding = "UTF-8")

      if (length(newres$Err)>0){
        return(res$Err)
      }

      for(i in seq_len(length(newres$results))){
          found=FALSE
          for(j in seq_len(length(results))){
            if ( identical(newres$results[[i]]$source$doman_id,results[[j]]$source$doman_id) && identical(newres$results[[i]]$source$identifier,results[[j]]$source$identifier)) {
              results[[j]]$targets<-append(results[[j]]$targets,newres$results[[i]]$targets)
              totalMapping=totalMapping+length(newres$results[[i]]$targets)
              found=TRUE
              break
            }
          }
      }

      lastpagekey=newres$nextpage

    }
  }

  if (lite){

    multiInput<-FALSE
    if(length(results)>1 || showInputColumn){
      multiInput<-TRUE
    }

    inAttrsNames<-NULL
    inAttrsVals<-NULL

    if(length(inattrs)>0){
      multiInput<-TRUE
      inAttrsNames<-unlist(strsplit(inattrs,","))
      inAttrsVals<-list()
      for(at in inAttrsNames){
        inAttrsVals[at] <- c()
      }
    }

    attrsNames<-NULL
    attrsVals<-NULL

    if(length(attrs)>0){
      attrsNames<-unlist(strsplit(attrs,","))
      attrsVals<-list()
      for(at in attrsNames){
        attrsVals[at] <- c()
      }
    }

    if(multiInput){
      in_id<-c()
      in_source<-c()
    }
    map_id<-c()

    index=1
    for(i in seq_len(length(results))){

      for(j in seq_len(length(results[[i]]$targets))){

        if(multiInput){
          if(length(results[[i]]$source$keyword)>0){
            in_id[index]<-p(results[[i]]$source$keyword,"-",results[[i]]$source$identifier)
          }else{
            in_id[index]<-results[[i]]$source$identifier
          }
        }

        map_id[index]<-results[[i]]$targets[[j]]$identifier

        if(j==1){

          if(multiInput){
            source_id<-results[[i]]$source$dataset
            in_source[index]<-getConfig()@datasetMetaByNum[[source_id]]$id
          }
        }else{
          if(multiInput){
            in_id[index]<-"-"
            in_source[index]<-"-"
          }
        }
        if(length(attrsNames)>0){
          attrName<-names(results[[i]]$targets[[j]]$Attributes)[1]
          if(attrName!="Empty"){
            attrsPath<-p("results[[i]]$targets[[j]]$Attributes$",attrName)
            for(at in attrsNames){
              atPath<-p(attrsPath,"$",at)
              attrsVals[[at]][index]<-eval(parse(text=atPath))
            }
          }else{
            for(at in attrsNames){
              attrsVals[[at]][index]<-""
            }
          }

        }

        if(length(inAttrsNames)>0){
          attrsPath<-p("results[[i]]$source$Attributes$",names(results[[i]]$source$Attributes)[1])
          for(at in inAttrsNames){
            atPath<-p(attrsPath,"$",at)
            inAttrsVals[[at]][index]<-eval(parse(text=atPath))
          }
        }

        index=index+1
      }

    }
    if(multiInput){
      df<-data.frame(input=in_id,input_dataset=in_source)

      if(length(inAttrsNames)>0){
        for(at in inAttrsNames){
          df[at]<-inAttrsVals[at]
        }
      }

      df["mapping_id"]=map_id

    }else{
      df<-data.frame(mapping_id=map_id)
    }

    if(length(attrsNames)>0){
      for(at in attrsNames){
        df[at]<-attrsVals[at]
      }
    }

    return(df)

  }

  if(length(lastpagekey)>0){
    return(list(results=results,nextpage=lastpagekey))
  }

  return(list(results=results))

}

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

  searchurl <- p(getConfig()@endpoint,"/ws/entry/?i=",encodeURIComponent(identifer),"&s=",source)
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

  searchurl = p(getConfig()@endpoint,"/ws/filter/?i=",encodeURIComponent(identifer),'&s=', source , '&f=' ,filters)

  if (length(page) > 0) {
    searchurl =p(searchurl,"&p=" , page)
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

  searchurl = p(getConfig()@endpoint,"/ws/page/?i=" ,identifer , '&s=' , source , '&p=' , page , '&t=' , totalPage)

  res <- fromJSON(searchurl,simplifyVector = FALSE,encoding = "UTF-8")

  return(res)

}

#' @title Retrieve unique url of entry
#'
#' @description Provides unique url for the identifier and dataset
#'
#' @param identifer Identifer for the entry.
#' @param source Dataset identifier
#'
#' @return returns unique url
#'
#'  @author Tamer Gur
#'
#' @examples
#' bbStart()
#' bbURL("HGNC:12009","hgnc")
#' bbURL("p15498","uniprot")
#' bbStop()
#'

bbURL <-function(identifer,source){

  if(length(getConfig()@datasetMeta[[source]])==0){
     return(p("Error invalid dataset ",source))
  }
  if(length(getConfig()@datasetMeta[[source]]$url)==0){
     return("Error dataset has no url confiGured")
  }

  config<-getConfig()
  if(source=="ufeature"){

    pid<-unlist(strsplit(identifer,"_"))
    res<-gsub("\u00A3\\{id\\}",pid[1],config@datasetMeta[[source]]$url)

  }else if(source=="variantid"){

    pid<-tolower(identifer)
    res<-gsub("\u00A3\\{id\\}",pid[1],config@datasetMeta[[source]]$url)

  }else if (source=="ensembl" || source=="transcript" || source=="exon"){

    res<-""
    r<-bbEntry(identifer,source)

    if (length(r$Err)>0){
      return((r))
    }

    if (length(r[[1]]$Attributes$Ensembl)>0){
      branch<-r[[1]]$Attributes$Ensembl$branch
      if(branch==1){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$url)
      }else if(branch==2){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$bacteriaUrl)
      }else if(branch==3){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$fungiUrl)
      }else if(branch==4){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$metazoaUrl)
      }else if(branch==5){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$plantsUrl)
      }else if(branch==6){
        res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$protistsUrl)
      }
      if(source=="ensembl"){
        res<-gsub("\u00A3\\{sp\\}",r[[1]]$Attributes$Ensembl$genome,res)
      }

    }

  }else{
    res<-gsub("\u00A3\\{id\\}",identifer,config@datasetMeta[[source]]$url)
  }

  return(res)

}

#' @title Retrieve attributes of dataset
#'
#' @description Provides list of available attributes for a dataset to use in search and mapping queries. This function works with a sample identifer.
#'
#' @param identifer Identifer for the entry.
#' @param source Dataset identifier
#'
#' @return returns attributes names
#'
#' @author Tamer Gur
#'
#' @examples
#' bbBuildData(datasets="hgnc")
#' bbStart()
#' bbAttr("HGNC:12009","hgnc")
#'

bbAttr <- function(identifer,source){

    res<- bbSearch(identifer,source,lite=FALSE)

    if (length(res$Err)>0){
      return((res))
    }

    if(startsWith(source,"chembl")){
      attrsPath<-p("res$results[[1]]$Attributes$",names(res$results[[1]]$Attributes)[1],"$",names(res$results[[1]]$Attributes[[1]])[1])
    }else{
      attrsPath<-p("res$results[[1]]$Attributes$",names(res$results[[1]]$Attributes)[1])
    }

    reslist=eval(parse(text=attrsPath))
    return(names(reslist))

}

#'
#' @title Stop biobtree
#'
#' @description Stops running background biobtree process which started with \code{bbStart}
#' @return returns empty
#'
#' @examples
#' bbStop()
bbStop <- function(){

  if(isbbRunning(getConfig()@endpoint)){

    if (Sys.info()['sysname'] == "Windows") {

        system2("taskkill",args = "/IM biobtree.exe /F")

    }else if (Sys.info()['sysname'] == "Darwin"){

      system2("killall",args = "biobtree")

    }else if (Sys.info()['sysname'] == "Linux"){

      system2("killall",args = "biobtree")

    }

  }

}

deleteIfExist<-function(name){
  if(file.exists(name)){
    file.remove(name)
  }
}

p <- function(..., sep='') {
    paste(..., sep=sep, collapse=sep)
}
