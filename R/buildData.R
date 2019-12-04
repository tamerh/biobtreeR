#' @title Get pre build biobtree database
#'
#' @description Pre build biobtree database for commonly studied datasets and model organism genomes. Once this function called it retrieves
#' the pre build database saves to users output directory.
#'
#' @param type built in database type accepted values are 1,2 and 3. Currently there are 3 different builtin database;
#' Type 1
#' Included datasets hgnc,hmdb,taxonomy,go,efo,eco,chebi,interpro
#' Included uniprot proteins and ensembl genomes belongs to following organisms
#' homo_sapiens 9606 --> ensembl
#' danio_rerio 7955 zebrafish --> ensembl
#' gallus_gallus 9031 chicken --> ensembl
#' mus_musculus 10090 --> ensembl
#' Rattus norvegicus 10116 ---> ensembl
#' saccharomyces_cerevisiae 4932--> ensembl
#' arabidopsis_thaliana 3702--> ensembl
#' drosophila_melanogaster 7227 --> ensembl
#' caenorhabditis_elegans 6239 --> ensembl
#' Escherichia coli 562 --> ensembl_bacteria
#' Escherichia coli str. K-12 substr. MG1655 511145 --> ensembl_bacteria
#' Escherichia coli K-12 83333 --> ensembl_bacteria
#'
#' Type 2
#' Instead of genomes in in the type 1 it contains human and all the mouse strains genomes in the ensembl
#'
#' Type 3
#' Contains no genome but it contains all the uniprot data.
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbUseOutDir(tempdir()) # temp dir for demo purpose
#' bbBuiltInDB("demo") # small demo database for real database use 1, 2 or 3
#'
#'
bbBuiltInDB<- function(type="1"){
  retrbbData(type,"d")
}


# Retrieve pre built data hosted in the https://github.com/tamerh/biobtree-conf/releases
retrbbData<- function(index,type){

  if(isbbRunning()){
    stop("There is a running biobtree stop with bbStop()")
  }

  rootDir<-getwd()

  conf<-getConfig()

  if(length(conf@bbDir)==0){
    stop("out directory is not set")
  }

  tryCatch(
    {

      setwd(conf@bbDir)

      if(file.exists(file.path(conf@bbDir,"out"))){
         unlink(file.path(conf@bbDir,"out"),recursive = TRUE)
      }

      bbConfVersion<-latestbbConfVersion()
      targetFile<-paste0("biobtree-conf-",bbConfVersion,"-set",index,type,".tar.gz")
      targetFileUrl<-paste0("https://github.com/tamerh/biobtree-conf/releases/download/",bbConfVersion,"/",targetFile)
      download.file(targetFileUrl,targetFile,mode="wb")
      system2("tar",args=paste0(" -xzvf ",targetFile))
      file.remove(targetFile)

    },finally = {

      setwd(rootDir)

    }

  )

}

#' @title Build custom DB
#'
#' @description biobtree covers all the genomes in ensembl and ensembl genomes. If the the studied organism genome is not included
#' in the default pre built in databases then this function is used and build the biobtree database locally for given genomes.
#'
#' @param taxonomyIDs Comma seperated list of taxonomy identifiers for building the genomes
#'
#' @param rawArgs For using all available biobtree command line arguments directly
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' \dontrun{
#'
#' bbUseOutDir("your directory path")
#' bbBuildCustomDB(taxonomyIDs="1408103,206403")
#'
#'}
#'
bbBuildCustomDB<- function(taxonomyIDs=NULL,rawArgs=NULL){

  if(isbbRunning()){
    stop("There is a running biobtree stop with bbStop()")
  }

  rootDir<-getwd()

  conf<-getConfig()

  if(length(conf@bbDir)==0){
    stop("out directory is not set")
  }

  bbDir<-conf@bbDir

  tryCatch(
    {

      setwd(bbDir)

      execFile <- bbExeFile()

      if(length(rawArgs)>0){

        system2(execFile,args=rawArgs)

      }else{

        if(length(taxonomyIDs)==0){
          stop("At least one taxonomy identifier is required to build custom data")
        }

        retrbbData("3","r")

        args<-paste0(" -tax ",taxonomyIDs," --keep --ensembl-orthologs"," build")

        system2(execFile,args=args)

      }

      setConfig()

    },finally = {

      setwd(rootDir)

    }

  )

}
