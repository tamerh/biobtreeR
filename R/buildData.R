#' @title Get pre build biobtree database
#'
#' @description Pre build biobtree database for commonly studied datasets and model organism genomes. Once this function called it retrieves
#' the pre build database saves to users output directory.
#'
#' @param type built in database type accepted values are 1,2,3 and 4. Currently there are 4 different builtin database;
#' Type 1
#' Requires ~ 5 GB free storage
#' Included datasets hgnc,hmdb,taxonomy,go,efo,eco,chebi,interpro
#' Included uniprot proteins and ensembl genomes belongs to following organisms
#'
#' homo_sapiens 9606 --> ensembl
#' danio_rerio 7955 zebrafish --> ensembl
#' gallus_gallus 9031 chicken --> ensembl
#' mus_musculus 10090 --> ensembl
#' Rattus norvegicus 10116 ---> ensembl
#' saccharomyces_cerevisiae 4932--> ensembl,ensembl_fungi
#' arabidopsis_thaliana 3702--> ensembl_plants
#' drosophila_melanogaster 7227 --> ensembl,ensembl_metazoa
#' caenorhabditis_elegans 6239 --> ensembl,ensembl_metazoa
#' Escherichia coli 562 --> ensembl_bacteria
#' Escherichia coli str. K-12 substr. MG1655 511145 --> ensembl_bacteria
#' Escherichia coli K-12 83333 --> ensembl_bacteria
#'
#' Type 2
#' Requires ~ 5 GB free storage
#' Instead of genomes in in the type 1 it contains human and all the mouse strains genomes with their uniprot proteins.
#' In addition hgnc,hmdb,taxonomy,go,efo,eco,chebi,interpro datasets are included
#'
#' Type 3
#' Requires ~ 4 GB storage
#' Contains no genome but it contains all the uniprot data with hgnc,hmdb,taxonomy,go,efo,eco,chebi,interpro
#'
#' Type 4
#' Requires ~ 13 GB storage
#' Contains no genome but full uniprot and chembl data with hgnc,hmdb,taxonomy,go,efo,eco,chebi,interpro
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#'
#' bbUseOutDir(tempdir()) # temp dir for demo purpose
#' bbBuiltInDB("demo") # small demo database for real database use 1, 2, 3 or 4
#'
#'
bbBuiltInDB<- function(type="1"){

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


      execFile <- bbExeFile()

      args<-paste0(" --pre-built ",type," install")

      system2(execFile,args=args)

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

  if(file.exists(file.path(conf@bbDir,"out","db","db.meta.json"))){
    stop("Previously built data found in current out directory. Either delete the out directory inside current out directory or use new directory")
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

        # to speed up the process get raw index data of builtin db 3
        bbBuiltInDB("3r")

        args<-paste0(" -tax ",taxonomyIDs," --keep --ensembl-orthologs"," build")

        system2(execFile,args=args)

      }

      setConfig()

    },finally = {

      setwd(rootDir)

    }

  )

}
