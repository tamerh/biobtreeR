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
      setOutDir(outDir)
  }else{
      bbDir<-tempdir()
  }

  setwd(bbDir)

  execFile <- bbExeFile()

  if(length(rawArgs)>0){

    system2(execFile,args=rawArgs)

  }else if (length(datasets)>0 && datasets=="sample_data"){

    args<-" -d go,hgnc,uniprot,ensembl,interpro"
    args<- paste0(args," --uniprot.file ",system.file("exdata/uniprot_sample.xml.gz",package="biobtreeR"))
    args<- paste0(args," --interpro.file ",system.file("exdata/interpro_sample.xml.gz",package="biobtreeR"))
    untar(system.file("exdata/ensembl_sample.json.tar.gz",package="biobtreeR"))
    args<- paste0(args," --ensembl.file ","ensembl_sample.json")
    untar(system.file("exdata/go_sample.tar.gz",package="biobtreeR"))
    args<- paste0(args," --go.file ","go_sample.owl")
    args<-paste0(args," build")

    system2(execFile,args=args)

  }else{

    args<-""

    if(length(genome)>0){
      args<-paste0(args," -s ",genome)
    }else if (length(genomePattern)>0){
      args<-paste0(args," -sp ",genomePattern)
    }

    if(length(datasets)>0){
      args<-paste0(args," -d ",datasets)
    }

    if(length(targetDatasets)>0){
      args<-paste0(args," -t ",targetDatasets)
    }

    args<-paste0(args," build")

    system2(execFile,args=args)

  }

  setConfig()

  setwd(rootDir)
}
