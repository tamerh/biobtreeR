clearGeneratedFiles<-function(bbDir){

  unlinkIfExist(file.path(bbDir,"conf"))
  unlinkIfExist(file.path(bbDir,"out"))
  unlinkIfExist(file.path(bbDir,"ensembl"))
  unlinkIfExist(file.path(bbDir,"website"))
  unlinkIfExist(file.path(bbDir,"userOut"))
  deleteIfExist(file.path(bbDir,"biobtree.exe"))
  deleteIfExist(file.path(bbDir,"ensembl_sample.json"))
  deleteIfExist(file.path(bbDir,"go_sample.owl"))
  deleteIfExist(file.path(bbDir,"biobtree.exe"))
  deleteIfExist(file.path(bbDir,"biobtree"))

}

unlinkIfExist<-function(targetDir){

  if(file.exists(targetDir)){
    unlink(targetDir,recursive = TRUE)
  }

}

testDatasetBBArgs <- function(hgnc=FALSE){

    # use only sample data without hgnc
    if (hgnc){
      args<-" -d hgnc,go,uniprot,ensembl,interpro"
    }else{
      args<-" -d go,uniprot,ensembl,interpro"
    }

    args<- p(args," --uniprot.file ",system.file("exdata/uniprot_sample.xml.gz",package="biobtreeR"))
    args<- p(args," --interpro.file ",system.file("exdata/interpro_sample.xml.gz",package="biobtreeR"))
    untar(system.file("exdata/ensembl_sample.json.tar.gz",package="biobtreeR"))
    args<- p(args," --ensembl.file ",file.path("ensembl_sample.json"))
    untar(system.file("exdata/go_sample.tar.gz",package="biobtreeR"))
    args<- p(args," --go.file ",file.path("go_sample.owl"))
    args<-p(args," build")
    return(args)
}

testDatasetBBArgs2 <- function(){

  args<-" -d interpro"
  args<- p(args," --interpro.file ",system.file("exdata/interpro_sample.xml.gz",package="biobtreeR"))
  args<-p(args," build")
  return(args)

}
