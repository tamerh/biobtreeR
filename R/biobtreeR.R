
# To hold dataset meta data tool configuration
#' @title class for holding configuration and dataset meta data
setClass("bbConfig", representation(endpoint="character",datasetIDs="data.frame", datasetMeta="list", datasetMetaByNum="list" ))

#' @title Init function for biobtreeR
#'
#' @description Initialize all dataset meta information such as dataset identifiers.
#' In biobtree each dataset has unique charachter and numeric identifier.
#' For instance Uniprot charachter identifier is "uniprot" and numeric identifier is 1.
#' When querying the dataset charachter identifier is used for convinience but in actual database it is saved numerically for efficiency.
#' When this function is executed global bbConfig variable is set. This variable holds these meta information.
#'
#' @param biobtreeURL Optional parameter. By default "http://localhost:8888" url is used to connect running actual biobtree.
#' This parameter is allowed to change this url if biobtree is running from another machine or port.
#'
#' @return returns empty
#'
#' @author Tamer Gur
#'
#' @examples
#' bbInit()
bbInit <- function(biobtreeURL=NULL) {

  endpoint <- "http://localhost:8888"
  datasetIDs <- list()
  datasetMeta <- list()
  datasetMetaByNum <- list()

  if (length(biobtreeURL) > 0) {
    endpoint <- biobtreeURL
  }

  rawmeta <- content(GET(p(endpoint,"/ws/meta")),as="parsed")

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
  i=1
  for(v in datasetNumericIds){
    strID[i]<-datasetMetaByNum[[v]]$id
    numID[i]<-v
    i=i+1
  }
  datasetIDs<-data.frame(id=strID,numeric_id=numID)

  assign("bbConfig",new("bbConfig",
                        endpoint=endpoint,
                        datasetIDs=datasetIDs,
                        datasetMeta=datasetMeta,
                        datasetMetaByNum=datasetMetaByNum), envir = .GlobalEnv)

}

#' @title Search identifiers or special keywords such as gene name.
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
#' bbInit()
#' bbSearch("vav_human,tpi1,homo sapiens")
#' bbSearch("tpi1","hgnc")
#' bbSearch("tpi1","ensembl",filter='ensembl.genome=="homo_sapiens"')
#'

bbSearch <- function(terms,source=NULL,filter=NULL, page=NULL,lite=TRUE,limit=1000){

  wsurl <- function(terms,source,filter,page){

      searchurl <- p(bbConfig@endpoint,"/ws/?i=",encodeURIComponent(terms))

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

  res <- content(GET(urlstr),as="parsed")

  if (length(res$Err)>0){
    stop(res$Err)
  }

  results<-res$results

  if(length(res$nextpage)>0 && length(results)< limit ){
        lastpagekey=res$nextpage
        while(length(lastpagekey)>0 && length(results)<limit){
          urlstr<- wsurl(terms,source,filter,lastpagekey)
          r <- content(GET(urlstr),as="parsed")
          if (length(r$Err)>0){
            stop(r$Err)
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
      source[i]<-bbConfig@datasetMetaByNum[[r$dataset]]$id
      i=i+1
    }
    df<-data.frame(input=input,identfier=id,dataset=source)
    return(df)
  }
  return(res)
}


#' @title Maps bioinformatics datasets identifiers and keywords
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
#' bbInit()
#' #Map protein to its go terms and retrieve go term types
#' bbMapFilter("AT5G3_HUMAN",'map(go)',attrs = "type")
#'
#' #Map protein to its go terms with filter by its type and retrieve their types
#' bbMapFilter("AT5G3_HUMAN",'map(go).filter(go.type=="biological_process")',attrs = "type")
#'
#' #Map gene names to exon identifiers and retrieve the region
#' bbMapFilter("ATP5MC3,TP53",'map(transcript).map(exon)',attrs = "seq_region_name")
#'
#' #Map Affymetrix identifiers to Ensembl identifiers and gene names
#' bbMapFilter("202763_at,213596_at,209310_s_at",source ="affy_hg_u133_plus_2" ,'map(transcript).map(ensembl)',attrs = "name")
#'
bbMapFilter <- function(terms, mapfilter, page=NULL, source=NULL,lite=TRUE,limit=1000,inattrs=NULL,attrs=NULL,showInputColumn=FALSE){

  wsurl <- function(terms,mapfilter,source,page){

    mfurl <- p(bbConfig@endpoint,"/ws/map/?i=",encodeURIComponent(terms),"&m=",encodeURIComponent(mapfilter))

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
  res <- content(GET(urlstr),as="parsed")

  if (length(res$Err)>0){
      stop(res$Err)
  }

  results <- res$results

  for (r in results){
    totalMapping=totalMapping+length(r$targets)
  }

  lastpagekey<-res$nextpage

  if(length(lastpagekey)>0 && totalMapping< limit ){

    while(length(lastpagekey)>0 && totalMapping<limit){

      urlstr<- wsurl(terms,mapfilter,source,lastpagekey)
      newres <- content(GET(urlstr),as="parsed")

      if (length(newres$Err)>0){
        stop(res$Err)
      }

      for(i in 1:length(newres$results)){
          found=FALSE
          for(j in 1:length(results)){
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
    for(i in 1:length(results)){

      for(j in 1:length(results[[i]]$targets)){

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
            in_source[index]<-bbConfig@datasetMetaByNum[[source_id]]$id
          }
        }else{
          if(multiInput){
            in_id[index]<-"-"
            in_source[index]<-"-"
          }
        }
        if(length(attrsNames)>0){
          attrsPath<-p("results[[i]]$targets[[j]]$Attributes$",names(results[[i]]$targets[[j]]$Attributes)[1])
          for(at in attrsNames){
             atPath<-p(attrsPath,"$",at)
             attrsVals[[at]][index]<-eval(parse(text=atPath))
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
#' bbInit()
#' bbEntry("p15498","uniprot")

bbEntry <- function(identifer,source){

  searchurl <- p(bbConfig@endpoint,"/ws/entry/?i=",encodeURIComponent(identifer),"&s=",source)
  res <- content(GET(searchurl),as="parsed")
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
#' bbInit()
#' bbEntryFilter("p15498","uniprot","go,kegg")

bbEntryFilter <-function(identifer,source,filters,page=NULL) {

  searchurl = p(bbConfig@endpoint,"/ws/filter/?i=",encodeURIComponent(identifer),'&s=', source , '&f=' ,filters)

  if (length(page) > 0) {
    searchurl =p(searchurl,"&p=" , page)
  }

  print(searchurl)
  res <- content(GET(searchurl),as="parsed")
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
#' @examples
#' bbInit()
#' bbEntryPage("p15498","uniprot",0,6)
bbEntryPage <- function (identifer, source, page, totalPage) {

  searchurl = p(bbConfig@endpoint,"/ws/page/?i=" ,identifer , '&s=' , source , '&p=' , page , '&t=' , totalPage)

  print(searchurl)
  res <-content(GET(searchurl),as="parsed")
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
#' bbInit()
#' bbURL("p15498","uniprot")

bbURL <-function(identifer,source){

  if(length(bbConfig@datasetMeta[[source]])==0){
     stop(p("Invalid dataset ",source))
  }
  if(length(bbConfig@datasetMeta[[source]]$url)==0){
      stop("This dataset has no url confiGured")
  }

  if(source=="ufeature"){

    pid<-unlist(strsplit(identifer,"_"))
    res<-gsub("\u00A3\\{id\\}",pid[1],bbConfig@datasetMeta[[source]]$url)

  }else if(source=="variantid"){

    pid<-tolower(identifer)
    res<-gsub("\u00A3\\{id\\}",pid[1],bbConfig@datasetMeta[[source]]$url)

  }else if (source=="ensembl" || source=="transcript" || source=="exon"){

    res<-""
    r<-bbEntry(identifer,source)

    if (length(r[[1]]$Attributes$Ensembl)>0){
      branch<-r[[1]]$Attributes$Ensembl$branch
      if(branch==1){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$url)
      }else if(branch==2){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$bacteriaUrl)
      }else if(branch==3){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$fungiUrl)
      }else if(branch==4){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$metazoaUrl)
      }else if(branch==5){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$plantsUrl)
      }else if(branch==6){
        res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$protistsUrl)
      }
      if(source=="ensembl"){
        res<-gsub("\u00A3\\{sp\\}",r[[1]]$Attributes$Ensembl$genome,res)
      }

    }

  }else{
    res<-gsub("\u00A3\\{id\\}",identifer,bbConfig@datasetMeta[[source]]$url)
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
#' @exampless
#' bbInit()
#' bbAttr("p15498","uniprot")

bbAttr <- function(identifer,source){

    res<- bbSearch(identifer,source,lite=FALSE)
    if(startsWith(source,"chembl")){
      attrsPath<-p("res$results[[1]]$Attributes$",names(res$results[[1]]$Attributes)[1],"$",names(res$results[[1]]$Attributes[[1]])[1])
    }else{
      attrsPath<-p("res$results[[1]]$Attributes$",names(res$results[[1]]$Attributes)[1])
    }

    reslist=eval(parse(text=attrsPath))
    return(names(reslist))

}

p <- function(..., sep='') {
    paste(..., sep=sep, collapse=sep)
}
