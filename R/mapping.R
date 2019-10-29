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

    mfurl <- paste0(getConfig()@endpoint,"/ws/map/?i=",encodeURIComponent(terms),"&m=",encodeURIComponent(mapfilter))

    if (length(page) > 0) {
      mfurl <-paste0(mfurl,"&p=" , page)
    }
    if (length(source) > 0) {
      mfurl <-paste0(mfurl,"&s=" ,source)
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
          for(j in seq_len(length(results))){
            if ( identical(newres$results[[i]]$source$doman_id,results[[j]]$source$doman_id) && identical(newres$results[[i]]$source$identifier,results[[j]]$source$identifier)) {
              results[[j]]$targets<-append(results[[j]]$targets,newres$results[[i]]$targets)
              totalMapping=totalMapping+length(newres$results[[i]]$targets)
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

        map_id[index]<-results[[i]]$targets[[j]]$identifier

        if(j==1 && multiInput){
            if(length(results[[i]]$source$keyword)>0){
              in_id[index]<-paste0(results[[i]]$source$keyword,"-",results[[i]]$source$identifier)
            }else{
              in_id[index]<-results[[i]]$source$identifier
            }
            source_id<-results[[i]]$source$dataset
            in_source[index]<-getConfig()@datasetMetaByNum[[paste(source_id)]]$id
        }else if(multiInput){
            in_id[index]<-"-"
            in_source[index]<-"-"
        }

        if(length(attrsNames)>0){
          attrName<-names(results[[i]]$targets[[j]]$Attributes)[1]
          if(attrName!="Empty"){
            attrsPath<-paste0("results[[i]]$targets[[j]]$Attributes$",attrName)
            for(at in attrsNames){
              atPath<-paste0(attrsPath,"$",at)
              attrsVals[[at]][index]<-eval(parse(text=atPath))
            }
          }else{
            for(at in attrsNames){
              attrsVals[[at]][index]<-""
            }
          }

        }

        if(length(inAttrsNames)>0){
          attrsPath<-paste0("results[[i]]$source$Attributes$",names(results[[i]]$source$Attributes)[1])
          for(at in inAttrsNames){
            atPath<-paste0(attrsPath,"$",at)
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
