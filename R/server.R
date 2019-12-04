#' @title Start biobtreeR
#'
#' @description Once target datasets is built with \code{bbBuildData} this function used to start biobtree server
#' in the background for performing search/mapping queries.
#'
#' @return character
#'
#' @examples
#'
#' bbStart()
#' bbStop()
#'
bbStart<-function() {

    if(isbbRunning()){
      print("There is a running biobtree which can be used or first bbStop() to start again")
      return(FALSE)
    }

    rootDir<-getwd()

    conf<-getConfig()

    if(length(conf@bbDir)==0){
      stop("out directory is not set. It must be set first with useOutDir function")
    }

    bbDir<-conf@bbDir

    tryCatch(
      {

        print(bbDir)

        setwd(bbDir)

        execFile <- bbExeFile()


        if(!file.exists(file.path(bbDir,"out","db","db.meta.json"))){
          stop("Built data couldn't found. Make sure data built or directory set correctly")
        }

        system2(execFile,args = "--no-web-popup web",wait = FALSE)

        # wait here until biobtree data process complete
        print("Starting biobtree...")
        elapsed<-0
        timeoutDuration<-50
        while(TRUE){

          if (elapsed > timeoutDuration){
            bbStop()
            setwd(rootDir)
            stop("biobtree could not started. Please check that you have built data correctly")
          }

          Sys.sleep(1)

          if(!isbbRunning()){
            elapsed<-elapsed+1
            next
          }

          setConfig()
          return("biobtreeR started")

        }


      },finally = {
        setwd(rootDir)
      }

    )


}

#'
#' @title Stop biobtree
#'
#' @description Stops running background biobtree process which started with \code{bbStart}
#'
#' @return returns empty
#'
#' @examples
#' bbStop()
bbStop <- function(){

    if (Sys.info()['sysname'] == "Windows") {

        system2("taskkill",args = "/IM biobtree.exe /F")

    }else if (Sys.info()['sysname'] == "Darwin"){

      system2("killall",args = "biobtree")

    }else if (Sys.info()['sysname'] == "Linux"){

      system2("killall",args = "biobtree")

    }

}

#
#Checks for running biobtree if found use it
#
isbbRunning <- function(biobtreeURL=NULL){

  conf<- getConfig()

  response <- tryCatch(
    HEAD(conf@metaEndpoint),
    error=function(e) e
  )

  if(is.null(response)){
    return(FALSE)
  }

  return(!inherits(response, "error"))

}

