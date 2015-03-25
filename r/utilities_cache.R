## ------------------------------------------------------------------------
#' @title internal function to create directory if it doesn't already exist
#' @param new.dir \code{character}  character of new directory names
#' @param quiet \code{boolean}  whether to suppress messages
# no export

dir.conditional.create <- function( new.dir, quiet=T ) {
     new.dir <- paste0("./",new.dir)
     if ( new.dir %in% list.dirs() )     {
          if ( !quiet ) {
               cat( paste("Directory", new.dir, "already exists; no duplicate directory created\n") )
          }
     }

     else {
          dir.create( new.dir )
          if (!quiet) {
               cat(paste("New directory created.","Current list of directories:",paste(list.dirs()[-1],collapse=", "),"\n"))
          }
     }
}

## ------------------------------------------------------------------------
#' @title Sets up directory structure for cache and temporary files
#' @description Creates directories with locations for cached catchment polygons, weather polygons, and weather data; log files; and temporary local storage
#' @param  cache.dir \code{character} file path of cache directory location
#' @param  quiet \code{should messages be suppressed}
#' @export

cache.setup <- function(cache.dir, quiet=T) {
     
     if ( !is.na(file.info(cache.dir)$isdir) && file.info(cache.dir)$isdir==TRUE ) {
          #check for write permissions, too?
          
          orig.dir <- getwd()
          
          setwd(cache.dir) 
                              
          dir.conditional.create("logs", quiet=quiet)
          dir.conditional.create("temp", quiet=quiet)
          dir.conditional.create("data", quiet=quiet)
          setwd("./data")
               dir.conditional.create("hucs", quiet=quiet)
               dir.conditional.create("catchments", quiet=quiet)
               dir.conditional.create("weather_grid", quiet=quiet)
               dir.conditional.create("weather_data", quiet=quiet)
               dir.conditional.create("basin_char", quiet=quiet)
               dir.conditional.create("general_spatial", quiet=quiet)
          
          if ( !quiet )
               message("Cache directory creation complete")
               
          setwd(orig.dir)
     }
     
     else
          stop("Must specify a valid directory")
     
}



## ------------------------------------------------------------------------
#' @title Checks that cache exist and contains all necessary subfolders
#' @param cache.dir \code{character}  character of cache directory location
#' @return \code{boolean}
# no export

cache.check.dir <- function(cache.dir) {
     
     data.dir<-file.path(cache.dir,"data")
     
     if ( is.na(file.info(cache.dir)$isdir) || file.info(cache.dir)$isdir==FALSE ) 
          return(FALSE)
     
     else if ( is.na(file.info(data.dir)$isdir) || file.info(data.dir)$isdir==FALSE ) 
          return(FALSE)               
     
     else
          return(TRUE)
}


## ------------------------------------------------------------------------
#' @title Sets the cache directory location
#' @description Sets the cache directory location.  Also checks that the directory conrtains all necessary subdirectories
#' @param cache.dir \code{character}  file path of cache directory location
#' @export

cache.set <- function( cache.dir ) {
     if ( cache.check.dir( cache.dir ) == TRUE )
          assign( "cache.dir.global", value=cache.dir, envir=.GlobalEnv )
     else 
          stop("Cache directory is missing needed file structure.  Please re-run cache.setup()")
}

## ------------------------------------------------------------------------
#' @title Check that the cache directory location has been set
#' @description Checks that the cache directory location has been set.  Also checks that the directory contains all necessary subdirectories
# no export

cache.check <- function() {
     if ( !exists( "cache.dir.global", envir=.GlobalEnv ) ) 
          stop("Cannot find cache directory.  Please run cache.set() function.  If you haven't ever set up a cache directory, run cache.setup() function first.") 
     
     cache.dir <- get( "cache.dir.global", envir=.GlobalEnv )
     if ( !cache.check.dir( cache.dir ) )
          stop("Cache directory is missing needed file structure.  Please re-run cache.setup()")   
     else {
          assign( "cache.dir", value=cache.dir.global, envir=.GlobalEnv )
#           return(TRUE)
     }
}


## ------------------------------------------------------------------------
#' @title Downloads and/or loads data from cache directory
#' @description Load data from cache directory.  If it does not exist, download it first and then load it.  Reassign the object within the parent environment
#' @export

cache.load.dataOLD <- function( file, dir, 
                             cache.only=F, is.rdata=T, col.names=NULL,
                             server.url="http://felek.cns.umass.edu:9283", 
                             message="default", quiet=F,
                             object=NULL ) {
#      print(file.path(cache.dir.global,"data",dir))
#      print(list.files(path=file.path(cache.dir.global,"data",dir) ))
#      print(file)
     
     if ( !cache.only & is.rdata & is.null(object) )
          object <- file
#           stop( "Must specify an object within rdata file, unless using cache.only option")
     
     if ( !( file %in% list.files(path=file.path(cache.dir.global,"data",dir) ) ) ) {
          if ( message == "default" )
               cat( paste0("Downloading file ",file,". (This will be cached locally for future use.)\n") )
          else if ( !is.null(message) )     
               cat(message)
          
          status <- download.file( paste0(server.url,"/data/",dir,"/",file),
                    file.path(cache.dir.global, "data",dir,file), 
                    method="wget", quiet=quiet)
          if ( status != 0 ) {    
               file.remove( file.path(cache.dir.global, "data",dir,file) )
               stop(paste( "Unable to download file:", file.path(cache.dir.global, "data",dir,file) ))
          }
               
     }
     
     if (!cache.only) {  
          
          if (is.rdata) {
               load( file.path(cache.dir.global, "data", dir, file), verbose = !(quiet) )
               assign( x=object, 
                       value=get( object, envir=environment() ), 
                       envir=parent.frame() )               
          }
          else {
               if (!is.null(col.names))
                    temp <- read.table(file=file.path(cache.dir.global, "data", dir, file),
                                   col.names=col.names)
               else 
                    temp <- read.table(file=file.path(cache.dir.global, "data", dir, file))
               assign( x=object, 
                       value=temp, 
                       envir=parent.frame() ) 
          }
     }
          
}

## ------------------------------------------------------------------------
#' @title Downloads and/or loads data from cache directory
#' @description Load data from cache directory.  If it does not exist, download it first and then load it.  Reassign the object within the parent environment
#' @export

cache.load.data <- function( file, dir, 
                             server.url="http://felek.cns.umass.edu:9283/data", 
                             cache.only=F, col.names=NULL,
                             message="default", quiet=F,
                             object=NULL, is.rdata=NULL ) {
     
     if (!is.null(object))
          warning("specification of object is deprecated. please ensure that the rdata file contains only one object, with the same name as the filename.")
     if (!is.null(is.rdata))
          warning("is.rdata argument is deprecated")
     
     
     is.rdata <- grepl( ".rdata", file, ignore.case=T )
     if ( length( gregexpr( ".rdata", file, ignore.case=T )[[1]] ) > 1 )
          stop("file with .rdata file extension cannot also have .rdata in the filename")
     if ( is.rdata & is.null(object) )
          object <- sub( pattern=".rdata", replacement="", x=file, ignore.case=T )


     if ( !( file %in% list.files(path=file.path(cache.dir.global,"data",dir) ) ) ) {
          if ( message == "default" )
               cat( paste0("Downloading file ",file,". (This will be cached locally for future use.)\n") )
          else if ( !is.null(message) )     
               cat(message)
          
          status <- download.file( paste0(server.url,"/data/",dir,"/",file),
                    file.path(cache.dir.global, "data",dir,file), 
                    method="auto", quiet=quiet)
          if ( status != 0 ) {    
               file.remove( file.path(cache.dir.global, "data",dir,file) )
               stop(paste( "Unable to download file:", file.path(cache.dir.global, "data",dir,file) ))
          }
               
     }
     
     if (!cache.only) {  
          
          if (is.rdata) {
               load( file.path(cache.dir.global, "data", dir, file), verbose = !(quiet) )
               assign( x=object, 
                       value=get( object, envir=environment() ), 
                       envir=parent.frame() )               
          }
          else if ( grepl( ".txt", file, ignore.case=T ) ) {
               temp <- readLines( file.path(cache.dir.global, "data", dir, file) )
               assign( x=object, 
                       value=temp, 
                       envir=parent.frame() ) 
          }
          else {
               if (!is.null(col.names))
                    temp <- read.table(file=file.path(cache.dir.global, "data", dir, file),
                                   col.names=col.names)
               else 
                    temp <- read.table(file=file.path(cache.dir.global, "data", dir, file))
               assign( x=object, 
                       value=temp, 
                       envir=parent.frame() ) 
          }
     }
          
}

