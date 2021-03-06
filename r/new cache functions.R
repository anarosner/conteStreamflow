## ------------------------------------------------------------------------

# setup functions
# cache_create()
# cache_set()

# download
# cache_download()

# read functions
# cache_read_rdata()
# cache_read_table()
# cache_read_shapefile()
# cache_read_lines()

# internal functions
# cache_check()
# dir_create_conditional()


## ------------------------------------------------------------------------
#' @title Downloads and/or loads data from cache directory
#' @description Load data from cache directory.  If it does not exist, download it first and then load it.  Reassign the object within the parent environment
#' @export

cache_download <- function( file, dir="", server_url, quiet=F ) {
     
     require(curl)
     cache_check()

#      file <- tolower( file )
#      dir <- tolower( dir )
#      if( is.null(server_url) )
#           server_url <- "http://felek.cns.umass.edu:9283/data"
     # https://github.com/anarosner/conteStreamflow_data.git

     ### download files as needed
     # check first to see if it exists in local cache, download if not
     if ( !( tolower(file) %in% tolower(list.files(path=file.path(cache_dir_global,"data",dir)) ) ) ) {
          if ( !quiet )
               cat( paste0("Downloading file ",file,". (This will be cached locally for future use.)\n") )
#           print(file.path( server_url, file, fsep="/" ))
          status <- curl_download( url=file.path( server_url, file, fsep="/" ),
                                   destfile=tolower(file.path( cache_dir_global, "data", dir, file )), 
                                   quiet=quiet )
#           status <- download.file( url=file.path( server_url, file, fsep="/" ),
#                                    destfile=tolower(file.path( cache_dir_global, "data", dir, file )), 
#                     method="auto", quiet=quiet )
          
#           if ( status != 0 ) {    
#                file.remove( file.path(cache_dir_global, "data",dir,file) )
#                stop(paste( "Unable to download file:", file.path(cache_dir_global, "data",dir,file) ))
#           }
     }
}

## ------------------------------------------------------------------------
# # cache_download( file="pointdelineationstats_4_3_2015.rdata", dir="basin_char", 
# #                 server_url="https://github.com/anarosner/conteStreamflow_data/blob/master/basin_char", server_param="?raw=true" ) 
# 
# 
# status <- download.file( 
#      url="https://raw.githubusercontent.com/anarosner/conteStreamflow_data/master/weather/livneh_1950_2013/weather_set_info.bz2", 
#      destfile="c:/alr/models/conteStreamflow_sandbox/weather_set_info.bz2", 
#      method = "auto")
# "https://github.com/anarosner/conteStreamflow_data/blob/master/weather/livneh_1950_2013/weather_set_info.rdata?raw=true"
# 
# load("c:/alr/models/conteStreamflow_sandbox/weather_set_info.bz2", verbose = T)
# load("c:/alr/models/cache/data/weather/livneh_1950_2013/weather_set_info.bz2", verbose = T)
# rm("weather_set_url", "weather_set_cols","weather_set_dates")
#   

## ------------------------------------------------------------------------
cache_setup_weather_set <- function( weather_set, weather_region=NULL, 
                                   server_url="https://raw.githubusercontent.com/anarosner/conteStreamflow_data/master",
                                   quiet=F ) {
     
     cache_check()
     #will create local cache directory for this weather set iff doesn't already exist
     local_dir <- file.path( cache_dir_global, "data", "weather", weather_set )
     dir_create_conditional( local_dir, quiet=quiet )
     
     #will download weather set info iff it isn't already cached locally
     cache_download( file="weather_set_info.bz2", dir=file.path("weather",weather_set), 
                     server_url=file.path( server_url, "weather", weather_set, fsep="/") ) 
     cache_download( file="weather_grid_poly.bz2", dir=file.path("weather",weather_set), 
                     server_url=file.path( server_url, "weather", weather_set, fsep="/") ) 
     cache_download( file="weather_grid_coords.bz2", dir=file.path("weather",weather_set), 
                     server_url=file.path( server_url, "weather", weather_set, fsep="/") ) 
     
     #if weather regions are specified, will create local directory for each weather region iff doesn't exist already
     if ( !is.null(weather_region) ) {
          for ( i in 1:length(weather_region) )
               dir_create_conditional( file.path(local_dir,weather_region[i]), quiet=quiet )
     }
}

#      if ( is.na( file.info(local_dir)$isdir || file.info(local_dir)$isdir ) ) 
#           dir.create( file.path( cache_dir_global, "data", "weather", weather_set ) )
#      status <- download.file( url="https://github.com/anarosner/conteStreamflow_data/blob/master/weather/livneh_1950_2013/weather_grid_coords.rdata?raw=true",
#                               destfile="C:/ALR/Models/cache/data/weather/livneh_1950_2013/weather_set_info.rdata") 
#                     
     # method="auto", quiet=quiet )
     # weather/livneh_1950_2013/weather_grid_coords.rdata
    


## ------------------------------------------------------------------------
cache_read_rdata <-function( file, dir="", server_url=NULL, quiet=F ) {

     cache_check()
     cache_download( file, dir, server_url=server_url, quiet=quiet )
     
     names <- load( file.path(cache_dir_global, "data", dir, file), verbose = !(quiet) )
     for ( i in 1:length(names) ) {
          temp <- get( names[i] )
          assign( x=names[i], 
             value=temp, 
             envir=parent.frame() )
     }
}
#      assign( x=name[1], 
#              value=temp, 
#              envir=parent.frame() ) 
#           }

cache_read_table <- function( file, dir="", server_url=NULL, quiet=F, ... ) {
     
     cache_check()
     cache_download( file, dir, server_url=server_url, quiet=quiet )
     
     temp <- read.table( file=file.path(cache_dir_global, "data", dir, file, ... ) )
     return( temp )
}

cache_read_lines <-function( file, dir="", server_url=NULL, quiet=F, ... ) {
     
     cache_check()
     cache_download( file, dir, server_url=server_url, quiet=quiet )
     
     temp <- readLines( file.path(cache_dir_global, "data", dir, file) )
     return( temp )
}

cache_read_shapefile <-function( file, dir="", server_url=NULL, quiet=F, proj4=NULL, ... ) {
     
     cache_check()
     require(rgdal)
     cache_download( file, dir, server_url=server_url, quiet=quiet )
     
     temp <- readOGR( dsn = file.path(cache_dir_global, "data", dir), layer=file, p4s=proj4 )
     return( temp )
}



## ------------------------------------------------------------------------
#' @title internal function to create directory if it doesn't already exist
#' @param new.dir \code{character}  character of new directory names
#' @param quiet \code{boolean}  whether to suppress messages
# no export

dir_create_conditional <- function( new_dir, quiet=T ) {
     # new_dir <- paste0("./",new_dir)
      # new_dir %in% list.dirs(parent_dir) &&
     if ( !is.na( file.info(new_dir)$isdir ) && file.info(new_dir)$isdir )     {
          if ( !quiet ) {
               # cat( paste("Directory", new_dir, "exists; no duplicate directory created\n") )
          }
     }

     else {
          dir.create( new_dir )
          if (!quiet) {
               cat(paste("New directory",new_dir,"created.","\n"))
          }
     }
}

## ------------------------------------------------------------------------
#' @title Sets up directory structure for cache and temporary files
#' @description Creates directories with locations for cached catchment polygons, weather polygons, and weather data; log files; and temporary local storage
#' @param  cache.dir \code{character} file path of cache directory location
#' @param  quiet \code{should messages be suppressed}
#' @export

cache_setup <- function( cache_dir, quiet=T) {
     
     if ( !is.na(file.info(cache_dir)$isdir) && file.info(cache_dir)$isdir==TRUE ) {
          #check for write permissions, too?
          
          orig_dir <- getwd()
          
          setwd(cache_dir) 
                              
          dir_create_conditional("temp", quiet=quiet)
          dir_create_conditional("data", quiet=quiet)
          setwd("./data")
               dir_create_conditional("catchments", quiet=quiet)
               dir_create_conditional("weather", quiet=quiet)
               dir_create_conditional("basin_char", quiet=quiet)
          
          if ( !quiet )
               message("Cache directory creation complete")
               
          setwd(orig_dir)
     }
     
     else
          stop("Must specify a valid directory")
     
}



## ------------------------------------------------------------------------
#' @title Check that the cache directory location has been set
#' @description Checks that the cache directory location has been set.  Also checks that the directory contains all necessary subdirectories
# no export

#internal, not exported 

cache_check <- function( cache_dir=NULL, automatic_setup=F ) {
     
     #check global cache exists
     if ( !exists( "cache_dir_global", envir=.GlobalEnv ) ) {
     
          #if it doesn't exist, try to set it     
          if ( is.null(cache_dir) ) 
               stop( "missing cache dir info" )
          else if ( is.na( file.info(cache_dir)$isdir) || !file.info(cache_dir)$isdir ) #only assign if it's valid dir
               stop( "cache dir not valid" )
          else
              assign( "cache_dir_global", value=cache_dir, envir=.GlobalEnv )
     }
          
     #if cache exists, check that it's a valid directory
     else if ( is.na(file.info(cache_dir_global)$isdir) || !file.info(cache_dir_global)$isdir )
          stop( "cache_dir not valid" )
     
     #check that cache has necessary directories
     if ( !( "data" %in% list.files( cache_dir_global ) )||
          !( "temp" %in% list.files( cache_dir_global ) )||
          !( file.info(file.path(cache_dir_global,"data"))$isdir ) ||
          !( file.info(file.path(cache_dir_global,"temp"))$isdir ) )  {
          
          if ( automatic_setup )
               cache_set_up() #if user allows, set up cache for them
          else
               stop("must set up cache first")
     }
}
     # else, global cache exists and is directory
     
     
     #check global cache is valid

     
#      if ( exists( "cache_dir_global", envir=.GlobalEnv ) &&
#                !is.na(file.info(cache_dir_global)$isdir) && 
#                file.info(cache_dir_global)$isdir )
#           return()
#      
#      else if ( !is.null(cache_dir) ) {
#           if ( !is.na( file.info(cache_dir)$isdir) && file.info(cache_dir)$isdir ) {
#                     assign( "cache_dir_global", value=cache_dir, envir=.GlobalEnv )
#                     return()
#           }
#           else
#                stop( "cache dir not valid" )
#      }
#      else if ( !exists( "cache_dir_global", envir=.GlobalEnv ) )
#           stop( "missing cache dir info" )
#      else 
#           stop( "cache_dir not valid" )
# }

## ------------------------------------------------------------------------
#wrapper funciton for cache_check that is exported
cache_set <- function( cache_dir ) {
     temp <- cache_check( cache_dir )
}


