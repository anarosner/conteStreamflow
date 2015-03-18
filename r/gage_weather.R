## ------------------------------------------------------------------------

#' @title plot gages to weather grid
#' @description plot to a weather grid polygon, based on all the files in the mauer daily east data set
#' @param gages.spatial  \code{SpatialPolygonsDataFrame} 
#' @param plot  \code{boolean} if true, will plot a map of gages with the weather grid overlaid
#' @return  \code{SpatialPolygonsDataFrame}
#' @seealso \code{\link{conteStreamflow::gage.retrieve}}, \code{\link{conteStreamflow::weather.retrieve}}
#' @export
gage.place.weather<-function(gages.spatial, set="mauer", plot=F) {
     

     cache.check()
     cache.load.data( "weather.grid.poly", file="weather_grid_poly.rdata", dir="weather_grid" )

     cat("Mapping gages to weather grid cells...\n")
     temp<-over( gages.spatial, weather.grid.poly )
#      gages.spatial[ , c("weather.filename","region") ] <- 
#           over( gages.spatial,weather.grid.poly )[ , c("weather.filename","region") ]
     gages.spatial$weather.filename <- temp$weather.filename
     gages.spatial$region <- temp$region
     cat("Completed mapping gages to weather grid cells...\n")
          
     gages.spatial$weather.filename<-as.character(gages.spatial$weather.filename) 
                    #I can't remember why this was necessary, but leaving it in for now.. a factor issue?
     
     if (sum(is.na(gages.spatial$weather.filename))>0)
         warning(paste(is.na(gages.spatial$weather.filename), "gages were unable to map to weather grid cell\n", 
                       gages.spatial[is.na(gages.spatial$weather.filename), "weather.filename"],collapse=""))
     
     
     cat(paste(length(unique(gages.spatial$weather.filename)),"unique weather files to be used for",nrow(gages.spatial),"flow gages\n"))  
     
     if ( plot ) {
          cat("Drawing plot...")
          cache.load.data( object="states.poly", file="states.rdata", dir="general_spatial" )
          plot(gages.spatial,col="red")
          plot(weather.grid.poly,border="blue",add=T)
          plot(states.poly,add=T)
     }
     
     return(gages.spatial)
     
}


