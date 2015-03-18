## ----voronoi function----------------------------------------------------
#' @title voronoi polygons
#' @export
#voronoi function by Carson Farmer
#http://www.carsonfarmer.com/2009/09/voronoi-polygons-with-r/
# To create a nice bounded Voronoi polygons tessellation of a point layer in R, we need two libraries: sp and deldir. 
# The following function takes a SpatialPointsDataFrame as input, and returns a SpatialPolygonsDataFrame 
# that represents the Voronoi tessellation of the input point layer.


voronoipolygons = function(layer, create.filename=T) {

     library(deldir)

     crds = layer@coords
     z = deldir(crds[,1], crds[,2])
     w = tile.list(z)
     polys = vector(mode='list', length=length(w))
     for (i in seq(along=polys)) {
               #cat(paste0(floor(i/nrow(layer)*100),"%, "))
        pcrds = cbind(w[[i]]$x, w[[i]]$y)
        pcrds = rbind(pcrds, pcrds[1,])
        polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
     }
     SP = SpatialPolygons(polys,proj4string=layer@proj4string)
     
     voronoi = SpatialPolygonsDataFrame(SP, 
                                        data=data.frame( x=z$summary$x, y=z$summary$y, 
                                             row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))))
     if (create.filename) {
          voronoi@data$weather.filename <-apply( voronoi@data, MARGIN=1, FUN=function(df) paste0("data_",df["y"],"_",df["x"]) )
          voronoi <- merge.sp( voronoi, layer@data[ , c("weather.filename","region")], by = "weather.filename", all.x=T )
     }
     else {
          voronoi <- merge.sp( voronoi, layer@data, by = c("x","y"), all.x=T )
     }
     return( voronoi )
    
}


## ------------------------------------------------------------------------
#' @title create weather grid
#' @export

weather.grid.create<-function(weather.dir="C:/ALR/Data/ClimateData/Mauer/daily",
                              regions=c("east"), 
                              proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                              shapefile.dir=NULL) {
     
     # note about mauer data structure
     # mauer data is split into regions, each region given its own directory
     #    and then split into cells
     # within the region directory, there is a file for each grid cell
     # the filename is a concatenation of the lat and long, 
     #    so this is where the spatial info is extracted from
     
     orig.dir <- getwd() #save starting working directory, so we can return to it at the end, it's only polite
     
     #set up table to store weather file info
     weather.filenames<-as.data.frame( matrix( ncol=2, nrow=0 ) )
     names(weather.filenames)<-c("weather.filename", "region")
     
     #extract coordinate info from filenames
     for (i in regions) { #loop through regions (directories)
          cat( paste( "Retrieving grid centroids for region", i, "from file names...\n" ) )
          setwd( file.path( weather.dir, i ) )
          temp.files<-list.files() 
          #save filename and region name to table
          weather.filenames[ (nrow(weather.filenames)+1):(nrow(weather.filenames)+length(temp.files)),]<-
               cbind( temp.files, rep( i, times=length(temp.files)) )
               #(there was a reason that I did this the tedious way 
               #    by specifying the row # instead of rbind... )
     }
     weather.filenames <- weather.filenames[ !(duplicated(weather.filenames$weather.filename)), ]
     
     #pull out each lat and long coordinates from concatenated filename character, 
     #    and convert to numeric
     weather.filenames$y <- as.vector( sapply( weather.filenames$weather.filename,  
                   FUN=function(x) as.numeric( unlist(strsplit( x, "[_]" ))[2] )  ) )
     weather.filenames$x <- as.vector( sapply( weather.filenames$weather.filename,  
                   FUN=function(x) as.numeric( unlist(strsplit( x, "[_]" ))[3] )  ) )

     #turn numeric lat/long coordinates into r spatial object 
     cat("Creating spatial object of grid centroids...\n")
     grid.points<-SpatialPointsDataFrame(coords=weather.filenames[ , c("x","y") ],
                                             data=weather.filenames,
                                             proj4string=CRS( proj4 ) )
          
     #finally, using spatial points of centroids, create grid polygons 
     cat("Creating Voronoi polygons around grid centroids...\n")
     cat("    (this part could take a while)    \n")
     weather.grid.poly<-voronoipolygons(grid.points)
#      weather.grid.poly@data<-weather.grid.poly@data[,c(3,1,2)]
     
     #if user opted to save grid as shapefile (by specifying shapefile directory), save it
     if ( !is.null( shapefile.dir) ) {
          print("Saving grid shapefile...")
          grid.temp<-weather.grid.poly
          names(grid.temp)[1]<-"filename" #make column names esri-friendly
          setwd(shapefile.dir)
          writeOGR(grid.temp,  ".", layer="weather_grid", driver="ESRI Shapefile")
     }

     cat("Completed creating weather grid")

     setwd( orig.dir ) #clean up

     return( weather.grid.poly )

}


