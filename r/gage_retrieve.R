
## ------------------------------------------------------------------------
#' @title Get gage location and information from NWIS
#' @description Get gage information, including site_no and lat/long coordinates from NWIS, by specifying either 
#'             1) a shapefile with a single polygon outlining an area of interest in the eastern U.S., or 
#'             2) a list of states to search within, or
#'             3) a vector of one or more NWIS gage IDs (as characters)
#' @param buffer.file \code{character} file name and location of a shapefile (polygon) that outlines area to find gages within 
#' @param states \code{character vector} of state abbreviations to search within.  This will only be used if buffer.file is not provided.
#' @param sites \code{character vector} of NWIS gage ID's ("gage_no")
#' @param max.da.sqkm \code{numeric} filter gages by min and max drainage area
#' @param min.da.sqkm
####' @param cache.dir \code{character} directory to store cached data files, save temporary files, and save gage retrieval logs/metadata
#' @return \code{SpatialPointsDataFrame} of gages within the buffer, with gage info from NWIS
#' @keywords nwis, gage
#' @export

gage.retrieve<-function( buffer.file=NULL, 
                         sites=NULL,
                         states=NULL, 
                         max.da.sqkm=50, min.da.sqkm=0, 
#                          cache.dir,
                         proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                         keep.other.coords=F, keep.sv.data=F) {
#                          temp.dir="C:/ALR/Models_processed_data/flow_timeseries", ,log.dir=NULL
#                          ) {     
     cache.check()
                         
     if ( sum( !is.null(buffer.file), !is.null(sites), !is.null(states)) != 1 )
          stop("Must provide one and only one of method of selecting gages; either specify buffer shapefile, list of sites, or list of states.")
     
     #      if ( !cache.check() )
#           stop("Please run setup.cache function first to create directories for local, cached files")

     if(!is.null(buffer.file)) {
          buffer<-readShapePoly(buffer.file,proj4string=CRS(proj4))
          cache.load.data( object="states.poly",file="states.rdata", dir="general_spatial" )
          match<-gIntersects(states.poly,buffer, byid=T)
#           states<-states.spatial$STATE_ABB[match]
          states<-states.poly$STUSPS[match]
     }
     
     # #get gage info, sites w/ daily flow

     # write url to retrieve gage info from nwis 
               # (at time of writing, can't do this using waterData package 
               # or other webservices I know of, so we're creating/using a url)

     #if user specified one or more sites
     if (!is.null(sites)) {    
          #single site
          if ( length(sites)==1 ) {
               gage.info.url1 <- paste0("&search_site_no=",sites,"&search_site_no_match_type=exact")
               gage.info.url2 <- "&list_of_search_criteria=search_site_no%2Csite_tp_cd%2Crealtime_parameter_selection"
          }
               
          #multiple sites
          else {
               gage.info.url1 <- paste0("&multiple_site_no=", paste0(sites, collapse="%2C"))
               gage.info.url2 <- "&list_of_search_criteria=multiple_site_no%2Csite_tp_cd%2Crealtime_parameter_selection"
          }
               
     }

     #if user specified either list of state, or buffer file (which is converted into list of states above)
     else if ( !is.null(states) ) {     
          gage.info.url1 <-  paste0("&state_cd=",states,collapse="")
          gage.info.url2 <- "&list_of_search_criteria=state_cd%2Csite_tp_cd%2Crealtime_parameter_selection"
     } 
     
     #there is an error check on user's inputs above, so this should only catch errors converting buffer into states
     else
          stop(  paste("Unable to select by buffer, state, or site id",
                       buffer.file,states,sites,sep="      ")  )


     gage.info.url<-paste0("http://nwis.waterdata.usgs.gov/nwis/dvstat?referred_module=sw",
          gage.info.url1,
          "&site_tp_cd=ST&index_pmcode_00060=1&group_key=NONE&format=sitefile_output&sitefile_output_format=rdb",
          "&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=lat_va&column_name=long_va&column_name=dec_lat_va&column_name=dec_long_va&column_name=coord_datum_cd&column_name=dec_coord_datum_cd&column_name=huc_cd&column_name=drain_area_va&column_name=sv_begin_date&column_name=sv_end_date&column_name=sv_count_nu",
#           "&column_name=site_no&column_name=station_nm&column_name=site_tp_cd&column_name=lat_va&column_name=long_va&column_name=dec_lat_va&column_name=dec_long_va&column_name=coord_meth_cd&column_name=coord_acy_cd&column_name=coord_datum_cd&column_name=dec_coord_datum_cd&column_name=drain_area_va",
          gage.info.url2)


     #use wget to save into temporary folder, and then read it in and parse.  probably there's a better way to do this, but this works for now.  
     orig.dir <- getwd()
     setwd(file.path(cache.dir, "temp"))
     system(paste("wget -O ./raw_gage_info.txt",gage.info.url) )

     #remove extra header 
     raw<-readLines("raw_gage_info.txt")
     line<-max(grep("agency_cd",raw))
     clean<-raw[c(line,(line+2):length(raw))]
     clean <- gsub( pattern="\'", replacement="-", x=clean)
     clean <- gsub( pattern="\"", replacement="-", x=clean)
     clean <- gsub( pattern="#", replacement="no. ", x=clean)
     setwd(orig.dir)

     #save metadata from raw nwis file 
#      if (!is.null(log.dir)) {    
          save.log( text=raw[1:(line-1)], filename="gages_meta", ext="txt" )
#           writeLines(raw[1:(line-1)],"gages_meta.txt")
          # [13] "#  agency_cd       -- Agency"                                              
          # [14] "#  site_no         -- Site identification number"                          
          # [15] "#  station_nm      -- Site name"                                           
          # [16] "#  dec_lat_va      -- Decimal latitude"                                    
          # [17] "#  dec_long_va     -- Decimal longitude"                                   
          # [18] "#  coord_acy_cd    -- Latitude-longitude accuracy"                         
          # [19] "#  dec_coord_datum_cd -- Decimal Latitude-longitude datum"                 
          # [20] "#  huc_cd          -- Hydrologic unit code"                                
          # [21] "#  drain_area_va   -- Drainage area"                                       
          # [22] "#  sv_begin_date   -- Site-visit data begin date"                          
          # [23] "#  sv_end_date     -- Site-visit data end date"                            
          # [24] "#  sv_count_nu     -- Site-visit data count"
#      }
     
     #read back in raw file saved in temp directory
     #change from raw "lines" to table
     gages.all<-read.table(text=clean,header=T,sep="\t",fill=T,
                       colClasses=c("site_no"="character","huc_cd"="character",
                                    "sv_begin_date"="Date", "sv_end_date"="Date"))
     
     #make sure all gage info includes IDs, coords
     if (sum(is.na(gages.all$site_no))>0)
          warning(paste("Missing site identifiers:\n", sum(is.na(gages.all$site_no)), "sites do not have site_no values and are being ignored"))
     if (sum(is.na(gages.all$dec_lat_va),is.na(gages.all$dec_long_va))>0) {
          warning(paste("NWIS gage data missing geographic coordinates (decimal, NAD83 coordinates):\n", 
                        sum(is.na(gages.all$dec_lat_va)), "sites do not have lat coordinates and are being ignored\n",
                        sum(is.na(gages.all$dec_long_va)), "sites do not have long coordinates and are being ignored\n",
                        paste0(gages.all$site_no[is.na(gages.all$dec_long_va)], collapse = ", ") ))
#           warning(paste(gages.all[is.na(gages.all$dec_long_va),c("site_no","station_nm","coord_datum_cd","lat_va","long_va")]),collapse="\t",sep="\n")
     }

     #convert to sq km and rename drainage area column in sq mi
     gages.all$da_nwis_sqmi<-gages.all$drain_area_va
     gages.all$da_nwis_sqkm<-gages.all$drain_area_va*2.58999
#      gages.all$da_sqkm<-gages.all$da_nwis_sqkm #to comply with older code

     #remove extraneous columns
     gages.all <- gages.all<-gages.all[,!(names(gages.all) %in% c("agency_cd","coord_acy_cd","coord_acy_cd.1","drain_area_va"))]
     if (!keep.other.coords)
          gages.all <- gages.all<-gages.all[,!(names(gages.all) %in% c("coord_datum_cd","lat_va","long_va"))]
     if (!keep.sv.data)
          gages.all <- gages.all<-gages.all[,!(names(gages.all) %in% c("sv_begin_date","sv_end_date","sv_count_nu"))]

     #filter by da size
     gages.subset<-subset(x=gages.all,  gages.all$da_nwis_sqkm<=max.da.sqkm & gages.all$da_nwis_sqkm>min.da.sqkm )
     #filter by missing coordinates or site identifier
     gages.subset<-subset(gages.subset,
                       subset=(!is.na(gages.subset$site_no) & !is.na(gages.subset$dec_lat_va) & !is.na(gages.subset$dec_long_va) ))
     

#      message(paste0("Gage retrieval complete\r",
#                     nrow(gages.subset),  " gages identified","\r",
#                     "Drainage area >", min.da.sqkm, " and <=", max.da.sqkm, " sq km)" ))


     gages.spatial<-gage.place(gages.df = gages.subset)
     if(!is.null(buffer.file)) 
          gages.spatial<-gage.buffer(gages.spatial, buffer=buffer)

     cat("Gage retrieval complete \n")
     cat(paste(nrow(gages.spatial),  "gages identified"))
     cat(paste("(Drainage area >", min.da.sqkm, " and <=", max.da.sqkm, " square km)" ))

     return(gages.spatial)

}



## ----plot gages spatially------------------------------------------------

#' @title Internal function to create SpatialPointsDataFrame from data.frame of gage information
#' @description This function is used internally by gage.retrieve and doesn't need to be called explicitly by the user for normal uses.  This function uses lat and long coordinates from gage info data frame, and turns it into a spatial object w/ the data attached.
#' @param gages.df \code{data.frame} of gage information, including columns with lat and long coordinates
#' @param lat_column \code{character} defaults to NWIS column name "dec_lat_va", but can be customized for use with a different data source
#' @param lat_column \code{character} defaults to NWIS column name "dec_lat_va", but can be customized for use with a different data source
#' @param proj4 \code{character}  coordinate system, in proj4 syntax.  Defaults to GCS NAD83, which is used by NHDplus
#' @param plot \code{boolean}  if true, will plot a map of gages with state outlines
#' @return \code{SpatialPointsDataFrame}
#' @keywords gage, SpatialPointsDataFrame
#' @seealso \code{\link{conteStreamflow::gage.retrieve}}
#' @export

gage.place<-function(gages.df, 
                             lat_column="dec_lat_va", long_column="dec_long_va", proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                             plot=F) {
#      if (!("dec_long_va" %in% names(gages.df) & "dec_lat_va" %in% names(gages.df) ))
#           stop("Input \"gages.df\" must include columns \"dec_long_va\" and \"dec_lat_va\"")
     gages.spatial<-SpatialPointsDataFrame(coords=as.matrix(gages.df[,c(long_column,lat_column)]), 
                      data=gages.df,
                      proj4string=CRS(proj4),
                      match.ID=F)
     if (plot) {
          plot(gages.spatial)
          plot(states.poly,add=T,border="blue")
     }
     
     return(gages.spatial)
}

# gage.place.spatial<-function(gages.df, proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",plot=F) {
#      if (!("dec_long_va" %in% names(gages.df) & "dec_lat_va" %in% names(gages.df) ))
#           stop("Input \"gages.df\" must include columns \"dec_long_va\" and \"dec_lat_va\"")
#      gages.spatial<-SpatialPointsDataFrame(coords=as.matrix(gages.df[,c("dec_long_va","dec_lat_va")]), 
#                       data=gages.df,
#                       proj4string=CRS(proj4),
#                       match.ID=F)
#      if (plot) {
#           plot(gages.spatial)
#           plot(states.spatial,add=T,border="blue")
#      }
#      
#      return(gages.spatial)
# }


## ----refine gages within a buffer----------------------------------------

#' @title Internal function to filter gages based on whether they fall within a geographic buffer 
#' @description This function is used internally by gage.retrieve and doesn't need to be called explicitly by the user for normal uses.  Input spatial gage object is overlaid with buffer polygon, and gages falling outside the buffer are filtered out.
#' @param gages.spatial \code{SpatialPointsDataFrame}
#' @param buffer \code{SpatialPolygonsDataFrame} must provide either a SpatialPolygonsDataFrame buffer, or a file name and location of a shapefile of a single polygon to outline area of interest
#' @param buffer.file \code{character}
#' @param proj4 character  coordinate system, in proj4 syntax.  Defaults to GCS NAD83, which is used by NHDplus
#' @param plot \code{boolean}  if true, will plot a map of gages with state outlines
#' @param message \code{boolean} if true, will display a message indicating how many gages were removed
#' @return SpatialPointsDataFrame
#' @keywords gage, SpatialPointsDataFrame
#' @seealso \code{\link{conteStreamflow::gage.retrieve}}
#' @export




gage.buffer<-function(gages.spatial, plot=F, 
                                   buffer=NULL,
                                   buffer.file=NULL,
#                       "C:/ALR/Models_processed_data/flow_gages/ctr_states_buffer_no_islands2",
                                   proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                                   message=FALSE) {
                         #right now, using my local directory as the defaults.  this is bad, will replace later, etc.

     orig.n.gages<-nrow(gages.spatial)
     
     #eliminate gages outside a defined buffer

     ###put a buffer shapefile in the package's data directory, as a sample data set
          
     
     #load buffer
     if (is.null(buffer))
          buffer<-readShapePoly(buffer.file,proj4string=CRS(proj4))

     in.buffer<-!is.na(over(gages.spatial,buffer)[,1])
     gages.spatial<-gages.spatial[in.buffer,]


     if (plot) {
          #create spatialpoint object from coordinates (coordinates are listed in the order long, lat)
          par(mfrow=c(1,1))
          plot(gages.spatial)
          plot(gages.spatial,add=T,col="red")
     }
     
     if( message ) {
     message(paste(nrow(gages.spatial), "gages","\r",
                   "     (", orig.n.gages-nrow(gages.spatial)," gages eliminated)"))
     }

     return(gages.spatial)

}


## ------------------------------------------------------------------------
#' @title Assigns each gage to a NHDplus catchment/stream reach
#' @description Uses spatial gage object, usually one created by gage.retrieve().
#' Loads NHDplus catchment file, determines which catchment each gages falls within, and assigns a FEATUREID
#' @param gages.spatial \code{SpatialPointsDataFrame} usually produced by gage.retrieve()
#' @param cache.dir \code{character} local directory where spatial data files can be cached
#' @return \code{SpatialPointsDataFrame}
#' @keywords gage, NHDplus
#' @seealso \code{\link{conteStreamflow::gages.retrieve}}
#' @export

gage.place.nhdplus<-function(gages.spatial) {
     #, cache.dir ) #, server.url="http://felek.cns.umass.edu:9283") {
     
     #      server.url <- "http://felek.cns.umass.edu:9283"
     
#      if ( !check.cache() )
#           stop("Please run setup.cache function first to create directories for local, cached files")
#      
     cache.check()
     
     ### download huc file if needed
     cache.load.data( object="huc6", file="hucs.rdata", dir="hucs", 
                      message="Downloading regional spatial data files.  This may take a while. (Files will be cached locally for future use.)", quiet=F)
#      assign( "huc6", value=huc6 )
#      setwd( file.path(cache.dir, "data", "hucs"))
#      if ( !("hucs.rdata" %in% list.files() ) ) {
#           cat("Downloading regional spatial data files. (This will be cached locally for future use.)")
#           download.file( paste0(server.url,"/data/hucs/hucs.rdata"),
#                paste0(cache.dir, "/data/hucs/hucs.rdata"), 
#                method="wget", quiet=F)
# #           load( url(paste0(server.url,"/data/hucs/hucs.rdata")) )
#      }
     
     #spatial query huc to determine which catchment files are needed
     g <- gages.spatial
     g$huc <- row.names(huc6)[ over( g, huc6 ) ]

#      ### download catchment files as needed
#      setwd( file.path(cache.dir, "data", "catchments"))
#      to.load <- unique( paste0(g$huc,".rdata") ) 
#      to.load <- to.load[ !(to.load %in% list.files()) ]
# 
#      if ( length(to.load)>0 ) {
#           cat(paste( "Downloading", length(to.load), "spatial data files. (This will be cached locally for future use.)"  ))
#           for ( i in 1:length(to.load) ) {
#                cat(paste( "... now downloading file",i,"of",length(to.load)  ))
#                download.file( paste0(server.url,"/data/catchments/",to.load[i]),
#                               paste0(cache.dir, "/data/catchments/",to.load[i]), 
#                               method="wget", quiet=T)
#           }
#           cat("Download complete")
#      }

#      
#      setwd( file.path(cache.dir, "data", "catchments"))
#      load( paste0(data.dir, "/catchments/",i,".rdata") ) 
#      cache.load.data( object="huc6", file="hucs.rdata", dir="hucs", 
#                       message="Downloading regional spatial data files.  This may take a while. (Files will be cached locally for future use.)", quiet=F)


     #first loop just to download/cache catchment files
     for ( i in unique(g$huc) )  {
          cache.load.data( file=paste0(i,".rdata"), dir="catchments", 
                           cache.only=T, 
                           quiet=T, message=paste0("Downloading catchment information for basins in region (HUC) ", i, ". ") )  
     }

     ### place gages into catchments
          #loop through hucs, load its catchment files, and place gages into catchments
     cat("Matching gages to catchments")
     for ( i in unique(g$huc) ) { 
          cache.load.data( object="catchments",
                           file=paste0(i,".rdata"), dir="catchments", 
                           cache.only=F, quiet=T )  
#           load( file=paste0(cache.dir, "/data/catchments/",i,".rdata"))
          match<-over( g[g$huc==i,], catchments )          
          g@data[g$huc==i,"FEATUREID"]<-match$FEATUREID
          cat(paste("...completed matching",sum(!is.na(g@data$FEATUREID)),
                    "gages out of",nrow(g)," "))
     }


     cat("\nCompleted plotting gages to catchments")

     if ( sum(is.na(g$FEATUREID))>0 ) {
          warning(paste(  sum(is.na(g$FEATUREID)), "gages did not map to a NHDplus catchment:",
                          paste(g@data[is.na(g$FEATUREID),"site_no"], collapse="; " )  ))     
     }
          
     gages.spatial <- g[,-which(names(g)=="huc")]
     return(gages.spatial)

}



## ----gage export---------------------------------------------------------
#' @title Export shapefile of gage information
#' @export
# eventually move into a generic function for exporting shapefiles, make gage.export a wrapper function

gage.export <- function( gages.spatial, shapefile.dir, filename="gages_conteStreamflow", overwrite=T ) {
     
          orig.dir <- getwd()
          setwd(shapefile.dir)
          if ( paste0(filename,".shp") %in% list.files() ) {
               if (!overwrite)
                    stop( paste0("A shapefile with the name \"", filename, "\" already exists in this directory. Please specify a unique filename.") )
               else {
                    for ( i in c("shx","shp","dbf","prj") ) {
                         file.remove(paste0(filename,".",i))
                         }
               }
          }
          
          orig <- names( gages.spatial )
          names( gages.spatial ) <- gsub( pattern="[[:punct:]]", replacement="_", x=names( gages.spatial) )
          x<-names(gages.spatial)[ nchar(names(gages.spatial)) >10 ]
          k<-1
          for ( i in 1:length(x) ) {
               a <- substr( x[i], 1, 1 )
               b <- substr( x[i], 2, nchar(x[i]) )
               for ( j in c("a","e","i","o","u") )
                    b<-gsub( pattern=j, replacement="", x=b )
               x[i] <- paste0( a, b, collapse="" )
               if ( nchar(x[i])>10 ) {
                    x[i] <- substr( x[i], 1, 8 )
                    x[i] <- paste0( x[i], k, collapse="" )
                    k <- k+1
               }
          }
          x
          names(gages.spatial)[ nchar(names(gages.spatial)) >10 ] <- x
          writeOGR( gages.spatial,  ".", layer=filename, driver="ESRI Shapefile" )
          write.csv( data.frame( orig_names=orig, esri_names=names(gages.spatial) ), file=paste0(filename,"_col_names.csv"), row.names=F )
          
          setwd(orig.dir)
}



## ------------------------------------------------------------------------
# plot gages to nhdplus catchments and assign featureid




# gage.place.nhdplusOLD<-function(gages.spatial,  
#                             catchments=NULL,
#                              catchment.file="C:/ALR/Data/StreamData/NHDplus/NHDPlusCatchment/NENY/Catchment", 
#                             proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs", save.catch=T) {
#                               #right now, using my local directory as the defaults.  this is bad, will replace later, etc.
#      
#      if (is.null(catchments)) {
#           cat("Starting to load catchments...\r")
#           cat("    (this part could take a while)    \r")
#           catchments<-readShapePoly(catchment.file,proj4string=CRS(proj4),IDvar="FEATUREID")
#           cat("Completed loading catchments...\r")
#           if (save.catch)
#                assign(x = "catchments",value = catchments,envir = .GlobalEnv)     
#      }
#           
#      cat("Starting to plot gages to catchments\r")
#      match<-over(gages.spatial,catchments)
#      
#      gages.spatial@data$FEATUREID<-match$FEATUREID
#      cat("Completed plotting gages to catchments")
# 
#      if ( sum(is.na(gages.spatial$FEATUREID))>0 ) 
#           warning(paste(sum(is.na(gages.spatial$FEATUREID)), "gages did not map to a NHDplus catchment:"))
#           warning(paste(gages.spatial@data[is.na(gages.spatial$FEATUREID),"site_no"], collapse="; " ))     
#      
#      return(gages.spatial)
#}


