## ------------------------------------------------------------------------
#' @title pre agg weather 
#' @description x 
#' @export

weather.pre.agg.function <- function( x, centroid ) {
     
     #run snowmelt model
     suppressWarnings(
          x.snow<-SnowMelt( Date=x$date, lat_deg=centroid$y,
                           precip_mm=x$precip.mm, Tmax_C=x$tmax, Tmin_C=x$tmin,
                           windSp=x$wind,windHt=10 )  )
     #calculate last doy w/ snow pack (snow melt)
     x.snow$doy <- as.numeric(format(x.snow$Date,"%j"))
     x.snow$snow.pack <- x.snow$SnowDepth_m>0
     x.snow$snow.pack.doy <- 0
     x.snow$snow.pack.doy[x.snow$snow.pack] <- x.snow$doy[x.snow$snow.pack]
     x.snow$snow.pack.doy[x.snow$doy>200]<-0  #last day in the *spring* for which there is some snow pack
     #i've gone back and forth on whether to save rain & melt, or just effective precip
#                x[,c("rain","melt","melt.doy")]<-x.snow[,c("Rain_mm","SnowMelt_mm", "snow.pack.doy")]
     x[,"melt.doy"]<-x.snow[,"snow.pack.doy"]
     
     #save effective precip
     x[,"precip.e"] <- x.snow[,"Rain_mm"] + x.snow[,"SnowMelt_mm"]

     #temperature metrics, including estimated potential evapotranspiration and gdd
     x[,"pet"]<-PET_fromTemp(Jday=yday(x$date),Tmax_C=x$tmax ,Tmin_C=x$tmin, lat_radians=centroid$y*pi/180)
     x[,"tavg"]<-(x$tmin+x$tmax)/2
     x[,"gdd"]<-sapply(x$tavg, FUN=function(y) max( y-10, 0) )
     x[,"frozen"]<-sapply(x$tmax, FUN=function(y) y<=0) 

     #rename total precip
     x[,"precip.raw"] <-  x[,"precip.mm"]

     x[,"doy"] <- as.numeric(format(x$date, format="%j"))

     return(x)

}

## ------------------------------------------------------------------------
#' @title agg weather 
#' @description x 
#' @export

weather.agg.function<-function( df, ... ) {
     j<-names(df)[1] #way to determine the period, w/o requiring it to be passed as parameter, so only one paramter is needed
     cutoff<-create.template.periods()[j,"min.records"] #determine number of records for this periods to be considered "complete"
     return( c(
          precip.raw=sum(df$precip.raw),
          precip.e=sum(df$precip.e),
          tmin=mean(df$tmin),
          tmax=mean(df$tmax),
          tavg=mean(df$tavg),
          pet=sum(df$pet),
          gdd=sum(df$gdd),
          frozen=sum(df$frozen),
#           rain=sum(df$rain),
#           melt=sum(df$melt),
          melt.doy=max(df$melt.doy),
          complete=sum(!is.na(df$precip.raw))>=cutoff) )
}


## ------------------------------------------------------------------------
#' @title weather lag calculations
#' @export

weather.lag.function <- function( x ) {
     
     for ( j in c("precip.e", "pet","gdd","tmax","tmin","melt.doy") ) {          
          for ( i in 1:2 ) {    
               x <- slide( x, Var=j, NewVar=paste0(j,".lag",i),
                           slideBy=-i, reminder=F )          }     }
     
     return( x )
}


## ------------------------------------------------------------------------
#' @title col names for weather metrics
#' @description col names for our processed/saved weather metrics
#' @export

create.cols.weather<-function( weather.pre.agg.function=(conteStreamflow::weather.pre.agg.function),
                               weather.agg.function=(conteStreamflow::weather.agg.function),
                               weather.lag.function=(conteStreamflow::weather.lag.function)
                               ) {
     
     dummy <- data.frame( daily=as.Date(1:100, origin=now() ), date=as.Date(1:10, origin=now() ),
                          precip.mm=c(1:100), wind=rep(5,times=100), 
                          tmax=rep(10,times=100), tmin=rep(0,times=100) )
     dummy2 <- weather.pre.agg.function( dummy, centroid=data.frame(x=-72,y=41) ) 
     dummy3 <- ddply( dummy2,"daily",weather.agg.function )
     dummy4 <- weather.lag.function( dummy3 )

     return(  names( dummy4 )[-1]  )
}


## ------------------------------------------------------------------------
#' @title import weather 
#' @description import weather
#' @export
weather.retrieve2 <- function(gages.spatial, 
                              set="livneh_1950_2013",
                              periods=c("monthly","annual"),
                              weather.pre.agg.function = (conteStreamflow::weather.pre.agg.function), 
                              weather.agg.function = (conteStreamflow::weather.agg.function), 
                              weather.lag.function = (conteStreamflow::weather.lag.function), 
                              template.period = NULL) { 
                                   #      template.date = NULL, cols.weather = NULL,  ...
                              #set="mauer_1949_2010"
                              #set="livneh_1915_2010"
      
     ### set up
     ##
     #
     
     cache.check()
     orig.dir <- getwd()
     
          #warn user if hasn't assigned weather grid.  
          #later, change to just call function to assign weather grid if it hasn't been done already
     if ( !("weather.filename" %in% names(gages.spatial)) )
          stop("Please first plot gages to weather grids using \"gage.place.weather.grid\" ")
     
     #generate a data.frame of *unique* weather grids used by all gages
     selected.weather.files<-gages.spatial[,c("weather.filename","region")]
     selected.weather.files<-selected.weather.files[ !duplicated(selected.weather.files$weather.filename), ]

     
     if (is.null(template.period))
          template.period<-create.template.periods()

     #load date and column name templates for chosen weather data set
     cache.load.data( file="template.weather.rdata", dir=paste0("weather_sets/",set) )

     #load table of lat/long of centroids of each weather grid cell. 
     #these lat values are used for snow melt and PET models
     cache.load.data( file="weather.grid.coords.rdata", dir=paste0("weather_sets/",set) )
     
     
     cols.weather<-create.cols.weather(
                              weather.pre.agg.function = weather.pre.agg.function, 
                              weather.agg.function = weather.agg.function, 
                              weather.lag.function = weather.lag.function )
     
#      if (is.null(template.date))
#           template.date<-create.template.date()
     

     #      #for imported mauer data, user can't specify custom columns/dates, hard coded
     #      #change if we instead get mauer data from rest api
     #      cols.mauer <- create.cols.mauer()     
     #      template.date.mauer <- create.template.date.mauer()
          
     
     #create matrix for storing flow data
     w.matrices<-create.w.matrices(selected.weather.files$weather.filename, periods=periods,
                                   template.date=template.date, template.period=template.period, cols.weather=cols.weather)

     
     #downloads weather data as needed
     for (j in unique(selected.weather.files$region)) {
          #set up directory for weather region, if it doesn't exist
          setwd( file.path(cache.dir.global, "data", "weather_data") )
          dir.conditional.create( new.dir=j, quiet=T )

          #download weather files as needed, and save in cache
          #(only download, doesn't read them into r at this point)
          for (k in selected.weather.files$weather.filename[selected.weather.files$region==j]) {
               cache.load.data( file=k, dir=paste0("weather_data","/",j), cache.only=T, quiet=T )
          }
     }


     
     ### load data
     ## 
     # 
     
     #loop through weather grid cells and pull and aggregate observation records
     cat(paste( "Begin reading", nrow(selected.weather.files), "unique weather files used for",nrow(gages.spatial),"gages","\n" ))
     for (i in 1:nrow(selected.weather.files) ) {
     
          cat( paste( "  --  Loading file", i, "of", nrow(selected.weather.files), "  --  \n" ))
                    
          centroid <- weather.grid.coords[i, c("x","y")]

          #load mauer weather data, using static mauer column names
          cache.load.data( object="x", 
                           dir=file.path("weather_data",selected.weather.files$region[i]), 
                           file=selected.weather.files$weather.filename[i],
                           is.rdata=F, col.names = cols.mauer )
          # assign period and date from template
          x[,c( "date", template.period$name )]<- 
               template.date.mauer[,c( "date", template.period$name )]
                              #date is date format  
                              #others are characters, so they can be used as col names
               
          ### weather calculations
          ##
          #
          x <- weather.pre.agg.function( x, centroid )




          ### loop through periods
          ##
          #

          for (j in periods ){ 

               ### weather aggregation
               ##
               #
                         
               suppressWarnings(
                    x.agg<-ddply( .data=x[,c(which(names(x)==j),which(names(x)!=j))], #reorder columns
                                             #so column w/ name of period is first
                            .variables=j, 
                            .fun=weather.agg.function)
               )
             
               x.agg[x.agg$complete==0,-which(names(x.agg) %in% c(j,"complete"))]<-NA 


               ### weather lag values
               ##
               #
               x.lag <- weather.lag.function( x.agg )

               x.final<-as.matrix(x.lag[,cols.weather])
               w.matrices[[j]][,selected.weather.files$weather.filename[i],]<-x.final

          }#end loop periods
               

          
     }#end loop weather grids

     setwd( orig.dir )
     return(w.matrices)

}



## ------------------------------------------------------------------------
#' @title import weather 
#' @description import weather
#' @export
weather.retrieve<-function(gages.spatial, 
                              periods=c("seasonal","annual"),
                              weather.pre.agg.function = (conteStreamflow::weather.pre.agg.function), 
                              weather.agg.function = (conteStreamflow::weather.agg.function), 
                              weather.lag.function = (conteStreamflow::weather.lag.function), 
                              template.date = NULL, template.period = NULL, cols.weather = NULL,
                           ...) { 

      
     ### set up
     ##
     #
     
     cache.check()
     orig.dir <- getwd()
     
          #warn user if hasn't assigned weather grid.  
          #later, change to just call function to assign weather grid if it hasn't been done already
     if ( !("weather.filename" %in% names(gages.spatial)) )
          stop("Please first plot gages to weather grids using \"gage.place.weather.grid\" ")
     
     #generate a data.frame of *unique* weather grids used by all gages
     selected.weather.files<-gages.spatial[,c("weather.filename","region")]
     selected.weather.files<-selected.weather.files[ !duplicated(selected.weather.files$weather.filename), ]

     
     #create templates and columns 
     if (is.null(template.date))
          template.date<-create.template.date()
     if (is.null(template.period))
          template.period<-create.template.periods()
     if (is.null(cols.weather)) {
          cols.weather<-create.cols.weather(
                              weather.pre.agg.function = weather.pre.agg.function, 
                              weather.agg.function = weather.agg.function, 
                              weather.lag.function = weather.lag.function )
          }
     #maybe later add a test here.  if user specifies custom cols.weather and agg.function, 
     #     need to make sure the columns match the agg function outputs

     #for imported mauer data, user can't specify custom columns/dates, hard coded
     #change if we instead get mauer data from rest api
     cols.mauer <- create.cols.mauer()     
     template.date.mauer <- create.template.date.mauer()
     
     #load table of lat/long of centroids of each weather grid cell. 
     #these lat values are used for snow melt and PET models
     cache.load.data( "weather.grid.coords", file="weather_grid_coords.rdata", dir="weather_grid" )
     
     
     #create matrix for storing flow data
     w.matrices<-create.w.matrices(selected.weather.files$weather.filename, periods=periods,
                                   template.date=template.date, template.period=template.period, cols.weather=cols.weather)

     
     #downloads weather data as needed
     for (j in unique(selected.weather.files$region)) {
          #set up directory for weather region, if it doesn't exist
          setwd( file.path(cache.dir.global, "data", "weather_data") )
          dir.conditional.create( new.dir=j, quiet=T )

          #download weather files as needed, and save in cache
          #(only download, doesn't read them into r at this point)
          for (k in selected.weather.files$weather.filename[selected.weather.files$region==j]) {
               cache.load.data( file=k, dir=paste0("weather_data","/",j), cache.only=T, quiet=T )
          }
     }


     
     ### load data
     ## 
     # 
     
     #loop through weather grid cells and pull and aggregate observation records
     cat(paste( "Begin reading", nrow(selected.weather.files), "unique weather files used for",nrow(gages.spatial),"gages","\n" ))
     for (i in 1:nrow(selected.weather.files) ) {
     
          cat( paste( "  --  Loading file", i, "of", nrow(selected.weather.files), "  --  \n" ))
                    
          centroid <- weather.grid.coords[i, c("x","y")]

          #load mauer weather data, using static mauer column names
          cache.load.data( object="x", 
                           dir=file.path("weather_data",selected.weather.files$region[i]), 
                           file=selected.weather.files$weather.filename[i],
                           is.rdata=F, col.names = cols.mauer )
          # assign period and date from template
          x[,c( "date", template.period$name )]<- 
               template.date.mauer[,c( "date", template.period$name )]
                              #date is date format  
                              #others are characters, so they can be used as col names
               
          ### weather calculations
          ##
          #
          x <- weather.pre.agg.function( x, centroid )




          ### loop through periods
          ##
          #

          for (j in periods ){ 

               ### weather aggregation
               ##
               #
                         
               suppressWarnings(
                    x.agg<-ddply( .data=x[,c(which(names(x)==j),which(names(x)!=j))], #reorder columns
                                             #so column w/ name of period is first
                            .variables=j, 
                            .fun=weather.agg.function)
               )
             
               x.agg[x.agg$complete==0,-which(names(x.agg) %in% c(j,"complete"))]<-NA 


               ### weather lag values
               ##
               #
               x.lag <- weather.lag.function( x.agg )

               x.final<-as.matrix(x.lag[,cols.weather])
               w.matrices[[j]][,selected.weather.files$weather.filename[i],]<-x.final

          }#end loop periods
               

          
     }#end loop weather grids

     setwd( orig.dir )
     return(w.matrices)

}



