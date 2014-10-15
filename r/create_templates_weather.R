
## ------------------------------------------------------------------------
#' @title col names for weather input data
#' @description col names for mauer weather data.  original inputs do not have column headers
#columns from mauer
# no export

create.cols.mauer<-function() {
     cols.mauer<-c("year", "month", "day","precip.mm", "tmax","tmin","wind")
     return(cols.mauer)
}



## ------------------------------------------------------------------------
#' @title Create template of dates in mauer weather data
#' @description mauer dates
# no export

create.template.date.mauer<-function(create.template.periods=(conteStreamflow::create.template.periods)) {
# create.template.weather<-function(mauer.dir="C:/ALR/Data/ClimateData/Mauer/daily/east") {

     
     cols.mauer<-create.cols.mauer()
     cache.load.data( object="template.mauer",file="data_sample", dir="weather_data", 
                      is.rdata=F, col.names=cols.mauer, cache.only=F, 
                      quiet=F, message="default" )
     
     
     cache.load.data( file="data_sample", dir="weather_data", cache.only=T, quiet=T, message="default" )
     template.mauer<-read.table(file=file.path(cache.dir.global,"data","weather_data","data_sample"),
                                  col.names=cols.mauer)

     
     template.mauer$date<-apply(template.mauer[,1:3],MARGIN=1,FUN=function(d) (paste(d,collapse="-")))
     template.mauer$date<-as.Date(template.mauer$date)
     
     #eventually change this to automatically retrieve periods, fetch functions that assign date for that period, and assign date
     template.mauer$daily<-as.character(template.mauer$date)
     template.mauer$monthly<-as.character( to.month(template.mauer$date) )
     template.mauer$seasonal<-as.character( to.season(template.mauer$date) )
     template.mauer$annual<-as.character( to.water.year(template.mauer$date) )

     period.names<-create.template.periods()$name
     template.mauer<-template.mauer[,c("date",period.names)]
     
     return(template.mauer)
}



## ------------------------------------------------------------------------
#' @title Create monster list of 3d weather matrices
#' @description used to store aggregated weather metrics
# no export

create.w.matrices<-function( weather.filenames, periods=c("daily", "monthly", "seasonal", "annual"),
                             template.date, template.period, cols.weather ) {
                                                                 #template.date=NULL
#      if (!("weather.filename" %in% names(gages.spatial)))
#           stop("Must provide gages.spatial object that has been assigned weather.filename")
#      
#      if (is.null(template.date))
#      template.date<-create.template.date()
#      template.period<-create.template.periods()
#      cols.weather<-create.cols.weather()
     
     w.matrices<-list()
     for ( j in template.period$name ) {
          if ( j %in% periods ) {
               w.matrices[[j]]<-array(dim=c(  nrow(template.date[[j]]), 
                                         length(weather.filenames), 
                                         length(cols.weather))  ) 
               dimnames(   w.matrices[[j]]   )[[1]]<-template.date[[j]][,1]
               dimnames(   w.matrices[[j]]    )[[2]]<-weather.filenames
               dimnames(   w.matrices[[j]]   )[[3]]<-cols.weather
          }
          else 
               w.matrices[[j]] <- NA
     }
          
#      for (j in periods) {
# #           print(j)
#           i<-which(template.period$name==j)
#           w.matrices[[i]]<-array(dim=c(  nrow(template.date[[j]]), 
#                                          length(weather.filenames), 
#                                          length(cols.weather))  ) 
#           dimnames(   w.matrices[[i]]   )[[1]]<-template.date[[j]][,1]
#           dimnames(   w.matrices[[i]]    )[[2]]<-weather.filenames
#           dimnames(   w.matrices[[i]]   )[[3]]<-cols.weather
#      }
#      names(w.matrices)<-template.period$name
     return(w.matrices)
}




