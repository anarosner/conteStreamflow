## ------------------------------------------------------------------------
#' @title col names for mauer weather input data
#' @description col names for mauer weather data.  original inputs do not have column headers
# no export

create.cols.mauer<-function() {
     cols.mauer<-c("year", "month", "day","precip.mm", "tmax","tmin","wind")
     return(cols.mauer)
}


## ------------------------------------------------------------------------
#' @title col names for livneh weather input data
#' @description col names for livneh weather data.  original files do not have column headers
# no export

create.cols.livneh<-function() {
     cols.livneh<-c("precip.mm", "tmax","tmin","wind")
     return(cols.livneh)
}


## ------------------------------------------------------------------------
#' @title Create template of dates in mauer weather data
#' @description mauer dates
# no export

create.template.date.mauer<-function(create.template.periods=(conteStreamflow::create.template.periods)) {
     
     
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
#' @title Create template of dates in mauer weather data
#' @description livneh A dates 1915-2011
# no export

create.template.date.livnehA<-function() {
     
     template.livneh <- data.frame( date=seq.Date( from=as.Date("1915-01-01"), to=as.Date("2011-12-31"), by="day" ) )
     
     template.livneh$daily<-as.character(template.livneh$date)
     template.livneh$monthly<-as.character( to.month(template.livneh$date) )
     template.livneh$seasonal<-as.character( to.season(template.livneh$date) )
     template.livneh$annual<-as.character( to.water.year(template.livneh$date) )
     
     return(template.livneh)
}


## ------------------------------------------------------------------------
#' @title Create template of dates in mauer weather data
#' @description livneh B dates 1950-2013
# no export

create.template.date.livnehB<-function() {
     
     template.livneh <- data.frame( date=seq.Date( from=as.Date("1950-01-01"), to=as.Date("2013-12-31"), by="day" ) )
     
     template.livneh$daily<-as.character(template.livneh$date)
     template.livneh$monthly<-as.character( to.month(template.livneh$date) )
     template.livneh$seasonal<-as.character( to.season(template.livneh$date) )
     template.livneh$annual<-as.character( to.water.year(template.livneh$date) )
     
     return(template.livneh)
}


