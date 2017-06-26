## ------------------------------------------------------------------------
#' @title Create template of dates 
#' @description Create template of dates to mark each period for both flow and weather data
#' @export

create.template.date<-function (start.year=1949, end.year=2010) {
     # set up data frames for loading mauer historic weather data
     
     #the four periods here are hard-coded in
     #  in order to add different periods, need to create new function to create date templates
     daily <- seq(as.Date(paste0(start.year,"/1/1")), 
                  as.Date(paste0(end.year,"/12/31")), "days")
     
     #create sequences of dates
     template.date<-list(daily=data.frame(date=as.character(daily),
                                   stringsAsFactors=F))
     template.date[["monthly"]] <- data.frame(date=as.character(unique(to.month(daily))),
                                            stringsAsFactors=F )
     template.date[["seasonal"]] <- data.frame(date=as.character(unique(to.season(daily))),
                                            stringsAsFactors=F )
     template.date[["annual"]] <- data.frame(date=as.character(unique(to.water.year(daily))),
                                            stringsAsFactors=F )
     return(template.date)
}


# create.template.date<-function (start.year=1949, end.year=2010) {
#      # set up data frames for loading mauer historic weather data
#      #(using flow data since 1949 only, even though there are some records back to the 30s, 
#      #because Mauer met data only available starting 1950)
#      
#      #the four periods here are hard-coded in
#      #  in order to add different periods, need to create new function to create date templates
#      
#      #create sequences of dates
#      template.date<-list(daily=data.frame(date=as.character(
#                                    seq(as.Date(paste0(start.year,"/1/1")), 
#                                        as.Date(paste0(end.year,"/12/31")), "days")),stringsAsFactors=F),
#                          monthly=data.frame(date=as.character(
#                                    seq(as.Date(paste0(start.year,"/1/1")), 
#                                        as.Date(paste0(end.year,"/12/1")), "months")),stringsAsFactors=F),  
#                                    #first date of month
#                          seasonal=data.frame(date=as.character(
#                                    seq(as.Date(paste0(start.year,"/02/28")), 
#                                        as.Date(paste0(end.year+1,"/2/28")), "3 month")),stringsAsFactors=F), 
#                                    #LAST date of season (so the year of winter is the same as following spring)
#                          annual=data.frame(date=as.character(
#                                     seq(as.Date(paste0(start.year,"/09/30")), 
#                                         as.Date(paste0(end.year+1,"/9/30")), "years")),stringsAsFactors=F)  ) 
#                                     #LAST date of year 
#                                     #(so the date of oct-dec months is the same as following jan-sept months)
#      return(template.date)
# }



## ------------------------------------------------------------------------
#' @title define periods
#' @description if we want to add different period (i.e. bkt bioperiod), can do it here
#' @export
create.template.periods<-function( min.records.monthly=25, 
                                   min.records.seasonal=80, min.records.annual=345 ) {
     template.period<-data.frame(name=c(        "daily",                 "monthly",   
                                                "seasonal",              "annual"), 
                                 min.records=c( 1,                       min.records.monthly, 
                                                min.records.seasonal, min.records.annual), 
                                 stringsAsFactors = F)
     row.names(template.period)<-template.period$name
     return( template.period )
#      assign( "template.period", value=template.period, envir=.GlobalEnv )
}

## ------------------------------------------------------------------------
# #' @title names of seasons
# #' @description yep, that's all for now
# # no export
# 
# create.template.season<-function() {
#      template.season<-c("winter","spring","summer","fall")
#      return(template.season)
# }


