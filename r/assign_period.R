## ------------------------------------------------------------------------
# functions to label each daily records with date representing season month year

## ----to season-----------------------------------------------------------

#' @title Associate calendar dates either a date or string representing its season.
#' @description Given a date, determines which season it falls into, and assigns it either a string of the season name, 
#  or a date object corresponding to the 28th day of the last month of the season.
#' @param d \code{date} vector of dates
#' @param return.type \code{character} one of "date", "season", or "all".  
#' "date" will return a vector of date object (which includes year)
#' "season" will return vector of strings of the season name (either "winter", "spring", "summer", or "fall") (does not include year)
#' "all" will return a data.frame, with date, string of season name, and numeric of year
#' @return see above
#' @seealso \code{\link{conteStreamflow::to.water.year}}, \code{\link{conteStreamflow::to.month}}
#' @keywords dates, season
#' @export

to.season<-function(d, return.type="date", input.type="date" ) {           #,out.fmt="seasons"
     
     if ( input.type=="date" ) {     
          x<-data.frame(date=d,year=year(d),month=month(d))  # ,season=time2season(d,out.fmt=out.fmt))  
          
          x[x$month>=12,"year"]<-x[x$month>=12,"year"]+1
          x[,c("season.date","season")]<-matrix(unlist(
               lapply(x$month, function(y) {
                    if(y %in% 9:11) c("11/28","fall")
                    else if (y %in% c(12,1:2)) c("2/28","winter")
                    else if (y %in% 3:5) c("5/28","spring") 
                    else if (y %in% 6:8) c("8/28","summer")})),ncol=2,byrow=T)
          x$season.date<-as.Date(paste0(x$year,"/",x$season.date))
          if (return.type=="date")
               return(x[,"season.date"])  
          else if (return.type=="season")
               return(x[,"season"])  
          else if (return.type=="all")
               return(as.data.frame(x[,c("season","season.date","year")]) )  
     }
     
     else if ( input.type == "character" ) {
          if (!( "season" %in% names(d) & "year" %in% names(d) ))
               stop( "Must include columns \"year\" and \"season\" ")
          s <- as.data.frame( rbind( c("11/28","fall"),
                                     c("2/28","winter"),
                                     c("5/28","spring"),
                                     c("8/28","summer") ),
                              stringsAsFactors=F )
          names(s) <- c("date.prefix","season")
                         
          d2 <- merge( d, s, by="season" )
          d2$data.char <- apply( d2[,c("year","date.prefix")], MARGIN=1, FUN=function(x) paste(x, sep="", collapse="/") )
          d$date <- as.Date.character( d2$data.char )     
          return(d)
     }
}
# to.season<-function(d,return.type="date") {           #,out.fmt="seasons"
#      x<-data.frame(date=d,year=year(d),month=month(d))  # ,season=time2season(d,out.fmt=out.fmt))  
#      
#      x[x$month>=12,"year"]<-x[x$month>=12,"year"]+1
#      x[,c("season.date","season")]<-matrix(unlist(
#           lapply(x$month, function(y) {
#                if(y %in% 9:11) c("11/28","fall")
#                else if (y %in% c(12,1:2)) c("2/28","winter")
#                else if (y %in% 3:5) c("5/28","spring") 
#                else if (y %in% 6:8) c("8/28","summer")})),ncol=2,byrow=T)
#      x$season.date<-as.Date(paste0(x$year,"/",x$season.date))
#      if (return.type=="date")
#           return(x[,"season.date"])  
#      else if (return.type=="season")
#           return(x[,"season"])  
#      else if (return.type=="all")
#           return(as.data.frame(x[,c("season","season.date","year")]) )  
# }

## ------------------------------------------------------------------------
#' @title Associate calendar dates a date object or numeric year, representing its water year.
#' @description Given a date, determines which water year it falls into, and assigns it either a date corresponding to the last calendar day of the water year, or a numeric representation of the year.
#' @param d \code{vector of dates}  
#' @param date.only \code{boolean} if TRUE (default), will only return vector of dates
#' if FALSE, will return data.frame with column of dates, and numeric column of water years
#' @return see above
#' @seealso \code{\link{conteStreamflow::to.season}}, \code{\link{conteStreamflow::to.month}}
#' @keywords dates, water year
#' @export

to.water.year<-function(d,date.only=T) {
     x<-data.frame(date=d,year=year(d),month=month(d))
     x[x$month>=10,"year"]<-x[x$month>=10,"year"]+1
     x$water.date<-as.Date(paste0(x$year,"/9/30"))
     if (!date.only)
          return(as.data.frame(x[,c("year","water.date")]))
     else
          return(x[,"water.date"])
}

## ------------------------------------------------------------------------
#' @title Associates calendar dates with a date representing its month.
#' @description Given a date, determines which month it falls into, and assigns the date corresponding to the first calendar day of the month.  Yeah, this one is probably not necessary, but it's easier to have a function analogous to season and water.year assignment functions
#' @param d \code{vector of dates}
#' @return \code{vector of dates}
#' @seealso \code{\link{conteStreamflow::to.water.year}}, \code{\link{conteStreamflow::to.season}}
#' @export

to.month<-function(d) {
     return(month.date<-as.Date(paste0(year(d),"/",month(d),"/1")))
}


