
## ----aggregate function to use w/ ddply to aggregate---------------------
#' @title aggregate flow function 
#' @description default function to aggregagte flow.  
#' @export

flow.agg.function<-function( df ) {
     template.period <- get( "template.period", envir=.GlobalEnv )
     j<-names(df)[1] #a way to determine the period, w/o requiring it to be 
                     #   passed as parameter, so only one paramter is needed
     cutoff <- template.period[j,"min.records"] #determine number of records  
                                                 #require to be considered "complete"
                                                 #for this period type     
     return(c(
          mean=mean(df$val,na.rm=T),
          max=max(df$val,na.rm=T),
          min=min(df$val,na.rm=T),
          low=min(df$rolling7,na.rm=T),
          complete=sum(!is.na(df$val))>=cutoff,
          records.period=sum(!is.na(df$val))
               #records.period.rolling=sum(!is.na(df$rolling7)) 
     ))
} 



## ------------------------------------------------------------------------
#' @title calculations to perform on daily records before aggregation  
#' @description by default, calculates rolling mean, and returns
#' @export

flow.pre.agg.function <- function( x.all ) {
     #7 day rolling mean
          #convert to zoo timeseries, all columns
     x.zoo<-zoo(x.all$val,x.all$dates) 
          #rollapply, feed it only values, b/c rollapply works better that way
     suppressWarnings( 
          rolling7<-rollapply(data = x.zoo,
                              FUN = function(x) mean(x), 
                              width = 7,
                              align="center", 
                              partial = F) 
     )
          #recreate zoo timeseries, using zoo with all columns, plus rolling mean calucated above
     x.roll<-as.data.frame(merge(x.zoo, rolling7))
     names(x.roll)<-c("val","rolling7")
     x.roll$date<-as.Date(row.names(x.roll))
#      x.roll <- x.roll[,c(ncol(x.roll),1:(ncol(x.roll)-1))] #reorder so date is first, val is second, all other follow


     x.roll$doy <- as.numeric(format(x.roll$date, format="%j"))
#      x$doy <- as.numeric(format(x$date, format="%j"))

     return(x.roll)
}



## ----import and aggregate into flow metrics------------------------------
#' @title import flow 
#' @description import flow from nwis web service, and aggregate to various metrics and by various periods
#' @export
flow.retrieve<-function(gages.spatial, 
                         periods=c("monthly","annual"),
                         flow.pre.agg.function = (conteStreamflow::flow.pre.agg.function),
                         flow.agg.function = (conteStreamflow::flow.agg.function), 
                         start.year=1890, end.year=2015, 
                         min.records.monthly=25, min.records.seasonal=80, min.records.annual=345,
                         return.spatial=F) {
     
     ### set up 
     ##
     #
     cache.check()
     #set up data.frame for counting # records     
     gages.temp<-gages.spatial[,c("site_no","station_nm","da_nwis_sqkm")] #data.frame or spatial data frame that will store the number of records per period timestep
     if (!return.spatial)
          gages.temp <- gages.temp@data

     #create templates and lists of column names
     template.date <- create.template.date( start.year=start.year, end.year=end.year )     
     template.period <- create.template.periods( min.records.monthly=25, 
                                                 min.records.seasonal=80, min.records.annual=345 )
     
     assign( "template.period", template.period, envir=.GlobalEnv )
     cols.flow <- create.cols.flow( flow.pre.agg.function, flow.agg.function )

     #create matrix for storing flow data
     q.matrices <- create.q.matrices( gages.spatial=gages.spatial, periods=periods,  
                                     template.date=template.date, template.period=template.period, 
                                     cols.flow=cols.flow )
     

     ### Load data
     ##
     #
     cat("Begin loading and aggregating stream flow observations...\n")
     missing<-c() #save site_no id's of gages missing all data, or if unable to retrieve records
     for (k in 1:length(gages.spatial$site_no))     {
          cat(paste("  --  Loading/aggregating gage", k, "of", length(gages.spatial$site_no), "  --  \n"))
          flag<-F
          
          #read raw, daily flow data
          tryCatch(
               # uses waterData package functions to pull NWIS data from USGS web services 
                                             #  flow/discharge cfs, code 00060
                                             #  daily mean, code 00003
               x1<-importDVs(gages.spatial$site_no[k], code = "00060", stat = "00003"),
               error = function(e) {
                    missing<-c(missing,k)
                    warning(paste("Gage",gages.spatial$site_no[k],"missing data", 
                                  "\n",e))
                    flag<-T
               })
     
           
          if (!flag) #check for errors
          {                    
               # save meta data for daily flow records
               #    only save this for one gage
               if ( k ==1 ) {
                    meta.url <- paste0(
                         tellMeURL(gages.spatial$site_no[k], code = "00060", stat = "00003",
                              sdate = "1851-01-01",
                              edate = as.Date(Sys.Date(), format = "%Y-%m-%d")),
                         "&format=rdb")
                    meta <- readLines( con=url(meta.url), n=23 )
                    save.log( text=meta, filename="flow_retrieval_meta", ext="txt" ) 
                    close( url( meta.url) )
               }
               
               x1<-cleanUp(x1,task="fix",replace=NA) #waterData function to fix common problems (sets ice to NAs, etc, I think)
               cat(paste0("        Gage ",gages.spatial$site_no[k],", loaded ",nrow(x1)," rows\n"))
               if ( sum(duplicated(x1$dates)) ) {  
                    #wouldn't think this would be an issue, but some gages have duplicated dates
                    #for now, just keeping the first record for a date and removing the rest.  down the line, maybe want to do some sort of comparison of duplicates
                    warning(paste("      Duplicated dates in gage",gages.spatial$site_no[k],":",
                                  x1[(which(duplicated(x1$dates))-1):which(duplicated(x1$dates)),], "removed"))
                    x1<-x1[!duplicated(x1$dates),]
               }
               
               ### flow.calculations
               ##
               #
               x2 <- flow.pre.agg.function( x1 )  
#                cols <- unique(c("val",names(x2)[names(x2)!="date"]))
#                cols <- unique(c("val",names(x2)))
               
               #assign dates representing different periods and years
               #change this to loop through specified periods and automatically 
               #    use the correct function to assign representative date, so it's easy to add new period definitions
               #saved as character so they can be assigned as row names 
               x2[,"daily"]<-as.character( x2$date )
               x2[,"monthly"]<-as.character( to.month(x2$date) )
               x2[,"seasonal"]<-as.character( to.season(x2$date) )
               x2[,"annual"]<-as.character( to.water.year(x2$date) )
               
               
               ### aggregate
               ##
               #


          
               for (j in periods){     #loop through periods specified in function parameter
                    
                    #aggregate, i.e. mean, max, low, etc
                    suppressWarnings(
                         x3<-ddply( x2[,c(which(names(x2)==j),which(names(x2)!=j))], 
                                   #feed data frame, with reordered columns, 
                                   #    so column w/ period name is first 
                                    j, #periods
                                    flow.agg.function #agg function 
                                   ) 
                    )
                    
                    #set records that don't have min # records required for period to NA
                    #   this includes all columns besides the period name, 
                    #         the indicator of whether it's complete, 
                    #         and the # of records per timestep
                    #   this should be able to work if the names of columns to aggregate                          
                    #         are changed, *except* for "complete" and "records.period"
                    #         which are specified explicitly
                    x3[x3$complete==0,!(names(x3) %in% c(j,"complete", "records.period"))] <- NA 

                    #save a count of # complete records in the temporary gages data frame
                    gages.temp[k,paste0("records.",j)]<-sum(x3$complete)
                    gages.temp[k,paste0("first.",j)]<-year(min(x3[x3$complete==1,j], na.rm=T))
                    gages.temp[k,paste0("last.",j)]<-year(max(x3[x3$complete==1,j], na.rm=T))
                    
                    #merge values back w/ date template, 
                    #    so the right dates line up when added to the 3d matrix
                    x4<-merge(template.date[[j]],x3,
                                   by.x="date",by.y=j,all.x=T,all.y=F)

                    #assign values to the gage's "column" in the 3d matrix
                    #   loops through columns.  later replace this? 
                    #   but I couldn't get apply to correctly assign the 1st and 3rd dimension and 
                    #         for one "column" in the 2nd dimension (gage)
                    #but change using procedure from weather_retrieve
                    for (l in cols.flow) {
                         q.matrices[[j]][,gages.spatial$site_no[k],l]<-x4[,l]
                    }

                    }#end loop periods


          }#end check flag
     }#end loop gages

     
     #save counts of how many records each gages has, for each period aggregated
     q.matrices[["records"]]<-gages.temp  
     
     
     ### write log
     ##
     #

     orig.dir <- getwd()
     setwd( file.path(cache.dir.global, "logs") )
     log<-c("flow data retrieval log", format.Date(now()),"\r")
     
     #create lines slightly differently depending on whether there were any sights missing *all* rows 
     if (length(missing)>0) {
          log<-c(log,             
               paste(length(gages.spatial$site_no[-missing]),"sites"),
               paste(length(missing),"gages missing data, ignored"),
               "\r","\r",
               "gages missing all data",
               gages.spatial$site_no[missing],
               "\r","\r",
               "gages used",
               gages.spatial$site_no[-missing])
          write.table(gages.spatial$site_no[missing],sep="/r",file="gages_missing_all_data.txt")
          write.table(gages.spatial$site_no[-missing],sep="\r",file="gages_site_no.txt",row.names=F,col.names=F)
          }
     
     else {
          log<-c(log,             
               paste(length(gages.spatial$site_no),"sites"),
               "\r","\r",
               "sites",
               gages.spatial$site_no)
          write.table(gages.spatial$site_no,sep="\r",file="gages_site_no.txt",row.names=F,col.names=F)
          }
     
     save.log( text=log, filename="flow_retrieval_log", ext="txt" )
     setwd(orig.dir)

     ### finish up
     ##  
     #

     rm( template.period, envir=.GlobalEnv )

     
     return(q.matrices)
     
     
}


## ------------------------------------------------------------------------
#' @title col names for flow stats
#' @description col names for flow stats
#' @export
#stats to save from flow

create.cols.flow <- function( flow.pre.agg.function=(conteStreamflow::flow.pre.agg.function),
                              flow.agg.function=(conteStreamflow::flow.agg.function) ) {
     template.period <- get( "template.period", envir=.GlobalEnv )
     temp <- flow.pre.agg.function(  data.frame( dates=as.Date(1:10, origin=now() ), val=1:10)  )
     temp2 <- flow.agg.function( temp )
     return(  names( temp2 )  )
}




