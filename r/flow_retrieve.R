
## ----aggregate function to use w/ ddply to aggregate---------------------
#' @title aggflow 
#' @description x 
#' @export

agg.function.flow<-function(df,create.template.periods=(conteStreamflow::create.template.periods)) {
     j<-names(df)[1] #a kinda sneaky way to determine the period, w/o requiring it to be passed as parameter, so only one paramter is needed
     cutoff<-create.template.periods()[j,"min.records"] #determine #records for this periods type
                                                        #to be considered "complete"     
     return(c(
          mean=mean(df$val,na.rm=T),
          max=max(df$val,na.rm=T),
          min=min(df$val,na.rm=T),
          low=min(df$rolling7,na.rm=T),
          complete=sum(!is.na(df$val))>=cutoff,
          records.period=sum(!is.na(df$val)), 
          records.period.rolling=sum(!is.na(df$rolling7)) 
     ))
} 



## ----import and aggregate into flow metrics------------------------------
#' @title import flow 
#' @description import flow from nwis web service, and aggregate to various metrics and by various periods
#' @export
flow.retrieve<-function(gages.spatial, 
                         periods=c("seasonal","annual"),
                         agg.function.flow = (conteStreamflow::agg.function.flow), 
                         template.date = NULL, template.period = NULL, cols.flow=NULL,
                         return.spatial=F) {
#                         create.cols.function = create.cols.flow,
#                          create.template.date = (conteStreamflow::create.template.date), 
#                          create.template.periods = (conteStreamflow::create.template.periods),
                         
     
     cache.check()
     
#      gages<-gages.spatial@data  # match old code

     
     gages.temp<-gages.spatial[,c("site_no","station_nm")] #data.frame or spatial data frame that will store the number of records per period timestep
     if (!return.spatial)
          gages.temp <- gages.temp@data

     #create templates and lists of column names
     if (is.null(template.date))
          template.date<-create.template.date()
     if (is.null(template.period))
          template.period<-create.template.periods()
     if (is.null(cols.flow))
          cols.flow<-create.cols.flow( agg.function.flow )
     
     #create matrix for storing flow data
     q.matrices<-create.q.matrices(gages.spatial=gages.spatial, periods=periods, 
                                   template.date=template.date, template.period=template.period, cols.flow=cols.flow)
     
     cat("Begin loading and aggregating stream flow observations...\n")
     missing<-c() #save site_no id's of gages missing all data, or if unable to retrieve records
     for (k in 1:length(gages.spatial$site_no))     {
          cat(paste("  --  Loading/aggregating gage", k, "of", length(gages.spatial$site_no), "  --  \n"))
          flag<-F
          
          #read raw, daily flow data
          tryCatch(
               x.all<-importDVs(gages.spatial$site_no[k], code = "00060", stat = "00003"),
               error = function(e) {
                    missing<-c(missing,k)
                    warning(paste("Gage",gages.spatial$site_no[k],"missing data", 
                                  "\n",e))
                    flag<-T
               })
     
          #calculate 7-day rolling mean and aggregate to periods 
          if (!flag)
          {                              
               # uses waterData package functions to pull NWIS data from USGS web services 
                                             #  flow/discharge cfs, code 00060
                                             #  daily mean, code 00003
               x.all<-cleanUp(x.all,task="fix",replace=NA) #waterData function to fix common problems (sets NAs, I think)
               cat(paste0("        Gage ",gages.spatial$site_no[k],", loaded ",nrow(x.all)," rows\n"))
               if ( sum(duplicated(x.all$dates)) ) {  
                    #wouldn't think this would be an issue, but some gages have duplicated dates
                    #for now, just keeping the first record for a date and removing the rest.  down the line, maybe want to do some sort of comparison of duplicates
                    warning(paste("      Duplicated dates in gage",gages.spatial$site_no[k],":",
                                  x.all[(which(duplicated(x.all$dates))-1):which(duplicated(x.all$dates)),], "removed"))
                    x.all<-x.all[!duplicated(x.all$dates),]
               }
               
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

               #assign dates representing different periods and years
               #   replace this later w/ something automated, so it's easier to add new periods (i.e bioperiods for different species)
               #   saved as character so they can be assigned as row names (check on this later)
               x.roll[,"daily"]<-as.character( x.roll$date )
               x.roll[,"monthly"]<-as.character( to.month(x.roll$date) )
               x.roll[,"seasonal"]<-as.character( to.season(x.roll$date) )
               x.roll[,"annual"]<-as.character( to.water.year(x.roll$date) )
          
               for (j in periods){     #loop through periods specified in function parameter
                    #determine number of records for this periods to be considered "complete"
#                     cutoff<-template.period[j,"min.records"]  
#                     assign("cutoff",value = template.period[j,"min.records"], envir = )
                    
                    #aggregate, i.e. mean, max, low, etc
                    suppressWarnings(
                         x.agg<-ddply(x.roll[,c(j,"val","rolling7")], j, agg.function.flow)
                    )
                    
                    #set records that don't have min # records required for period to NA
                    #   this includes all columns besides the period name, the indicator of whether it's complete, and the # of records per timestep
                    #   this should be able to work if the columns to agg are changed, *exept* for the complete, records.period, etc specified explicitly
                    x.agg[x.agg$complete==0,!(names(x.agg) %in% c(j,"complete", "records.period", "records.period.rolling"))]<-NA

                    #save a count of # complete records in the temporary gages data frame
                    gages.temp[k,paste0("records.",j)]<-sum(x.agg$complete)
                    
                    #merge values back w/ date template, so the right dates line up when added to the 3d matrix
                    x.merge<-merge(template.date[[j]],x.agg,
                                   by.x="date",by.y=j,all.x=T,all.y=F)

                    #assign values to the gage's "column" in the 3d matrix
                    #   loops through columns.  later replace this? 
                    #   but I couldn't get apply to correctly assign the 1st and 3rd dimension and for one "column" in the 2nd dimension (gage)
                    #but change using procedure from weather_retrieve
                    for (l in cols.flow) {
                         q.matrices[[j]][,gages.spatial$site_no[k],l]<-x.merge[,l]
                    }

                    }#end loop periods
          }#end check flag
     
#      print(paste0("     end gage ",k))
     }#end loop gages
     
     #if a directory for log file is specified, generate and save one
     #      if(!is.null(log.dir)) {     
     #           setwd(log.dir)

     setwd( file.path(cache.dir.global, "logs") )
          
     log<-c("flow data retrieval log", format.Date(now()),"\r")
     {
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
     
     }
     save.log( text=log, filename="flow_retrieval_log", ext="txt" )
          

     q.matrices[["records"]]<-gages.temp  #save counts of how many records each gages has, for each period aggregated
     
     return(q.matrices)
     
     
}


## ------------------------------------------------------------------------
#' @title col names for flow stats
#' @description col names for flow stats
#' @export
#stats to save from flow

create.cols.flow <- function( agg.function.flow=(conteStreamflow::agg.function.flow) ) {
     return(  names( agg.function.flow(data.frame(val=1,rolling7=1)) )  )
}

# create.cols.flow<-function() {
#    cols.flow<-c("mean","max","min","low","records.period","records.period.rolling")
#    return(cols.flow)   
# } 

