
## ----create calibration and validation sets------------------------------
#' @title  create calibration and validation sets
#' @description x 
#' @export

create.calib <-function( d.records, 
                         valid.type="gages.years", 
                         valid.fraction=0.5, seed=NULL, 
                         id.col="site_no", year.col="year",
                         select.gages=NULL, select.years=NULL ) {
     
     #      gages.df<-check.args.gages(gages, args.vector=F, 
     #                                args.id.col=gage.id.col, gage.id.class)  
     #    gage.id.class="character", 
     
     gages <- unique( d.records[,id.col] )
     years <- unique( d.records[,year.col] )
     
     if ( is.null(seed) ) {     
          # randomly generate seed 
          #    but then save that seed, so it's reproducible
          seed<-ceiling(runif(1, min = 1, max = 10^8))
     }
     
          
     if ( valid.type == "split.gages" ) {
          #error checking
          if ( is.null(select.gages) )
               stop( "Must provide list of selected gages" )
          
          calib<-d.records[ !(d.records[,id.col] %in% select.gages), ] #add all records not in selected gages 
          valid <- NULL          
          
          for ( i in 1:length(select.gages) ) {
               gage.records <- d.records[ d.records[,id.col]==select.gages[i], ]               
               gage.records <- gage.records[ order(gage.records$date), ]
               
               #add 1st 1/2 of years to calib set
               calib<-rbind(calib,
                            gage.records[ 1:floor(nrow(gage.records)*valid.fraction), ] ) 
               
               #add 2nd half of year to valid set
               valid.temp<-gage.records[ (floor(nrow(gage.records)*valid.fraction)+1):nrow(gage.records), ] 
               
               if ( is.null(valid) )
                    valid<-valid.temp
               else
                    valid<-rbind( valid, valid.temp )
          }
          sets <- list( calib=calib, valid=valid, select.gages=select.gages )
          
     }
     
     else if ( valid.type == "mixed" ) {
          set.seed(seed)
          rows <- sample( row.names(d.records), size=floor(nrow(d.records)*valid.fraction), replace=F )
          valid<-d.records[(row.names(d.records) %in% rows), ]
          calib<-d.records[!(row.names(d.records) %in% rows), ]
          sets <- list( calib=calib, valid=valid, seed=seed )
     }
     
     
     else {
          
          val.years<-val.gages<-NULL
          
          if ( valid.type == "gages.years" ) {    
                    set.seed(seed)
                    val.gages<-sample( gages, size=ceiling(length(gages)*(valid.fraction/2)), replace=F )  
                    set.seed(seed+1)
                    val.years<-sample( years, size=ceiling(length(years)*(valid.fraction/2)), replace=F )  
          }
          
          else if ( valid.type == "gages" ) {
               set.seed(seed)
               val.gages<-sample( gages, size=ceiling(length(gages)*valid.fraction), replace=F )       
          }
          
          else if ( valid.type == "year" ) {
               set.seed(seed)
               val.years<-sample( years, size=ceiling(length(years)*valid.fraction), replace=F )            
          }
     
          #select records and return
          valid<-d.records[ d.records[,id.col] %in% val.gages, ]
          valid<-rbind(valid,
                       d.records[ d.records[, year.col] %in% val.years, ])
          
          calib<-d.records[ !(d.records[,id.col] %in% val.gages), ]
          calib<-calib[ !(calib[, year.col ] %in% val.years), ]
          
          sets <- list( calib=calib, valid=valid, seed=seed )
          if ( !is.null(val.gages) )
               sets$val.gages=val.gages
          if ( !is.null(val.years) )
               sets$val.years=val.years
               #           sets <- list( calib=calib, valid=valid,
               #                         seed=seed, val.gages=val.gages, val.years=val.years )
     }
     
               #           else if ( valid.type == "select.years") {
               #                print("not written yet...")
               #           }
     
     return( sets )
}



## ----create calibration and validation sets old--------------------------
          # #' @title  create calibration and validation sets
          # #' @description x 
          # #' @export
          # 
          # create.calib <-function( d.records, 
          #                          valid.type="gages.years", 
          #                          valid.fraction=0.5, seed=NULL, 
          #                          id.col="site_no", year.col="year",
          #                          select.gages=NULL, select.years=NULL ) {
          #      
          #      #      gages.df<-check.args.gages(gages, args.vector=F, 
          #      #                                args.id.col=gage.id.col, gage.id.class)  
          #      #    gage.id.class="character", 
          #      
          #      gages <- unique( d.records[,id.col] )
          #      years <- unique( d.records[,year.col] )
          #      
          #      if ( is.null(seed) ) {     
          #           # randomly generate seed 
          #           #    but then save that seed, so it's reproducible
          #           seed<-ceiling(runif(1, min = 1, max = 10^8))
          #      }
          #      
          #      
          #      if ( !(valid.type %in% c("select.gages", "select.years")) ) {                   
          #           if ( valid.type == "gages.years" ) {    
          #                set.seed(seed)
          #                val.gages<-sample( gages, size=ceiling(length(gages)*(valid.fraction/2)), replace=F )  
          #                set.seed(seed+1)
          #                val.years<-sample( years, size=ceiling(length(years)*(valid.fraction/2)), replace=F )  
          #           }
          #           else if ( valid.type == "gages" ) {
          #                set.seed(seed)
          #                val.gages<-sample( gages, size=ceiling(length(gages)*valid.fraction), replace=F )       
          #           }
          #           else if ( valid.type == "year" ) {
          #                set.seed(seed)
          #                val.years<-sample( years, size=ceiling(length(years)*valid.fraction), replace=F )            
          #           }
          #           
          #           #select records and return
          #           valid<-d.records[ d.records[,id.col] %in% val.gages, ]
          #           valid<-rbind(valid,
          #                        d.records[ d.records[, year.col] %in% val.years, ])
          #           
          #           calib<-d.records[ !(d.records[,id.col] %in% val.gages), ]
          #           calib<-calib[ !(calib[, year.col ] %in% val.years), ]
          #           
          #           sets <- list( calib=calib, valid=valid,
          #                         seed=seed, val.gages=val.gages, val.years=val.years )
          #      }
          #      
          #      else {
          #           
          #           if ( valid.type == "select.gages") {
          #                #error checking
          #                if ( is.null(select.gages) )
          #                     stop( "Must provide list of selected gages" )
          #                
          #                calib<-d.records[ !(d.records[,id.col] %in% select.gages), ] #add all records not in selected gages 
          #                for ( i in 1:length(select.gages) ) {
          #                     gage.records <- d.records[ d.records[,id.col]==select.gages[i], ]               
          #                     gage.records <- gage.records[ order(gage.records$date), ]
          #                     
          #                     calib<-rbind(calib,
          #                                  gage.records[ 1:floor(nrow(gage.records)*valid.fraction), ] ) #add 1st 1/2 of years to calib set
          #                     
          #                     valid.temp<-gage.records[ (floor(nrow(gage.records)*valid.fraction)+1):nrow(gage.records), ] #add 2nd half of year to valid set
          #                     if ( i==1 )
          #                          valid<-valid.temp
          #                     else
          #                          valid<-rbind( valid, valid.temp )
          #                }
          #                sets <- list( calib=calib, valid=valid, select.gages=select.gages )
          #                
          #           }
          #           else if ( valid.type == "select.years") {
          #                print("not written yet...")
          #           }
          #      } 
          #      
          #      
          #      return( sets )
          # }


