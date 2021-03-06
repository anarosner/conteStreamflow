## ------------------------------------------------------------------------
#' @title import weather 
#' @description import weather
#' @export
weather_retrieve <- function( sites, 
                              periods=c("day","month","wateryear"),
                              alter_temp=NULL, alter_precip=NULL ) {
                              #later, add arguments by which user can customize summarization and lag and antecedents
     
     # check cache
     cache_check()
     
     # check packages
     require(plyr)
     require(lubridate)
     require(EcoHydRology)
     require(DataCombine)
     require(zoo)
     require(dplyr)
     require(tidyr)
     
     ### customizable parameters... eventually
     ##
     #
     
     #a whole bunch of stuff to be added as parameters
     # will figure out how best to include them without creating something unreadable, and then add them in
     #summary columns
     #      cols_total <- c( "precip_e", "precip_raw", "pet", "gdd", "swe_day", "frozen_day"  )
     #      cols_mean <- c( "tmax", "tmin" )
     #      cols_max <- c( "precip_e", "precip_raw", 
     #         "precip_e_sum_3", "precip_e_sum_5", "precip_raw_sum_3", "precip_raw_sum_5", 
     #         "swe_doy"  )
     #      cols_min <- c()
     
     #lag columns month and wateryear
     #      lag1_cols=c( 
     #           "n_days_weather","precip_e_total","pet_total","gdd_total","swe_day_total",
     #           "frozen_day_total","tmax_mean","tmin_mean" )
     #      lag2_cols=c( 
     #           "n_days_weather","precip_e_total","pet_total",
     #           "swe_day_total","tmax_mean","tmin_mean" )
     #lag columns daily
     #      lag1_cols=
     #           c("precip_e","precip_raw","swe","pet","gdd","tmax","tmin"),
     #      lag2_cols=
     #           c("precip_e","precip_raw","swe","pet","gdd","tmax","tmin")
                          
     #which antecedent periods to calculate for which variables
     ante_days_precip_e=c(7,30,60,90)
     # ante_days_precip_raw3=c(30,60)
     ante_days_gdd=NULL
     ante_days_pet=c(30,90)
     #right now only calculates the antecedent conditions for the max precip_e event
     #maybe customize this in the future
     
     #function parameters to add
     # require_lag1=TRUE, require_lag2=TRUE
     
     
     ### setup and checks
     ##
     #
     
     #check that sites have been assigned weather grid.  
          #(later, change to just call function to assign weather grid if needed rather than warning)
     if ( !( "weather_set" %in% names(sites) & "weather_region" %in% names(sites) & "weather_filename" %in% names(sites)) )
          stop("Please first plot sites to weather grids using")
     
     #allow function to take sites argument as *either* data frame or spatial data frame
     if ( !is.null(attributes(class(sites))) && attributes(class(sites))$package == "sp" )
          sites <- sites@data

          # create master table
     weather_records <- list()
     for ( j in periods ) 
          weather_records[[j]] <- NULL
          #    wide by metric, 
          #    long by site and date/time-step
          #    stored in a list, one table for each period
     
     ### weather grid setup: determine grid cells needed, read weather set info&grid, download any files not in cache
     ##
     #
     
     #if all rows don't have the same weather set, throw an error
     if ( length( unique( sites$weather_set ) ) > 1 )
          stop( "you must use a single weather set")
     weather_set <- unique( sites$weather_set )
     
     # generate a data.frame of *unique* weather grids used by all sites
     select_weather_files<-sites[,c("weather_filename","weather_region" ,"weather_set")]
     select_weather_files<-select_weather_files[ !duplicated(select_weather_files$weather_filename), ] 
               #note: it's assumed that there aren't identical filenames within different directories
     
     
     ## load weather set info/data
     cache_setup_weather_set( weather_set=weather_set, weather_region=unique(select_weather_files$weather_region) )
     cache_read_rdata( file="weather_set_info.bz2", dir=paste0("weather/",weather_set) )
          # this will load the url, column names, and dates
               # the column names unfortunately have to be hardcoded, with different values for each (of the three) data sets
               #    neither livneh or mauer data sets have column names
               #    livneh sets have no dates. mauer set has dates, but it's not in standard date format, 
               #         and it's easier to deal with it the same way as we deal w/ the other two sets
               #    so we're going to just cbind to assign the date and assign the column names
               #         (trusting they're all in the right order, which I usually try to avoid, 
               #         but I've quadruple checked that it all matches up...)


     # load table of lat/long of centroids of each weather grid cell. 
     #    these lat values are used for snow melt and PET models
     cache_read_rdata( file="weather_grid_coords.bz2", dir=paste0("weather/",weather_set) )

      
     # check/download weather data files
     for (j in unique(select_weather_files$weather_region)) {
          files <- select_weather_files[ select_weather_files$weather_region==j, "weather_filename"]
          for ( k in files ) {
               cache_download( k, dir=file.path("weather",weather_set,j), 
                               server_url=file.path(weather_set_url,j,fsep="/") )
          }
     }

     
     ### read and process weather data timeseries
     ##
     #
     
     
     ## loop through weather gridcells/files (MAIN LOOP)
     #
     cat(paste( "Begin processing", nrow(select_weather_files), 
                "unique weather files used for", nrow(sites),"sites","\n" ))
     
     for (i in 1:nrow(select_weather_files) ) {
     
          cat( paste( "  --  Processing file", i, "of", nrow(select_weather_files), "  --  \n" ))
                    
          #lookup the centroid for this gridcell/file
          centroid <- weather_grid_coords[i, c("x","y")]

          ## load raw data file for grid cell
          #
          x <- cache_read_table( file=select_weather_files$weather_filename[i], 
                                 dir=file.path("weather",weather_set,select_weather_files$weather_region[i]),
                                 weather_set_table_params )
          x <- setNames( x, weather_set_cols )
          x <- cbind( weather_set_dates, x )
          
                        
          ## alter raw to generate shifted weather scenarios (daily timestep)
          #
          if ( !is.null(alter_temp) || !is.null(alter_precip) ) {
                x <- weather_alter( x, alter_temp, alter_precip )
          }
          
          ## weather process calculations (daily timestep)
          #
          x_calc <- weather_raw_calc( x, centroid )
          
          # calculate antecedent conditions 
          if ( !is.null(ante_days_precip_e) | !is.null(ante_days_pet) | !is.null(ante_days_gdd) )  {
               x_calc <- weather_antecedent( x_calc, ante_days_precip_e, ante_days_pet, ante_days_gdd )
          } #this calculates antecedent conditions for each day.  later, will fetch those conditions for the specific days of interest, i.e. date of max precip

          
          ## summarize and calulate lags for each period (LOOP THROUGH PERIODS within each file)
          #
          
          for (j in periods ) { 
               
               ## summarize
               #
               
               #summarize (for month and wateryear only, no summarization for daily)
               #day
               if ( j == "day" ) 
                    x_period <- select( x_calc, -matches("swe_doy") )
               #month or wateryear
               else  {
                    x_period <- weather_summarize( x_calc, j )
                    x_period <- weather_summarize_date( x_calc, x_period, j )
               }
               
               
               ## calculate lags and antecedent conditions
               #
               # day (lags are a little different for day)
               if ( j == "day" )  {
                    x_period <- weather_lag( x_period, 
                                    lag1_cols=
                                        c("precip_e","precip_raw","swe","pet","gdd","tmax","tmin"),
                                    lag2_cols=
                                         c("precip_e","precip_raw","swe","pet","gdd","tmax","tmin") )
               }
               #month or wateryear
               else  
                    x_period <- weather_lag( x_period )
               
               # for month or wateryear only, 
               #    fetch the (daily) antecedent conditions from that date of max precip
               if ( j != "day" ) {
                    if ( !is.null(ante_days_precip_e) | !is.null(ante_days_pet) |
                              !is.null(ante_days_gdd) )  {
                         
                         x_period <- weather_fetch_ante( x_period, x_calc, j, date_col="max_precip_e_date",
                                                         ante_days_precip_e, ante_days_pet, ante_days_gdd )
                    }
               }
               
               
               
               ## clean out NA's
               #
               
               #for months, remove records at the beginning that don't have lag values
               if ( j=="month" ) {
                    x_period <- x_period %>% 
                         filter( !is.na(n_days_weather_lag2) )
                    x_period <- x_period %>% 
                         select( -n_days_weather_lag1, -n_days_weather_lag2 )
               }
               
               #for wateryear, remove records at beginning and end that don't have complete wateryear
                    #all records have complete calendar year, so our criteria are a bit different
               if ( j=="wateryear" ) {
                    x_period <- x_period %>% 
                         filter( n_days_weather >= 365 )
                    x_period <- x_period %>% 
                         select( -n_days_weather_lag1, -n_days_weather_lag2 )
               }
               
               
               ## save weather file/set info
               #
               x_period$weather_set <- weather_set
               x_period$weather_region <- select_weather_files$weather_region[i]
               x_period$weather_filename <- select_weather_files$weather_filename[i]
               
               ## save to master table, within list
               #
               if ( is.null(weather_records[[j]]) ) 
                    weather_records[[j]] <- x_period
               else
                    weather_records[[j]] <- rbind( weather_records[[j]], x_period )
          
          } #end loop through periods
          

     }#end loop through weather grid files
     

     return( weather_records )

}#end weather_retrieve function
     

## ------------------------------------------------------------------------
#' @title raw calculations for weather import
#' @description raw calculations for weather import
#' @export

weather_raw_calc <- function( x, centroid ) {
     
     #run snowmelt model
     #    (created in new temporary frame to hold all snow calculations)
     suppressWarnings(
          x.snow<-SnowMelt( Date=x$date, lat_deg=centroid$y,
                           precip_mm=x$precip_mm, Tmax_C=x$tmax, Tmin_C=x$tmin,
                           windSp=x$wind,windHt=10 )  )
     
     #save effective precip (rain plus melt) to main data.frame
     x[,"precip_e"] <- x.snow[,"Rain_mm"] + x.snow[,"SnowMelt_mm"]
     #(decided not to save rain and melt, but we could: they're generated in columns "Rain_mm","SnowMelt_mm")
     #save snowpack to main data.frame
     x[,"swe"] <- x.snow[,"SnowWaterEq_mm"] 
     x.snow$with.snow.pack <- x.snow$SnowDepth_m>0 # is there any snow pack on this date?
     x[,"swe_day"] <- x.snow[,"with.snow.pack"] 

     
     #day of year with snowpack
     #    this can be used later to find the last day in spring with snowpack/ get time of spring melt
     #    in these raw calculations, we identify which days in the first part of year (doy<200) have snowpack
     #         and we note their doy
     #    later, when aggregating, we can take the max of these values to get the last date in spring w/ snowpack
     x.snow$doy <- as.numeric(format(x.snow$Date,"%j")) #just the day of year
     x.snow$snow.pack.doy <- 0 #create new column
     # if there is snow pack and it's the first part of year (doy<200), record the doy
     x.snow[x.snow$with.snow.pack&x.snow$doy<200,"snow.pack.doy"] <- x.snow[x.snow$with.snow.pack&x.snow$doy<200,"doy"]
     
     # save the dates w/ snowpack to main data.frame
     x[,"swe_doy"]<-x.snow[,"snow.pack.doy"]
     
     #temperature metrics, including estimated potential evapotranspiration and gdd
     x[,"pet"]  <- PET_fromTemp( Jday=yday(x$date), Tmax_C=x$tmax ,Tmin_C=x$tmin, lat_radians=centroid$y*pi/180 )*1000 
     x[,"tavg"] <- ( x$tmin+x$tmax )/2
     x[,"gdd"]  <- sapply( x$tavg, FUN=function(y) max( y-10, 0) )
     x[,"frozen_day"]<-x$tmax<=0 #simple indicator of whether max temp above freezing, might remove if not useful

     #rename total precip
     x[,"precip_raw"] <-  x[,"precip_mm"]

          #     x[,"doy"] <- as.numeric(format(x$date, format="%j"))

     x[,"precip_e_sum_3"] <- rollsum( x$precip_e, k=3, align="right", fill=NA )
     x[,"precip_e_sum_5"] <- rollsum( x$precip_e, k=5, align="right", fill=NA )
     x[,"precip_raw_sum_3"] <- rollsum( x$precip_raw, k=3, align="right", fill=NA )
     x[,"precip_raw_sum_5"] <- rollsum( x$precip_raw, k=5, align="right", fill=NA )
     
     return(x)

}


     


## ------------------------------------------------------------------------
#' @title calculate antecedent precip for set number of days 
#' @description calculate antecedent precip for set number of days 
#' @export

weather_antecedent <- function( x, ante_days_precip_e=c(7,30,60),
                                ante_days_pet=c(30,60,90),
                                ante_days_gdd=c(30,60,90) ) {
     #ante_days_pet
     for ( k in c("precip_e","pet","gdd") ) {
          days = get( paste0( "ante_days_",k ) )

          for ( l in days ) {
               x[,paste0(k,"_ante_",l)] <- rollsum( x[k], k=l, align="right", fill=NA )
               x <- slide( data=x, Var=paste0(k,"_ante_",l), NewVar=paste0(k,"_ante_",l), slideBy=-1, reminder=F )
          } #end loop number of days
          
     } #end loop variables

     return(x)
}


## ------------------------------------------------------------------------
#' @title weather aggregation
#' @description weather aggregation
#' @export

### weather aggregation
##
#

weather_summarize <- function( x_calc, j ) { 

     # these will soon be moved up as arguments, to this function and 
     #         the weather_retrieve function that calls it
     #         so the summary metrics can be customized by the user
     
     cols_total <- c( "precip_e", "precip_raw", "pet", "gdd", "swe_day", "frozen_day"  )
     cols_mean <- c( "tmax", "tmin" )
     cols_max <- c( "precip_e", "precip_raw", 
                    "precip_e_sum_3", "precip_e_sum_5", "precip_raw_sum_3", "precip_raw_sum_5", 
                    "swe_doy"  )
     cols_min <- c()

     # swe_doy_total is used as an indicator of the last day of the wateryear (in spring) 
          # on which there's snowpack 
          # so, it doesn't make sense on a monthly time step, remove it if j isn't wateryear
     if ( j!="wateryear" )
          if ("swe_doy" %in% cols_max)
               cols_max <- cols_max[-which(cols_max=="swe_doy")]

     #set up table   
     x_summary <- x_calc %>% 
               select_( paste0(j,"_date") ) %>%
               group_by_( paste0(j,"_date") )  %>% #group by date representing the period, 
                                                  # here and in code block below
               summarize( n_days_weather = n() ) #count the number of records in that period
     
     #add each summary metric
     if (length(cols_total) > 0 ) { #summarize and merge only if there columns we want to total of
          #summarize by summing
          x_total <- x_calc %>% 
               group_by_( paste0(j,"_date") ) %>% 
               summarize_each_( "sum", cols_total ) #take the sum of each of the columns specified 
          #add suffix to columns (except the period) to indicate that this is the total
          x_total <- setNames( x_total, c(  names(x_total[,1]),  paste0(names(x_total[,-1]), "_total") ))
          #merge with the main table
          x_summary <- merge( x_summary, x_total, by=paste0(j,"_date") )
     }
     if (length(cols_mean) > 0 ) {
          x_mean <- x_calc %>% 
               group_by_( paste0(j,"_date") ) %>% 
               summarize_each_( "mean", cols_mean ) 
          x_mean <- setNames( x_mean, c(  names(x_mean[,1]),  paste0(names(x_mean[,-1]), "_mean") ))
          x_summary <- merge( x_summary, x_mean, by=paste0(j,"_date") )
     }
     if (length(cols_max) > 0 ) {
          x_max <- x_calc %>% 
               group_by_( paste0(j,"_date") ) %>% 
               summarize_each_( "max", cols_max ) 
          x_max <- setNames( x_max, c(  names(x_max[,1]),  paste0(names(x_max[,-1]), "_max") ))
          x_summary <- merge( x_summary, x_max, by=paste0(j,"_date") )
     }
     if (length(cols_min) > 0 ) {
          x_min <- x_calc %>% 
               group_by_( paste0(j,"_date") ) %>% 
               summarize_each_( "min", cols_min ) 
          x_min <- setNames( x_min, c(  names(x_min[,1]),  paste0(names(x_min[,-1]), "_min") ))
          x_summary <- merge( x_summary, x_min, by=paste0(j,"_date") )
     }
     
     
     return(x_summary)
}

               

## ------------------------------------------------------------------------
max_which_date <- function( col_max, col_date ) {
     #if all records are NA, return NA and escape function
     if ( sum( !is.na(col_max) )==0 )
          return( NA )
     row <- which( col_max == max(col_max) )[1]  #find row w/ max value
     return( col_date[row] ) #return date for that row
}
          
max_count_date <- function( col_max, col_date ) {
     #if all records are NA, return NA and escape function
     if ( sum( !is.na(col_max) )==0 )
          return( NA )
     row <- which( col_max == max(col_max) )  #find row w/ max value
     return( length(row) ) #return date for that row
          
}


## ------------------------------------------------------------------------
#this function is customized for finding dates of daily max values within each month/year time-step
#(at the time of writing this, it's not possible to call custom functions using standard evaluation, 
# so it's hard-coded using NSE which columns to find the max date for)

weather_summarize_date <- function( x_calc, x_summary, j ) {
     x_date <- x_calc %>% 
          group_by_( paste0(j,"_date") ) %>% 
          summarize( max_precip_e_date=max_which_date( precip_e, date ), 
                     max_precip_e_ties=max_count_date( precip_e, date ),
                     max_precip_raw_date=max_which_date( precip_raw, date ), 
                     max_precip_raw_ties=max_count_date( precip_raw, date ),
                     max_precip_e_3_date=max_which_date( precip_e_sum_3, date ), 
                     max_precip_e_3_ties=max_count_date( precip_e_sum_3, date ),
                     max_precip_raw_3_date=max_which_date( precip_raw_sum_3, date ), 
                     max_precip_raw_3_ties=max_count_date( precip_raw_sum_3, date ) ) 
     
     x_summary <- merge( x_summary, x_date, by=paste0(j,"_date") )

     return( x_summary )
}

## ------------------------------------------------------------------------
#' @title weather lag
#' @description weather lag
#' @export

weather_lag <- function( y,
                         lag1_cols=c( 
                              "n_days_weather","precip_e_total", "precip_raw_total",
                              "pet_total","gdd_total","tmax_mean","tmin_mean",
                              "swe_day_total","frozen_day_total" ),
                         lag2_cols=c( 
                              "n_days_weather","precip_e_total", "precip_raw_total",
                              "pet_total","tmax_mean","tmin_mean",
                              "swe_day_total" ) ) {
     
     # these will soon be moved up as arguments, to this function and the weather_retrieve function that calls it
     #    so the summary metrics can be customized by the user
     
     #      lag1_cols <- c( "n_days","precip_e_total","pet_total","gdd_total",
     #                      "swe_day_total","frozen_day_total","tmax_mean","tmin_mean" )
     #      lag2_cols <- c( "n_days","precip_e_total","pet_total","swe_day_total",
     #                      "tmax_mean","tmin_mean" )
     
     for ( k in lag1_cols ) {          
          y <- slide( y, Var=k, NewVar=paste0(k,"_lag1"), slideBy=-1, reminder=F )          
     }   
     for ( k in lag2_cols ) {          
          y <- slide( y, Var=k, NewVar=paste0(k,"_lag2"), slideBy=-2, reminder=F )          
     }  
     
     return( y )

}
     

## ------------------------------------------------------------------------
#' @title get the antecedent precip leading up to high precip events
#' @description get the antecedent precip leading up to high precip events
#' @export

weather_fetch_ante <- function(  x_period, x_calc, j, date_col="max_precip_e_date",
                                 ante_days_precip_e, ante_days_pet, ante_days_gdd ) {
     
     cols <- c()
     for ( k in c("precip_e","pet","gdd") ) {
          days = get( paste0( "ante_days_",k ) )
          if ( !is.null(days) )
               cols <- c( cols, paste0(k,"_ante_",days) )
     }
 
     #easiest way to access rows by date, save as row names    
     temp <- x_calc
     row.names(temp) <- as.character(x_calc$date)
     
     #find the dates w/ max precip and save the antecedent columns
     x_period[,cols] <- temp[ as.character(x_period[,date_col]), cols]
     
     return( x_period )
     
}

## ------------------------------------------------------------------------

#' @title wrapper function to retrieve weather, alter it by prescribed amounts at the daily step, and aggregate
#' @description wrapper function to retrieve weather, alter it by prescribed amounts at the daily step, and aggregate
#' @export

# wrapper function which iterates through combinations of changes in temp and changes in precip
# it would be more computationally efficient to embed loop within weather_retrieve funciton
# but I want to minimize the amount of extra code added to weather_retrieve function 
# for simplicity, especially as weather_retrieve will most frequently be used withouot any alterations

weather_retrieve_alter <- function( sites, 
                                    periods=c("day","month","wateryear"),
                                    alter_temp_vector=NULL, alter_precip_vector=NULL,
                                    save_dir=NULL, save_zero_change=F ) {
     
     if ( is.null(save_dir) | save_zero_change ) {
          # calculate current/unchanged (which also sets up data structure)
          weather_records <- weather_retrieve( sites=sites, periods=periods )
          for( j in periods ) {
               weather_records[[j]]$alter_temp <- 0
               weather_records[[j]]$alter_precip <- 0
               
          }
     }
     
     if ( !is.null(save_dir) & save_zero_change )
          save( weather_records, file=paste0(save_dir,"/","weather_temp_0_precip_0.rdata") )
     
     # if either is null, just set to zero change
     if( is.null(alter_temp_vector) )
          alter_temp_vector <- 0
     if( is.null(alter_precip_vector) )
          alter_precip_vector <- 0

     for ( k in alter_temp_vector ) { #loop through temp changes
          for ( l in alter_precip_vector ) { #loop through precip changes

               # check that at least one of temp or precip has been altered (otherwise would duplicate current conditions)
               if( !( k==0 && l==0 ) ) {
                    
                    print( paste0("altering raw observed weather: temperature ",k,"; precipitation ",l))

                    # calculate altered weather
                    weather_records_temp <- weather_retrieve( sites=sites, periods=periods,
                                                              alter_temp=k, alter_precip=l )
                    
                    # save amount by which temp/precip were changed, and bind to main data structure
                    for( j in periods ) {
                         weather_records_temp[[j]]$alter_temp <- k
                         weather_records_temp[[j]]$alter_precip <- l
                    }

                    if ( is.null(save_dir) ) {
                         for( j in periods )
                              weather_records[[j]] <- rbind( weather_records[[j]], weather_records_temp[[j]] )
                    }
                    else {
                         weather_records <- weather_records_temp
                         save( weather_records, file=paste0(save_dir,"/","weather_temp_",k,"_precip_",l,".rdata") )
                    }
               }
          } #end loop precip
     } #end loop temp

     if ( is.null(save_dir) )
          return( weather_records )
}

#      alter_temp=NULL, alter_precip=NULL
#      weather_new_england <- weather_retrieve( gages_no_barr[1:10,], periods=c("month","wateryear") )


## ------------------------------------------------------------------------
#' @title basic function to alter precip or temp by set increment
#' @descriptionbasic function to alter precip or temp by set increment
#' @export
# straightforward modificaiton of historic weather timeseries
# this is pulled out into a separate function in order to minimize the amount of code in weather_retrieve function 
#    that has to do with altering weather

weather_alter <- function( x, alter_temp=NULL, alter_precip=NULL ) {
     #created for weather sets with the following column names
          # "precip_mm" "tmax"      "tmin"      "wind" 
          # precip_mm is modified by the percent change specified in alter_precip
          # both tmax and tmin are modified by absolute change in degreec C specified in alter_temp
          # wind is not changed

     if ( !is.null(alter_precip) )
          x$precip_mm <- x$precip_mm*(1+alter_precip)
     if ( !is.null(alter_temp) ) {
          x$tmin <- x$tmin+alter_temp
          x$tmax <- x$tmax+alter_temp
     }
     
     return( x )
}



