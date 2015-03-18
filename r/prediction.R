## ----predict-------------------------------------------------------------
#' @title predict continuous timeseries from a set (list) of models by period
#' @description x 
#' @export

predict.model.list<-function( model.list, new.data, 
                      id.col="site_no", period.type="season", dep.var="flow.mean",
#                       year.col="year", 
                      retransform=exp, 
                      smearing.correction=T, rnorm.correction=T,
                      bootstrap.correction=T, correction.summary="mean",
                      correction.iter=100, correction.seed=NULL, ... ) {

     #check on index column, is this needed?
     
     periods <- names( model.list )
     pred <- NULL
     
     for ( j in periods ) {
          
          season.new.data <- new.data[ as.character(new.data[,period.type])==j, ]
          pred.temp <- season.new.data[ , c(id.col, "date", period.type) ]
          pred.temp$obs <- season.new.data[,dep.var]
#           pred.temp <- season.new.data[ ,c(id.col, "date", period.col, year.col) ] 
          {
          if ( class(model.list[[j]]) == "lm" )
               pred.temp$pred.log <- predict(  model.list[[j]], newdata=season.new.data, ...  )
          else if ( class(model.list[[j]]) == "lmerMod" )
               pred.temp$pred.log <- predict(  model.list[[j]], 
                                               newdata=season.new.data, 
                                               allow.new.levels=T, ...  )
          else
               stop( "Model is not of class lm or lmerMod" )
          }
               
          pred.temp$pred.real <- retransform( pred.temp$pred.log )
          
          if ( smearing.correction ) {
               smearing <- mean(retransform(residuals(model.list[[j]])))
                    #           return( exp(y)*correction )
               pred.temp$pred.smearing <- retransform( pred.temp$pred.log )*smearing
          }
          if ( rnorm.correction ) {
               corrected <- stochastic.correction( y=pred.temp$pred.log, 
                                   residuals=residuals(model.list[[j]]),
                                   correction.method="rnorm", correction.summary="all",
                                   correction.seed=correction.seed )
#                corrected <- as.data.frame(
#                               apply( corrected, MARGIN=c(1,2), FUN=retransform )
#                               )
               names(corrected)<-paste0("pred.rnorm.",names(corrected))
               pred.temp[,names(corrected)] <- corrected
          }
          if ( bootstrap.correction ) {
               corrected <- stochastic.correction( y=pred.temp$pred.log, 
                                   residuals=residuals(model.list[[j]]),
                                   correction.method="bootstrap", correction.summary="all",
                                   correction.seed=correction.seed )
#                corrected <- as.data.frame(
#                               apply( corrected, MARGIN=c(1,2), FUN=retransform )
#                               )
               names(corrected)<-paste0("pred.boostrap.",names(corrected))
               pred.temp[,names(corrected)] <- corrected
          }
          
               
          if( is.null(pred) )
               pred <- pred.temp
          else 
               pred <- rbind( pred, pred.temp )
     }
     
     pred <- pred[ order(pred$date), ]
#      pred <- pred[ order(pred$index), ]


     return( pred )     

}


## ------------------------------------------------------------------------
#' @title  bias correction using stochastically generated residuals
#' @description resid are generated either by random generation from normal distribution, or bootstrapping observed residuals
#' @export
stochastic.correction <- function( y, residuals, retransform=exp,
                              correction.method="rnorm",                
                              correction.summary="mean", interval=c(.05, .5, .95), 
                              resid.mean=NA, #use sample mean of residuals, or force to zero?
                              correction.iter=100, correction.seed=NULL ) {  
                    #             resid.sd=NULL, 
                    #             # for resid generated from rnorm 
                    #                            #assume mean=0 or specify?
                    #             # for resid generated from bootstrapping
#             retransform=retransform.log ) {
          
     set.seed(correction.seed)
     
     

     #generate stochastic residuals from normal distribution
     if ( correction.method=="rnorm" ) {
          resid.sd<-sd(residuals)
          if ( is.na(resid.mean) )
               resid.mean <- mean(residuals)     
          
          e <- matrix( rnorm( n=length(y)*correction.iter, 
                              mean=resid.mean, sd=resid.sd ), 
                       ncol=correction.iter )
     }
     
     #generate stochastic residuals by bootstrapping
     else if ( correction.method=="bootstrap" ) {    
          e <- matrix( sample( x=residuals, size=length(y)*correction.iter, replace=T ), 
                       ncol=correction.iter )
     }
     
               #      boo1<-as.data.frame(matrix(10^(1:20),ncol=1))
               #      row.names(boo1)<-paste0("sdf",1:20)
               #      
               #      boo2<-matrix(rnorm(200),nrow=20) #e
               #      
               #      boo3<-as.data.frame(apply(boo2,c(2),function(x) x+boo1)) #pred
               #      row.names(boo3)<-row.names(boo1)
               #      
               #      boo4 <- apply(boo3, 1, mean) #mean
               #      
               #      boo5 <- apply(boo3, 1, FUN=quantile, probs=interval, type=9,na.rm=T)
               #      boo5<-as.data.frame(t(boo5))
               # #      row.names(boo5)<-row.names(boo1)
               # #      boo5
               #      
               #      boo3
               #      boo4
               #      boo5
               #      
               #      , na.rm=T, names=paste0("p",exc.prob))
     
     
     #create new, stochastic predictions w/ reintroduced generated residuals
     pred <- apply( e, MARGIN=2, FUN=function(e) y+e ) 
     pred <- apply( pred, MARGIN=c(1,2), FUN=retransform )
     pred <- as.data.frame( pred )
     row.names(pred) <- row.names(y)
     
     #return values
     
     #return all generated stochastic predictions
     if ( correction.summary=="raw" )
          return(pred)
     
     #return mean of stochastic predictions for each site
     else if ( correction.summary=="mean" ) {
          x <- apply(pred, MARGIN=1, FUN=mean)
          return(as.data.frame(x))
     }
          
     
     #return median of stochastic predictions for each site
     else if ( correction.summary=="median" ) {
          x <- apply(pred, MARGIN=1, FUN=median)
          return(as.data.frame(x))
     }
     
     
     #return median and 5-95% interval of stochastic predictions for each site
     else if ( correction.summary=="interval" | correction.summary=="all" ) {
          x <- apply( pred, MARGIN=1, FUN=quantile, 
                      probs=interval, type=9, na.rm=T, names=T )
                    #calculate 5, 50, & 95% using unbiased estimator for normal distrib.
                    #         stats:quantile function type 9 (Blom estimator)
                    
          x <- as.data.frame(t(x))
          names(x)<-interval*100
          if ( correction.summary=="all" ) {
               x$mean <- apply(pred, MARGIN=1, FUN=mean)
          }
          x <- x[,c("mean",interval*100)]
          return(x)          
     }
}
     
     

## ------------------------------------------------------------------------
#' @title  smearing correction
#' @export

smearing.correction <- function( y, residuals, retransform=exp) {
     correction <- mean(restransform(residuals))
     corrected <- retransform(y)*correction
     return(corrected)
}


## ----retransform---------------------------------------------------------
# #' @title  retransform dependent variable
# #' @description default to revert from log scale.  option for smearing bias correction
# #' @export
# 
# retransform.log <- function( y, residuals=NULL ) {
#      
#      if ( !smearing.correction )
#           return( exp(y) )
#      else if ( smearing.correction ) {
#           if ( is.null(residuals) )
#                s
#           correction <- mean(exp(residuals))
#           return( exp(y)*correction )
#      }
# }


