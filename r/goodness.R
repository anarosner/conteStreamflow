## ----goodness of fit/validation functions--------------------------------
#' @title Calculate coefficient of variation of error
#' @description Calculates CV of error, given 2 vectors of observed and predicted values
#' @param obs \code{numeric vector} observed values
#' @param pred \code{numeric vector} predicted values (vector of same length as observed values)
#' @return \code{numeric}
#' @keywords cv, goodness of fit
#' @seealso goodness
#' @export

cv.error<-function(obs,pred) {
     sd(pred-obs)/mean(obs)
}


## ------------------------------------------------------------------------
#' @title Calculate model bias
#' @description Calculates model bias, given 2 vectors of observed and predicted values
#' @param obs \code{numeric vector} observed values
#' @param pred \code{numeric vector} predicted values (vector of same length as observed values)
#' @return \code{list}
#' @keywords bias, goodness of fit
#' @seealso goodness
#' @export
bias<-function(obs,pred) {
     b<-mean(pred)-mean(obs)
     return(list(bias=b,percent.bias=b/mean(obs)*100))
}


## ------------------------------------------------------------------------
#' @title Some goodness of fit, validation metrics 
#' @description Calculates and prints a variety of goodness of fit, validation metrics, including rmse, NS eff, bias, percent bias, pearson's R, and cv of error
#' @param obs \code{numeric vector} observed values
#' @param pred \code{numeric vector} predicted values (vector of same length as observed values)
#' @return \code{data.frame}
#' @keywords bias, goodness of fit
#' @seealso bias, cv.error
#' @export

goodness<-function(df=NULL,obs=NULL,pred=NULL) {
     if(is.null(obs) | is.null(pred)){
          if(is.null(df)){
               stop("Must define obs and pred vectors, 
                     or data frame w/ obs and pred columns")
          }
          obs<-df$obs
          pred<-df$pred
     }

     f<-data.frame(sample.n=length(obs),mean=mean(obs))
     f$RMSE<-rmse(obs,pred)
     f$NSEff<-NSeff(obs,pred)
     f$bias<-mean(pred)-mean(obs)
     f$percent.bias<-(mean(pred)-mean(obs))/mean(obs)*100
     f$pearsonR<-cor(obs,pred,method="pearson")
     f$CV.error<-sd(pred-obs)/mean(obs)
     return(round(f,3))
}



## ------------------------------------------------------------------------
#' @title goodness of fit stats nseff and bias 
#' @export
g.fit<-function( obs=NULL, pred=NULL, set, iteration, period ) {
     
     f<-data.frame( NSEff=NSeff(obs,pred) )
     f$bias<-mean(pred)-mean(obs)
     f$percent.bias<-(mean(pred)-mean(obs))/mean(obs)*100
     f$mean<-mean(obs)
     f<-round(f,3)
     f$set <- set
     f$iteration <- iteration
     f$period <- period
     return(f)
}


## ------------------------------------------------------------------------
#' @title  g  fit boxplots
#' @description x 
#' @export


g.fit.boxplot<-function( goodness, measure.vars=c("NSEff","bias","percent.bias") ) {
#      o.goodness[,"period.name"]<-o.goodness[,period.name]
#      oo.goodness<-melt( o.goodness, measure.vars=measure.vars )

     goodness<-subset(goodness,set!="Calibration")
     gg <- melt( goodness, measure.vars=measure.vars )
     
     for ( k in measure.vars ) {
          gg.goodness<-ggplot( data=subset(gg,variable==k), aes(x=set, y=value, fill=set) ) + 
          geom_abline(intercept=0, slope=0,col="grey50", lty=2) + 
          geom_boxplot(color="grey20") +
          theme_bw()
          print(
               gg.goodness + facet_wrap(~period, nrow=2, scales="free_y") + ggtitle(k)
               )
          
     }
#           scale_fill_manual(values = c("darkred","dodgerblue3")) +


     gg <- melt( goodness, measure.vars=measure.vars )
     gg.goodness<-ggplot( data=gg, aes(x=set, y=value, fill=set) ) + 
          geom_abline(intercept=0, slope=0,col="grey50", lty=2) + 
#           scale_fill_manual(values = c("darkred","dodgerblue3")) +
          theme_bw()
     gg.goodness + geom_boxplot(color="grey20") + facet_grid(period~variable, scales="free_y")


}


## ----calculate goodness of fit statistics--------------------------------
#' @title  goodness of fit statistics
#' @description x 
#' @export

goodnessOLD<-function( obs.real=NULL, pred.log=NULL, bias.correction=NULL ) {

     residuals <- pred.log - log(obs.real)
     pred.real <- retransform( pred.log, residuals=residuals, bias.correction=bias.correction )
#      corr.factor <- mean( exp( pred.real-obs.real ) )
#      pred.corr <- pred.real*corr.factor
     
     f <- data.frame(sample.n=length(obs.real),mean=mean(obs.real))
     f$NSEff.real <- NSeff( obs.real, pred.real )
#      f$NSEff.corr <- NSeff( obs.real, pred.corr )
     f$NSEff.log <- NSeff( log(obs.real), pred.log )
     
     f$bias<-mean(pred.real)-mean(obs.real)
     f$percent.bias<-(mean(pred.real)-mean(obs.real))/mean(obs.real)*100
#      f$percent.bias<-mean( (pred.real-obs.real)/(obs.real) )*100
     f$bias.log<-mean(pred.log)-mean(log(obs.real))
     
#      f$bias.corr<-mean(pred.corr)-mean(obs.real)
#      f$percent.bias.corr<-(mean(pred.corr)-mean(obs.real))/mean(obs.real)*100
     
     f$pearsonR <- cor( obs.real, pred.real, method="pearson" )
#      f$pearsonR.corr <- cor( obs.real, pred.corr, method="pearson" )
     
     f$CV.error <- sd(pred.real-obs.real) / mean(obs.real)
#      f$CV.error.corr <- sd(pred.corr-obs.real) / mean(obs.real)
#      f$corr.factor<-corr.factor
     return(round(f,3))
}


## ----goodness of fit boxplots--------------------------------------------
#' @title  goodness of fit boxplots
#' @description x 
#' @export


goodness.boxplot<-function( o.goodness, measure.vars=c("NSEff.real", "NSEff.log", "bias", "percent.bias"), period.name="season" ) {
     o.goodness[,"period.name"]<-o.goodness[,period.name]
     oo.goodness<-melt( o.goodness, measure.vars=measure.vars )

     gg.goodness<-ggplot( data=oo.goodness, aes(x=eval(period.name), y=value, fill=set) ) + 
          geom_abline(intercept=0, slope=0,col="grey50", lty=2) + 
          scale_fill_manual(values = c("darkred","dodgerblue3")) + theme_bw()
     gg.goodness + geom_boxplot(color="grey20") + facet_wrap(~variable, nrow=2, scales="free_y")

}

