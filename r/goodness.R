
## ----calculate goodness of fit statistics--------------------------------
#' @title  goodness of fit statistics
#' @description x 
#' @export

goodness<-function( obs.real=NULL, pred.log=NULL, bias.correction=NULL ) {

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

