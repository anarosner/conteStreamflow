## ------------------------------------------------------------------------
# q flow
# w weather
# d data of combined flow, weather, and basin stats
# m model
# o model output (calibration fitted values, predictions, goodness of fit)

# oo model output, melted for ggplots
# gg ggplot object




## ----run cross validation of model---------------------------------------
#' @title  cross validate
#' @description x 
#' @export

cross.val<-function( d.records, model.list, iter=10, valid.fraction=0.5, dep.var="flow.mean", seed=NULL, bias.correction=NULL ) {
     
     #replace this w/ create template function
     
     #goodness cols, plus "melt-like" rows
                                        # period (season)
                                        # valid/calib
                                        # iteration
                                        # 10*4*2, 80 rows 
     
     
#      seasons=c("winter","spring","summer","fall") 
     
     periods <- names(model.list)
     o.goodness<-NULL
     
     if (is.null(seed))
          seed <- ceiling( runif(n=1, max = 10^9, min=1) )
     set.seed(seed)         
     seed.list <- ceiling(runif(iter,max=10^9))
     
     for ( i in 1:iter ) {
#           print(paste("iteration",i))
          
          sets <- create.calib( d.records, valid.type="gages.years", 
                                valid.fraction=0.5, seed=seed.list[i] )

          
          model.list.iter<-model.list
          for ( j in 1:length(periods) ) {
               c <- subset(sets$calib, season==periods[j])
               v <- subset(sets$valid, season==periods[j])
               model.list.iter[[j]] <- update( model.list.iter[[j]], data=c )
               o.goodness.temp <- data.frame(
                                   goodness( obs.real=c[, dep.var], 
                                        pred.log= predict(  model.list.iter[[j]], newdata=c ), 
                                        bias.correction=bias.correction ),
                                   season=periods[j], set="calib", iter=i,
                                   stringsAsFactors=F )
               pred <- predict(  model.list.iter[[j]], newdata=v )
               o.goodness.temp[2,] <- c( as.vector(goodness( obs.real=v[, dep.var], pred.log=pred, bias.correction=bias.correction )),
                                periods[j], "valid", i )
#              
               if( is.null(o.goodness) )
                    o.goodness<-o.goodness.temp
               else 
                    o.goodness <- rbind( o.goodness, o.goodness.temp )
          }
          
          
          
     } #end iterations, i loop     

     return( list(values=o.goodness, seed=seed) )
}


