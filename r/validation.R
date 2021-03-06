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

cross.val<-function( d.records, model.list, dep.var="flow.mean", 
                     period.type="month",
                     iter=10, valid.fraction=0.1, valid.type="mixed", seed=NULL,
                     correction.iter=100 ) {
     
     d.records$index <- 1:nrow(d.records)
     
     set.seed(seed)         
     seed.list <- ceiling(runif(iter,max=10^9))
     
     n<-c("NSEff","bias", "percent.bias", "mean", "set", "correction","iteration","period")
     goodness <- as.data.frame(matrix(nrow=0,ncol=length(n)))
     names(goodness) <- n

     cat("Cross validation iteration ")
     for ( i in 1:iter ) {
          cat(paste0(i,"... "))
          
          sets <- create.calib( d.records, valid.type=valid.type, 
                                valid.fraction=valid.fraction, seed=seed.list[i] )

          m <- model.list
          for ( j in names(model.list) ) {
               m[[j]] <- update( m[[j]], 
                                 data=sets$calib[ sets$calib[,period.type]==j,] )
          }

          p <- predict.model.list( m, new.data=sets$valid, dep.var=dep.var, 
                    period.type=period.type,
                    correction.seed=seed.list[i], correction.iter=correction.iter,
                    correction.summary="median",
                    smearing.correction=T, rnorm.correction=T,
                    bootstrap.correction=T )

          c <- predict.model.list( m, new.data=sets$calib, dep.var=dep.var,
                    period.type=period.type,
                    smearing.correction=T, rnorm.correction=T,
                    bootstrap.correction=T )



          for ( j in names(model.list) ) {

#                goodness.temp <- rbind( 
#                     g.fit( obs=log(c$obs), pred=c$pred.log, set="calibration", correction="log", i, j ),
#                     g.fit( obs=log(p$obs), pred=p$pred.log, set="validation", correction="log", i, j ),
#                     g.fit( obs=c$obs, pred=c$pred.real, set="calibration", correction="none", i, j ),
#                     g.fit( obs=p$obs, pred=p$pred.real, set="validation", correction="none", i, j ),
#                     g.fit( obs=p$obs, pred=p$pred.smearing, set="validation", correction="smearing", i, j ),
#                     g.fit( obs=p$obs, pred=p$pred.rnorm.mean, set="validation", correction="rnorm", i, j ),
#                     g.fit( obs=p$obs, pred=p$pred.boostrap.mean, set="validation", correction="bootstrap", i, j )
#                )
               c.j<-c[c[,period.type]==j,]
               p.j<-p[p[,period.type]==j,]
               goodness.temp <- rbind( 
                    g.fit( obs=log(c.j$obs), pred=c.j$pred.log, set="Calibration log-scale", i, j ),
                    g.fit( obs=log(p.j$obs), pred=p.j$pred.log, set="Log-scale", i, j ),
                    g.fit( obs=c.j$obs, pred=c.j$pred.real, set="Calibration no correction", i, j ),
                    g.fit( obs=p.j$obs, pred=p.j$pred.real, set="No correction", i, j ),
                    g.fit( obs=p.j$obs, pred=p.j$pred.smearing, set="Smearing correction", i, j ),
                    g.fit( obs=p.j$obs, pred=p.j$pred.rnorm.mean, set="Stochastic rnorm correction", i, j ),
                    g.fit( obs=p.j$obs, pred=p.j$pred.boostrap.mean, set="Stochastic bootstrap correction", i, j )
               )
               if( nrow(goodness)==0 ) {
                    goodness<-goodness.temp
                    goodness$set <- factor(goodness$set,levels=unique(goodness.temp$set))
               }
               else  {
                    goodness <- rbind( goodness, goodness.temp )
                    goodness$set <- as.factor(goodness$set)
               }
          }
          
     } #end iterations, i loop     

     return( goodness )
#      return( list(values=o.goodness, seed=seed) )
}

## ----run cross validation of model old-----------------------------------
# #' @title  cross validate
# #' @description x 
# #' @export
# 
# cross.val<-function( d.records, model.list, iter=10, valid.fraction=0.5, dep.var="flow.mean", seed=NULL, bias.correction=NULL ) {
#      
#      #replace this w/ create template function
#      
#      #goodness cols, plus "melt-like" rows
#                                         # period (season)
#                                         # valid/calib
#                                         # iteration
#                                         # 10*4*2, 80 rows 
#      
#      
# #      seasons=c("winter","spring","summer","fall") 
#      
#      periods <- names(model.list)
#      o.goodness<-NULL
#      
#      if (is.null(seed))
#           seed <- ceiling( runif(n=1, max = 10^9, min=1) )
#      set.seed(seed)         
#      seed.list <- ceiling(runif(iter,max=10^9))
#      
#      for ( i in 1:iter ) {
# #           print(paste("iteration",i))
#           
#           sets <- create.calib( d.records, valid.type="gages.years", 
#                                 valid.fraction=0.5, seed=seed.list[i] )
# 
#           
#           model.list.iter<-model.list
#           for ( j in 1:length(periods) ) {
#                c <- subset(sets$calib, season==periods[j])
#                v <- subset(sets$valid, season==periods[j])
#                model.list.iter[[j]] <- update( model.list.iter[[j]], data=c )
#                o.goodness.temp <- data.frame(
#                                    goodness( obs.real=c[, dep.var], 
#                                         pred.log= predict(  model.list.iter[[j]], newdata=c ), 
#                                         bias.correction=bias.correction ),
#                                    season=periods[j], set="calib", iter=i,
#                                    stringsAsFactors=F )
#                pred <- predict(  model.list.iter[[j]], newdata=v )
#                o.goodness.temp[2,] <- c( as.vector(goodness( obs.real=v[, dep.var], pred.log=pred, bias.correction=bias.correction )),
#                                 periods[j], "valid", i )
# #              
#                if( is.null(o.goodness) )
#                     o.goodness<-o.goodness.temp
#                else 
#                     o.goodness <- rbind( o.goodness, o.goodness.temp )
#           }
#           
#           
#           
#      } #end iterations, i loop     
# 
#      return( list(values=o.goodness, seed=seed) )
# }

## ------------------------------------------------------------------------
# cross.val<-function( d.records, model.list, dep.var="flow.mean", 
#                      period.type="season",
#                      iter=100, valid.fraction=0.1, seed=NULL, 
#                      bias.correction=NULL ) {
#      
#      #replace this w/ create template function
#      
#      #goodness cols, plus "melt-like" rows
#                                         # period (season)
#                                         # valid/calib
#                                         # iteration
#                                         # 10*4*2, 80 rows 
#      
#      
# #      seasons=c("winter","spring","summer","fall") 
#      
#      periods <- names(model.list)
#      o.goodness<-NULL
#      
#      d.records$index <- 1:nrow(d.records)
#      
# #      if (is.null(seed))
# #           seed <- ceiling( runif(n=1, max = 10^9, min=1) )
#      set.seed(seed)         
#      seed.list <- ceiling(runif(iter,max=10^9))
#      
#      for ( i in 1:iter ) {
# #           print(paste("iteration",i))
#           
#           sets <- create.calib( d.records, valid.type="gages", 
#                                 valid.fraction=valid.fraction, seed=seed.list[i] )
# 
# #           c <- v <- NULL
#           m <- model.list
# #           model.list.iter<-model.list
#           for ( j in periods ) {
# #                c <- sets$calib[ sets$calib[,period.type]==j,]
# #                v <- sets$valid[ sets$calib[,period.type]==j,]
#                
#                m[[j]] <- update( m[[j]], 
#                                  data=sets$calib[ sets$calib[,period.type]==j,] )
#           }
# #                c <- subset(sets$calib, season==j)
# #                v <- subset(sets$valid, season==j)#                
# #                m <- list(update( model.list[[j]], data=c ))
# #                names(m) <- j
#           p <- predict.model.list( m, new.data=sets$valid, dep.var="flow.max",
#                     correction.seed=seed.list[i], correction.iter=100,
#                     smearing.correction=T, rnorm.correction=T,
#                     bootstrap.correction=T )
# 
#           v <- predict.model.list( m, new.data=sets$calib, dep.var="flow.max",
#                     smearing.correction=T, rnorm.correction=T,
#                     bootstrap.correction=T )
# 
# 
#                #                     cbind(p,v[,c("site_no","date","flow.max")])
# 
#                # #               predict.model.list<-function( model.list, new.data, 
#                # #                       id.col="site_no", period.type="season", 
#                # #                       retransform=exp, 
#                # #                       smearing.correction=T, rnorm.correction=T,
#                # #                       bootstrap.correction=T, correction.summary="mean",
#                # #                       correction.iter=100, correction.seed=NULL )
#                #                                               
#                #                model.list.iter[[j]] <- update( model.list.iter[[j]], data=c )
#           for ( j in periods ) {
#                g.temp <- goodness(obs=v$obs, pred=v$pred.real)
#                
#           }
# 
# #                o.goodness.temp <- data.frame(
# #                                    goodness( obs.real=c[, dep.var], 
# #                                         pred.log= predict(  model.list.iter[[j]], newdata=c ), 
# #                                         bias.correction=bias.correction ),
# #                                    season=periods[j], set="calib", iter=i,
# #                                    stringsAsFactors=F )
# #                pred <- predict(  model.list.iter[[j]], newdata=v )
# #                o.goodness.temp[2,] <- c( as.vector(goodness( obs.real=v[, dep.var], pred.log=pred, bias.correction=bias.correction )),
# #                                 periods[j], "valid", i )
# #              
#                if( is.null(o.goodness) )
#                     o.goodness<-o.goodness.temp
#                else 
#                     o.goodness <- rbind( o.goodness, o.goodness.temp )
#           }
#           
#           
#           
#      } #end iterations, i loop     
# 
#      return( o.goodness )
# #      return( list(values=o.goodness, seed=seed) )
# }

