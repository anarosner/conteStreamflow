## ------------------------------------------------------------------------
#' @title CliFSSS model create model list
#' @description creates a list of models, one for each period
#' @param  df \code{data.frame} 
#' @param  period.type \code{character} 
#' @param  formula \code{character} 
#' @return sort \code{boolean}
#' @export

clifs.model.list <- function( df, period.type="month", formula, sort=T, model.type="lm" ) {
     l<-unique(df[,period.type])
     if (sort)
          l <- sort(l)
     if (model.type=="lm") {
          ml <- lapply(  l,
                         function(j) lm( formula=as.formula(formula),
                                         data=df[df[,period.type]==j,]) )
          names(ml) <- l
               
     }
     else if (model.type=="lmer") {
          print("will create lmer models soon")
     }
     else 
          stop("model.type not recognized")
     return(ml)
}



## ------------------------------------------------------------------------
#' @title prints overall summary of CliFSSS model list performance for each period 
#' @description prints summary of CliFSSS model performance for each period
#' @param  ml \code{list} model list, of  \code{lm} or \code{merMod} class models
#' @export

clifs.summary<- function(ml) {
     s <- sapply(ml,function(m) round( c(
          adj.rqr=summary(m)$adj.r.squared,
          rsq=summary(m)$r.squared, 
          n.obs=length(fitted.values(m)),
          n.terms=length(coefficients(m)),
          press=press(m), aic=AIC(m), 
          max.vif=max(vif(m))), 3
     ) )
     s<-as.data.frame(t(s))
     return(s) 
}


## ------------------------------------------------------------------------
#' @title CliFSSS model, updates model for specified months.  Other months remain as is
#' @description CliFSSS model, updates model for specified months.  Other months remain as is
#' @param  ml \code{list} model list, of  \code{lm} or \code{merMod} class models
#' @param  periods \code{array} 
#' @param  terms \code{character} terms to be appended to formulat (with + or - operator) 
#' @param  df \code{data.frame} 
#' @param  period.type \code{character} 
#' @export

clifs.update <- function( ml, periods, terms, df, period.type="month" ) {
     ml[periods] <- lapply( periods,
                            function(j) update( ml[[j]],
                                                formula=as.formula(paste0(".~.",terms)) )  )
     return(ml)
}



## ------------------------------------------------------------------------
#' @title prints summary of CliFSSS model performance, prints detailed summary of  model for each period 
#' @description prints summary of CliFSSS model performance, prints detailed summary of  model for each period 
#' @param  ml \code{list} model list, of  \code{lm} or \code{merMod} class models
#' @param  periods \code{array} of characters 
#' @param  generic \code{boolean} 
#' @param  pause \code{boolean} defaults to T, should the output be paused between each period 
#' @export

clifs.periods.summary <- function(ml, periods=NULL, generic=T, pause=F) {
     if (is.null(periods))
          periods <- names(ml)
     for (j in periods) {
          print(paste0("++++++ Period  ",j,"  ++++++"))
          m<-ml[[j]]
          if ( generic ) {
               print(summary( m ))
          }
          else {
               coef <- as.data.frame(summary(m)$coefficients)
               #                coef[,4] <- signif( coef[,4], digits=4)
               coef[ coef[,4]<.01, 4 ] <- signif(coef[ coef[,4]<.01, 4 ], digits=4)
               coef[ coef[,4]>=.01, 4 ] <- format(coef[ coef[,4]>=.01, 4 ], scientific=FALSE, digits=3)
               #                coef[,4] <- format(coef[,4], trim = TRUE, scientific=T )
               #                coef[,4] <- scientific( coef[,4], digits=4)
               print( coef )
               print(round(c (adj.rqr=summary(m)$adj.r.squared,
                              rsq=summary(m)$r.squared ),3))
               print( c(n.obs=length(fitted.values(m)),
                        n.terms=length(coefficients(m)) ))
               print(round(c( PRESS=press(m), AIC=AIC(m) ),0))
               print(c( max.VIF=max(vif(m)) ))
          }
          if ( pause )
               readline("press \"enter\" to continue")
     }
}

