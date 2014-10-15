
## ------------------------------------------------------------------------
#' @title create the monster list of 3-d matrices for flow
#' @description used to store aggregated flow data
# no export

create.q.matrices<-function( gages.spatial, periods=c("daily",  "monthly",   "seasonal",    "annual"),
                            template.date, template.period, cols.flow ) { 
#                           template.date=NULL) { #, cols.flow=NULL
     
#      if (is.null(template.date))
#           template.date<-create.template.date()
#      if (is.null(cols.flow))
#      cols.flow<-create.cols.flow()
#      template.period<-create.template.periods()

     #create list the length of all periods
     #   but only create matrices for the periods specified
     #   this is so the index in the list for period x is consistent no matter how many periods are specified
     q.matrices<-list() #nrow(template.period)
     for (j in periods) {
#           print(j)
          i<-which(template.period$name==j)
          q.matrices[[i]]<-array(dim=c(   nrow(template.date[[j]]), 
                                          length(gages.spatial$site_no), 
                                          length(cols.flow))  ) 
          dimnames(   q.matrices[[i]]   )[[1]]<-template.date[[j]][,1]
          dimnames(   q.matrices[[i]]    )[[2]]<-gages.spatial$site_no
          dimnames(   q.matrices[[i]]   )[[3]]<-cols.flow
     }
     q.matrices[[ ( length(template.period$name)+1 ) ]]<-NA
     names(q.matrices)<-c(template.period$name,"records")
     return(q.matrices)
}


