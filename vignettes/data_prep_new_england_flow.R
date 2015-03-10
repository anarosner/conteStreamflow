
## ----load pkg------------------------------

library(conteStreamflow)



## ----set up cache--------------------------------------------------------

#set up cache
cache.setup( cache.dir="c:/alr/models/cache", quiet=T )
cache.set( cache.dir="c:/alr/models/cache" )



## ----load gages----------------------------------------------
g.spatial <- gage.retrieve( states=c("NH","VT"), max.da.sqkm=100, min.da.sqkm=0 )

# number of records from initial retrieval
nrow(g.spatial)
head(g.spatial@data)

cache.load.data( object="states.poly",file="states.rdata", dir="general_spatial" )
plot( g.spatial, pch=16, col="blue" )
plot( states.poly, border="black", add=T )




## ----load flow-----------------------------------------------

#Load/calculate/aggregate flow data for seasonal and annual timesteps
q.matrices<-flow.retrieve( gages.spatial=g.spatial, flow.agg.function=flow.agg.function, 
                           flow.pre.agg.function=flow.pre.agg.function,
                           periods=c("monthly","annual") )




## ------------------------------------------------------------------------
# Sample flow data
str(q.matrices)

# Single gage, all columns
q.matrices[["annual"]][106:126,5,]
# Single column, several gages
q.matrices[["annual"]][106:126,5:15,1]

# Single gage, all columns
q.matrices[["monthly"]][1000:1020,5,]
# Single column, several gages
q.matrices[["monthly"]][1000:1020,5:15,1]

#see number/date ranges of records for each gage
q.matrices[["records"]]


#save locally
# setwd("c:/mydirectory")
# save(q.matrices, file="q_matrices.rdata")



## ------------------------------------------------------------------------
#objects in session
ls()

#session info and package versions
print(sessionInfo()) 





