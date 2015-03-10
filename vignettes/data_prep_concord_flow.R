
## ----load pkg------------------------------

library(conteStreamflow)



## ----set up cache--------------------------------------------------------

#set up cache
cache.setup( cache.dir="c:/alr/models/cache", quiet=T )
cache.set( cache.dir="c:/alr/models/cache" )



## ----buffer file locations-----------------------------------

#this is the only input file needed, that is not accessed through a web service, or downloaded/cached from felek
buffer.file <- "C:/ALR/Models/conteStreamflow/data/concord_huc8"



## ----load gages----------------------------------------------
g.spatial <- gage.retrieve( buffer.file=buffer.file, max.da.sqkm=500, min.da.sqkm=0 )

# number of records from initial retrieval
nrow(g.spatial)

plot( readShapePoly( buffer.file ) )
plot( g.spatial, pch=16, col="blue", add=T )



## ----load flow-----------------------------------------------

#Load/calculate/aggregate flow data for seasonal and annual timesteps
q.matrices<-flow.retrieve( gages.spatial=g.spatial, flow.agg.function=flow.agg.function, 
                           flow.pre.agg.function=flow.pre.agg.function,
                           periods=c("daily","annual") )




## ------------------------------------------------------------------------
# Sample flow data
str(q.matrices)

# Single gage, all columns
q.matrices[["annual"]][106:126,4,]
# Single column, several gages
q.matrices[["annual"]][106:126,1:10,1]

# Single gage, all columns
q.matrices[["daily"]][45000:45020,4,]
# Single column, several gages
q.matrices[["daily"]][45000:45020,4:13,1]

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



