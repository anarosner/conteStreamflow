boo<- q.matrices[["annual"]][,"01142500",]
boo2<- q.matrices[["monthly"]][,"01142500",]

View(boo2)
dim(boo2)

View(boo2[1000:1512,])

g.test <- gage.retrieve( sites="01142500" )

q.test<-flow.retrieve( gages.spatial=g.test, 
                       periods=c("daily","monthly","annual") )
#      flow.agg.function=flow.agg.function, 
#      flow.pre.agg.function=flow.pre.agg.function,

str(q.test)
row<- which(!is.na( q.test[["daily"]][,1,1] ))
daily <- q.test[["daily"]][min(row):max(row),1,]

tail( daily )
daily[year(as.Date(row.names(daily)))==2011,]
daily[year(as.Date(row.names(daily)))==1998,]

x <- q.matrices[[period]][,site_no,]
x <- x[min(row):max(row),]

k <- "01142500"
k2 <- "01077400"
y <- importDVs(staid=k, code = "00061", stat = "00003")
y <- importDVs(staid=k, code = "00060", stat = "00001")
y <- importDVs(staid=k, code = "00060", stat = "00002")

install.packages("dataRetrieval")


a <- readNWISdv(siteNumber=k, parameterCd="00060",
           statCd = "00003")
str(a)

b <- readNWISdv(siteNumber=k, parameterCd="00060",
                statCd = "00001")
str(b)

c <- readNWISdv(siteNumber=k, parameterCd="00061")
str(c)

d <- readNWISpeak( siteNumbers=k, startDate = "1900-01-01", endDate = "2014-12-31" )
e <- readNWISpeak( siteNumbers=k2 )
f <- readNWISpeak( siteNumbers=c(k,k2) )
str(d)
str(e)
str(f)
f <- f[order(f$)]
View(f[f$site_no==k,])
View(f[f$site_no==k,])

x <- whatNWISdata(siteNumbers = k2)
View(x[order(x$data_type_cd),])

g <- importDVs(staid=k2, code = "00061")
h <- readNWISdv(k, parameterCd = "00060")
h
readNWISpeak(siteNumbers = k)
readNWISdv


url <- constructNWISURL(k, NA, "1900-01-01", "2014-12-31", "peak")
data <- importRDB1(url, asDateTime = FALSE)
data <- data[!is.na(data$peak_va),]
data$peak_dt <- as.Date(data$peak_dt)
attributes(data)


siteInfo <- readNWISsite(k)
attr(data, "siteInfo") <- siteInfo
attr(data, "variableInfo") <- NULL
attr(data, "statisticInfo") <- NULL
return(data)


