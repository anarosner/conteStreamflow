---
output:
  html_document:
    css: C:/ALR/Models/custom.css
    theme: null
---

# conteStreamflow Vignette: data prep
## Data prep for Massachusetts are monthly, seasonal, and annual data

### load packages

```r
library(conteStreamflow)
```

### Set up cache and set local dir

```r
#set up cache
cache.setup( cache.dir="c:/alr/models/cache", quiet=T )
cache.set( cache.dir="c:/alr/models/cache" )

#local drive, for saving flow, weather, gage, and merged results
local.dir <- "c:/alr/models/conteStreamflow/vignettes/temp"
setwd(local.dir)
```

### Locate buffer file

```r
#this is the only input file needed, that is not accessed through a web service, or downloaded/cached from felek
buffer.file <- "C:/ALR/Models/conteStreamflow/data/mass_buffer_poly"
```

### Load gages using a buffer (polygon outline of area of interest)

```r
g.spatial <- gage.retrieve( buffer.file=buffer.file, max.da.sqkm=50, min.da.sqkm=0 )
```

```
## Warning: NWIS gage data missing geographic coordinates:
##  3 sites do not have lat coordinates and are being ignored
##  3 sites do not have long coordinates and are being ignored
##  01095503, 01098500, 01102500
```

```
## Gage retrieval complete 
## 485 gages identified(Drainage area > 0  and <= 50  square km)
```

```r
# number of records from initial retrieval
nrow(g.spatial)
```

```
## [1] 198
```

### Place gages into NHDplus catchments to pair w/ stream reaches and basin characteristics

```r
g.spatial <- gage.place.nhdplus( g.spatial )
```

```
## Matching gages to catchments...completed matching 8 gages out of 198  ...completed matching 26 gages out of 198  ...completed matching 104 gages out of 198  ...completed matching 137 gages out of 198  ...completed matching 138 gages out of 198  ...completed matching 193 gages out of 198  ...completed matching 198 gages out of 198  
## Completed plotting gages to catchments
```

```r
#all gages plotted to an nhdplus catchment
sum(is.na(g.spatial$FEATUREID))
```

```
## [1] 0
```

### Load impoundments

```r
#Load impoundment info
g.spatial<-impound.retrieve( gages.spatial=g.spatial, cols=char.columns.default( impound=T ) )


#Check if gages are missing impoundment data 
apply(g.spatial@data, MARGIN=2, FUN=function(x) sum(is.na(x)))
```

```
##            FEATUREID            agency_cd              site_no 
##                    0                    0                    0 
##           station_nm               lat_va              long_va 
##                    0                    0                    0 
##         coord_acy_cd           dec_lat_va          dec_long_va 
##                    0                    0                    0 
##       coord_acy_cd.1       coord_datum_cd   dec_coord_datum_cd 
##                    0                    0                    0 
##               huc_cd        drain_area_va        sv_begin_date 
##                    0                    0                   80 
##          sv_end_date          sv_count_nu              da_sqkm 
##                   80                    0                    0 
##         TNC_DamCount   OnChannelWaterSqKM OnChannelWetlandSqKM 
##                    0                    0                    0
```

### Remove gages missing impoundment data, and filter out gages on impounded streams

```r
#Remove gages with dams or large on channel (open water) impoundments
#(and check #records before and after)
# Number of columns BEFORE gages w/ impoundments are removed
nrow(g.spatial)
```

```
## [1] 198
```

```r
g.spatial<-g.spatial[g.spatial$TNC_DamCount==0,]
g.spatial<-g.spatial[g.spatial$OnChannelWaterSqKM<.5,]
# Number of columns AFTER gages w/ impoundments are removed
nrow(g.spatial)
```

```
## [1] 60
```


### Load basin characteristics

```r
#Let's use the default basin char columns
# Basin characteristic columns being used
print(char.columns.default()) 
```

```
##  [1] "FEATUREID"         "ReachLengthKM"     "Forest"           
##  [4] "Herbacious"        "Agriculture"       "Developed"        
##  [7] "DevelopedNotOpen"  "Impervious"        "CONUSOpenWater"   
## [10] "CONUSWetland"      "DrainageClass"     "HydrologicGroupAB"
## [13] "SurficialCoarseC"  "PercentSandy"      "ReachElevationM"  
## [16] "BasinElevationM"   "ReachSlopePCNT"    "BasinSlopePCNT"   
## [19] "NHDplusTotDASqKM"
```

```r
#and pick a subset of those to log transform
log.cols <- c("Forest", "Herbacious","Agriculture","Developed","DevelopedNotOpen", "Impervious",  
               "CONUSOpenWater","CONUSWetland", "DrainageClass","HydrologicGroupAB","SurficialCoarseC","PercentSandy",
              "ReachElevationM", "BasinElevationM")

#Load basin characteristics
g.spatial<-char.retrieve( gages.spatial=g.spatial, cols=char.columns.default(), log.cols=log.cols )
```

### Remove gages/basins missing data

```r
#See how many gages are missing each of the basin char
apply(g.spatial@data, MARGIN=2, FUN=function(x) sum(is.na(x)))
```

```
##             FEATUREID             agency_cd               site_no 
##                     0                     0                     0 
##            station_nm                lat_va               long_va 
##                     0                     0                     0 
##          coord_acy_cd            dec_lat_va           dec_long_va 
##                     0                     0                     0 
##        coord_acy_cd.1        coord_datum_cd    dec_coord_datum_cd 
##                     0                     0                     0 
##                huc_cd         drain_area_va         sv_begin_date 
##                     0                     0                    20 
##           sv_end_date           sv_count_nu               da_sqkm 
##                    20                     0                     0 
##          TNC_DamCount    OnChannelWaterSqKM  OnChannelWetlandSqKM 
##                     0                     0                     0 
##         ReachLengthKM                Forest            Herbacious 
##                     0                     0                     0 
##           Agriculture             Developed      DevelopedNotOpen 
##                     0                     0                     0 
##            Impervious        CONUSOpenWater          CONUSWetland 
##                     0                     0                     0 
##         DrainageClass     HydrologicGroupAB      SurficialCoarseC 
##                     0                     0                     0 
##          PercentSandy       ReachElevationM       BasinElevationM 
##                     0                     1                     0 
##        ReachSlopePCNT        BasinSlopePCNT      NHDplusTotDASqKM 
##                     1                     0                     0 
##            log.Forest        log.Herbacious       log.Agriculture 
##                     0                     0                     0 
##         log.Developed  log.DevelopedNotOpen        log.Impervious 
##                     0                     0                     0 
##    log.CONUSOpenWater      log.CONUSWetland     log.DrainageClass 
##                     0                     0                     0 
## log.HydrologicGroupAB  log.SurficialCoarseC      log.PercentSandy 
##                     0                     0                     0 
##   log.ReachElevationM   log.BasinElevationM 
##                     0                     0
```

```r
# One is missing ReachSlopePCNT.  Which gages?
g.spatial[is.na(g.spatial$ReachSlopePCNT),"station_nm"]
```

```
##         coordinates                                    station_nm
## 307 (-70.56, 41.58) BACKUS RIVER, AT RT. 28, NEAR E. FALMOUTH, MA
```

```r
#Limit to gages that have all basin data
#(eliminate one missing ReachSlopePCNT, and recheck)
g.spatial<-g.spatial[!is.na(g.spatial$ReachSlopePCNT),]
apply(g.spatial@data, MARGIN=2, FUN=function(x) sum(is.na(x)))
```

```
##             FEATUREID             agency_cd               site_no 
##                     0                     0                     0 
##            station_nm                lat_va               long_va 
##                     0                     0                     0 
##          coord_acy_cd            dec_lat_va           dec_long_va 
##                     0                     0                     0 
##        coord_acy_cd.1        coord_datum_cd    dec_coord_datum_cd 
##                     0                     0                     0 
##                huc_cd         drain_area_va         sv_begin_date 
##                     0                     0                    20 
##           sv_end_date           sv_count_nu               da_sqkm 
##                    20                     0                     0 
##          TNC_DamCount    OnChannelWaterSqKM  OnChannelWetlandSqKM 
##                     0                     0                     0 
##         ReachLengthKM                Forest            Herbacious 
##                     0                     0                     0 
##           Agriculture             Developed      DevelopedNotOpen 
##                     0                     0                     0 
##            Impervious        CONUSOpenWater          CONUSWetland 
##                     0                     0                     0 
##         DrainageClass     HydrologicGroupAB      SurficialCoarseC 
##                     0                     0                     0 
##          PercentSandy       ReachElevationM       BasinElevationM 
##                     0                     0                     0 
##        ReachSlopePCNT        BasinSlopePCNT      NHDplusTotDASqKM 
##                     0                     0                     0 
##            log.Forest        log.Herbacious       log.Agriculture 
##                     0                     0                     0 
##         log.Developed  log.DevelopedNotOpen        log.Impervious 
##                     0                     0                     0 
##    log.CONUSOpenWater      log.CONUSWetland     log.DrainageClass 
##                     0                     0                     0 
## log.HydrologicGroupAB  log.SurficialCoarseC      log.PercentSandy 
##                     0                     0                     0 
##   log.ReachElevationM   log.BasinElevationM 
##                     0                     0
```

```r
# Total number of gages after removing those w/ missing basin char data
nrow(g.spatial)
```

```
## [1] 59
```

```r
#View sample of data
# Sample of gage data
head(g.spatial@data)
```

```
##     FEATUREID agency_cd    site_no
## 161   5848382      USGS   01072850
## 164   5845058      USGS   01073000
## 171   5845268      USGS   01073785
## 172   5845576      USGS   01073810
## 220   6078383      USGS   01095434
## 224   6075275      USGS 0109650657
##                                             station_nm lat_va long_va
## 161             MOHAWK BROOK NEAR CENTER STRAFFORD, NH 431547  710550
## 164                       OYSTER RIVER NEAR DURHAM, NH 430855  705756
## 171     WINNICUT RIVER AT GREENLAND, NR PORTSMOUTH, NH 430209  705051
## 172 BERRYS BROOK AT SAGAMORE ROAD, NEAR PORTSMOUTH, NH 430210  704459
## 220                 GATES BROOK NEAR WEST BOYLSTON, MA 422152  714633
## 224                      LYLE REED BROOK AT NASHUA, NH 424324  713155
##     coord_acy_cd dec_lat_va dec_long_va coord_acy_cd.1 coord_datum_cd
## 161            S      43.26      -71.10              S          NAD27
## 164            S      43.15      -70.97              S          NAD27
## 171            S      43.04      -70.85              S          NAD83
## 172            S      43.04      -70.75              S          NAD27
## 220            S      42.36      -71.78              S          NAD27
## 224            S      42.72      -71.53              S          NAD27
##     dec_coord_datum_cd   huc_cd drain_area_va sv_begin_date sv_end_date
## 161              NAD83 01060003          7.34    2000-06-05  2000-09-18
## 164              NAD83 01060003         12.10    1936-03-19  2014-08-14
## 171              NAD83 01060003         14.10    1999-08-26  2014-09-18
## 172              NAD83 01060003          5.38    2003-05-22  2004-10-05
## 220              NAD83 01070004          3.13    1994-03-30  2014-08-01
## 224              NAD83 01070002          2.14          <NA>        <NA>
##     sv_count_nu da_sqkm TNC_DamCount OnChannelWaterSqKM
## 161           4  19.011            0           0.387960
## 164         316  31.339            0           0.445174
## 171         166  36.519            0           0.087852
## 172          21  13.934            0           0.023641
## 220         105   8.107            0           0.008301
## 224           0   5.543            0           0.025976
##     OnChannelWetlandSqKM ReachLengthKM Forest Herbacious Agriculture
## 161               0.9192        25.267  85.85      4.123       4.393
## 164               2.3047        32.524  73.53      5.739       5.886
## 171               5.8884        30.528  54.21      8.156      11.460
## 172               3.0684         9.809  66.04      6.684       2.056
## 220               0.3869         3.618  33.89      1.580       5.894
## 224               0.3780         3.229  38.78      4.654      10.004
##     Developed DevelopedNotOpen Impervious CONUSOpenWater CONUSWetland
## 161     4.972            1.102      0.581         1.6731        7.227
## 164    11.823            3.647      2.162         1.7065       11.029
## 171    26.009           11.925      5.893         1.0011       23.368
## 172    24.676           14.575      7.945         0.6249       39.887
## 220    58.099           44.841     23.232         0.1099        6.952
## 224    45.520           39.430     18.203         2.0879       16.877
##     DrainageClass HydrologicGroupAB SurficialCoarseC PercentSandy
## 161         3.130             31.25           3.1493       0.0000
## 164         3.347             49.55           7.8422       0.2672
## 171         4.204             39.24           0.8487       0.8487
## 172         4.619             54.44           4.6669       4.6669
## 220         3.142             58.84          26.6560      26.2877
## 224         2.486             86.28          41.1809       5.5246
##     ReachElevationM BasinElevationM ReachSlopePCNT BasinSlopePCNT
## 161         143.910          177.13         2.1672          9.291
## 164          48.704           58.71         0.8770          4.996
## 171          13.417           21.74         0.3951          3.787
## 172           7.786           14.89         0.2148          2.334
## 220         155.095          199.48         1.9181          6.205
## 224          53.225           60.82         0.3467          5.103
##     NHDplusTotDASqKM log.Forest log.Herbacious log.Agriculture
## 161           24.694      4.453         1.4166          1.4799
## 164           32.043      4.298         1.7473          1.7725
## 171           37.322      3.993         2.0987          2.4388
## 172           14.822      4.190         1.8997          0.7207
## 220            8.203      3.523         0.4573          1.7739
## 224            5.154      3.658         1.5378          2.3030
##     log.Developed log.DevelopedNotOpen log.Impervious log.CONUSOpenWater
## 161         1.604              0.09674        -0.5431           0.514679
## 164         2.470              1.29404         0.7709           0.534428
## 171         3.258              2.47865         1.7738           0.001149
## 172         3.206              2.67930         2.0726          -0.470239
## 220         4.062              3.80313         3.1455          -2.208146
## 224         3.818              3.67453         2.9016           0.736140
##     log.CONUSWetland log.DrainageClass log.HydrologicGroupAB
## 161            1.978            1.1409                 3.442
## 164            2.401            1.2081                 3.903
## 171            3.151            1.4360                 3.670
## 172            3.686            1.5302                 3.997
## 220            1.939            1.1449                 4.075
## 224            2.826            0.9108                 4.458
##     log.SurficialCoarseC log.PercentSandy log.ReachElevationM
## 161               1.1472         -13.8155               4.969
## 164               2.0595          -1.3199               3.886
## 171              -0.1641          -0.1641               2.596
## 172               1.5405           1.5405               2.052
## 220               3.2830           3.2691               5.044
## 224               3.7180           1.7092               3.975
##     log.BasinElevationM
## 161               5.177
## 164               4.073
## 171               3.079
## 172               2.701
## 220               5.296
## 224               4.108
```

```r
class(g.spatial)
```

```
## [1] "SpatialPointsDataFrame"
## attr(,"package")
## [1] "sp"
```

```r
str(g.spatial@data)  #structure of data slot, of sp object
```

```
## 'data.frame':	59 obs. of  53 variables:
##  $ FEATUREID            : int  5848382 5845058 5845268 5845576 6078383 6075275 6744556 6772189 6772189 6772977 ...
##  $ agency_cd            : Factor w/ 1 level "USGS": 1 1 1 1 1 1 1 1 1 1 ...
##  $ site_no              : chr  "01072850" "01073000" "01073785" "01073810" ...
##  $ station_nm           : Factor w/ 1376 levels " EAST BRANCH NAUGATUCK RIVER AT TORRINGTON, CONN",..: 698 867 1363 83 425 621 1235 119 118 276 ...
##  $ lat_va               : num  431547 430855 430209 430210 422152 ...
##  $ long_va              : num  710550 705756 705051 704459 714633 ...
##  $ coord_acy_cd         : Factor w/ 6 levels "1","5","F","H",..: 5 5 5 5 5 5 5 6 6 6 ...
##  $ dec_lat_va           : num  43.3 43.1 43 43 42.4 ...
##  $ dec_long_va          : num  -71.1 -71 -70.8 -70.7 -71.8 ...
##  $ coord_acy_cd.1       : Factor w/ 6 levels "1","5","F","H",..: 5 5 5 5 5 5 5 6 6 6 ...
##  $ coord_datum_cd       : Factor w/ 3 levels "NAD27","NAD83",..: 1 1 2 1 1 1 1 1 1 1 ...
##  $ dec_coord_datum_cd   : Factor w/ 2 levels "","NAD83": 2 2 2 2 2 2 2 2 2 2 ...
##  $ huc_cd               : chr  "01060003" "01060003" "01060003" "01060003" ...
##  $ drain_area_va        : num  7.34 12.1 14.1 5.38 3.13 2.14 0.54 1.32 1.6 3.44 ...
##  $ sv_begin_date        : Date, format: "2000-06-05" "1936-03-19" ...
##  $ sv_end_date          : Date, format: "2000-09-18" "2014-08-14" ...
##  $ sv_count_nu          : int  4 316 166 21 105 0 29 0 0 0 ...
##  $ da_sqkm              : num  19.01 31.34 36.52 13.93 8.11 ...
##  $ TNC_DamCount         : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ OnChannelWaterSqKM   : num  0.388 0.4452 0.0879 0.0236 0.0083 ...
##  $ OnChannelWetlandSqKM : num  0.919 2.305 5.888 3.068 0.387 ...
##  $ ReachLengthKM        : num  25.27 32.52 30.53 9.81 3.62 ...
##  $ Forest               : num  85.9 73.5 54.2 66 33.9 ...
##  $ Herbacious           : num  4.12 5.74 8.16 6.68 1.58 ...
##  $ Agriculture          : num  4.39 5.89 11.46 2.06 5.89 ...
##  $ Developed            : num  4.97 11.82 26.01 24.68 58.1 ...
##  $ DevelopedNotOpen     : num  1.1 3.65 11.93 14.57 44.84 ...
##  $ Impervious           : num  0.581 2.162 5.893 7.945 23.232 ...
##  $ CONUSOpenWater       : num  1.673 1.706 1.001 0.625 0.11 ...
##  $ CONUSWetland         : num  7.23 11.03 23.37 39.89 6.95 ...
##  $ DrainageClass        : num  3.13 3.35 4.2 4.62 3.14 ...
##  $ HydrologicGroupAB    : num  31.2 49.5 39.2 54.4 58.8 ...
##  $ SurficialCoarseC     : num  3.149 7.842 0.849 4.667 26.656 ...
##  $ PercentSandy         : num  0 0.267 0.849 4.667 26.288 ...
##  $ ReachElevationM      : num  143.91 48.7 13.42 7.79 155.1 ...
##  $ BasinElevationM      : num  177.1 58.7 21.7 14.9 199.5 ...
##  $ ReachSlopePCNT       : num  2.167 0.877 0.395 0.215 1.918 ...
##  $ BasinSlopePCNT       : num  9.29 5 3.79 2.33 6.21 ...
##  $ NHDplusTotDASqKM     : num  24.7 32 37.3 14.8 8.2 ...
##  $ log.Forest           : num  4.45 4.3 3.99 4.19 3.52 ...
##  $ log.Herbacious       : num  1.417 1.747 2.099 1.9 0.457 ...
##  $ log.Agriculture      : num  1.48 1.773 2.439 0.721 1.774 ...
##  $ log.Developed        : num  1.6 2.47 3.26 3.21 4.06 ...
##  $ log.DevelopedNotOpen : num  0.0967 1.294 2.4787 2.6793 3.8031 ...
##  $ log.Impervious       : num  -0.543 0.771 1.774 2.073 3.146 ...
##  $ log.CONUSOpenWater   : num  0.51468 0.53443 0.00115 -0.47024 -2.20815 ...
##  $ log.CONUSWetland     : num  1.98 2.4 3.15 3.69 1.94 ...
##  $ log.DrainageClass    : num  1.14 1.21 1.44 1.53 1.14 ...
##  $ log.HydrologicGroupAB: num  3.44 3.9 3.67 4 4.07 ...
##  $ log.SurficialCoarseC : num  1.147 2.06 -0.164 1.54 3.283 ...
##  $ log.PercentSandy     : num  -13.816 -1.32 -0.164 1.54 3.269 ...
##  $ log.ReachElevationM  : num  4.97 3.89 2.6 2.05 5.04 ...
##  $ log.BasinElevationM  : num  5.18 4.07 3.08 2.7 5.3 ...
```

### Look at mismatch of drainage area b/w NWIS gage info and NHDplus catchment

```r
#since basin char are based on approximate mapping of gages to stream reaches using nhdplus catchments,
# some will be off/ will be mapped to the wrong stream sections or wrong stream/river
# look at mismatch between gage drainage area  and nhdplus drainage area

plot(g.spatial$da_sqkm,g.spatial$NHDplusTotDASqKM, main="Drainage areas of NWIS vs assigned NHDplus streams")
text(g.spatial$da_sqkm+1,g.spatial$NHDplusTotDASqKM-1,g.spatial$site_no,cex=.5)
abline( a=0, b=1, lty=2, col="red" )
abline( h=50, lty=2, col="black" )
```

<img src="C:/Users/arosner/AppData/Local/Temp/1/RtmpqO9jqJ/preview-140c7f503d9d.dir/data_prep_MA_files/figure-html/da mismatch.png" title="plot of chunk da mismatch" alt="plot of chunk da mismatch" width="672" />

```r
# Will decide later what to do about this.  
# Not many problems in MA (after all, we have already removed dammed streams), but there are more problems elsewhere
```



### Place gages into weather grid










