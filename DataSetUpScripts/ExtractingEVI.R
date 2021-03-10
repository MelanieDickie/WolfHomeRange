library(sf)
library(mapview)
library(tidyverse)
require(rgdal)
require(raster)
library(rgee)

# ##First create a polygon of my study area
# p1 <- rbind(c(-1579554.6,1659865.0), c(-130044.2,1659865.0), c(-130044.2,2620199.1), c(-1579554.6,2620199.1), c(-1579554.6,1659865.0))
# 
# pol <-st_polygon(list(p1))%>%
#   st_cast("POLYGON")%>%
#   st_sfc()%>%
#   st_as_sf(crs = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# #st_write(pol, here::here("data", "pol.shp"))


########10/01/2020
####Using rgee to acquire MOD13Q1 EVI data for a date range, mask pixels  
####that are covered in clouds and snow, as well as "water" from Hansen, 
####calculate median by season, and download the image as a TIF.

#initialize earth engine with drive = T so we can download our finished product
#will have to install the package googledrive if you haven't already
ee_Initialize(drive = T)

#set working directory
setwd("G:\\My Drive\\CMU\\WolfHomeRange")

##Read in spatial layer with your area of interest and turn it into an earth 
##engine object
roi_big <- st_read(here::here("data", "pol.shp")) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

##This function loads ALL of the existing MOD13Q1 data
modis_all <- ee$ImageCollection("MODIS/006/MOD13A1")

##Filter by the date  
Modis_filtered <- modis_all$
  filter(ee$Filter$date("2013-07-01", "2013-10-01"))

##get information about the dataset
ee_print(Modis_filtered)

##Mask and Visualize:
VizParams <- list(palette = c(
  'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
  '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
  '012E01', '011D01', '011301'
))

##Create a mask layer. We want all poor quality pixels (-1, 2, 3) to become 0s
getmask <- function(image) {
  mask_img_0 <- image$eq(0) #any pixel that does not equal 0 turns into 0. The 0s become 1s
  mask_img_1 <- image$eq(1) #any pixel that does not equal 1 turns into 0. The 1s stay 1s.
  return(mask_img1and2 <- mask_img_1$add(mask_img_0)) #add them together so all good quality pixels = 1
}

MOD13Q1_clean <- function(image) {
  #clip first to the roi - have to use clip bc filter bounds doesn't work for global composites
  #aka MODIS data
  clip_img <- image$clip(roi_big)
  #extract the NDVI band 
  ndvi_values <- clip_img$select("EVI") #add this to scale between -1 and 1 $multiply(0.0001)
  #extract the quality band
  ndvi_qa <- clip_img$select("SummaryQA")
  #select pixels to mask
  quality_mask <- getmask(ndvi_qa)
  #All quality pixels with value 0 (so poor quality pixels) become 0s
  ndvi_values$updateMask(quality_mask)
  ##I need to mask out water bodies.
  hansenImage <- ee$Image("UMD/hansen/global_forest_change_2015")
  datamask <- hansenImage$select('datamask')
  #Create a binary mask.
  Hansenmask<-datamask$eq(1)
  #Update the composite mask with the water mask.
  ndvi_values$updateMask(Hansenmask)
  
}

##Now map the function over the image collection 
NDVI_cleaned <- Modis_filtered$map(MOD13Q1_clean)

##Summarize the data into median EVI in summer and fall

##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2013-07-01", "2013-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

ee_as_raster(
  NDVI_compositeSummer,
  region = roi_big,
  dsn = file.path(my_dir, "Summer2013.tif"),
  via = "drive",
  crs = "SR-ORG:6974",
  scale = 231.6564
)

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2013-09-01", "2013-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())
ee_as_raster(
  NDVI_compositeFall,
  region = roi_big,
  dsn = file.path(my_dir, "Fall2013.tif"),
  via = "drive",
  crs = "SR-ORG:6974",
  scale = 231.6564
)


Delta2013<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2013, region = roi_big,
  dsn = file.path(my_dir, "Delta2013.tif"),
  via = "drive", crs = "SR-ORG:6974",scale = 231.6564)


##I'm sure there is a better way to loop through years, 
##but given I'm still learning
##I'm going to do this manually
#################### 2014 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2014-07-01", "2014-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2014-09-01", "2014-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2014<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2014, region = roi_big,
             dsn = file.path(my_dir, "Delta2014.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################### 2015 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2015-07-01", "2015-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2015-09-01", "2015-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2015<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2015, region = roi_big,
             dsn = file.path(my_dir, "Delta2015.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################### 2016 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2016-07-01", "2016-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2016-09-01", "2016-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2016<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2016, region = roi_big,
             dsn = file.path(my_dir, "Delta2016.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################### 2017 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2017-07-01", "2017-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2017-09-01", "2017-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2017<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2017, region = roi_big,
             dsn = file.path(my_dir, "Delta2017.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

ee_as_raster(Delta2017, region = roi_big,
             dsn = file.path(my_dir, "Delta2017.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################### 2018 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2018-07-01", "2018-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2018-09-01", "2018-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2018<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

raster2018<-ee_as_raster(Delta2018, region = roi_big,
                         dsn = file.path(my_dir, "Delta2018.tif"),
                         via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################### 2019 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2019-07-01", "2019-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2019-09-01", "2019-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2019<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

raster2019 <- ee_as_raster(Delta2019, region = roi_big,
              dsn = file.path(my_dir, "Delta2019.tif"),
              via = "drive", crs = "SR-ORG:6974",scale = 231.6564)


#################### 2020 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2020-07-01", "2020-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2020-09-01", "2020-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2020<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

raster2020<-ee_as_raster(Delta2020, region = roi_big,
             dsn = file.path(my_dir, "Delta2020.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)


#################### 2012 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2012-07-01", "2012-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2012-09-01", "2012-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2012<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2012, region = roi_big,
             dsn = file.path(my_dir, "Delta2012.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)


#################### 2011 ###########################
##SUMMER## 
#July 1 - August 1 
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2011-07-01", "2011-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

##FALL## 
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2011-09-01", "2011-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Delta2011<-NDVI_compositeSummer$subtract(NDVI_compositeFall)

ee_as_raster(Delta2011, region = roi_big,
             dsn = file.path(my_dir, "Delta2011.tif"),
             via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#################################################################################
###################### Extract to home ranges ###################################
#################################################################################

Delta2017R <- raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2017.tif")
crs(Delta2017R)

RICCHR <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBuffer.shp") 
##project home ranges into same prj as raster
RICCHRprj <- st_transform(RICCHR, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")


############################## 2013 ###########################################

raster2013<-raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2013.tif")

#Extract mean dEVI for each home range
RICCHRprj$EVI2013Mean <- raster::extract(raster2013, RICCHRprj, fun = mean, na.rm = TRUE)

############################## 2014 ###########################################
raster2014<-raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2014.tif")

#Extract mean dEVI for each home range
RICCHRprj$EVI2014Mean <- raster::extract(raster2014, RICCHRprj, fun = mean, na.rm = TRUE)

############################## 2015 ###########################################
raster2015<-raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2015.tif")

#Extract mean dEVI for each home range
RICCHRprj$EVI2015Mean <- raster::extract(raster2015, RICCHRprj, fun = mean, na.rm = TRUE)

############################## 2016 ###########################################
raster2016<-raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2016.tif")

#Extract mean dEVI for each home range
RICCHRprj$EVI2016Mean <- raster::extract(raster2016, RICCHRprj, fun = mean, na.rm = TRUE)

############################## 2017 ###########################################
raster2017<-raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2017.tif")

#Extract mean dEVI for each home range
RICCHRprj$EVI2017Mean <- raster::extract(raster2017, RICCHRprj, fun = mean, na.rm = TRUE)

RICCHRprjatt<-as.data.frame(RICCHRprj)
RICCHRprjatt$geometry<-NULL
#write.csv(RICCHRprjatt, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBuffer_Att.csv")

################# SK ###########################
SKHR <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBuffer.shp") 
SKHRprj <- st_transform(SKHR, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

SKHRprj$EVI2013Mean <- raster::extract(raster2013, SKHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2014Mean <- raster::extract(raster2014, SKHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2015Mean <- raster::extract(raster2015, SKHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2016Mean <- raster::extract(raster2016, SKHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2017Mean <- raster::extract(raster2017, SKHRprj, fun = mean, na.rm = TRUE)

SKHRprjatt<-as.data.frame(SKHRprj)
SKHRprjatt$geometry<-NULL
#write.csv(SKHRprjatt, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBuffer_Att.csv")

################# BC ###########################
BCHR <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBuffer.shp") 
BCHRprj <- st_transform(BCHR, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#Extract mean dEVI for each home range
BCHRprj$EVI2011Mean <- raster::extract(raster2011, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2012Mean <- raster::extract(raster2012, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2013Mean <- raster::extract(raster2013, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2014Mean <- raster::extract(raster2014, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2015Mean <- raster::extract(raster2015, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2016Mean <- raster::extract(raster2016, BCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2017Mean <- raster::extract(raster2017, BCHRprj, fun = mean, na.rm = TRUE)

BCHRprjatt<-as.data.frame(BCHRprj)
BCHRprjatt$geometry<-NULL
#write.csv(BCHRprjatt, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBuffer_Att.csv")

################# WHEC ###########################
WHECHR <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBuffer.shp") 
WHECHRprj <- st_transform(WHECHR, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

WHECHRprj$EVI2013Mean <- raster::extract(raster2013, WHECHRprj, fun = mean, na.rm = TRUE)
WHECHRprj$EVI2014Mean <- raster::extract(raster2014, WHECHRprj, fun = mean, na.rm = TRUE)
WHECHRprj$EVI2015Mean <- raster::extract(raster2015, WHECHRprj, fun = mean, na.rm = TRUE)
WHECHRprj$EVI2016Mean <- raster::extract(raster2016, WHECHRprj, fun = mean, na.rm = TRUE)
WHECHRprj$EVI2017Mean <- raster::extract(raster2017, WHECHRprj, fun = mean, na.rm = TRUE)

WHECHRprjatt<-as.data.frame(WHECHRprj)
WHECHRprjatt$geometry<-NULL
#write.csv(WHECHRprjatt, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBuffer_Att.csv")

############################## 2011 ###########################################
raster2011<-raster("G:/My Drive/CMU/WolfHomeRange/Data/Delta2011.tif")

WHECHRprj$EVI2011Mean <- raster::extract(raster2011, WHECHRprj, fun = mean, na.rm = TRUE)
RICCHRprj$EVI2011Mean <- raster::extract(raster2011, RICCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2011Mean <- raster::extract(raster2011, BCHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2011Mean <- raster::extract(raster2011, SKHRprj, fun = mean, na.rm = TRUE)

############################## 2012 ###########################################
raster2012<-raster("G:/My Drive/CMU/WolfHomeRange/Data/Delta2012.tif")

WHECHRprj$EVI2012Mean <- raster::extract(raster2012, WHECHRprj, fun = mean, na.rm = TRUE)
RICCHRprj$EVI2012Mean <- raster::extract(raster2012, RICCHRprj, fun = mean, na.rm = TRUE)
BCHRprj$EVI2012Mean <- raster::extract(raster2012, BCHRprj, fun = mean, na.rm = TRUE)
SKHRprj$EVI2012Mean <- raster::extract(raster2012, SKHRprj, fun = mean, na.rm = TRUE)

################################################# 
################## COMBINE ###################### 
#################################################

WHECHRprjatt<-as.data.frame(WHECHRprj)
WHECHRprjatt$geometry<-NULL
RICCHRprjatt<-as.data.frame(RICCHRprj)
RICCHRprjatt$geometry<-NULL
BCHRprjatt<-as.data.frame(BCHRprj)
BCHRprjatt$geometry<-NULL
SKHRprjatt<-as.data.frame(SKHRprj)
SKHRprjatt$geometry<-NULL
SKHRprjatt$BufArea<-NULL


WHECHRprjatt$Year<-str_sub(WHECHRprjatt$name,6,9)
BCHRprjatt$Year<-str_sub(BCHRprjatt$name,6,9)
BCHRprjatt$Year2<-str_sub(BCHRprjatt$name,9,12)
BCHRprjatt$Year[BCHRprjatt$Year == "nex2"] <- BCHRprjatt$Year2[BCHRprjatt$Year == "nex2"]
BCHRprjatt$Year2<-NULL
RICCHRprjatt$Year<-str_sub(RICCHRprjatt$name,6,9)
SKHRprjatt$Year<-str_sub(SKHRprjatt$name,7,10)

AllSAs<-rbind(WHECHRprjatt, RICCHRprjatt, SKHRprjatt, BCHRprjatt)

ALLSA2011<-subset(AllSAs, Year == 2011)
ALLSA2012<-subset(AllSAs, Year == 2012)
ALLSA2013<-subset(AllSAs, Year == 2013)
ALLSA2014<-subset(AllSAs, Year == 2014)
ALLSA2015<-subset(AllSAs, Year == 2015)
ALLSA2016<-subset(AllSAs, Year == 2016)
ALLSA2017<-subset(AllSAs, Year == 2017)

ALLSA2011<-dplyr::select(ALLSA2011, name, SA, est, Year, ExtraTerr, EVI2011Mean)
ALLSA2012<-dplyr::select(ALLSA2012, name, SA, est, Year, ExtraTerr, EVI2012Mean)
ALLSA2013<-dplyr::select(ALLSA2013, name, SA, est, Year, ExtraTerr, EVI2013Mean)
ALLSA2014<-dplyr::select(ALLSA2014, name, SA, est, Year, ExtraTerr, EVI2014Mean)
ALLSA2015<-dplyr::select(ALLSA2015, name, SA, est, Year, ExtraTerr, EVI2015Mean)
ALLSA2016<-dplyr::select(ALLSA2016, name, SA, est, Year, ExtraTerr, EVI2016Mean)
ALLSA2017<-dplyr::select(ALLSA2017, name, SA, est, Year, ExtraTerr, EVI2017Mean)


ALLSA2011<-ALLSA2011 %>% rename(EVIMean=EVI2011Mean)
ALLSA2012<-ALLSA2012 %>% rename(EVIMean=EVI2012Mean)
ALLSA2013<-ALLSA2013 %>% rename(EVIMean=EVI2013Mean)
ALLSA2014<-ALLSA2014 %>% rename(EVIMean=EVI2014Mean)
ALLSA2015<-ALLSA2015 %>% rename(EVIMean=EVI2015Mean)
ALLSA2016<-ALLSA2016 %>% rename(EVIMean=EVI2016Mean)
ALLSA2017<-ALLSA2017 %>% rename(EVIMean=EVI2017Mean)

AllSAs<-rbind(ALLSA2011, ALLSA2012, ALLSA2013, ALLSA2014, ALLSA2015, ALLSA2016, ALLSA2017)

##write.csv(AllSAs, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\AllSAs.csv")

require(ggplot2)
gp <- ggplot(data=AllSAs, aes(x = EVIMean, y =log(est), shape=SA, colour=SA))
gp <- gp + geom_point(size=2)
gp <- gp + theme_set(theme_bw())
gp <- gp + geom_smooth(method = "lm", alpha = .15, aes(fill = SA))
gp <- gp + labs(x="dEVI", y = "Log Seasonal home range area (km2)")
gp <- gp + theme(axis.text.x = element_text(size=16, vjust=0.6), axis.title = element_text(size=18))
gp <- gp + theme(axis.text.y = element_text(size=16), axis.title = element_text(size=18))
gp <- gp + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16))
gp <- gp + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#gp <- gp + facet_wrap(~StudyArea)
print(gp)

# #st_write(WHECHRprj, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHEC_EVI.shp")
# #st_write(RICCHRprj, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICC_EVI.shp")
# #st_write(BCHRprj, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BC_EVI.shp")
# #st_write(SKHRprj, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SK_EVI.shp")
# 
# all_SKestBuff$EVIMean <- subset(AllSAs, SA ="SK")$EVIMean[match(all_SKestBuff$name, subset(AllSAs, SA ="SK")$name)]
# all_BCestBuff$EVIMean <- subset(AllSAs, SA ="BC")$EVIMean[match(all_BCestBuff$name, subset(AllSAs, SA ="BC")$name)]
# all_RICCestBuff$EVIMean <- subset(AllSAs, SA ="RICC")$EVIMean[match(all_RICCestBuff$name, subset(AllSAs, SA ="RICC")$name)]
# all_WHECestBuff$EVIMean <- subset(AllSAs, SA ="WHEC")$EVIMean[match(all_WHECestBuff$name, subset(AllSAs, SA ="WHEC")$name)]
# 
# #st_write(all_SKestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBufferEVI.shp")
# #st_write(all_BCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBufferEVI.shp")
# #st_write(all_RICCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBufferEVI.shp")
# #st_write(all_WHECestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBufferEVI.shp")
