library(sf)
library(mapview)
library(tidyverse)
library(rgee)
library(sf)

################################################################
################### Extract and Save dEVI ######################
################################################################

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
roi_big <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\pol.shp") %>% 
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
  ##mask out water bodies.
  hansenImage <- ee$Image("UMD/hansen/global_forest_change_2015")
  #Select the land/water mask.
  datamask <- hansenImage$select('datamask')
  #Create a binary mask.
  #var mask = datamask.eq(1);
  #We use eq(1) to create a binary image in which all the pixels 
  #that do not have the value of 1 in the datamask band 
  #(those that are water or no data) get a value of 0 in the resulting image
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

##FALL## 
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

#################################################################
#################### EXTRACT TO WMUs ############################
#################################################################

##Bring in home ranges
ABWMUs2 <- st_read(here::here("data", "AB_WMU-OSR-Intersect.shp"))

WMUs <-
  ABWMUs2 %>%
  summarise(area = sum(kmsquare))

##project  into same prj as raster>
crs(ABWMUs2)
ABWMUs2prj <- st_transform(ABWMUs2, "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
crs(ABWMUs2prj)

############################## 2018 ###########################################
# ##Save as raster to do intersection
# raster2018<-ee_as_raster(Delta2018, region = roi_big,
#                          dsn = file.path(my_dir, "Delta2018.tif"),
#                          via = "drive", crs = "SR-ORG:6974",scale = 231.6564)

#Extract mean dEVI for each home range
ABWMUs2prj$EVI2018Mean <- raster::extract(raster2018, ABWMUs2prj, fun = mean, na.rm = TRUE)

############################## 2019 ###########################################
# ##Save as raster to do intersection
# raster2019<-ee_as_raster(Delta2019, region = roi_big,
#                          dsn = file.path(my_dir, "Delta2019.tif"),
#                          via = "drive", crs = "SR-ORG:6974",scale = 231.6564)
# #Extract mean dEVI for each home range
ABWMUs2prj$EVI2019Mean <- raster::extract(raster2019, ABWMUs2prj, fun = mean, na.rm = TRUE)

############################## 2020 ###########################################
# ##Save as raster to do intersection
# raster2020<-ee_as_raster(Delta2020, region = roi_big,
#                          dsn = file.path(my_dir, "Delta2020.tif"),
#                          via = "drive", crs = "SR-ORG:6974",scale = 231.6564)
#Extract mean dEVI for each home range

ABWMUs2prj$EVI2014Mean <- raster::extract(raster2014, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2015Mean <- raster::extract(raster2015, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2016Mean <- raster::extract(raster2016, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2017Mean <- raster::extract(raster2017, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2018Mean <- raster::extract(raster2018, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2019Mean <- raster::extract(raster2019, ABWMUs2prj, fun = mean, na.rm = TRUE)
ABWMUs2prj$EVI2020Mean <- raster::extract(raster2020, ABWMUs2prj, fun = mean, na.rm = TRUE)

##Merge into Moose density estimates:
OSRDensities<-read.csv(here::here("data","OSRMooseEstimates_2014-2020.csv"))

ABWMUs2prjatt<-as.data.frame(ABWMUs2prj)
ABWMUs2prjatt$WMUNIT_COD<-substr(ABWMUs2prjatt$WMUNIT_COD, 3,6)
ABWMUs2prjatt<-merge(ABWMUs2prjatt,OSRDensities, by= "WMUNIT_COD") 

ABWMUs2prjatt2014<-subset(ABWMUs2prjatt, year == 2014)
ABWMUs2prjatt2015<-subset(ABWMUs2prjatt, year == 2015)
ABWMUs2prjatt2016<-subset(ABWMUs2prjatt, year == 2016)
ABWMUs2prjatt2017<-subset(ABWMUs2prjatt, year == 2017)
ABWMUs2prjatt2018<-subset(ABWMUs2prjatt, year == 2018)
ABWMUs2prjatt2019<-subset(ABWMUs2prjatt, year == 2019)
ABWMUs2prjatt2020<-subset(ABWMUs2prjatt, year == 2020)

ABWMUs2prjatt2014<-dplyr::select(ABWMUs2prjatt2014, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2014Mean)
ABWMUs2prjatt2015<-dplyr::select(ABWMUs2prjatt2015, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2015Mean)
ABWMUs2prjatt2016<-dplyr::select(ABWMUs2prjatt2016, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2016Mean)
ABWMUs2prjatt2017<-dplyr::select(ABWMUs2prjatt2017, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2017Mean)
ABWMUs2prjatt2018<-dplyr::select(ABWMUs2prjatt2018, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2018Mean)
ABWMUs2prjatt2019<-dplyr::select(ABWMUs2prjatt2019, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2019Mean)
ABWMUs2prjatt2020<-dplyr::select(ABWMUs2prjatt2020, WMUNIT_COD, WMUNIT_NAM, year, method, region, density_avg, EVI2020Mean)

ABWMUs2prjatt2014<-ABWMUs2prjatt2014 %>% rename(EVIMean=EVI2014Mean)
ABWMUs2prjatt2015<-ABWMUs2prjatt2015 %>% rename(EVIMean=EVI2015Mean)
ABWMUs2prjatt2016<-ABWMUs2prjatt2016 %>% rename(EVIMean=EVI2016Mean)
ABWMUs2prjatt2017<-ABWMUs2prjatt2017 %>% rename(EVIMean=EVI2017Mean)
ABWMUs2prjatt2018<-ABWMUs2prjatt2018 %>% rename(EVIMean=EVI2018Mean)
ABWMUs2prjatt2019<-ABWMUs2prjatt2019 %>% rename(EVIMean=EVI2019Mean)
ABWMUs2prjatt2020<-ABWMUs2prjatt2020 %>% rename(EVIMean=EVI2020Mean)

ABWMUs2prjatt<-rbind(ABWMUs2prjatt2014, ABWMUs2prjatt2015, ABWMUs2prjatt2016, ABWMUs2prjatt2017, ABWMUs2prjatt2018, ABWMUs2prjatt2019, ABWMUs2prjatt2020)
write.csv(ABWMUs2prjatt, here::here("data", "MooseDneistyEVI.csv"))
