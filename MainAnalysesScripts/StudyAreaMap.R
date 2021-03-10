library(sf)
library(here)
library(readxl)
library(psych)
library(raster)
library(stringr)
library(lwgeom)
library(rworldmap)
library(rnaturalearth)
library(rmapshaper)
library(ggspatial)
library(ggsflabel)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(rgee)


#Bring in polygons from CleaningaDKEs.R

#all_SKest<-st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEst.shp")
#all_BCest<-st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEst.shp")
#all_RICCest<-st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEst.shp")
#all_WHECest<-st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEst.shp")

##Bring in attributes from Attributing_NewaKDEs.R
#all_estBuff<-"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\All_estBuffEVILFDAtt.csv")

all_SKest$LFD<-all_estBuff$LFD[match(all_SKest$name, all_estBuff$name)]
all_BCest$LFD<-all_estBuff$LFD[match(all_BCest$name, all_estBuff$name)]
all_RICCest$LFD<-all_estBuff$LFD[match(all_RICCest$name, all_estBuff$name)]
all_WHECest$LFD<-all_estBuff$LFD[match(all_WHECest$name, all_estBuff$name)]

all_SKest$EVIMean<-all_estBuff$EVIMean[match(all_SKest$name, all_estBuff$name)]
all_BCest$EVIMean<-all_estBuff$EVIMean[match(all_BCest$name, all_estBuff$name)]
all_RICCest$EVIMean<-all_estBuff$EVIMean[match(all_RICCest$name, all_estBuff$name)]
all_WHECest$EVIMean<-all_estBuff$EVIMean[match(all_WHECest$name, all_estBuff$name)]

all_SKest$ExtraTerr<-all_estBuff$ExtraTerr[match(all_SKest$name, all_estBuff$name)]
all_BCest$ExtraTerr<-all_estBuff$ExtraTerr[match(all_BCest$name, all_estBuff$name)]
all_RICCest$ExtraTerr<-all_estBuff$ExtraTerr[match(all_RICCest$name, all_estBuff$name)]
all_WHECest$ExtraTerr<-all_estBuff$ExtraTerr[match(all_WHECest$name, all_estBuff$name)]


ee_Initialize(drive = T)

#set working directory
setwd("G:\\My Drive\\CMU\\WolfHomeRange")

##Read in spatial layer with your area of interest and turn it into an earth 
##engine object
roi_big <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\pol.shp")
roi_bigplot<-st_buffer(roi_big, 1000000)
st_write(roi_bigplot, "G:\\My Drive\\CMU\\WolfHomeRange\\roi_bigplot3.shp")
roi_bigplot <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\roi_bigplot3.shp") %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

##This function loads ALL of the existing MOD13Q1 data
modis_all <- ee$ImageCollection("MODIS/006/MOD13A1")

##Filter by the date  
Modis_filtered <- modis_all$
  filter(ee$Filter$date("2013-07-01", "2013-10-01"))

##Create a mask layer. We want all poor quality pixels (-1, 2, 3) to become 0s
getmask <- function(image) {
  
  mask_img_0 <- image$eq(0) #any pixel that does not equal 0 turns into 0. The 0s become 1s
  mask_img_1 <- image$eq(1) #any pixel that does not equal 1 turns into 0. The 1s stay 1s.
  
  return(mask_img1and2 <- mask_img_1$add(mask_img_0)) #add them together so all good quality pixels = 1
  
}

MOD13Q1_clean <- function(image) {
  #clip first to the roi - have to use clip bc filter bounds doesn't work for global composites
  #aka MODIS data
  clip_img <- image$clip(roi_bigplot)
  
  #might as well do other things within this function. 
  #extract the NDVI band 
  ndvi_values <- clip_img$select("EVI") #add this to scale between -1 and 1 $multiply(0.0001)
  
  #extract the quality band
  ndvi_qa <- clip_img$select("SummaryQA")
  
  #select pixels to mask
  quality_mask <- getmask(ndvi_qa)
  
  #All quality pixels with value 0 (so poor quality pixels) become 0s
  ndvi_values$updateMask(quality_mask)
  
  ##I need to mask out water bodies.
  #hansenImage = ee.Image('UMD/hansen/global_forest_change_2015')
  hansenImage <- ee$Image("UMD/hansen/global_forest_change_2015")
  
  #// Select the land/water mask.
  #var datamask = hansenImage.select('datamask');
  datamask <- hansenImage$select('datamask')
  
  #// Create a binary mask.
  #var mask = datamask.eq(1);
  #We use eq(1) to create a binary image in which all the pixels 
  #that do not have the value of 1 in the datamask band 
  #(those that are water or no data) get a value of 0 in the resulting image
  Hansenmask<-datamask$eq(1)
  
  #Update the composite mask with the water mask.
  ndvi_values$updateMask(Hansenmask)
  
}

##Now map the function over the image collection so that each image is processed.
NDVI_cleaned <- Modis_filtered$map(MOD13Q1_clean)

##Summarize the data into median EVI in summer and fall
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
raster2020<-ee_as_raster(Delta2020, region = roi_bigplot,
                         dsn = file.path(my_dir, "Delta2020plot.tif"),
                         via = "drive", crs = "SR-ORG:6974",scale = 500)

Delta2020plot <- raster("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\Delta2020plot.tif")
Delta2020plot<-projectRaster(Delta2020plot, crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", res=5000)
Delta2020plot_df  <- as.data.frame(Delta2020plot, xy=TRUE)
Delta2020plot_df<-Delta2020plot_df[!is.na(Delta2020plot_df$Delta2020plot),]
Delta2020plot_df<-subset(Delta2020plot_df, Delta2020plot >=0)#Remove the negative values that didn't get masked out

us <- ne_countries(country="United States of America", scale='medium',returnclass = 'sf')%>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
can <- ne_countries(country="Canada", scale='medium',returnclass = 'sf')%>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

bor<-read_sf("G:/My Drive/CMU/WolfHomeRange/Data/Boreal_Caribou_Range_Boundaries_May2012.shp")%>%
  st_make_valid()%>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

bor<-rmapshaper::ms_simplify(input = as(bor, 'Spatial'))
bor <- read_sf("G:/My Drive/CMU/WolfHomeRange/Data/Ecozones.shp")%>%
  #filter(ZONE_NAME%in%c("Taiga Plain", "Boreal PLain", "Boreal Shield", "Taiga Shield", "Boreal Cordillera"))%>%
  st_as_sf()%>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")%>%
  st_cast()

provs<-read_sf("G:/My Drive/CMU/WolfHomeRange/Data/Provs.shp")%>%
  st_make_valid()%>%
  st_transform("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

world <- getMap(resolution = "high")
world <- st_as_sf(world)

##prep extents
extent = matrix(c(-1.8E6 ,1.2E6 , -1.8E6 , 2.8E6 , 0.1E6  , 2.8E6, 0.1E6 ,1.2E6 , -1.8E6 ,1.2E6 ),ncol=2, byrow=TRUE)

pts = list(extent)
pl1 = st_polygon(pts)
pl1 <- st_sfc(pl1, crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")%>%
  st_transform("+proj=laea +lat_0=40 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# inset<-ggplot() +
#   geom_sf(data = pl1, fill=NA, color="red", size = 0.5) +
#   geom_sf(data=bor%>%st_transform("+proj=laea +lat_0=52 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "), aes(fill=ZONE_NAME), col=NA, alpha=0.6)+
#   geom_sf_label(data=bor%>%st_transform("+proj=laea +lat_0=52 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "), aes(label = ZONE_),fill="transparent", label.size=0)+
#   scale_fill_brewer(palette="Dark2")+
#   labs(fill="Ecozone")+
#   coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs", clip="on")+
#   theme(panel.grid.major = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         panel.border = element_rect(fill = NA, color = NA),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         plot.margin=unit(c(0,0,0,0),"mm"),
#         legend.position = "none",
#         plot.background = element_rect(fill = "transparent",colour = NA))

inset<-ggplot() +
  geom_sf(data=provs%>%st_transform("+proj=laea +lat_0=52 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))+
  #geom_sf_label(data=provs%>%st_transform("+proj=laea +lat_0=52 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))+
  geom_sf(data = pl1, fill=NA, color="red", size = 0.5) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs", clip="on")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent",colour = NA))



breaks <- c(0, 2500, 5000)

PlotA<-ggplot() +
  geom_raster(data = Delta2020plot_df, aes(x = x, y = y, fill=Delta2020plot))+
  scale_fill_gradient2(breaks=breaks, n.breaks=10, low="brown4", mid="lightgrey", high="darkgreen", guide=FALSE)+
  #scale_fill_gradient2(breaks=breaks, n.breaks=10, low="brown4", mid="lightgrey", high="darkgreen", guide = guide_colourbar(direction = "horizontal"), name = "Δ EVI")+
  #geom_sf(data = lines, size=0.5)+
  geom_sf(data = us, fill =NA, size=1) +
  geom_sf(data = can, fill =NA, size=1) +
  geom_sf(data = provs, fill = NA, size=1)+
  geom_sf(data=subset(all_SKest, ExtraTerr==0), aes(colour=LFD), alpha=1/3, size=1)+
  geom_sf(data=subset(all_BCest, ExtraTerr ==0), aes(colour=LFD), alpha=1/3, size=1)+
  geom_sf(data=subset(all_RICCest, ExtraTerr==0), aes(colour=LFD), alpha=1/3, size=1)+
  geom_sf(data=subset(all_WHECest, ExtraTerr==0), aes(colour=LFD), alpha=1/3, size=1)+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, breaks=c(0, 0.3,0.5), guide = guide_colourbar(direction = "horizontal"), name = expression(paste("Linear Feature Density (km/", km^2, ")")))+
  #coord_sf(xlim =c(-1.8E6,0.1E6), ylim = c(1.2E6, 2.8E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  coord_sf(xlim =c(-1.7E6,-0.15E6), ylim = c(1.6E6, 2.7E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "lightgrey"), 
        panel.border = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "bottom",
        legend.background = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.25)

##Second Panel:
#Load linear features
#lines = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\LFs\\EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp", quiet=TRUE)
#lines = lines %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

PlotB<-ggplot() +
  geom_sf(data = lines, size=0.5, colour="grey")+
  geom_sf(data = us, fill =NA, size=1) +
  geom_sf(data = can, fill =NA, size=1) +
  geom_sf(data = provs, fill = NA, size=1)+
  # geom_sf(data=subset(all_SKest, ExtraTerr==0), aes(fill=EVIMean, colour=EVIMean), alpha=1/2, size=1)+
  # geom_sf(data=subset(all_BCest, ExtraTerr ==0), aes(fill=EVIMean,colour=EVIMean), alpha=1/2, size=1)+
  # geom_sf(data=subset(all_RICCest, ExtraTerr==0), aes(fill=EVIMean,colour=EVIMean), alpha=1/2, size=1)+
  # geom_sf(data=subset(all_WHECest, ExtraTerr==0), aes(fill=EVIMean,colour=EVIMean), alpha=1/2, size=1)+  
  geom_sf(data=subset(all_SKest, ExtraTerr==0), aes(colour=EVIMean), alpha=1/2, size=1)+
  geom_sf(data=subset(all_BCest, ExtraTerr ==0), aes(colour=EVIMean), alpha=1/2, size=1)+
  geom_sf(data=subset(all_RICCest, ExtraTerr==0), aes(colour=EVIMean), alpha=1/2, size=1)+
  geom_sf(data=subset(all_WHECest, ExtraTerr==0), aes(colour=EVIMean), alpha=1/2, size=1)+
  scale_colour_gradient(low = "brown4", high = "darkgreen", na.value = NA, breaks=c(0, 1000,2000), guide = guide_colourbar(direction = "horizontal"), name = "Δ EVI")+
  scale_fill_gradient2(breaks=breaks, n.breaks=10, low="brown4", mid="lightgrey", high="darkgreen", guide=FALSE)+
  coord_sf(xlim =c(-1.7E6,-0.15E6), ylim = c(1.6E6, 2.7E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        #legend.position = c(0.65,0.075),
        legend.position = "bottom",
        legend.background = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.25)+
  annotation_custom(ggplotGrob(inset), xmin =-0.55E6, xmax = -0.15E6, ymin = 2.2E6, ymax = 2.88E6)

plot_grid(PlotA, PlotB, ncol = 2 , nrow = 1)


mid <- median(Delta2020plot_df$Delta2020plot)
inset2<-ggplot() +
  geom_raster(data = Delta2020plot_df, aes(x = x, y = y, fill=Delta2020plot))+
  scale_fill_gradient2(n.breaks=5, low="brown4", high="darkgreen", name = "Δ EVI")+
  geom_sf(data=provs%>%st_transform("+proj=laea +lat_0=52 +lon_0=-100 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"), fill = "transparent", size=0.75)+
  #geom_sf(data = pl1, fill=NA, color="red", size = 0.5) +
  coord_sf(xlim =c(-2E6, 0.2E6), ylim = c(1.0E6, 2.9E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(colour = "black", size=1, fill="white"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))
inset2

PlotC<-ggplot() +
  #geom_raster(data = Delta2020plot_df, aes(x = x, y = y, fill=Delta2020plot))+
  #scale_fill_gradient2(breaks=breaks, n.breaks=10, low="brown4", high="darkgreen", guide=FALSE)+
  geom_sf(data = lines, size=0.5, colour="grey")+
  geom_sf(data = us, fill =NA, size=1) +
  geom_sf(data = can, fill =NA, size=1) +
  geom_sf(data = provs, fill = NA, size=1)+
  geom_sf(data=subset(all_SKest, ExtraTerr==0), aes(colour=LFD), alpha=1/2, size=1)+
  geom_sf(data=subset(all_BCest, ExtraTerr ==0), aes(colour=LFD), alpha=1/2, size=1)+
  geom_sf(data=subset(all_RICCest, ExtraTerr==0), aes(colour=LFD), alpha=1/2, size=1)+
  geom_sf(data=subset(all_WHECest, ExtraTerr==0), aes(colour=LFD), alpha=1/2, size=1)+
  scale_colour_gradient(low = "yellow3", high = "red3", na.value = NA, breaks=c(0, 0.3,0.5), guide = guide_colourbar(direction = "horizontal"), name = expression(paste("Linear Feature Density (km/", km^2, ")")))+
  scale_fill_gradient(low = "yellow3", high = "red3", na.value = NA, breaks=c(0, 0.3,0.5), guide = guide_colourbar(direction = "horizontal"), name = expression(paste("Linear Feature Density (km/", km^2, ")")))+
  coord_sf(xlim =c(-1.7E6,-0.15E6), ylim = c(1.6E6, 2.7E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "bottom",
        legend.background = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.25)+
  annotation_custom(ggplotGrob(inset2), xmin =-0.8E6, xmax = -0.14E6, ymin = 2.0E6, ymax = 3.1E6)
PlotC
