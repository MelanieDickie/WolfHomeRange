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
library(mapview)

#G:\My Drive\CMU\WolfHomeRange\Data\AB_Moose_Densities
MooseSurveys <- st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\MooseDensity_OSR\\AB_WMU-OSR-Intersect.shp") 
OSRDensities<-read.csv("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\MooseDensity_OSR\\OSRMooseEstimates_2014-2020.csv")

MooseSurveys$WMUNIT_COD<-substr(MooseSurveys$WMUNIT_COD, 3,6)

MooseSurveysdens<-merge(MooseSurveys,OSRDensities, by= "WMUNIT_COD", all.x=TRUE) 
mapview(MooseSurveys)

ggplot() +
  geom_raster(data = Delta2020plot_df, aes(x = x, y = y, fill=Delta2020plot))+
  scale_fill_gradient2(breaks=breaks, n.breaks=10, low="brown4", mid="lightgrey", high="darkgreen", guide=guide_colourbar(direction = "horizontal"), name = "Δ EVI")+
  geom_sf(data = MooseSurveysdens, fill=NA, aes(colour=density_avg), size=1.5)+
  geom_sf(data = us, fill =NA, size=1) +
  geom_sf(data = can, fill =NA, size=1) +
  geom_sf(data = provs, fill = NA, size=1)+
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA, breaks=c(0, 0.3,0.5), guide = guide_colourbar(direction = "horizontal"), name = expression(paste("Moose Density (/", km^2, ")")))+
  coord_sf(xlim =c(-1.7E6,-0.5E6), ylim = c(1.1E6, 2.5E6), crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.position = "bottom",
        legend.background = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.25)
