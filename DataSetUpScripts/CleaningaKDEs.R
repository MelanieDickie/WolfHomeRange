library(sf)
library(mapview)
require(stringr)

##Shapefiles were created in CreatingaKDEs.R and saved to folders
#List all shapefiles
file_list <- list.files(here::here("data", "WHECIndSeason\\Poly"), pattern = "*shp", full.names = TRUE)
#read all shapefiles, store as a list
shapefile_list <- lapply(file_list, read_sf)
#append
all_WHEC <- do.call(rbind, shapefile_list)
#mapview(all_WHEC)
##remove CIs
all_WHEC$Esttype<-str_sub(all_WHEC$name,-3,-1)
all_WHECest<-subset(all_WHEC, Esttype == "est")
mapview(all_WHECest)


#List all shapefiles
file_list <- list.files(here::here("data", "RICCIndSeason\\Poly"), pattern = "*shp", full.names = TRUE)
#read all shapefiles, store as a list
shapefile_list <- lapply(file_list, read_sf)
#append
all_RICC <- do.call(rbind, shapefile_list)
#mapview(all_WHEC)
##remove CIs
all_RICC$Esttype<-str_sub(all_RICC$name,-3,-1)
all_RICCest<-subset(all_RICC, Esttype == "est")
mapview(all_RICCest)

#List all shapefiles
file_list <- list.files(here::here("data", "BCIndSeason\\Poly"), pattern = "*shp", full.names = TRUE)
#read all shapefiles, store as a list
shapefile_list <- lapply(file_list, read_sf)
#append
all_BC <- do.call(rbind, shapefile_list)
#mapview(all_WHEC)
##remove CIs
all_BC$Esttype<-str_sub(all_BC$name,-3,-1)
all_BCest<-subset(all_BC, Esttype == "est")
mapview(all_BCest)

#List all shapefiles
file_list <- list.files(here::here("data", "SKIndSeason\\Poly"), pattern = "*shp", full.names = TRUE)
#read all shapefiles, store as a list
shapefile_list <- lapply(file_list, read_sf)
#append
all_SK <- do.call(rbind, shapefile_list)
#mapview(all_WHEC)
##remove CIs
all_SK$Esttype<-str_sub(all_SK$name,-3,-1)
all_SKest<-subset(all_SK, Esttype == "est")
mapview(all_SKest)

all_SKest$SA<-"SK"
all_WHECest$SA<-"WHEC"
all_RICCest$SA<-"RICC"
all_BCest$SA<-"BC"

##Identify and remove extra-territorial movements - IOU are not home ranges
##Bring in metadata (which is saved "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\metadata.csv" from CreatingaKDEs.R)
metadata<-read.csv(here::here("data", "metadata.csv"))
head(metadata)
metadata$metricshort<-str_sub(metadata$metric,0,4)
metadata2<-subset(metadata, metricshort == "area")
metadata2<-metadata2[, c("ID", "type", "est")]

all_SKest$ID<-str_sub(all_SKest$name,0,-9)
all_SKest[which(all_SKest$Esttype=='igh'), ]$ID<-str_sub(all_SKest[which(all_SKest$Esttype=='igh'), ]$name,0,-10)
all_SKest<-merge(all_SKest, metadata2, by="ID")
mapview(all_SKest, zcol="type")

all_BCest$ID<-str_sub(all_BCest$name,0,-9)
all_BCest[which(all_BCest$Esttype=='igh'), ]$ID<-str_sub(all_BCest[which(all_BCest$Esttype=='igh'), ]$name,0,-10)
all_BCest<-merge(all_BCest, metadata2, by="ID")
mapview(all_BCest, zcol="type")

all_RICCest$ID<-str_sub(all_RICCest$name,0,-9)
all_RICCest[which(all_RICCest$Esttype=='igh'), ]$ID<-str_sub(all_RICCest[which(all_RICCest$Esttype=='igh'), ]$name,0,-10)
all_RICCest<-merge(all_RICCest, metadata2, by="ID")
mapview(all_RICCest, zcol="type")

all_WHECest$ID<-str_sub(all_WHECest$name,0,-9)
all_WHECest[which(all_WHECest$Esttype=='igh'), ]$ID<-str_sub(all_WHECest[which(all_WHECest$Esttype=='igh'), ]$name,0,-10)
all_WHECest<-merge(all_WHECest, metadata2, by="ID")
mapview(all_WHECest, zcol="type")

##Bring in visual assessment of extra-territorial based on GPS data
ExtraterrHRs<-read.csv(here::here("data", "AllBasic.csv"))
ExtraterrHRs$ID<-paste(ExtraterrHRs$AnimalID,ExtraterrHRs$Year, ExtraterrHRs$Season, sep="")
ExtraterrHRs<-select(ExtraterrHRs, ID, ExtraTerr)
ExtraterrHRs<-unique(ExtraterrHRs)
ExtraterrHRs2<-subset(ExtraterrHRs, KDEClass == "% ML")
ExtraterrHRsSK<-subset(ExtraterrHRs2, StudyArea == "SK")
ExtraterrHRsBC<-subset(ExtraterrHRs2, StudyArea == "BC")
ExtraterrHRsRICC<-subset(ExtraterrHRs2, StudyArea == "RICC")
ExtraterrHRsWHEC<-subset(ExtraterrHRs2, StudyArea == "WHEC")

all_SKest$ExtraTerr <- ExtraterrHRsSK$ExtraTerr[match(all_SKest$ID, ExtraterrHRsSK$ID)]
mapview(subset(all_SKest, ExtraTerr ==0), zcol="est")

all_RICCest$ExtraTerr <- ExtraterrHRsRICC$ExtraTerr[match(all_RICCest$ID, ExtraterrHRsRICC$ID)]
#all_RICCest<-merge(all_RICCest, ExtraterrHRs, by="ID", all.y=FALSE)
mapview(subset(all_RICCest, ExtraTerr ==0), zcol="est")

all_WHECest$ExtraTerr <- ExtraterrHRsWHEC$ExtraTerr[match(all_WHECest$ID, ExtraterrHRsWHEC$ID)]
#all_WHECest<-merge(all_WHECest, ExtraterrHRs, by="ID", all.y=FALSE)
mapview(subset(all_WHECest, ExtraTerr ==0), zcol="est")

all_BCest$ExtraTerr <- ExtraterrHRsBC$ExtraTerr[match(all_BCest$ID, ExtraterrHRsBC$ID)]
#all_BCest<-merge(all_BCest, ExtraterrHRs, by="ID", all.y=FALSE)
mapview(subset(all_BCest, ExtraTerr ==0), zcol="est")

st_write(subset(all_SKest, ExtraTerr ==0), here::here("data", "SKEstNoET.shp"))
st_write(subset(all_BCest, ExtraTerr ==0), here::here("data", "BCEstNoET.shp"))
st_write(subset(all_RICCest, ExtraTerr ==0), here::here("data", "RICCEstNoET.shp"))
st_write(subset(all_WHECest, ExtraTerr ==0), here::here("data", "WHECEstNoET.shp"))

st_write(all_SKest, here::here("data", "SKEst.shp"))
st_write(all_BCest, here::here("data", "BCEst.shp"))
st_write(all_RICCest, here::here("data", "RICCEst.shp"))
st_write(all_WHECest, here::here("data", "WHECEst.shp"))
