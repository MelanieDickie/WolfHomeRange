require(sf)

#Bring in polygons from CleaningaDKEs.R

all_SKest<-st_read(here::here("data", "SKEst.shp"))
all_BCest<-st_read(here::here("data", "BCEst.shp"))
all_RICCest<-st_read(here::here("data", "RICCEst.shp"))
all_WHECest<-st_read(here::here("data", "WHECEst.shp"))

##Calculate average radius of non-ExtraTerritorial HRs
all_estnonspatial<-rbind(as.data.frame(all_SKest), as.data.frame(all_BCest), as.data.frame(all_RICCest), as.data.frame(all_WHECest))
summary(all_estnonspatial$est)
bufrad<-1000*sqrt(mean(subset(all_estnonspatial, ExtraTerr == 0)$est)/pi)##Converts km to m

##Claculate centroid and buffer by average radius
all_SKestBuff = st_buffer(st_centroid(all_SKest), bufrad)
all_RICCestBuff = st_buffer(st_centroid(all_RICCest), bufrad)
all_WHECestBuff = st_buffer(st_centroid(all_WHECest), bufrad)
all_BCestBuff = st_buffer(st_centroid(all_BCest), bufrad)

st_write(all_SKestBuff, here::here("data", "SKEstBuffer.shp"))
st_write(all_BCestBuff, here::here("data", "BCEstBuffer.shp"))
st_write(all_RICCestBuff, here::here("data", "RICCEstBuffer.shp"))
st_write(all_WHECestBuff, here::here("data", "WHECEstBuffer.shp"))
