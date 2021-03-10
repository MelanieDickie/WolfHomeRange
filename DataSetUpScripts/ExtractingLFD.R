require(sf)
library(dplyr)

#all_SKestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBufferEVI.shp")
#all_BCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBufferEVI.shp")
#all_RICCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBufferEVI.shp")
#all_WHECestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBufferEVI.shp")
#From ExtractingEVI_NewaKDEs.R

#Load linear features
lines = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\LFs\\EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp", quiet=TRUE)
lines = lines %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

##SK ##
SKpolys = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBufferEVI.shp", quiet=TRUE)
SKpolys = SKpolys %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
SKints = st_intersection(lines, SKpolys)
SKLFD<-as.data.frame(tapply(st_length(SKints), SKints$name,sum))/1000/(pi*(50^2))

##RICC ##
RICCpolys = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBufferEVI.shp", quiet=TRUE)
RICCpolys = RICCpolys %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
RICCints = st_intersection(lines, RICCpolys)
RICCLFD<-as.data.frame(tapply(st_length(RICCints), RICCints$name,sum))/1000/(pi*(50^2))

## WHEC ##
WHECpolys = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBufferEVI.shp", quiet=TRUE)
WHECpolys = WHECpolys %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
WHECints = st_intersection(lines, WHECpolys)
WHECLFD<-as.data.frame(tapply(st_length(WHECints), WHECints$name,sum))/1000/(pi*(50^2))

## BC ##
BCpolys = st_read("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBufferEVI.shp", quiet=TRUE)
BCpolys = BCpolys %>% st_transform("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
BCints = st_intersection(lines, BCpolys)
mapview(list(BCpolys, BCints))
BCLFD<-as.data.frame(tapply(st_length(BCints), BCints$name,sum))/1000/(pi*(50^2))

########### Combine Together ###############
colnames(BCLFD)<- c("LFD")
BCLFD$name<-rownames(BCLFD)
BCLFD$StudyArea<-"BC"

colnames(RICCLFD)<- c("LFD")
RICCLFD$name<-rownames(RICCLFD)
RICCLFD$StudyArea<-"RICC"

colnames(WHECLFD)<- c("LFD")
WHECLFD$name<-rownames(WHECLFD)
WHECLFD$StudyArea<-"WHEC"

colnames(SKLFD)<- c("LFD")
SKLFD$name<-rownames(SKLFD)
SKLFD$StudyArea<-"SK"

ALLLFD<-rbind(SKLFD, WHECLFD, RICCLFD, BCLFD)

############# Combine with main data ###################
all_SKestBuff$LFD <- subset(ALLLFD, StudyArea ="SK")$LFD[match(all_SKestBuff$name, subset(ALLLFD, StudyArea ="SK")$name)]
all_BCestBuff$LFD <- subset(ALLLFD, StudyArea ="BC")$LFD[match(all_BCestBuff$name, subset(ALLLFD, StudyArea ="BC")$name)]
all_RICCestBuff$LFD <- subset(ALLLFD, StudyArea ="RICC")$LFD[match(all_RICCestBuff$name, subset(ALLLFD, StudyArea ="RICC")$name)]
all_WHECestBuff$LFD <- subset(ALLLFD, StudyArea ="WHEC")$LFD[match(all_WHECestBuff$name, subset(ALLLFD, StudyArea ="WHEC")$name)]

#st_write(all_SKestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKEstBufferEVILFD.shp")
#st_write(all_BCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCEstBufferEVILFD.shp")
#st_write(all_RICCestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCEstBufferEVILFD.shp")
#st_write(all_WHECestBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECEstBufferEVILFD.shp")

all_estBuff<-rbind(as.data.frame(all_SKestBuff), as.data.frame(all_BCestBuff), as.data.frame(all_RICCestBuff), as.data.frame(all_WHECestBuff))
all_estBuff$geometry<-NULL
all_estBuff$LFD[is.na(all_estBuff$LFD)] <- 0

#write.csv(all_estBuff, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\All_estBuffEVILFD.csv")

require(ggplot2)
gp <- ggplot(data=all_estBuff, aes(x = LFD, y =log(est), shape=SA, colour=SA))
gp <- gp + geom_point(size=2)
gp <- gp + theme_set(theme_bw())
gp <- gp + geom_smooth(method = "lm", alpha = .15, aes(fill = SA))
gp <- gp + labs(x="LFD", y = "Log Seasonal home range area (km2)")
gp <- gp + theme(axis.text.x = element_text(size=16, vjust=0.6), axis.title = element_text(size=18))
gp <- gp + theme(axis.text.y = element_text(size=16), axis.title = element_text(size=18))
gp <- gp + theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16))
gp <- gp + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#gp <- gp + facet_wrap(~StudyArea)
print(gp)