##Attribute with animal, pack, pack size, sex, and monitoring period
all_estBuff<-read.csv(here::here("data", "All_estBuffEVILFD.csv"))

#PackID is in PackIDLookUp.csv
#Sex is in All_Metadata_Sex.csv
#PS is in MinimumPackSizes.csv


###################################################################################################################
################################################ MONITORING PERIOD ################################################
###################################################################################################################

##Need to create MonitoringPeriod again from the GPS data
##Same files as CreatingaKDEs.R:
RICCtest<-read.csv(here::here("data", "RICC3Hour.csv"))
BCtest<-read.csv(here::here("data", "BC3Hour.csv"))
WHECtest<-read.csv(here::here("data", "HEC3Hour.csv"))
SKtest<-read.csv(here::here("data", "SK3Hour.csv"))

require(lubridate)
require(data.table)
require(dplyr)

################### SK ########################
##Need an IDSeason
#Format the date column so I can pull out the Month and year
SKtest2<-as.data.frame(SKtest)
SKtest2$Month<-month(SKtest2$t_)
SKtest2$Year<-year(SKtest2$t_)

##Define seasons
SKtest2$SeasonYr[SKtest2$Month >= 5 & SKtest2$Month <= 10 & SKtest2$Year == 2012] <- "2012SnowFree"
SKtest2$SeasonYr[SKtest2$Month >= 5 & SKtest2$Month <= 10 & SKtest2$Year == 2013] <- "2013SnowFree"
SKtest2$SeasonYr[SKtest2$Month >= 5 & SKtest2$Month <= 10 & SKtest2$Year == 2014] <- "2014SnowFree"
SKtest2$SeasonYr[SKtest2$Month >= 5 & SKtest2$Month <= 10 & SKtest2$Year == 2015] <- "2015SnowFree"
SKtest2$SeasonYr[SKtest2$Month >= 5 & SKtest2$Month <= 10 & SKtest2$Year == 2016] <- "2016SnowFree"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2012 | SKtest2$Month >= 11 & SKtest2$Year == 2011] <- "2011Snow"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2013 | SKtest2$Month >= 11 & SKtest2$Year == 2012] <- "2012Snow"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2014 | SKtest2$Month >= 11 & SKtest2$Year == 2013] <- "2013Snow"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2015 | SKtest2$Month >= 11 & SKtest2$Year == 2014] <- "2014Snow"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2016 | SKtest2$Month >= 11 & SKtest2$Year == 2015] <- "2015Snow"
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2017 | SKtest2$Month >= 11 & SKtest2$Year == 2016] <- "2016Snow"
SKtest2$SeasonYr<-as.factor(SKtest2$SeasonYr)
summary(SKtest2$SeasonYr)

##combine season and ID to  get IDSeason
SKtest2$Season<-as.character(SKtest2$Season)
SKtest2$Season[SKtest2$Month >= 5 & SKtest2$Month <= 10] <- "SnowFree"
SKtest2$Season[SKtest2$Month <= 4 | SKtest2$Month >= 11] <- "Snow"
SKtest2$Season<-as.factor(SKtest2$Season)

SKtest2<-as.data.frame(SKtest2)
SKtest2$IndSeason <- do.call(paste, c(SKtest2[c("id", "Season")], sep = ""))
SKtest2$IndSeason <- as.factor(SKtest2$IndSeason)

SKtest2$IndSeasonYear <- do.call(paste, c(SKtest2[c("id", "SeasonYr")], sep = ""))
SKtest2$IndSeasonYear <- as.factor(SKtest2$IndSeasonYear)

##For each IndSeasonYear, calculate min and max t_
SKtest2$t_Num<-as.Date(SKtest2$t_)

IndSeasonYearList<-unique(SKtest2$IndSeasonYear)
SKAnimalMetadata<-data.frame()
for(i in IndSeasonYearList){
  subdat<-subset(SKtest2, IndSeasonYear == i)
  TimeDif<-max(subdat$t_Num) - min(subdat$t_Num)
  meta.sub<-data.frame(IndSeasonYear=i)
  meta.sub$AnimalId<-unique(subdat$id)
  meta.sub$Year<-min(unique(subdat$Year))
  meta.sub$Season<-unique(subdat$Season)
  meta.sub$TimeDif<-as.numeric(TimeDif)
  SKAnimalMetadata<-rbind(SKAnimalMetadata,meta.sub)
}

################### BC ########################
##Need an IDSeason
#Format the date column so I can pull out the Month and year
BCtest2<-as.data.frame(BCtest)
BCtest2$Month<-month(BCtest2$t_)
BCtest2$Year<-year(BCtest2$t_)

##Define seasons
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2012] <- "2012SnowFree"
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2013] <- "2013SnowFree"
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2014] <- "2014SnowFree"
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2015] <- "2015SnowFree"
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2016] <- "2016SnowFree"
BCtest2$SeasonYr[BCtest2$Month >= 5 & BCtest2$Month <= 10 & BCtest2$Year == 2017] <- "2017SnowFree"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2012 | BCtest2$Month >= 11 & BCtest2$Year == 2011] <- "2011Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2013 | BCtest2$Month >= 11 & BCtest2$Year == 2012] <- "2012Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2014 | BCtest2$Month >= 11 & BCtest2$Year == 2013] <- "2013Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2015 | BCtest2$Month >= 11 & BCtest2$Year == 2014] <- "2014Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2016 | BCtest2$Month >= 11 & BCtest2$Year == 2015] <- "2015Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2017 | BCtest2$Month >= 11 & BCtest2$Year == 2016] <- "2016Snow"
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2018 | BCtest2$Month >= 11 & BCtest2$Year == 2017] <- "2017Snow"
BCtest2$SeasonYr<-as.factor(BCtest2$SeasonYr)
summary(BCtest2$SeasonYr)

##combine season and ID to  get IDSeason
BCtest2$Season<-as.character(BCtest2$Season)
BCtest2$Season[BCtest2$Month >= 5 & BCtest2$Month <= 10] <- "SnowFree"
BCtest2$Season[BCtest2$Month <= 4 | BCtest2$Month >= 11] <- "Snow"
BCtest2$Season<-as.factor(BCtest2$Season)

BCtest2<-as.data.frame(BCtest2)
BCtest2$IndSeason <- do.call(paste, c(BCtest2[c("id", "Season")], sep = ""))
BCtest2$IndSeason <- as.factor(BCtest2$IndSeason)

BCtest2$IndSeasonYear <- do.call(paste, c(BCtest2[c("id", "SeasonYr")], sep = ""))
BCtest2$IndSeasonYear <- as.factor(BCtest2$IndSeasonYear)

##For each IndSeasonYear, calculate min and max t_
BCtest2$t_Num<-as.Date(BCtest2$t_)

IndSeasonYearList<-unique(BCtest2$IndSeasonYear)
BCAnimalMetadata<-data.frame()
for(i in IndSeasonYearList){
  subdat<-subset(BCtest2, IndSeasonYear == i)
  TimeDif<-max(subdat$t_Num) - min(subdat$t_Num)
  meta.sub<-data.frame(IndSeasonYear=i)
  meta.sub$Year<-min(unique(subdat$Year))
  meta.sub$AnimalId<-unique(subdat$id)
  meta.sub$Season<-unique(subdat$Season)
  meta.sub$TimeDif<-as.numeric(TimeDif)
  BCAnimalMetadata<-rbind(BCAnimalMetadata,meta.sub)
}

################### RICC ########################
##Need an IDSeason
#Format the date column so I can pull out the Month and year
RICCtest2<-as.data.frame(RICCtest)
RICCtest2$Month<-month(RICCtest2$t_)
RICCtest2$Year<-year(RICCtest2$t_)

##Define seasons
RICCtest2$SeasonYr[RICCtest2$Month >= 5 & RICCtest2$Month <= 10 & RICCtest2$Year == 2012] <- "2012SnowFree"
RICCtest2$SeasonYr[RICCtest2$Month >= 5 & RICCtest2$Month <= 10 & RICCtest2$Year == 2013] <- "2013SnowFree"
RICCtest2$SeasonYr[RICCtest2$Month >= 5 & RICCtest2$Month <= 10 & RICCtest2$Year == 2014] <- "2014SnowFree"
RICCtest2$SeasonYr[RICCtest2$Month >= 5 & RICCtest2$Month <= 10 & RICCtest2$Year == 2015] <- "2015SnowFree"
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2012 | RICCtest2$Month >= 11 & RICCtest2$Year == 2011] <- "2011Snow"
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2013 | RICCtest2$Month >= 11 & RICCtest2$Year == 2012] <- "2012Snow"
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2014 | RICCtest2$Month >= 11 & RICCtest2$Year == 2013] <- "2013Snow"
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2015 | RICCtest2$Month >= 11 & RICCtest2$Year == 2014] <- "2014Snow"
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2016 | RICCtest2$Month >= 11 & RICCtest2$Year == 2015] <- "2015Snow"
RICCtest2$SeasonYr<-as.factor(RICCtest2$SeasonYr)
summary(RICCtest2$SeasonYr)

##combine season and ID to  get IDSeason
RICCtest2$Season<-as.character(RICCtest2$Season)
RICCtest2$Season[RICCtest2$Month >= 5 & RICCtest2$Month <= 10] <- "SnowFree"
RICCtest2$Season[RICCtest2$Month <= 4 | RICCtest2$Month >= 11] <- "Snow"
RICCtest2$Season<-as.factor(RICCtest2$Season)

RICCtest2<-as.data.frame(RICCtest2)
RICCtest2$IndSeason <- do.call(paste, c(RICCtest2[c("id", "Season")], sep = ""))
RICCtest2$IndSeason <- as.factor(RICCtest2$IndSeason)

RICCtest2$IndSeasonYear <- do.call(paste, c(RICCtest2[c("id", "SeasonYr")], sep = ""))
RICCtest2$IndSeasonYear <- as.factor(RICCtest2$IndSeasonYear)

##For each IndSeasonYear, calculate min and max t_
RICCtest2$t_Num<-as.Date(RICCtest2$t_)

IndSeasonYearList<-unique(RICCtest2$IndSeasonYear)
RICCAnimalMetadata<-data.frame()
for(i in IndSeasonYearList){
  subdat<-subset(RICCtest2, IndSeasonYear == i)
  TimeDif<-max(subdat$t_Num) - min(subdat$t_Num)
  meta.sub<-data.frame(IndSeasonYear=i)
  meta.sub$AnimalId<-unique(subdat$id)
  meta.sub$Year<-min(unique(subdat$Year))
  meta.sub$Season<-unique(subdat$Season)
  meta.sub$TimeDif<-as.numeric(TimeDif)
  RICCAnimalMetadata<-rbind(RICCAnimalMetadata,meta.sub)
}

################### WHEC ########################
##Need an IDSeason
#Format the date column so I can pull out the Month and year
WHECtest2<-as.data.frame(WHECtest)
WHECtest2$Month<-month(WHECtest2$t_)
WHECtest2$Year<-year(WHECtest2$t_)

##Define seasons
WHECtest2$SeasonYr[WHECtest2$Month >= 5 & WHECtest2$Month <= 10 & WHECtest2$Year == 2012] <- "2012SnowFree"
WHECtest2$SeasonYr[WHECtest2$Month >= 5 & WHECtest2$Month <= 10 & WHECtest2$Year == 2013] <- "2013SnowFree"
WHECtest2$SeasonYr[WHECtest2$Month >= 5 & WHECtest2$Month <= 10 & WHECtest2$Year == 2014] <- "2014SnowFree"
WHECtest2$SeasonYr[WHECtest2$Month >= 5 & WHECtest2$Month <= 10 & WHECtest2$Year == 2015] <- "2015SnowFree"
WHECtest2$SeasonYr[WHECtest2$Month >= 5 & WHECtest2$Month <= 10 & WHECtest2$Year == 2016] <- "2016SnowFree"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2012 | WHECtest2$Month >= 11 & WHECtest2$Year == 2011] <- "2011Snow"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2013 | WHECtest2$Month >= 11 & WHECtest2$Year == 2012] <- "2012Snow"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2014 | WHECtest2$Month >= 11 & WHECtest2$Year == 2013] <- "2013Snow"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2015 | WHECtest2$Month >= 11 & WHECtest2$Year == 2014] <- "2014Snow"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2016 | WHECtest2$Month >= 11 & WHECtest2$Year == 2015] <- "2015Snow"
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2017 | WHECtest2$Month >= 11 & WHECtest2$Year == 2016] <- "2016Snow"
WHECtest2$SeasonYr<-as.factor(WHECtest2$SeasonYr)
summary(WHECtest2$SeasonYr)

##combine season and ID to  get IDSeason
WHECtest2$Season<-as.character(WHECtest2$Season)
WHECtest2$Season[WHECtest2$Month >= 5 & WHECtest2$Month <= 10] <- "SnowFree"
WHECtest2$Season[WHECtest2$Month <= 4 | WHECtest2$Month >= 11] <- "Snow"
WHECtest2$Season<-as.factor(WHECtest2$Season)

WHECtest2<-as.data.frame(WHECtest2)
WHECtest2$IndSeason <- do.call(paste, c(WHECtest2[c("id", "Season")], sep = ""))
WHECtest2$IndSeason <- as.factor(WHECtest2$IndSeason)

WHECtest2$IndSeasonYear <- do.call(paste, c(WHECtest2[c("id", "SeasonYr")], sep = ""))
WHECtest2$IndSeasonYear <- as.factor(WHECtest2$IndSeasonYear)

##For each IndSeasonYear, calculate min and max t_
WHECtest2$t_Num<-as.Date(WHECtest2$t_)

IndSeasonYearList<-unique(WHECtest2$IndSeasonYear)
WHECAnimalMetadata<-data.frame()
for(i in IndSeasonYearList){
  subdat<-subset(WHECtest2, IndSeasonYear == i)
  TimeDif<-max(subdat$t_Num) - min(subdat$t_Num)
  meta.sub<-data.frame(IndSeasonYear=i)
  meta.sub$AnimalId<-unique(subdat$id)
  meta.sub$Year<-min(unique(subdat$Year))
  meta.sub$Season<-unique(subdat$Season)
  meta.sub$TimeDif<-as.numeric(TimeDif)
  WHECAnimalMetadata<-rbind(WHECAnimalMetadata,meta.sub)
}

AllAnimalMetadata<-rbind(WHECAnimalMetadata, RICCAnimalMetadata, BCAnimalMetadata, SKAnimalMetadata)  
all_estBuff$TimeDif<-AllAnimalMetadata$TimeDif[match(all_estBuff$ID, AllAnimalMetadata$IndSeasonYear)]
all_estBuff$AnimalId<-AllAnimalMetadata$AnimalId[match(all_estBuff$ID, AllAnimalMetadata$IndSeasonYear)]
all_estBuff$Season<-AllAnimalMetadata$Season[match(all_estBuff$ID, AllAnimalMetadata$IndSeasonYear)]
all_estBuff$Year<-AllAnimalMetadata$Year[match(all_estBuff$ID, AllAnimalMetadata$IndSeasonYear)]

###################################################################################################################
################################################### PACK INFO #####################################################
###################################################################################################################

###PackID is in PackIDLookUp.csv
PackIDLookUp<-read.csv(here::here("data", "PackIDLookUp.csv"))
all_estBuff$PackID<-PackIDLookUp$PackID[match(all_estBuff$AnimalId, PackIDLookUp$AnimalID)]

#PS is in MinimumPackSizes.csv
PackSizes<-read.csv(here::here("data", "MinimumPackSizes.csv"))
##There are multiple estimates per pack from multiple years
##Because many don't have year, take year out and choose the minimum
PackSizes<-PackSizes %>% 
  group_by(PackID) %>% 
  summarise(PackSize = min(PackSize))

all_estBuff$PackSize<-PackSizes$PackSize[match(all_estBuff$PackID, PackSizes$PackID)]

#Sex is in All_Metadata_Sex.csv
SexLookUp<-read.csv(here::here("data", "All_Metadata_Sex.csv"))
all_estBuff$Sex<-SexLookUp$Sex[match(all_estBuff$AnimalId, SexLookUp$AnimalID)]


##Monitoring Period should be scaled:
range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}

all_estBuff$DiffDTScaled<-range01(all_estBuff$TimeDif)

##Note that "Year" currently reflects calendar year (i.e. in January 2014, it's called 2014), 
#but the snow season IDs come from the starting year (i.e. in January 2014, it's 2013's snow season)
##To replace calendar year with "ecological" year, we can either pull year from the IDs
##Or we can bring in the list of IDs and Years from:
#ExtractingEVI_NewaKDEs.R "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\AllSAs.csv")

AllSAs<-read.csv(here::here("data", "AllSAs.csv"))

head(AllSAs)
head(all_estBuff)
all_estBuff$EcoYear<-AllSAs$Year[match(all_estBuff$name, AllSAs$name)]
##Good, now Change "Year" to "CalYear", and "EcoYear" to "Year"
names(all_estBuff)[names(all_estBuff) == "Year"] <- "CalYear"
names(all_estBuff)[names(all_estBuff) == "EcoYear"] <- "Year"

write.csv(all_estBuff, here::here("data","All_estBuffEVILFDAtt.csv"))
