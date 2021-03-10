
########## LOAD DATA & PACKAGES #############

##These files were copied over to:
RICCtest<-read.csv(here::here("data", "RICC3Hour.csv"))
BCtest<-read.csv(here::here("data", "BC3Hour.csv"))
WHECtest<-read.csv(here::here("data", "WHEC3Hour.csv"))
SKtest<-read.csv(here::here("data", "SK3Hour.csv"))

require(lubridate)
require(data.table)
require(dplyr)
require(ctmm)

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
SKtest2$SeasonYr[SKtest2$Month <= 4 & SKtest2$Year == 2012 | SKtest2$Month >= 11 & SKtest2$Year == 2013] <- "2011Snow"
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

head(SKtest2)
SKtest22<-select(SKtest2, IndSeasonYear, x_, y_, t_)
colnames(SKtest22)<-c("individual.local.identifier", "location.long","location.lat", "timestamp")
head(SKtest22)
summary(SKtest22$individual.local.identifier)
#So each season/year/animal has roughly 8-1400 locations. Sample size is variable.

SKtest22<-as.data.table(SKtest22)

SKtest23<-as.telemetry(SKtest22,timeformat="%Y-%m-%d %H:%M:%S",timezone="UTC",projection=NULL,na.rm="row", drop=TRUE)
##This command is picky and you need to make sure you have the data in the right format

SKtest2list<-unique(SKtest22$individual.local.identifier)
metadata<-data.frame()
for(i in SKtest2list){
  subdat<-SKtest23[[i]]#subset to individual
  if (nrow(subdat) >=50) {
    vg.sub<-variogram(subdat)#Make variogram
    plot(vg.sub,level=c(0.5,0.95))#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #variogram.fit(vg.sub, name="GUESS")
    ##Fit with the standard guess parameters
    GUESS <- ctmm.guess(subdat,interactive=FALSE)#estimate parameters for Ctmm fit
    #Fit all possible models
    fitted.mods<-ctmm.select(subdat, CTMM=GUESS, verbose=TRUE)
    #Choose the one with lowest AICc
    FitWin <- fitted.mods [[1]]
    plot(vg.sub, CTMM=FitWin, col.CTMM="#1b9e77")#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #Create Autocorrelated KDE estimate
    akde.sub <- akde(subdat,CTMM=FitWin)
    ##Save metadata
    meta.sub<-as.data.frame(summary(FitWin)$CI)
    meta.sub$ID<-i
    meta.sub$type<-summary(FitWin)$name
    metadata<-rbind(metadata,meta.sub)
    ##Save shapefiles of points and aKDEs
    writeShapefile(akde.sub,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKIndSeason\\Poly",file=NULL, level.UD=0.95,level=0.95)
    writeShapefile(subdat,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKIndSeason\\Points",file=NULL)
    #Save plots:
    mypath <- file.path("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\SKIndSeason\\Plots\\",paste("aKDE", i, ".jpg", sep = ""))
    jpeg(file=mypath)
    mytitle = paste("IDSeasonaKDE", i)
    plot(subdat, UD=akde.sub)
    dev.off()
  } else { 
    print(paste("moveon", i))
  }
}

################## RICC #######################
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
RICCtest2$SeasonYr[RICCtest2$Month <= 4 & RICCtest2$Year == 2012 | RICCtest2$Month >= 11 & RICCtest2$Year == 2013] <- "2011Snow"
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

head(RICCtest2)
RICCtest22<-select(RICCtest2, IndSeasonYear, x_, y_, t_)
colnames(RICCtest22)<-c("individual.local.identifier", "location.long", "location.lat", "timestamp")
head(RICCtest22)
summary(RICCtest22$individual.local.identifier)
#So each season/year/animal has roughly 100-1000 locations. Sample size is extremely variable.

require(data.table)
RICCtest22<-as.data.table(RICCtest22)

require(ctmm)
RICCtest23<-as.telemetry(RICCtest22,timeformat="%Y-%m-%d %H:%M:%S",timezone="UTC",projection=NULL,timeout=Inf,na.rm="row", drop=TRUE)
RICCtest2list<-unique(RICCtest22$individual.local.identifier)

for(i in RICCtest2list){
  subdat<-RICCtest23[[i]]#subset to individual
  if (nrow(subdat) >=50) {
    vg.sub<-variogram(subdat)#Make variogram
    plot(vg.sub,level=c(0.5,0.95))#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #variogram.fit(vg.sub, name="GUESS")
    ##Fit with the standard guess parameters
    GUESS <- ctmm.guess(subdat,interactive=FALSE)#estimate parameters for Ctmm fit
    #Fit all possible models
    fitted.mods<-ctmm.select(subdat, CTMM=GUESS, verbose=TRUE)
    #Choose the one with lowest AICc
    FitWin <- fitted.mods [[1]]
    plot(vg.sub, CTMM=FitWin, col.CTMM="#1b9e77")#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #Create Autocorrelated KDE estimate
    akde.sub <- akde(subdat,CTMM=FitWin)
    ##Save metadata
    meta.sub<-as.data.frame(summary(FitWin)$CI)
    meta.sub$ID<-i
    meta.sub$type<-summary(FitWin)$name
    metadata<-rbind(metadata,meta.sub)
    ##Save shapefiles of points and aKDEs
    writeShapefile(akde.sub,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCIndSeason\\Poly",file=NULL, level.UD=0.95,level=0.95)
    writeShapefile(subdat,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCIndSeason\\Points",file=NULL)
    #Save plots:
    mypath <- file.path("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\RICCIndSeason\\Plots\\",paste("aKDE", i, ".jpg", sep = ""))
    jpeg(file=mypath)
    mytitle = paste("IDSeasonaKDE", i)
    plot(subdat, UD=akde.sub)
    dev.off()
  } else { 
    print(paste("moveon", i))
  }
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
BCtest2$SeasonYr[BCtest2$Month <= 4 & BCtest2$Year == 2012 | BCtest2$Month >= 11 & BCtest2$Year == 2013] <- "2011Snow"
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

head(BCtest2)
BCtest22<-select(BCtest2, IndSeasonYear, x_, y_, t_)
colnames(BCtest22)<-c("individual.local.identifier", "location.long","location.lat", "timestamp")
head(BCtest22)
summary(BCtest22$individual.local.identifier)

BCtest22<-as.data.table(BCtest22)

BCtest23<-as.telemetry(BCtest22,timeformat="%Y-%m-%d %H:%M:%S",timezone="UTC",projection=NULL,na.rm="row", drop=TRUE)

BCtest2list<-unique(BCtest22$individual.local.identifier)

for(i in BCtest2list){
  subdat<-BCtest23[[i]]#subset to individual
  if (nrow(subdat) >=50) {
    vg.sub<-variogram(subdat)#Make variogram
    plot(vg.sub,level=c(0.5,0.95))#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #variogram.fit(vg.sub, name="GUESS")
    ##Fit with the standard guess parameters
    GUESS <- ctmm.guess(subdat,interactive=FALSE)#estimate parameters for Ctmm fit
    #Fit all possible models
    fitted.mods<-ctmm.select(subdat, CTMM=GUESS, verbose=TRUE)
    #Choose the one with lowest AICc
    FitWin <- fitted.mods [[1]]
    plot(vg.sub, CTMM=FitWin, col.CTMM="#1b9e77")#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #Create Autocorrelated KDE estimate
    akde.sub <- akde(subdat,CTMM=FitWin)
    ##Save metadata
    meta.sub<-as.data.frame(summary(FitWin)$CI)
    meta.sub$ID<-i
    meta.sub$type<-summary(FitWin)$name
    metadata<-rbind(metadata,meta.sub)
    ##Save shapefiles of points and aKDEs
    writeShapefile(akde.sub,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCIndSeason\\Poly",file=NULL, level.UD=0.95,level=0.95)
    writeShapefile(subdat,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCIndSeason\\Points",file=NULL)
    #Save plots:
    mypath <- file.path("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\BCIndSeason\\Plots\\",paste("aKDE", i, ".jpg", sep = ""))
    jpeg(file=mypath)
    mytitle = paste("IDSeasonaKDE", i)
    plot(subdat, UD=akde.sub)
    dev.off()
  } else { 
    print(paste("moveon", i))
  }
}

################## WHEC #######################
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
WHECtest2$SeasonYr[WHECtest2$Month <= 4 & WHECtest2$Year == 2012 | WHECtest2$Month >= 11 & WHECtest2$Year == 2013] <- "2011Snow"
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

head(WHECtest2)
WHECtest22<-select(WHECtest2, IndSeasonYear, x_, y_, t_)
colnames(WHECtest22)<-c("individual.local.identifier", "location.long","location.lat", "timestamp")
head(WHECtest22)
summary(WHECtest22$individual.local.identifier)

WHECtest22<-as.data.table(WHECtest22)

WHECtest23<-as.telemetry(WHECtest22,timeformat="%Y-%m-%d %H:%M:%S",timezone="UTC",projection=NULL,na.rm="row", drop=TRUE)

WHECtest2list<-unique(WHECtest22$individual.local.identifier)

for(i in WHECtest2list){
  subdat<-WHECtest23[[i]]#subset to individual
  if (nrow(subdat) >=50) {
    vg.sub<-variogram(subdat)#Make variogram
    plot(vg.sub,level=c(0.5,0.95))#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #variogram.fit(vg.sub, name="GUESS")
    ##Fit with the standard guess parameters
    GUESS <- ctmm.guess(subdat,interactive=FALSE)#estimate parameters for Ctmm fit
    #Fit all possible models
    fitted.mods<-ctmm.select(subdat, CTMM=GUESS, verbose=TRUE)
    #Choose the one with lowest AICc
    FitWin <- fitted.mods [[1]]
    plot(vg.sub, CTMM=FitWin, col.CTMM="#1b9e77")#If you want to watch the fit as it goes, leave this. Otherwise, comment out
    #Create Autocorrelated KDE estimate
    akde.sub <- akde(subdat,CTMM=FitWin)
    ##Save metadata
    meta.sub<-as.data.frame(summary(FitWin)$CI)
    meta.sub$ID<-i
    meta.sub$type<-summary(FitWin)$name
    metadata<-rbind(metadata,meta.sub)
    ##Save shapefiles of points and aKDEs
    writeShapefile(akde.sub,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECIndSeason\\Poly",file=NULL, level.UD=0.95,level=0.95)
    writeShapefile(subdat,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECIndSeason\\Points",file=NULL)
    #Save plots:
    mypath <- file.path("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\aKDE\\WHECIndSeason\\Plots\\",paste("aKDE", i, ".jpg", sep = ""))
    jpeg(file=mypath)
    mytitle = paste("IDSeasonaKDE", i)
    plot(subdat, UD=akde.sub)
    dev.off()
  } else { 
    print(paste("moveon", i))
  }
}

metadata$metric<-rownames(metadata)
write.csv(metadata, here::here("data", "metadata.csv"))
