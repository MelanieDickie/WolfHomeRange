require(stringr)
library("dplyr")
library("tidyr")
library("broom.mixed")
require(performance)
require(lme4)
require(ggplot2)
require(tidyverse)
require(tidyr)

##"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\All_estBuffEVILFDAtt.csv"
##From Wolf_aKDEs ->Attributing_NewaKDEs.R

##Bring in main dataset:
DataLargerAvail<-read.csv("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\All_estBuffEVILFDAtt.csv")

DataLargerAvailNoET<-subset(DataLargerAvail, ExtraTerr == 0)
DataLargerAvailNoET<-subset(DataLargerAvailNoET, est <40000)

range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
DataLargerAvailNoET$EVIScaled<-range01(DataLargerAvailNoET$EVIMean)

meanx<-mean(DataLargerAvailNoET$EVIMean)
maxx<-max(DataLargerAvailNoET$EVIMean)
minx<-min(DataLargerAvailNoET$EVIMean)
(meanx-minx)/(maxx-minx)

head(DataLargerAvailNoET)


DataLargerAvailNoET$AnimalYear<-paste(DataLargerAvailNoET$AnimalId, DataLargerAvailNoET$Year, sep="")
DataLargerAvailNoET$PackYear<-paste(DataLargerAvailNoET$PackID, DataLargerAvailNoET$Year, sep="")

samplesize<-DataLargerAvailNoET %>% group_by(SA, Year, Season) %>% tally()
samplesize_wise <- spread(samplesize, Season, n)
write.csv(samplesize_wise, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\BufferedHRs\\SampleSizes.csv")

#############################################################################
######################### Optimize Random structure ########################
#############################################################################
##Zurr suggests to include all fixed effects and model with REML = TRUE

##Always maintain AnimalId, but should PackID be in there seperate, nested, or not at all
modpsrand1<-lmer(log(est) ~ Season + as.factor(Year) + SA + DiffDTScaled + LFD*EVIScaled + (1|AnimalId), REML=TRUE, data = DataLargerAvailNoET)
modpsrand2<-lmer(log(est) ~ Season + as.factor(Year) + SA + DiffDTScaled + LFD*EVIScaled + (1|AnimalId) + (1|PackID), REML=TRUE, data = DataLargerAvailNoET)#singular
modpsrand3<-lmer(log(est) ~ Season + as.factor(Year) + SA + DiffDTScaled + LFD*EVIScaled + (1|AnimalId/PackID), REML=TRUE, data = DataLargerAvailNoET)
modpsrand4<-lmer(log(est) ~ Season + as.factor(Year) + SA + DiffDTScaled + LFD*EVIScaled + (1|PackID), REML=TRUE, data = DataLargerAvailNoET)
modpsrand5<-lmer(log(est) ~ Season + SA + DiffDTScaled + LFD*EVIScaled + (1|AnimalYear), REML=TRUE, data = DataLargerAvailNoET)
modpsrand6<-lmer(log(est) ~ Season + SA + DiffDTScaled + LFD*EVIScaled + (1|PackYear) + (1|AnimalId), REML=TRUE, data = DataLargerAvailNoET)#singular
modpsrand7<-lmer(log(est) ~ Season + SA + DiffDTScaled + LFD*EVIScaled + (1|PackYear), REML=TRUE, data = DataLargerAvailNoET)
modpsrand8<-lmer(log(est) ~ Season + SA + DiffDTScaled + LFD*EVIScaled + (1|AnimalYear/PackID), REML=TRUE, data = DataLargerAvailNoET)

AIC(modpsrand5, modpsrand7, modpsrand8)#PackYear is best

AICTable_Rand<-as.data.frame(AIC(modpsrand5, modpsrand7, modpsrand8))


#modpsrand7 has the lowest AIC, so use (1|PackYear)

#############################################################################
############################ Optimize Basic modpsel ##########################
#############################################################################
##Zurr suggests to use REML = FALSE

##Always maintain AnimalId, but should PackID be in there seperate, nested, or not at all
modpshabnull<-lmer(log(est) ~ 1 + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
#modpsbasic1<-lmer(log(est) ~ DiffDTScaled  + Season + as.factor(Year) + SA  + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
modpsbasic2<-lmer(log(est) ~ DiffDTScaled  + Season + (1|PackYear) , REML=FALSE, data = DataLargerAvailNoET)
#modpsbasic3<-lmer(log(est) ~ DiffDTScaled  + as.factor(Year) + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
modpsbasic4<-lmer(log(est) ~ DiffDTScaled  + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
modpsbasic5<-lmer(log(est) ~ DiffDTScaled  + Season + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
#modpsbasic6<-lmer(log(est) ~ DiffDTScaled  + Season + as.factor(Year) + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
AIC(modpsbasic2, modpsbasic4, modpsbasic5)
min(AIC(modpshabnull, modpsbasic2, modpsbasic4, modpsbasic5)$AIC)

AICTable_Basic<-as.data.frame(AIC(modpsbasic2, modpsbasic4, modpsbasic5))

#modpsbasic5 wins, so include DiffDTScaled  + Season + SA

#############################################################################
########################## Optimize Habitat model ##########################
#############################################################################

modpshab1<-lmer(log(est) ~ DiffDTScaled  + Season + SA + LFD + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
modpshab2<-lmer(log(est) ~ DiffDTScaled  + Season + SA + EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
modpshab4<-lmer(log(est) ~ DiffDTScaled  + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
#modpshab5<-lmer(log(est) ~ DiffDTScaled  + SA + LFD*EVIScaled*Season + (1|PackYear), REML=FALSE, data = DataLargerAvailNoET)
min(AIC(modpshab1, modpshab2, modpshab4)$AIC)
AIC(modpshab1, modpshab2, modpshab4)
#modpshab4 is lowest, and all improved over null

AICTable_FULL<-as.data.frame(AIC(modpshab1, modpshab2, modpshab4))

summary(modpshab4)
r2_nakagawa(modpshab4)

######### Save Tables ############

AICTable_Basic$Component<-"Basic Structure"
AICTable_Rand$Component<-"Random Structure"
AICTable_FULL$Component<-"Full Structure"

AICTable<-rbind(AICTable_Rand, AICTable_Basic, AICTable_FULL)

write.csv(AICTable, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\AICTable_NewaKDEs.csv")

modeltable4<-tidy(modpshab4)
modeltable4$CI<-1.96*modeltable4$std.error

write.csv(modeltable4, "G:\\My Drive\\CMU\\WolfHomeRange\\Data\\IntModelTable_NewaKDE.csv")


################# PLOT ########################
#For plotting purposes, use the reference categories and mean value of continuous variables

meanEVI<-mean(DataLargerAvailNoET$EVIScaled)
meanDTDiff<-mean(DataLargerAvailNoET$DiffDTScaled)
minLD<-min(DataLargerAvailNoET$LFD)
maxLD<-max(DataLargerAvailNoET$LFD)
minEVI<-min(DataLargerAvailNoET$EVIScaled)
maxEVI<-max(DataLargerAvailNoET$EVIScaled)

modpred <- expand.grid(Season = "Snow", LFD=seq(minLD,maxLD, by =0.01), DiffDTScaled=meanDTDiff, EVIScaled=c(0,0.25,0.5,0.75,1), SA = "BC")
modpred$predArea <-  predict(modpshab4,modpred, type=c("response"),re.form=NA)

# ggplot(data=modpred, aes(x=(LFD), y=(predArea))) +
#   geom_line(size=1.25, aes(linetype=as.factor(EVIScaled))) +
#   geom_point(data=DataLargerAvailNoET, aes(x=(LFD), shape=as.factor(SA), y =log(est)), size=2.5, alpha = 1/3)+
#   scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), solid=F)+
#   scale_linetype_manual(values=c("solid","longdash", "dashed", "dotdash", "dotted"))+
#   theme_bw() +  
#   xlab(expression(paste("Linear Feature Density (km/", km^2, ")"))) +
#   ylab(expression(paste("Home Range Area (Log 100 ", km^2, ")"))) +
#   theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
#   theme(axis.text.y = element_text(size=12)) + 
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position="top", legend.box = "vertical")+
#   labs(colour="Scaled ΔEVI", shape = "Study Area", linetype="Scaled ΔEVI")

##BackTransformed:
ggplot(data=modpred, aes(x=(LFD), y=exp(predArea))) +
  geom_line(size=1.25, aes(linetype=as.factor(EVIScaled))) +
  geom_point(data=DataLargerAvailNoET, aes(x=(LFD), shape=as.factor(SA), y =(est)), size=2.5, alpha = 1/3)+
  scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), labels = c("BC", "AB NE", "AB N", "SK"), solid=F)+
  scale_linetype_manual(values=c("solid","longdash", "dashed", "dotdash", "dotted"))+
  theme_bw() +  
  xlab(expression(paste("Linear Feature Density (km/", km^2, ")"))) +
  ylab(expression(paste("Home Range Area( ",km^2, ")"))) +
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position=c(0.5, 0.9), legend.direction = "horizontal")+
  labs(colour="Scaled ΔEVI", shape = "Study Area", linetype="Scaled ΔEVI")


# modpred <- expand.grid(Season = "Snow", LFD=c(0,0.5,1,1.5,2), DiffDTScaled=meanDTDiff, EVIScaled=seq(minEVI,maxEVI, by =0.10), Year = "2013", SA = "BC")
# modpred$predArea <-  predict(modpshab4,modpred, type=c("response"),re.form=NA)
# 
# require(ggplot2)
# ggplot(data=modpred, aes(x=(EVIScaled), y=(predArea))) +
#   geom_line(size=1.25, aes(linetype=as.factor(LFD))) +
#   geom_point(data=DataLargerAvailNoET, aes(x=(EVIScaled), shape=as.factor(SA), y =log(est)), size=2.5, alpha = 1/3)+
#   scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), solid=F)+
#   scale_linetype_manual(values=c("solid","longdash", "dashed", "dotdash", "dotted"))+
#   theme_bw() +  
#   xlab("Scaled ΔEVI") +
#   ylab(expression(paste("Log Area (100", km^2, ")"))) +
#   theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
#   theme(axis.text.y = element_text(size=12)) + 
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position="top", legend.box = "vertical")+
#   labs(colour=expression(paste("Linear Feature Density (km/", km^2, ")")), shape = "Study Area", linetype=expression(paste("Linear Feature Density (km/", km^2, ")")))
# 
# #BACKTRANSFORMED
# ggplot(data=modpred, aes(x=(EVIScaled), y=exp(predArea))) +
#   geom_line(size=1.25, aes(linetype=as.factor(LFD))) +
#   geom_point(data=DataLargerAvailNoET, aes(x=(EVIScaled), shape=as.factor(SA), y =(est)), size=2.5, alpha = 1/3)+
#   scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), solid=F)+
#   scale_linetype_manual(values=c("solid","longdash", "dashed", "dotdash", "dotted"))+
#   theme_bw() +  
#   xlab("Scaled ΔEVI") +
#   ylab(expression(paste("Area ( ", km^2, ")"))) +
#   theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
#   theme(axis.text.y = element_text(size=12)) + 
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position="top", legend.box = "vertical")+
#   labs(colour=expression(paste("Linear Feature Density (km/", km^2, ")")), shape = "Study Area", linetype=expression(paste("Linear Feature Density (km/", km^2, ")")))


##Calculate example effect size:
meanscaledevi<-mean(DataLargerAvailNoET$EVIScaled)
meanx<-mean(DataLargerAvailNoET$EVIMean)
maxx<-max(DataLargerAvailNoET$EVIMean)
minx<-min(DataLargerAvailNoET$EVIMean)
exval<-(meanx-minx)/(maxx-minx)

meanlfd<-mean(DataLargerAvailNoET$LFD)
maxlfd<-max(DataLargerAvailNoET$LFD)
minlfd<-min(DataLargerAvailNoET$LFD)

modpredexa <- expand.grid(Season = "Snow", LFD=c(minlfd,meanlfd,maxlfd), DiffDTScaled=meanDTDiff, EVIScaled=c(0,exval,1), SA = "BC")
modpredexa$predArea <-  round(exp(predict(modpshab4,modpredexa, type=c("response"),re.form=NA)))

modpredex <- expand.grid(Season = "SnowFree", LFD=c(minlfd,meanlfd,maxlfd), DiffDTScaled=meanDTDiff, EVIScaled=c(0,exval,1), SA = "BC")
modpredex$predArea <-  round(exp(predict(modpshab4,modpredex, type=c("response"),re.form=NA)))


##########################################################################
############################### Residuals ################################
##########################################################################

wolflist<-unique(DataLargerAvailNoET$name)
pred.areadf <- data.frame()
for(i in wolflist){
  #print(i)
  testing <- subset(DataLargerAvailNoET, name == i)
  training <- subset(DataLargerAvailNoET, name != i)
  
  mymodel<-lmer(log(est) ~ DiffDTScaled + Season + as.factor(Year) + SA + LFD*EVIScaled + (1|AnimalId) + (1|PackID), REML=FALSE, data = training)
  testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
  testing<-as.data.frame(testing)
  pred.areadf <- rbind(pred.areadf,testing)
}

plot(pred.areadf$est ~ pred.areadf$predArea)
summary(lm(pred.areadf$est ~ pred.areadf$predArea))#Adjusted R-squared:  0.354
