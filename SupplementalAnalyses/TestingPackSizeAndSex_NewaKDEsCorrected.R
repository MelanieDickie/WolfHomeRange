require(stringr)
require(dplyr)
require(ggplot2)
require(cowplot)
require(nlme)

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


DataLargerAvailNoET$PackYear<-paste(DataLargerAvailNoET$PackID, DataLargerAvailNoET$Year, sep="")

#Need to change "f" to "F"
DataLargerAvailNoET$Sex[DataLargerAvailNoET$Sex == "f"] <- "F"

DataLargerAvailNoETPS<-subset(DataLargerAvailNoET, PackSize >1)
DataLargerAvailNoETPS<-subset(DataLargerAvailNoETPS, Sex !="Unknown")
DataLargerAvailNoETPS$Sex<-as.character(DataLargerAvailNoETPS$Sex)
DataLargerAvailNoETPS$Sex<-as.factor(DataLargerAvailNoETPS$Sex)

range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
DataLargerAvailNoETPS$EVIScaled<-range01(DataLargerAvailNoETPS$EVIMean)

meanx<-mean(DataLargerAvailNoETPS$EVIMean)
maxx<-max(DataLargerAvailNoETPS$EVIMean)
minx<-min(DataLargerAvailNoETPS$EVIMean)
(meanx-minx)/(maxx-minx)

# ############ Test if including sex and pack size is supported in the basic model structure
# modsexpshab1A<-lmer(log(est) ~ DiffDTScaled + Season + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
# modsexpshab1B<-lmer(log(est) ~ DiffDTScaled + Sex + Season + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
# modsexpshab1C<-lmer(log(est) ~ DiffDTScaled + PackSize + Season + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
# modsexpshab1D<-lmer(log(est) ~ DiffDTScaled + Sex + PackSize + Season + SA + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
# AIC(modsexpshab1A, modsexpshab1B, modsexpshab1C, modsexpshab1D)
# #So top model includes Pack Size 
# ##And the pack size is more than 2 AIC units lower
# ##Also including sex is close
# ##So test if including PS changes AIC units of the top model


############ Test if including sex and pack size is supported in the top model
modsexpshab3A<-lmer(log(est) ~ DiffDTScaled + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
modsexpshab3B<-lmer(log(est) ~ DiffDTScaled + Sex + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
modsexpshab3C<-lmer(log(est) ~ DiffDTScaled + PackSize + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
modsexpshab3D<-lmer(log(est) ~ DiffDTScaled + Sex + PackSize + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = DataLargerAvailNoETPS)
AIC(modsexpshab3A, modsexpshab3B, modsexpshab3C, modsexpshab3D)
#Lowest AIC is the 3C - but it's within 2 AIC units.
##Does not support including pack size

write.csv(as.data.frame(AIC(modsexpshab3A, modsexpshab3B, modsexpshab3C, modsexpshab3D)),"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\PackSize_Sex_AIC_Comp.csv" )
#But TEST IF PS SIGNIFICANT
summary(modsexpshab3C)#ps is insig
summary(modsexpshab3D)#ps is insig

##PS is insignificant
##LFD and EVI are still significant.
tablemodsexpshab3D<-tidy(modsexpshab3D)
tablemodsexpshab3D$CI<-1.96*tablemodsexpshab3D$std.error
write.csv(tablemodsexpshab3D,"G:\\My Drive\\CMU\\WolfHomeRange\\Data\\PackSize_Sex_Est.csv" )

##########################################################################
############################### Residuals ################################
##########################################################################
range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
DataLargerAvailNoET$EVIScaled<-range01(DataLargerAvailNoET$EVIMean)

meanx<-mean(DataLargerAvailNoET$EVIMean)
maxx<-max(DataLargerAvailNoET$EVIMean)
minx<-min(DataLargerAvailNoET$EVIMean)
(meanx-minx)/(maxx-minx)

wolflist<-unique(DataLargerAvailNoET$name)
pred.areadf <- data.frame()
for(i in wolflist){
  #print(i)
  testing <- subset(DataLargerAvailNoET, name == i)
  training <- subset(DataLargerAvailNoET, name != i)
  
  mymodel<-lmer(log(est) ~ DiffDTScaled + Season + SA + LFD*EVIScaled + (1|PackYear), REML=FALSE, data = training)
  testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackYear), allow.new.levels = TRUE))
  testing<-as.data.frame(testing)
  pred.areadf <- rbind(pred.areadf,testing)
}

plot(pred.areadf$est ~ pred.areadf$predArea)
summary(lm(pred.areadf$est ~ pred.areadf$predArea))#Adjusted R-squared:  0.354

pred.areadf$Dif<-pred.areadf$est-pred.areadf$predArea

##PackSize in the current dataframe is minimimum pack size estimated at capture
##Here I want to specifically look at pack size for that year
##There are a couple instances where pack size was estimated for > 1 year (because multiple captures)
##And instances where pack size wasn't estimated that year (because no captures)
##Because PackYear is the random intercept, I can use that information here
##In this analysis, only use pack size from the matching year

#PS is in MinimumPackSizes.csv
PackSizes<-read.csv("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\MinimumPackSizes.csv")
##There are multiple estimates per pack from multiple years
PackSizes$PackYear<-paste(PackSizes$PackID, PackSizes$Year, sep="")

pred.areadf$YearPackSize<-PackSizes$PackSize[match(pred.areadf$PackYear, PackSizes$PackYear)]
##There are no matches for RICC because year was not provided.

#Plot
PSPlot<-ggplot(data=subset(pred.areadf, Sex !="Unknown" & as.numeric(YearPackSize) <13), aes(x=as.factor(YearPackSize), y=(Dif))) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(shape=as.factor(SA)), size=2.5, alpha = 1/3)+
  scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), solid=F)+
  theme_bw() +  
  geom_hline(yintercept=0, linetype="dashed")+
  xlab("Pack Size") +
  ylab("Residuals (Observed - Predicted)") +
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(legend.text = element_blank(),legend.title = element_blank(), legend.position="top", legend.box = "vertical")


pred.areadf$Sex[pred.areadf$Sex == "f"] <- "F"
SexPlot<-ggplot(data=subset(pred.areadf, Sex !="Unknown" & as.numeric(PackSize) <13), aes(x=(Sex), y=(Dif))) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(shape=as.factor(SA)), size=2.5, alpha = 1/3)+
  scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), labels = c("BC", "AB NE", "AB N", "SK"), solid=F)+
  theme_bw() +  
  xlab("Sex") +
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("") +
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position="top", legend.box = "vertical")
  
require(cowplot)
plot_grid(PSPlot, SexPlot, ncol = 2 , nrow = 1)


##Model
difsex<-lm(Dif ~ Sex, subset(pred.areadf, Sex != "Unknown"))
difPS<-lm(Dif ~ as.numeric(PackSize), subset(pred.areadf, as.numeric(PackSize) >0))

##The difference between obesrved and predicted home ranges is insignificantly larger for females than males
##The difference between observed and predicted home ranges is significantly larger as pack size increases
##Despite the effect of pack size being insignificant in models
##Though the plots suggest there may be a lack of data from larger packs

##########################################################################
############################## BLUPs #####################################
##########################################################################

############## TEST IF RANDOM INTERCEPT DEVIATIONS ARE PREDICTED BY PACK SIZE

##Extract BLUPs
ranef(modsexpshab3A)
str(rr1 <- ranef(modsexpshab3A))
require(lattice)
str(dd <- as.data.frame(rr1))
ggplot(dd, aes(y=grp,x=condval)) +
   geom_point() + facet_wrap(~term,scales="free_x") +
   geom_errorbarh(aes(xmin=condval -2*condsd, xmax=condval +2*condsd), height=0)
 
dd$blup<-dd$condval+9.39324
 
##PackSize in the current dataframe is minimimum pack size estimated at capture
##Here I want to specifically look at pack size for that year
##There are a couple instances where pack size was estimated for > 1 year (because multiple captures)
##And instances where pack size wasn't estimated that year (because no captures)
##Because PackYear is the random intercept, I can use that information here
##In this analysis, only use pack size from the matching year

#PS is in MinimumPackSizes.csv
PackSizes<-read.csv("G:\\My Drive\\CMU\\WolfHomeRange\\Data\\MinimumPackSizes.csv")
##There are multiple estimates per pack from multiple years
PackSizes$PackYear<-paste(PackSizes$PackID, PackSizes$Year, sep="")

dd$YearPackSize<-PackSizes$PackSize[match(dd$grp, PackSizes$PackYear)]
##There are no matches for RICC because year was not provided.

ggplot(data=dd, aes(x=(YearPackSize), y=(condval))) +
  #geom_point()+
  #geom_errorbar(aes(ymin=condval -2*condsd, ymax=condval +2*condsd), width=0.2)+
  geom_pointrange(aes(ymin = condval-2*condsd, ymax = condval+2*condsd), 
                  position=position_jitter(width=0.3), 
                  linetype='dashed')+
  theme_bw() +  
  xlab("Pack Size") +
  ylab("Deviation") +
  geom_hline(yintercept=0, linetype = "dotted")+
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


ggplot(data=dd, aes(x=(YearPackSize), y=(blup))) +
  #geom_point()+
  #geom_errorbar(aes(ymin=condval -2*condsd, ymax=condval +2*condsd), width=0.2)+
  geom_pointrange(aes(ymin = (9.39324+condval)-2*condsd, ymax = (9.39324+condval)+2*condsd), 
                  position=position_jitter(width=0.3), 
                  linetype='dashed')+
  theme_bw() +  
  xlab("Pack Size") +
  ylab("BLUP") +
  #geom_hline(yintercept=0, linetype = "dotted")+
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

condvalPS<-lm(condval ~ as.numeric(YearPackSize), subset(dd, as.numeric(YearPackSize) >0))
summary(condvalPS)

