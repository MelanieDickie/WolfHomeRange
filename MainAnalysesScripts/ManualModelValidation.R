library(tidyverse)

##"Top" model

############## Interaction ###################
wolflist<-unique(AllBasicEVINONA$name)
pred.areadf <- data.frame()
for(i in wolflist){
  #print(i)
  testing <- subset(AllBasicEVINONA, name == i)
  training <- subset(AllBasicEVINONA, name != i)
  
  mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD*EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
  testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
  testing<-as.data.frame(testing)
  pred.areadf <- rbind(pred.areadf,testing)
}

plot(pred.areadf$Area ~ pred.areadf$predArea)
summary(lm(pred.areadf$Area ~ pred.areadf$predArea))#Adjusted R-squared:  0.4518

########### Check Residuals against pack size ###############
pred.areadf$Dif<-pred.areadf$Area-pred.areadf$predArea
pred.areadf$PackSize<-as.factor(pred.areadf$PackSize)
plot(pred.areadf$Dif~pred.areadf$PackSize)
plot(pred.areadf$Dif~pred.areadf$Season)
plot(pred.areadf$Dif~pred.areadf$StudyArea)#So it's doing ok at mid values but less well at the extremes
plot(pred.areadf$Dif~pred.areadf$EVIMean)#matches less at lower EVI
plot(pred.areadf$Dif~pred.areadf$LFD)#Not too much of a pattern here. Some very low LFD ranges are poorly prediced (home range is under predicted)



require(ggplot2)
ggplot(data=subset(pred.areadf, as.numeric(PackSize) <13), aes(x=(PackSize), y=(Dif))) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(shape=as.factor(StudyArea)), size=2.5, alpha = 1/3)+
  scale_shape_discrete(name="Study Area", breaks=c("BC", "RICC", "WHEC", "SK"), solid=F)+
  #scale_linetype_manual(values=c("solid","longdash", "dashed", "dotdash", "dotted"))+
  theme_bw() +  
  xlab("Pack Size") +
  ylab("Residuals (Observed - Predicted)") +
  theme(axis.text.x = element_text(size=12), axis.title = element_text(size=14) ) + 
  theme(axis.text.y = element_text(size=12)) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 14), legend.position="top", legend.box = "vertical")



# ############## NO Interaction ###################
# wolflist<-unique(AllBasicEVINONA$name)
# pred.areadfnoint <- data.frame()
# for(i in wolflist){
#   #print(i)
#   testing <- subset(AllBasicEVINONA, name == i)
#   training <- subset(AllBasicEVINONA, name != i)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD + EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
#   testing<-as.data.frame(testing)
#   pred.areadfnoint <- rbind(pred.areadfnoint,testing)
# }
# 
# plot(pred.areadfnoint$Area ~ pred.areadfnoint$predArea)
# summary(lm(pred.areadfnoint$Area ~ pred.areadfnoint$predArea))#Adjusted R-squared:  0.4497
# 
# 
# ############## EVI ONLY ###################
# wolflist<-unique(AllBasicEVINONA$name)
# pred.areadfEVIonly <- data.frame()
# for(i in wolflist){
#   #print(i)
#   testing <- subset(AllBasicEVINONA, name == i)
#   training <- subset(AllBasicEVINONA, name != i)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
#   testing<-as.data.frame(testing)
#   pred.areadfEVIonly <- rbind(pred.areadfEVIonly,testing)
# }
# 
# plot(pred.areadfEVIonly$Area ~ pred.areadfEVIonly$predArea)
# summary(lm(pred.areadfEVIonly$Area ~ pred.areadfEVIonly$predArea))#Adjusted R-squared:  0.4603
# 
# ############## LF ONLY ###################
# wolflist<-unique(AllBasicEVINONA$name)
# pred.areadfLFDonly <- data.frame()
# for(i in wolflist){
#   #print(i)
#   testing <- subset(AllBasicEVINONA, name == i)
#   training <- subset(AllBasicEVINONA, name != i)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
#   testing<-as.data.frame(testing)
#   pred.areadfLFDonly <- rbind(pred.areadfLFDonly,testing)
# }
# 
# plot(pred.areadfLFDonly$Area ~ pred.areadfLFDonly$predArea)
# summary(lm(pred.areadfLFDonly$Area ~ pred.areadfLFDonly$predArea))#Adjusted R-squared:  0.4602
# 
# 
# ############## NULL ###################
# wolflist<-unique(AllBasicEVINONA$name)
# pred.areadfNull <- data.frame()
# for(i in wolflist){
#   #print(i)
#   testing <- subset(AllBasicEVINONA, name == i)
#   training <- subset(AllBasicEVINONA, name != i)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
#   testing<-as.data.frame(testing)
#   pred.areadfNull <- rbind(pred.areadfNull,testing)
# }
# 
# plot(pred.areadfNull$Area ~ pred.areadfNull$predArea, ylim=c(0,3000))
# summary(lm(pred.areadfNull$Area ~ pred.areadfNull$predArea))
# 
# 
# 
# # ############## Interaction ###################
# pred.areadf <- data.frame()
# modelrsq<-data.frame()
# for(i in 1:100){
#   testing <- subset(AllBasicEVINONA, Year <2017)%>%
#     group_by(PackID)%>%
#     sample_frac(1, replace = FALSE)%>%
#     slice(1)
#   
#   training <- AllBasicEVINONA%>%
#     filter(!AnimalID %in% testing$AnimalID)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD*EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"), re.form= ~(1|PackID), allow.new.levels = TRUE))
#   testing<-as.data.frame(testing)
#   testing$iteration<-i
#   pred.areadf <- rbind(pred.areadf,testing)
#   
#   rsq<-summary(lm(testing$predArea ~ testing$Area))$adj.r.squared
#   rsq<-as.data.frame(rsq)
#   rsq$iteration<-i
#   modelrsq<-rbind(modelrsq, rsq)
# }
# 
# plot(pred.areadf$Area ~ pred.areadf$predArea)
# summary(lm(pred.areadf$Area ~ pred.areadf$predArea))
# summary(modelrsq)

# ##Also look at other models
# ################ No Interaction ##################
# pred.areadfnoint <- data.frame()
# modelrsqnoint<-data.frame()
# for(i in 1:100){
#   testing <- subset(AllBasicEVINONA, Year <2017)%>%
#     group_by(PackID)%>%
#     sample_frac(1, replace = FALSE)%>%
#     slice(1)
#   
#   training <- AllBasicEVINONA%>%
#     filter(!AnimalID %in% testing$AnimalID)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD+EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"),re.form=NA))
#   testing<-as.data.frame(testing)
#   testing$iteration<-i
#   pred.areadfnoint <- rbind(pred.areadfnoint,testing)
#   
#   rsq<-summary(lm(pred.areadfnoint$predArea ~ pred.areadfnoint$Area))$adj.r.squared
#   rsq<-as.data.frame(rsq)
#   rsq$iteration<-i
#   modelrsqnoint<-rbind(modelrsqnoint, rsq)
# }
# 
# plot(pred.areadfnoint$predArea ~ pred.areadfnoint$Area)
# summary(lm(pred.areadfnoint$predArea ~ pred.areadfnoint$Area))
# summary(modelrsqnoint)
# 
# 
# 
# #################### LFD Only ########################
# pred.areadflfd <- data.frame()
# modelrsqlfd<-data.frame()
# for(i in 1:100){
#   testing <- subset(AllBasicEVINONA, Year <2017)%>%
#     group_by(PackID)%>%
#     sample_frac(1, replace = FALSE)%>%
#     slice(1)
#   
#   training <- AllBasicEVINONA%>%
#     filter(!AnimalID %in% testing$AnimalID)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + LFD + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"),re.form=NA))
#   testing<-as.data.frame(testing)
#   testing$iteration<-i
#   pred.areadflfd <- rbind(pred.areadflfd,testing)
#   
#   rsq<-summary(lm(pred.areadflfd$predArea ~ pred.areadflfd$Area))$adj.r.squared
#   rsq<-as.data.frame(rsq)
#   rsq$iteration<-i
#   modelrsqlfd<-rbind(modelrsqlfd, rsq)
# }
# 
# plot(pred.areadflfd$predArea ~ pred.areadflfd$Area)
# summary(lm(pred.areadflfd$predArea ~ pred.areadflfd$Area))
# summary(modelrsqlfd)
# 
# 
# #################### EVI Only ########################
# pred.areadfevi <- data.frame()
# modelrsqevi<-data.frame()
# for(i in 1:100){
#   testing <- subset(AllBasicEVINONA, Year <2017)%>%
#     group_by(PackID)%>%
#     sample_frac(1, replace = FALSE)%>%
#     slice(1)
#   
#   training <- AllBasicEVINONA%>%
#     filter(!AnimalID %in% testing$AnimalID)
#   
#   mymodel<-lmer(log(Area) ~ DiffDTScaled + Season + as.factor(Year) + StudyArea + EVIScaled + (1|AnimalID) + (1|PackID), REML=FALSE, data = training)
#   testing$predArea <-  exp(predict(mymodel,testing, type=c("response"),re.form=NA))
#   testing<-as.data.frame(testing)
#   testing$iteration<-i
#   pred.areadfevi <- rbind(pred.areadfevi,testing)
#   
#   rsq<-summary(lm(pred.areadfevi$predArea ~ pred.areadfevi$Area))$adj.r.squared
#   rsq<-as.data.frame(rsq)
#   rsq$iteration<-i
#   modelrsqevi<-rbind(modelrsqevi, rsq)
# }
# 
# plot(pred.areadfevi$predArea ~ pred.areadfevi$Area)
# summary(lm(pred.areadfevi$predArea ~ pred.areadfevi$Area))
# summary(modelrsqevi)
# 
# 
# 
# summary(modelrsqevi)#highest mean
# summary(modelrsqlfd)
# summary(modelrsqnoint)
# summary(modelrsq) #- highest max 0.58264, mean 0.25317, Min 0.09595
# 
# 



