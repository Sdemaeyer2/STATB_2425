# script ZSO7

# vraag 1.1
DataZSO7$Iqz<-scale(DataZSO7$Iq)
DataZSO7$Begrlezenz<-scale(DataZSO7$Begrlezen)
Model1<-lm(Demotivatiez~Iqz+Begrlezenz,data= DataZSO7)
summary(Model1)

# vraag 1.2
DataZSO7$BSO<-(DataZSO7$Ondvorm==2)*1
table(DataZSO7$BSO,DataZSO7$Ondvorm)
Model2<-lm(Demotivatiez~Iqz+Begrlezenz+BSO,data=DataZSO7)
summary(Model2)

# vraag 1.3
Model3<-lm(Demotivatiez~Iqz+Begrlezenz+BSO+(BSO*Iqz),data=DataZSO7)
summary(Model3)

DataZSO7$BSOfac<-factor(DataZSO7$BSO,levels=c(0,1),labels=c("ASO of TSO","BSO"))
Model3bis<-lm(Demotivatiez~Iqz+Begrlezenz+BSOfac+(BSOfac*Iqz),data=DataZSO7)
summary(Model3bis)

library(effects)
plot(effect("Iqz:BSOfac",Model3bis),multiline=T,main="Visualisatie interactie-effect IQ en BSO")


# vraag 1.4
DataZSO7b<-na.omit(DataZSO7[c('Iqz','Demotivatiez','Begrlezenz','BSO')])
Model1b<-lm(Demotivatiez~Iqz+Begrlezenz,data=DataZSO7b)
Model2b<-lm(Demotivatiez~Iqz+Begrlezenz+BSO,data=DataZSO7b)
Model3b<-lm(Demotivatiez~Iqz+Begrlezenz+BSO+(BSO*Iqz),data=DataZSO7b)
anova(Model1b,Model2b,Model3b)
