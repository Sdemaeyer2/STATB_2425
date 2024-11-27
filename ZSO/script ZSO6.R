# script ZSO 6

# vraag 1.2
Model1<-lm(Belang~Zelfver,data=Zso6)
summary(Model1)

Model2<-lm(Belang~Onderwijservaring,data=Zso6)
summary(Model2)

# vraag 1.4
Model3<-lm(Belang~Zelfver+ErvaringBO+Onderwijservaring,data=Zso6)
par(mfrow=c(2,2))
plot(Model3)

residuals_plot(Model3)
cooks_plot(Model3)

library(car)
vif(Model3)

Zso6bis<-Zso6[c(1:344,346:1416,1418:2486),] # databestand zonder rij 345 en rij 1417
Model3<-lm(Belang~Zelfver+ErvaringBO+Onderwijservaring,data=Zso6bis)
summary(Model3)
