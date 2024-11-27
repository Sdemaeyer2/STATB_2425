# ZSO 2 - statistiek B

#vraag 1
#1.2
summary(Pisa2$Immigratie)

library(car)
Pisa2$Allochtoon<-recode(Pisa2$Immigratie, "'Native'='Autochtoon';'Second-Generation'='Allochtoon';'First-Generation'='Allochtoon'")
summary(Pisa2$Allochtoon)


tapply(Pisa2$Wetenschappenz, Pisa2$Allochtoon, mean, na.rm=TRUE)
tapply(Pisa2$Wetenschappenz, Pisa2$Allochtoon, sd, na.rm=TRUE)

leveneTest(Pisa2$Wetenschappenz, Pisa2$Allochtoon)
 # nagaan of binnengroepvariantie gelijk is voor beide groepen
t.test(Pisa2$Wetenschappenz~Pisa2$Allochtoon, var.equal=TRUE)
source(file.choose()) # OLP2 functies laden
d(Pisa2$Wetenschappenz,Pisa2$Allochtoon)
errorbar(Pisa2$Wetenschappenz~Pisa2$Allochtoon)


tapply(Pisa2$Attitudeschoolz, Pisa2$Allochtoon, mean, na.rm=TRUE)
tapply(Pisa2$Attitudeschoolz, Pisa2$Allochtoon, sd, na.rm=TRUE)

leveneTest(Pisa2$Attitudeschoolz, Pisa2$Allochtoon)
t.test(Pisa2$Attitudeschoolz~Pisa2$Allochtoon, var.equal=FALSE)
d(Pisa2$Attitudeschoolz,Pisa2$Allochtoon)

errorbar(Pisa2$Attitudeschoolz~Pisa2$Allochtoon)

#vraag2
#2.2

tapply(Pisa2$Wetenschappenz, Pisa2$Immigratie, mean, na.rm=TRUE)
tapply(Pisa2$Wetenschappenz, Pisa2$Immigratie, sd, na.rm=TRUE)

leveneTest(Pisa2$Wetenschappenz, Pisa2$Immigratie)
Model1<-aov(Pisa2$Wetenschappenz~Pisa2$Immigratie)
summary(Model1)

etasq(aov(Pisa2$Wetenschappenz~Pisa2$Immigratie)) #OLP2 functies nodig!!!

TukeyHSD(aov(Pisa2$Wetenschappenz~Pisa2$Immigratie))

errorbar(Pisa2$Wetenschappenz~Pisa2$Immigratie)


tapply(Pisa2$Attitudeschoolz, Pisa2$Immigratie, mean, na.rm=TRUE)
tapply(Pisa2$Attitudeschoolz, Pisa2$Immigratie, sd, na.rm=TRUE)

leveneTest(Pisa2$Attitudeschoolz, Pisa2$Immigratie)
oneway.test(Pisa2$Attitudeschoolz~Pisa2$Immigratie)
etasq(aov(Pisa2$Attitudeschoolz~Pisa2$Immigratie))

errorbar(Pisa2$Attitudeschoolz~Pisa2$Immigratie)
