# script ZSO8

# vraag 1.1
library(car)
Pisa3$Memocat<-recode(Pisa3$Memoriserenz, "-3:-0.5='lg'; -0.5:0.5='gem'; 0.5:3.3='hg'")
is.factor(Pisa3$Memocat)
Pisa3$Memocat<-as.factor(Pisa3$Memocat)
is.factor(Pisa3$Memocat)
summary(Pisa3$Memocat)

options(contrasts=c("contr.treatment", "contr.treatment"))
Pisa3$Memocat<-ordered(Pisa3$Memocat,levels=c("lg","gem", "hg"))
leveneTest(Pisa3$Wiskundez~Pisa3$Immigratie*Pisa3$Sex*Pisa3$Memocat)
Model1<-lm(Pisa3$Wiskundez~Pisa3$Sex+Pisa3$Memocat+Pisa3$Immigratie)
Anova(Model1)
summary(Model1) 
etasq(Model1)

# vraag 1.3
leveneTest(Pisa3$Wiskundez~Pisa3$Immigratie*Pisa3$Sex*Pisa3$Memocat)
Model2<-lm(Pisa3$Wiskundez~Pisa3$Sex+Pisa3$Memocat+Pisa3$Immigratie+Pisa3$Memocat*Pisa3$Immigratie)
Anova(Model2)
summary(Model2)
etasq(Model2)

Pisa4<-na.omit(Pisa3[ c("Immigratie","Memocat","Sex", "Wiskundez")])
Model1b<-lm(Pisa4$Wiskundez~Pisa4$Sex+Pisa4$Memocat+Pisa4$Immigratie)
Model2b<-lm(Pisa4$Wiskundez~Pisa4$Sex+Pisa4$Memocat+Pisa4$Immigratie+Pisa4$Memocat*Pisa4$Immigratie)
anova(Model1b,Model2b)

# vraag 1.5
Pisa5<-na.omit(Pisa3[ c("Immigratie","Memoriserenz","Sex", "Wiskundez")])
fligner.test(Wiskundez~Immigratie,data=Pisa5)
Model3<-lm(Pisa5$Wiskundez~Pisa5$Sex+Pisa5$Memoriserenz+Pisa5$Immigratie)
Anova(Model3)
summary(Model3)
etasq(Model3)

Model4<-lm(Pisa5$Wiskundez~Pisa5$Sex+Pisa5$Memoriserenz+Pisa5$Immigratie+
              Pisa5$Memoriserenz*Pisa5$Immigratie)
Anova(Model4)
summary(Model4)
etasq(Model4)

anova(Model3,Model4)
