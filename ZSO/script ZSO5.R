# script ZSO5

# vraag 1.1
Model1<-lm(Oecd$Reading~Oecd$Exptotal)
summary(Model1)

# vraag 1.2
plot(Oecd$Reading~Oecd$Exptotal)
abline(reg=Model1)

# vraag 1.3
#STAP1: Voorspelde leesscores berekenen (op basis van de coefficienten die we halen uit de regressieanalyse)
Oecd$Voorspeld<-4.677e+02+2.636e-04*Oecd$Exptotal
Oecd

# STAP 2: De predictiefouten per land berekenen en tonen
Oecd$Predictiefout<-Oecd$Reading-Oecd$Voorspeld 
Oecd

# vraag 1.4
Oecd$Expend2<-Oecd$Exptotal/1000
Oecd$Read2<-Oecd$Reading-mean(Oecd$Reading)
Model2<-lm(Oecd$Read2~Oecd$Expend2)
summary(Model2)
