# script ZSO4

# vraag 1.1
source(file.choose())
kruistabel.kolom(Zso4$Intrcat,Zso4$Gender)
chi.kwadraat.test(Zso4$Intrcat,Zso4$Gender)

kruistabel.kolom(Zso4$Intrcat,Zso4$Gsm)
chi.kwadraat.test(Zso4$Intrcat,Zso4$Gsm)

kruistabel.kolom(Zso4$Intrcat,Zso4$Immig)
chi.kwadraat.test(Zso4$Intrcat,Zso4$Immig)

# vraag 1.2
sapply(Vaardigheden,mean,na.rm=T)
sapply(Vaardigheden,sd,na.rm=T)
Vaardigheden<-data.frame(Zso4$Pv1eps,Zso4$Pv1isi,Zso4$Pv1use)
cor.prob(Vaardigheden)
plot(Vaardigheden)


# vraag 1.3
kruistabel.kolom(Zso4$Gsm,Zso4$Immig)
chi.kwadraat.test(Zso4$Gsm,Zso4$Immig)
Tabel<-table(Zso4$Immig,Zso4$Gsm)
assocplot(Tabel)


# vraag 1.4
Variabelen<-data.frame(Zso4$Pv1intr,Zso4$Escs,Zso4$Pv1eps,Zso4$Pv1isi,Zso4$Pv1use)
cor.prob(Variabelen)
