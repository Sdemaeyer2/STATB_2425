# ZSO 3 - Statistiek B

#Vraag1
source(file.choose())
kruistabel.kolom(Titanic2$Survived,Titanic2$Gender)
chi.kwadraat.test(table(Titanic2$Survived,Titanic2$Gender))

assocplot(table(Titanic2$Gender,Titanic2$Survived))


#Vraag2
kruistabel.kolom(Titanic2$Survived,Titanic2$Class)
chi.kwadraat.test(table(Titanic2$Survived,Titanic2$Class))

#Vraag 3
Overleefd<-c(rep("Niet overleefd",1490),rep("Wel overleefd",711))
Leeftijd<-c(rep("Kind",52),rep("Volwassen",1438),rep("Kind",57),rep("Volwassen",654))
table(Leeftijd,Overleefd)
addmargins(table(Leeftijd,Overleefd))

kruistabel.rij(Leeftijd,Overleefd)

kruistabel.kolom(Leeftijd,Overleefd)

assocplot(table(Leeftijd,Overleefd))

chi.kwadraat.test(table(Titanic2$Survived,Titanic2$Gender))
chi.kwadraat.test(table(Titanic2$Survived,Titanic2$Class))
