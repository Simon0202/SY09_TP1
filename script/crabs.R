Effectuer dans un premier temps une analyse descriptive des donn�es.
Faire une analyse descriptive g�n�rale des donn�es, en r�pondant par exemple aux questions
suivantes : combien de matches concernent-elles, combien de joueurs, sur quelle p�riode de
temps ? (Ces suggestions ne constituent �videmment pas une liste exhaustive.)
(nature, domaine de d�finition, volume, etc).
On s'interrogera notamment sur les diff�rences de caract�ristiques morphologiques, en particulier selon 
l'esp�ce ou le sexe : semble-t-il possible d'identifier l'une ou l'autre � partir d'une ou plusieurs 
caract�ristiques morphologiques ?
2. Dans un second temps, on �tudiera la corr�lation entre les diff�rentes variables. Quelle en est vraisemblablement
la cause ? Quel traitement est-il possible d'appliquer aux donn�es pour s'affranchir de ce ph�nom�ne ?C

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
######???partie 1 : comparer les caract�ristiques des individus selon le sexe et l'esp�ce par des boxplots#####
#r�partition des individus dans deux variables selon leur sexe
male<-crabs[crabs$sex=="M",]
female<-crabs[crabs$sex=="F",]
#cr�ation d'un data.frame pour comparer les param�tres selon le sexe en boxplot
female_male<-cbind(female,male)
#r�partition des individus dans deux variables selon leur esp�ce
orange<-crabs[crabs$sp=="O",]
blue<-crabs[crabs$sp=="B",]
#cr�ation d'un data.frame pour comparer les param�tres selon le sexe en boxplot
espece<-cbind(orange,blue)
######boxplot selon le sexe######
#comparaison de tous les param�tres
x11()
boxplot(female_male[,c(4,12,5,13,6,14,7,15,8,16)],notch=TRUE,ylab="Caract�ristiques morphologiques (mm)",names=c("Female","Male","Female","Male","Female","Male","Female","Male","Female","Male"),col = c("green","green","purple","purple","yellow","yellow","cyan","cyan","magenta","magenta"))
legend("topleft",inset=.02,legend=c("Frontal Lobe Size","Rear Width","Carapace Length","Carapace Width","Body Depth"),fill=c("green","purple","yellow","cyan","magenta"))
#pour chaque param�tre
x11()
boxplot(crabs[,4]~crabs[,2], notch=TRUE, names=c("Femelle", "Male"), ylab="Frontal Lobe Size (mm)", col=c("pink", "skyblue"), main="FL en fonction du sexe")
x11()
boxplot(crabs[,5]~crabs[,2], notch=TRUE, names=c("Femelle", "Male"), ylab="Rear width (mm)", col=c("pink", "skyblue"), main="RW en fonction du sexe")
x11()
boxplot(crabs[,6]~crabs[,2], notch=TRUE, names=c("Femelle", "Male"), ylab="Carapace length (mm)", col=c("pink", "skyblue"),main="CL en fonction  du sexe")
x11()
boxplot(crabs[,7]~crabs[,2], notch=TRUE, names=c("Femelle", "Male"), ylab="Carapace width (mm", col=c("pink", "skyblue"),main="CW en fonction du sexe")
x11()
boxplot(crabs[,8]~crabs[,2], notch=TRUE, names=c("Femelle", "Male"), ylab="Body depth (mm)", col=c("pink", "skyblue"),main="BD en fonction du sexe")
######boxplot selon l'esp�ce#####
x11()
boxplot(espece[,c(4,12,5,13,6,14,7,15,8,16)],notch=TRUE,ylab="Caract�ristiques morphologiques (mm)",names=c("Orange","Bleu","Orange","Bleu","Orange","Bleu","Orange","Bleu","Orange","Bleu"),col = c("green","green","purple","purple","yellow","yellow","cyan","cyan","magenta","magenta"))
legend("topleft",inset=.02,legend=c("Frontal Lobe Size","Rear Width","Carapace Length","Carapace Width","Body Depth"),fill=c("green","purple","yellow","cyan","magenta"))
#pour chaque param�tre
x11()
boxplot(crabs[,4]~crabs[,1], notch=TRUE, names=c("Bleu", "Orange"), ylab="Frontal Lobe Size (mm)", col=c("blue", "orange"),main="FL en fonction des esp�ces")
x11()
boxplot(crabs[,5]~crabs[,1], notch=TRUE, names=c("Bleu", "Orange"), ylab="Rear width (mm)", col=c("blue", "orange"), main="RW en fonction des esp�ces")
x11()
boxplot(crabs[,6]~crabs[,1], notch=TRUE, names=c("Bleu", "Orange"), ylab="Carapace length (mm)", col=c("blue", "orange"),main="CL en fonction des esp�ces")
x11()
boxplot(crabs[,7]~crabs[,1], notch=TRUE, names=c("Bleu", "Orange"), ylab="Carapace width (mm", col=c("blue", "orange"),main="CW en fonction des esp�ces")
x11()
boxplot(crabs[,8]~crabs[,1], notch=TRUE, names=c("Bleu", "Orange"), ylab="Body depth (mm)", col=c("blue", "orange"), main="BD en fonction des esp�ces")
#comparer les moyennes de chaque param�tre female vs male
#moy.male<-colMeans(male[,c(4:8)])
#moy.female<-colMeans(female[,c(4:8)])
#plot(moy.male,type = "b",col="blue",xlab = "parametres",ylab = "moyenne des parametres")
#lines(moy.female,type = "b",col="red")

#####partie 2: correlation entre les variables#####
#tableau de corr�lation entre les variables
print(cor(crabs[4:8]),digits=2)
#graphique matriciel en fonction du sexe
crabs1<-crabs
crabs1$color<-"black"
crabs1$color[crabs1$sex=="F"]<-"pink"
crabs1$color[crabs1$sex=="M"]<-"skyblue"
plot(crabs1[4:8],col=crabs1$color,main="En fonction du sexe")
#graphique matriciel en fonction de l'esp�ce
crabs2<-crabs
crabs2$color<-"black"
crabs2$color[crabs2$sp=="B"]<-"blue"
crabs2$color[crabs2$sp=="O"]<-"orange"
plot(crabs2[4:8],col=crabs2$color,main="En fonction de l'esp�ce")

#transformation des donn�es: division de chaque param�tre par la somme de tous les param�tres pour chaque individu
sum <- apply(crabsquant,1,sum)
crabs_norme<-cbind(crabs,sum)
crabs_norme$FL <- crabs_norme$FL/crabs_norme$sum
crabs_norme$RW <- crabs_norme$RW/crabs_norme$sum
crabs_norme$CL <- crabs_norme$CL/crabs_norme$sum
crabs_norme$CW <- crabs_norme$CW/crabs_norme$sum
crabs_norme$BD <- crabs_norme$BD/crabs_norme$sum
#####etude des corr�lations avec les donn�es transform�es#####
#tableau de corr�lation entre les variables
print(cor(crabs_norme[4:8]),digits=2)
#graphique matriciel en fonction du sexe
crabs3<-crabs_norme
crabs3$color<-"black"
crabs3$color[crabs3$sex=="F"]<-"pink"
crabs3$color[crabs3$sex=="M"]<-"skyblue"
plot(crabs3[4:8],col=crabs3$color,main="En fonction du sexe")
#graphique matriciel en fonction de l'esp�ce
crabs4<-crabs_norme
crabs4$color<-"black"
crabs4$color[crabs4$sp=="B"]<-"blue"
crabs4$color[crabs4$sp=="O"]<-"orange"
plot(crabs4[4:8],col=crabs4$color,main="En fonction de l'esp�ce")