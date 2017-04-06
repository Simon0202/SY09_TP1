notes <- read.csv("sy02-p2016.csv", na.strings="", header=T);
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")
median <- merge(moy.median, std.median)
moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")
final <- merge(moy.final, std.final)
correcteurs <- merge(median, final, all=T)
corr.acp <- correcteurs[-c(2,8),]

#####ACP sur les individus#####
#convertir le data.frame en matrice pour faciliter les calculs
X<-as.matrix(corr.acp[2:5],nrow=6,rcol=4)
#centrage de la matrice
Xc<-t(t(X)-apply(X,2,mean))
rownames(Xc)<-c("Cor1","Cor3","Cor4","Cor5","Cor6","Cor7")
round(Xc,digits=2)
#matrice de variance
Xv<-(1/6)*(t(Xc)%*%Xc)
round(Xv,digits = 2)
#calul des valeurs propres et des vecteurs propres
(resume<-eigen(Xv))
#calcul des pourcentages d'inertie expliquée pour chacun des axes et inertie cumulée
bilan<-data.frame(c(1:4),resume$values,100*resume$values/sum(resume$values))
colnames(bilan)<-c("Axis","Eigen value","Proportion")
bilan$Cummulative<-cumsum(bilan[3])
colnames(bilan[,4])<-"Cummulative"
bilan
#stockage des vecteurs propres dans la matrice U
U<-resume$vectors
colnames(U)<-c("U1","U2","U3","U4")
round(U,digits=2)
#calcul de la matrice des composantes principales
C<-Xc%*%U
rownames(C)<-c("Cor1","Cor3","Cor4","Cor5","Cor6","Cor7")
round(C,digits=2)
#calculer la contribution relative des axes aux individus
cor<-C^2/diag(Xc%*%t(Xc))
print("contribution relative des axes aux individus")
round(cor,digits=2)
#calculer la contribution relative des individus aux axes
ctr<-1/6*C^2/matrix(resume$values,nrow=6,ncol=4,byrow=TRUE)
round(ctr,digits=2)

######représentation des variables######
#calcul des corrélations entre les variables intiales normées et les composantes principales normées 
sigma<-diag(1/(sqrt(5/6*apply(Xc,2,sd)^2)))
vp<-diag(1/sqrt(resume$values))
Dp<-diag(1/6,nrow=6,ncol=6)
cor.var<-sigma%*%t(Xc)%*%Dp%*%C%*%vp
rownames(cor.var)<-c("moy.median","std.median","moy.final","std.final")
round(cor.var,digits=2)

######représentations graphiques######
#représentation des individus dans le premier plan factoriel
x11()
plot(C[,c(1,2)],xlab = "C[,1]",ylab="C[,2]",type="n",main="Représentation des individus dans le premier plan factoriel")
text(C[,c(1,2)],labels=corr.acp$correcteur)
abline(h=0,v=0)
#représentation des individus dans le deuxième plan factoriel
x11()
plot(C[,c(1,3)],xlab = "C[,1]",ylab="C[,3]",type="n",main="Représentation des individus dans le deuxième plan factoriel")
text(C[,c(1,3)],labels=corr.acp$correcteur)
abline(h=0,v=0)

#représentation des variables dans le premier plan factoriel
x11()
plot(-1.5:1.5,-1.5:1.5,xlab = "C[,1]",ylab="C[,2]",type="n", main="Représentation des variables dans le premier plan factoriel")
text(cor.var[,c(1,2)],colnames(correcteurs[-1]))
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)
abline(h=0,v=0)
#représentation des variables dans le deuxième plan factoriel
x11()
plot(-1:1,-1:1,xlab = "Axe 1",ylab="Axe 3",type="n",main="Représentation des variables dans le deuxième plan factoriel")
text(cor.var[,c(1,3)],colnames(correcteurs[-1]))
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)
abline(h=0,v=0)

######reconstitution######
round(C[,1]%*%t(resume$vectors[,1]),digits=2)
round(C[,1]%*%t(resume$vectors[,1])+C[,2]%*%t(resume$vectors[,2]),digits=2)
round(C[,1]%*%t(resume$vectors[,1])+C[,2]%*%t(resume$vectors[,2])+C[,3]%*%t(resume$vectors[,3]),digits=2)
round(C[,1]%*%t(resume$vectors[,1])+C[,2]%*%t(resume$vectors[,2])+C[,3]%*%t(resume$vectors[,3])+C[,4]%*%t(resume$vectors[,4]),digits=2)

######valeurs manquantes######
correcteurs$moy.median[8]<-mean(correcteurs$moy.median,na.rm = TRUE)
correcteurs$std.median[8]<-mean(correcteurs$std.median,na.rm = TRUE)
correcteurs$std.final[2]<-mean(correcteurs$std.final,na.rm = TRUE)
correcteurs$moy.final[2]<-mean(correcteurs$moy.final,na.rm = TRUE)
#acp sur le tableau avec les valeurs manquantes
#convertir le data.frame en matrice pour faciliter les calculs
Y<-as.matrix(correcteurs[2:5],nrow=8,rcol=4)
rownames(Y)<-c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8")
round(Y,digits=2)
#centrage de la matrice
Yc<-t(t(Y)-apply(Y,2,mean))
round(Yc,digits = 2)
#matrice de variance
Yv<-(1/8)*(t(Yc)%*%Yc)
round(Yv,digits = 2)
#calul des valeurs propres et des vecteurs propres
(resume<-eigen(Yv))
#recuperation de la matrice des vecteurs propres
UY<-resume$vectors
round(UY, digits=2)
#calcul de la matrice des composantes principales
CY<-Yc%*%UY
rownames(CY)<-c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8")
round(CY,digits = 2)

#####représentations graphiques#####
#représentation des individus dans le premier plan factoriel
x11()
plot(CY[,c(1,2)],xlab = "C[,1]",ylab="C[,2]",type="n", main="Représentation des individus dans le premier plan factoriel")
text(CY[,c(1,2)],labels=correcteurs$correcteur)
abline(h=0,v=0)
#représentation des individus dans le deuxième plan factoriel
x11()
plot(CY[,c(1,3)],xlab = "C[,1]",ylab="C[,3]",type="n", main="Représentation des individus dans le deuxième plan factoriel")
text(CY[,c(1,3)],labels=correcteurs$correcteur)
abline(h=0,v=0)
