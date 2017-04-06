library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]
acp.crabs<-princomp(crabsquant)
#valeurs propres
vp.crabs<-(acp.crabs$sdev)^2
#matrice des composantes principales
C.crabs<-acp.crabs$scores
#représentation des résultats sur le premier plan factoriel
x11()
biplot(acp.crabs,main = "ACP : premier plan factoriel, sans traitement")
x11()
plot(acp.crabs,main="Pourcentage d'inertie expliquée, sans traitement")
#transformation des données: division de chaque paramètre par la somme de tous les paramètres pour chaque individu
sum <- apply(crabsquant,1,sum)
crabs_norme<-cbind(crabs,sum)
crabs_norme$FL <- crabs_norme$FL/crabs_norme$sum
crabs_norme$RW <- crabs_norme$RW/crabs_norme$sum
crabs_norme$CL <- crabs_norme$CL/crabs_norme$sum
crabs_norme$CW <- crabs_norme$CW/crabs_norme$sum
crabs_norme$BD <- crabs_norme$BD/crabs_norme$sum
acp.crabs_norme<-princomp(crabs_norme[,c(4:8)])
x11()
biplot(acp.crabs_norme,main = "ACP : premier plan factoriel, avec traitement")
x11()
plot(acp.crabs_norme,main="Pourcentage d'inertie expliquée, avec traitement")