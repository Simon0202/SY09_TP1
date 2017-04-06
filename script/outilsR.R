M = matrix(c(6.0, 6.0, 5.0, 5.5, 8.0, 8.0, 8.0, 8.0, 8.0, 9.0, 6.0, 7.0, 11.0, 9.5, 11.0,
             14.5, 14.5, 15.5, 15.0, 8.0, 14.0, 14.0, 12.0, 12.5, 10.0, 11.0, 10.0, 5.5, 7.0, 13.0,
             5.5, 7.0, 14.0, 11.5, 10.0,13.0, 12.5, 8.5, 9.5,  12.0, 9.0, 9.5, 12.5, 12.0, 18.0),
           nrow = 9, byrow = T)
rownames(M) = c("jean", "aline", "annie", "monique", "didier",
                "andré", "pierre", "brigitte", "evelyne")
colnames(M) = c("math", "scie", "fran", "lati","d-m")

#centrage
(X <- scale(M, center=T, scale=F))

#Matrice de variance 
(V <- (1/9)*(t(X)%*%X))

#Axes principaux d’inertie
U <- eigen(V)

#valPropre
(ValeursPropres <- U$values)

#vect propres
(VecteursPropres <- U$vectors)

#Comp Princ
(C <- X%*%U$vectors)


RP <- princomp(X)
biplot(RP, ylab = "axe 2", xlab = "axe 1", var.axes = FALSE, main="ACP :Exemple des notes"
)
abline(h=0,v=0)

#Cercle de corrélation
cor <- cor(M, C)


#1er plan
plot(cor, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), col = "blue",
     pch=c(16), main="ACP : Exemple de notes dans le premier plan factoriel")
text(cor, pos = 1, labels = c("math", "scie", "fran", "lati","d-m"))
abline(h=0,v=0)
symbols(0, 0, circle = 1, inches = F, add = T)
#2ème plan
cor2 <- cbind(cor[,1], cor[,3])
plot(cor2, xlab = "axe 1", ylab = "axe 2", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     col = "blue", pch=c(16), main="ACP : Exemple de notes dans le deuxième plan factoriel")
text(cor2, pos = 1, labels = c("math", "scie", "fran", "lati","d-m"))
abline(h=0,v=0)
symbols(0, 0, circle = 1, inches = F, add = T)

summary(RP)