Pima <- read.csv("donnees/Pima.csv", header=T)
Pima$z <- factor(Pima$z)

# npreg: nombre de grossesses
# glu: taux plasmatique de glucose
# bp: pression artérielle diastolique
# skin: épaisseur du pli cutané au niveau du triceps
# bmi: indice de masse corporelle
# ped: fonction de pedigree du diabète (mesure de l’influence génétique espérée des proches,
# affectés ou non par le diabète, sur le risque éventuel du sujet)
# age: âge
# z: catégorie (diabétique si z = 1)

summary(Pima)
Pima$z <- as.numeric(Pima$z)
cor(Pima$z, 2*Pima$glu + 4*Pima$npreg + Pima$age)

# ACP : 
p <- princomp(Pima[-8], cor=T)
summary(p)
screeplot(p)
biplot(p, scale=0, col = c("gray", "red"))
