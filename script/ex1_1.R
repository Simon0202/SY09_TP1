notes <- read.csv("donnees/sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
                         ordered=T)

#Strategie d'etude
"
1)
Nature --> Qualitative / Quantitative ?

domaine de définition --> V_x = nombre de modalités

volume --> effectifs 

Etude statistique descriptive
  Etendue / etude inter quartile etc....

2)
 Il faut les normaliser afin d'avoir un jeu de données homogène et étudier les liens 
"

#Nature de la variable, domaine de définition, volume et stat descriptive pour chaque column
#nom
size <- length(notes)

for(i in 0:size){
  print(names(notes[i]))
  print(class(notes[i]))
  print(summary(notes[i],maxsum = 13))
}

notes <- na.omit(notes)
# Plotting different information
plot_notes <- function()
{
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(reshape2)
  # Data representation
  p0 <- ggplot(notes, aes(x=note.totale)) + geom_histogram(aes(fill= ..count..)) + scale_fill_gradient("Count", low="green", high="red") + ggtitle("Grades density")
  p1 <- ggplot(notes, aes(x=specialite, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Speciality influence")
  p2 <- ggplot(notes, aes(x=dernier.diplome.obtenu, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Last diploma influence")
  p3 <- ggplot(notes, aes(x=niveau, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Level influence")
  p4 <- ggplot(notes, aes(x=correcteur.final, note.totale)) + geom_boxplot(outlier.colour="red", outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + ggtitle("Marker influence")
  grid.arrange(p0, p1, p3, p2, p4, ncol=3, top="Notes plots")
}
