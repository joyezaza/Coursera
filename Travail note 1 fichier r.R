#importation
recup_data<-read.table("C:/Users/joyeb/Downloads/recup.txt",header=TRUE)
head(recup_data)
View(recup_data)
# récupération des données des temps de récupération après consommation
#du placebo, dans le jeu de donnée recup_data et visualisation
#des 6 premières lignes
data.placebo<-recup_data[recup_data$Type=="placebo",]
head(data.placebo)
# récupération des données des temps de récupération après consommation
#du Médicament, dans le jeu de donnée recup_data et
#visualisation des 6 premières lignes
data.med<-recup_data[recup_data$Type=="med",]
head(data.med)
# combiner les deux tableaux dans une même data frame et
#visionner les 6 premières lignes
temps_dataframe <-data.frame(cbind(data.med$Temps,data.placebo$Temps)) 
head(temps_dataframe)
# Renommer les colonnes et visionner les 6 premières lignes
colnames(temps_dataframe) <- c("Temps_med", "Temps_placebo")
head(temps_dataframe)
# ajouter et calculer la colonne des différences (Diff)
temps_dataframe$Diff<-(temps_dataframe$Temps_placebo - 
                         temps_dataframe$Temps_med)

# Afficher les 6 premières lignes
head(temps_dataframe)

#calculer les résidus et visionner les 5 premières lignes
residus<-temps_dataframe$Diff-mean(temps_dataframe$Diff)
head(residus)
#Installation du package nortest

library(nortest)
# le test
ad.test(residus)
#Le graphique quantile-quantile (Q-Q plot)

qqnorm(residus, main = "Graphique quantile-quantile",
ylab = "Quantiles de l'échantillon", xlab = "Quantiles théoriques")

#la droite théorique 
qqline(residus)
# t-test unilatéral à droite de la différence 
t.test(temps_dataframe$Temps_placebo,temps_dataframe$Temps_med,
       paired = TRUE, alternative = "greater")



