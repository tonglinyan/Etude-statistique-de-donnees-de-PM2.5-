#Importation des données
data = read.table("~/GM3/LFP_PM2_5.csv", sep=";", dec=".", header=TRUE, as.is=TRUE)
data = as.matrix(data)

#Affichage des données et régression linéaire
n = dim(data)[1]
png(file = "tendance.png", width = 600, height = 400)
plot(data[,2], pch=".", main = "Tendance (2015-2019)", sub = "Les années sont séparées par les droites verticales", xlab = "Données", ylab = "Concentrations de PM2.5 (μg/m^3)")
fit = lsfit(1:n, data[,2])
abline(fit, col = "blue")
abline(v=1, col = "red")
text(1, 100, labels =  2015, pos = 4, col = "red")
abline(v=8761, col = "red")
text(8761, 100, labels =  2016, pos = 4, col = "red")
abline(v=17545, col = "red")
text(17545, 100, labels =  2017, pos = 4, col = "red")
abline(v=26305, col = "red")
text(26305, 100, labels =  2018, pos = 4, col = "red")
abline(v=35065, col = "red")
text(35065, 100, labels =  2019, pos = 4, col = "red")
dev.off()

#Construction de la matrice qui contient les concentrations journalières avec les jours en ligne et les heures en colonne 
n2 = n/24
mat = matrix(nrow = n2, ncol = 24)
for(i in 1:n2){
  for(j in 1:24){
    mat[i,j] = as.double(data[(i-1)*24+j,2])
  }
}

#Résumé du résultat (min quantile(1/4) medianne moyenne quantile(3/4) max NAs) de chaque heure
matHeure = apply(mat, 2, summary)
png(file = "linechart_moyenne.png", width = 600, height = 400)
plot(matHeure[4,], type = "o", col = 'black', pch = 16, main = "Moyennes des concentrations horaires de PM2.5", xlab = "Heure", ylab = "Concentrations (μg/m^3)")
abline(h = 9:13, v = 1:24, col = "lightgray", lty = 3)
dev.off()

ecartTypeHeure = apply(mat, 2, sd, na.rm = TRUE)
png(file = "linechart_ecarttype.png", width = 600, height = 400)
plot(ecartTypeHeure,type = "o", pch = 16, main = "Ecart-types des concentrations horaires de PM2.5", xlab = "Heure", ylab = "Ecart-types (μg/m^3)")
abline(h = 9:13, v = 1:24, col = "lightgray", lty = 3)
dev.off()

png(file = "boxplot.png", width = 600, height = 400)
boxplot(mat, varwidth = T, na.rm = TRUE, main = "Boîtes à moustaches des concentrations horaires de PM2.5", xlab = "Heure", ylab = "Concentrations (μg/m^3)")
dev.off()

#Calcul de la moyenne de chaque jour
vectJour = apply(mat, 1, mean, na.rm = TRUE)
png(file = "linechart_moy_jour.png", width = 600, height = 400)
plot(vectJour, type = "l", main = "Moyennes des concentrations journalières de PM2.5 (2015-2019)", sub = "Les années sont séparées par les droites verticales", xlab = "Jours", ylab = "Concentrations (μg/m^3)")
fit2 = lsfit(1:1826, mat)
abline(fit2, col = "blue")
abline(v =1, col = "red")
text(1, 70, labels =  2015, pos = 4, col = "red")
abline(v =366, col = "red")
text(366, 70, labels =  2016, pos = 4, col = "red")
abline(v =732, col = "red")
text(732, 70, labels =  2017, pos = 4, col = "red")
abline(v =1097, col = "red")
text(1097, 70, labels =  2018, pos = 4, col = "red")
abline(v =1462, col = "red")
text(1462, 70, labels =  2019, pos = 4, col = "red")
dev.off()

matJour = matrix(nrow = 1826, ncol = 2)
matJour[,1] = 1:1826
matJour[,2] = vectJour

#Distribution des concentrations journalières
frequence = c(0,0,0,0)
for (i in 1:1826)
  {if (is.na(vectJour[i]))
    {frequence[4] = frequence[4] + 1}
  else if (vectJour[i]<=20)
    {frequence[1] = frequence[1] + 1}
  else if (vectJour[i]<=25)
    {frequence[2] = frequence[2] + 1}
  else
    frequence[3] = frequence[3] + 1
}

png(file = "circulaire")
labels=c("< 20(μg/m^3)", "20 - 25(μg/m^3)", "> 25(μg/m^3)", "non valide")
percent = round(frequence/sum(frequence)*100)
pie(frequence, paste(percent,"%"), col = rainbow(length(labels)), main = "Distribution des concentrations journalières de PM2.5(μg/m^3)")
legend("bottomleft", labels, pch = 16, col=rainbow(length(labels)),bg ="white")
dev.off()

#Fonction qui permet de vérifier si un jour est valide
JourValide = function(V, n) {
  temp = V[!is.na(V)]
  return (length(temp) >= 0.75*n)
}

#Retirer les lignes non valides
X = matrix(nrow = 1548, ncol = 24)
compteur = 0
for (i in 1:n2){
  if (JourValide(mat[i,],24)){
    compteur = compteur + 1
    X[compteur,] = mat[i,]
  }  
}

#Calcul de la moyenne de chaque jour valide
vectJourVal = apply(X, 1, mean, na.rm = TRUE)
png(file = "linechart_moy_jourVal.png", width = 600, height = 400)
plot(vectJourVal, type = "l", main = "Moyennes des concentrations journalières valides de PM2.5", sub = "Les années sont séparées par les droites verticales", xlab = "Jours", ylab = "Concentrations (μg/m^3)")
fit3 = lsfit(1:1548, X)
abline(fit3, col = "blue")
abline(v = 1, col = "red")
text(1, 70, labels =  2015, pos = 4, col = "red")
abline(v = 341, col = "red")
text(341, 70, labels =  2016, pos = 4, col = "red")
abline(v = 670, col = "red")
text(678, 70, labels =  2017, pos = 4, col = "red")
abline(v = 1009, col = "red")
text(1009, 70, labels =  2018, pos = 4, col = "red")
abline(v = 1205, col = "red")
text(1205, 70, labels =  2019, pos = 4, col = "red")
dev.off()

ecartTypeJour = apply(X, 1, sd, na.rm = TRUE)
png(file = "linechart_ECT_jourVal.png", width = 600, height = 400)
plot(ecartTypeJour, type = "l", , main = "ET des concentrations journalières valides de PM2.5 (2015-2019)", sub = "Les années sont séparées par les droites verticales", xlab = "Jours", ylab = "Ecart-types (μg/m^3)")
abline(v = 1, col = "red")
text(1, 20, labels =  2015, pos = 4, col = "red")
abline(v = 341, col = "red")
text(341, 20, labels =  2016, pos = 4, col = "red")
abline(v = 670, col = "red")
text(678, 20, labels =  2017, pos = 4, col = "red")
abline(v = 1009, col = "red")
text(1009, 20, labels =  2018, pos = 4, col = "red")
abline(v = 1205, col = "red")
text(1205, 20, labels =  2019, pos = 4, col = "red")
dev.off()

#saisionnier
#Dans saisons, le printemps va de 1 à 465, l'été de 466 à 935, l'automne de 936 à 1385 et l'hiver de 1386 à 1826
Printemps = list(79:171, 445:537, 810:902, 1175:1267, 1540:1632)
Ete = list(172:265, 538:631, 903:996, 1268:1361, 1633:1726)
Automne = list(266:355, 632:721, 997:1086, 1362:1451, 1727:1816)
Hiver = list(1:78, 356:444, 722:809, 1087:1174, 1452:1539, 1817:1826)
saisons = matrix(nrow = 1826, ncol = 24)
compteur2 = 0
for(i in Printemps){
  for (j in i){
    compteur2 = compteur2 + 1
    saisons[compteur2,] = mat[j,]  
  }
}
for(i in Ete){
  for (j in i){
    compteur2 = compteur2 + 1
    saisons[compteur2,] = mat[j,]  
  }
}
for(i in Automne){
  for (j in i){
    compteur2 = compteur2 + 1
    saisons[compteur2,] = mat[j,]  
  }
}
for(i in Hiver){
  for (j in i){
    compteur2 = compteur2 + 1
    saisons[compteur2,] = mat[j,]  
  }
}

#Retirer les jours non valides dans saisons
#Dans saisonsValide, le printemps va de 1 à 366, l'été de 367 à 792, l'automne de 793 à 1204 et l'hiver de 1205 à 1548 
saisonsValide = matrix(nrow = 1548, ncol = 24)
compteur3 = 0
for (i in 1:n2){
  if (JourValide(saisons[i,],24)){
    compteur3 = compteur3 + 1
    saisonsValide[compteur3,] = saisons[i,]
  }
}

#Moyenne des concentrations horaires par saison
moyennePrintemps = apply(saisonsValide[1:366,], 2, mean, na.rm = TRUE)
moyenneEte = apply(saisonsValide[367:792,], 2, mean, na.rm = TRUE)
moyenneAutomne = apply(saisonsValide[793:1024,], 2, mean, na.rm = TRUE)
moyenneHiver = apply(saisonsValide[1205:1548,], 2, mean, na.rm = TRUE)

png(file = "moyenneHorairesSaison.png", width = 600, height = 400)
plot(moyennePrintemps, col = "black", type="o", pch = 16, ylim=c(6,19), main = "Moyennes des concentrations horaires de PM2.5 par saison", xlab = "Heure",ylab ="Concentrations (μg/m^3)")
lines(moyenneEte, type = "o", pch = 16, col = "red")
lines(moyenneAutomne, type = "o", pch = 16, col = "green")
lines(moyenneHiver, type = "o", pch = 16, col = "blue")
legend("topleft", c("Printemps", "Eté","Automne","Hiver"), lty = 1, col=c("black","red","green","blue"),bg ="white")
dev.off()

#Construction de saisonsCol qui contient une saison par colonne
saisonsCol = matrix(nrow = 10224, ncol = 4)
for (i in 1:366){
  for (j in 1:24){
    saisonsCol[24*(i-1)+j,1] = saisonsValide[i,j]
  }
}

for (i in 367:792){
  for (j in 1:24){
    saisonsCol[24*(i-367)+j,2] = saisonsValide[i,j]
  }
}

for (i in 793:1204){
  for (j in 1:24){
    saisonsCol[24*(i-793)+j,3] = saisonsValide[i,j]
  }
}

for (i in 1205:1548){
  for (j in 1:24){
    saisonsCol[24*(i-1205)+j,4] = saisonsValide[i,j]
  }
}

colnames(saisonsCol) = c("Printemps", "Ete", "Automne", "Hiver")
png(file = "boxplotSaisons.png")
boxplot(saisonsCol, varwidth = T, na.rm = TRUE, main = "Boîtes à moustaches des concentrations saisonnières de PM2.5", xlab = "Saisons", ylab = "Concentrations (μg/m^3)")
dev.off()

#Construction de la matrice XCol qui contient une année par colonne 
XCol = matrix(nrow = 8256, ncol = 5)
for (i in 1:340){
  for (j in 1:24){
    XCol[(i-1)*24+j, 1] = X[i,j]
  }
}

for (i in 341:669){
  for (j in 1:24){
    XCol[(i-341)*24+j, 2] = X[i,j]
  }
}

for (i in 670:1008){
  for (j in 1:24){
    XCol[(i-670)*24+j, 3] = X[i,j]
  }
}

for (i in 1009:1204){
  for (j in 1:24){
    XCol[(i-1009)*24+j, 4] = X[i,j]
  }
}

for (i in 1205:1548){
  for (j in 1:24){
    XCol[(i-1205)*24+j, 5] = X[i,j]
  }
}

colnames(XCol) = c("2015", "2016", "2017", "2018", "2019") 
png(file = "boxplotAnnées.png")
boxplot(XCol, varwidth = T, na.rm = TRUE, main = "Boîtes à moustaches des concentrations annuelles de PM2.5", xlab = "Années", ylab = "Concentrations (μg/m^3)")
dev.off()

data2 = data[,2]
data2 = data2[!is.na(data2)]
data2 = as.numeric(data2)
png(file = "histAnnées")
hist(data2, main = "Histogramme des concentrations de PM2.5 (2015-2019)", xlab = "Concentrations (μg/m^3)", ylab = "Fréquences")
dev.off()

#Construction des matrices contenant les jours de la semaine
Lundi = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Lundi[i,] = mat[5+(i-1)*7,]}
Mardi = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Mardi[i,] = mat[6+(i-1)*7,]}
Mercredi = matrix(nrow = 261, ncol = 24)
for(i in 1:260){Mercredi[i,] = mat[i*7,]}
Jeudi = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Jeudi[i,] = mat[1+(i-1)*7,]}
Vendredi = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Vendredi[i,] = mat[2+(i-1)*7,]}
Samedi = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Samedi[i,] = mat[3+(i-1)*7,]}
Dimanche = matrix(nrow = 261, ncol = 24)
for(i in 1:261){Dimanche[i,] = mat[4+(i-1)*7,]}

#Construction des matrices contenant les jours de la semaine valides
LundiVal = matrix(nrow = 216, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Lundi[i,],24)){
    compteur = compteur + 1
    LundiVal[compteur,] = Lundi[i,]
  }
}
MardiVal = matrix(nrow = 215, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Mardi[i,],24)){
    compteur = compteur + 1
    MardiVal[compteur,] = Mardi[i,]
  }
}
MercrediVal = matrix(nrow = 218, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Mercredi[i,],24)){
    compteur = compteur + 1
    MercrediVal[compteur,] = Mercredi[i,]
  }
}
JeudiVal = matrix(nrow = 220, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Jeudi[i,],24)){
    compteur = compteur + 1
    JeudiVal[compteur,] = Jeudi[i,]
  }
}
VendrediVal = matrix(nrow = 224, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Vendredi[i,],24)){
    compteur = compteur + 1
    VendrediVal[compteur,] = Vendredi[i,]
  }
}
SamediVal = matrix(nrow = 230, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Samedi[i,],24)){
    compteur = compteur + 1
    SamediVal[compteur,] = Samedi[i,]
  }
}
DimancheVal = matrix(nrow = 225, ncol = 24)
compteur = 0 
for (i in 1:261){
  if (JourValide(Dimanche[i,],24)){
    compteur = compteur + 1
    DimancheVal[compteur,] = Dimanche[i,]
  }
}

#Construction de la matrice JourCol contenant les données d'un jour valide par colonne
JourCol = matrix(nrow = 5520, ncol = 7)
for (i in 1:216){
  for (j in 1:24){
    JourCol[(i-1)*24+j,1] = Lundi[i,j]
  }
}
for (i in 1:215){
  for (j in 1:24){
    JourCol[(i-1)*24+j,2] = Mardi[i,j]
  }
}
for (i in 1:218){
  for (j in 1:24){
    JourCol[(i-1)*24+j,3] = Mercredi[i,j]
  }
}
for (i in 1:228){
  for (j in 1:24){
    JourCol[(i-1)*24+j,4] = Jeudi[i,j]
  }
}
for (i in 1:224){
  for (j in 1:24){
    JourCol[(i-1)*24+j,5] = Vendredi[i,j]
  }
}
for (i in 1:230){
  for (j in 1:24){
    JourCol[(i-1)*24+j,6] = Samedi[i,j]
  }
}
for (i in 1:225){
  for (j in 1:24){
    JourCol[(i-1)*24+j,7] = Dimanche[i,j]
  }
}

colnames(JourCol) = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
png(file = "boxplotJours.png", width = 600, height = 400)
boxplot(JourCol, varwidth = T, na.rm = TRUE, main = "Boîtes à moustaches des concentrations journalières de PM2.5", xlab = "Jours", ylab = "Concentrations (μg/m^3)")
dev.off()

