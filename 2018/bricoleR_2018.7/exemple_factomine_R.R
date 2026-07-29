#############################################################################################################
######### Bricole'R script de presentation Factomine R sur un exemple de données macro INSEE  ###############
#########                                 seance n° 2 20 juillet  2017                        ###############
########                                  séances du 8 et 22 août 2018                        ############### 
#############################################################################################################

### inital steps
# libraries loading
library(graphics)
library(ggplot2)
#library(lattice)
#library(ConsRank)
#library(pmr)
library(FactoMineR)
library(factoextra)
#library(vcd)
library(scatterplot3d)
#library(arules)

#### definition du fichier de donnees R
save(factomineR, file = "c:/bricoleR/exemple_factomineR.rda")
setwd(dir = "c:/bricoleR")
load("c:/bricoleR/exemple_factomineR.rda")

### Exemple d'ACP
# declaration des variables actives et illustratives:
actives <- factomineR[,1:14]
illustratives <- factomineR[,15:43]
summary(actives)
summary(illustratives)
### declaration de l'ACP:
#rm(res.pca)
res.pca <- PCA(factomineR, ncp=14, quanti.sup = 15:43)
summary(res.pca)
### exploration de contenu de la liste de res.pca:
# on peut afficher le graphique normal en taille réduite par cex=% taille défaut, paramètre de base des graphiques
plot.PCA(res.pca, choix="var",cex=0.7)
# on peut afficher le cercle de corrélations avec seulement les actives:
plot.PCA(res.pca, choix="var",invisible = "quanti.sup", cex=0.7)

### maintenant avec les individus:
### une petit ajout creation d'une variable "année" notée year
year <- seq(1959:2016)+1958 ### seq créé une séquence d'entiers entre deux bornes quelconques. 
# Pour retrouver mes années je dois ajouter l'année précédent le premier terme de la séquence.
year
### représentons les individus, (points année) dans le plan princpal:
plot.PCA(res.pca, choix="ind",cex=0.7)
text(res.pca$ind$coord[,1], res.pca$ind$coord[,2], labels=year, cex=0.7)
### mais il a laissé les numéros d'observations !!! normal = commande label ="ind" actif
plot.PCA(res.pca, choix="ind",cex=0.7, label="none")
text(res.pca$ind$coord[,1], res.pca$ind$coord[,2], labels=year, cex=0.7)
### cette fois il a bien enlevé les labels d'observations !
plot.PCA(res.pca, choix="ind",cex=0.7, label="none")
text(res.pca$ind$coord[,1], res.pca$ind$coord[,2], labels=year, cex=0.7, col ="red")

plot(year, res.pca$ind$coord[,1], col ="red")


### usage de FVIZ dans factoextra
## on peut aussi passer en graphique avancés avec factoextra:
fviz_pca_ind(res.pca, col.ind = "blue")
fviz_pca_ind(res.pca, axes=c(2,3), col.ind = "blue")
fviz_pca_ind(res.pca, axes=c(1,3), col.ind = "blue")

### utiliser les données pour une régression
attach(factomineR) # attacher un jeu de données == accès direct aux noms de variables (opposé == detach(mydata))
## construction d'un cadre de donné car concaténation de colonnes:
reg <- cbind(res.pca$ind$coord[,3], Année, factomineR[,43]) # cbind == jointure par colonne ; on prend des objets de deux endroits:
# l'objet res.pca, et le cadre factomineR: on y accède par le nom de colonne (Année <== par attach) ou la référence de colonne [43] de factomineR

#rm(reg) == commande utile pour effacer (remove) un objet
mode(reg) # vérifie le mode soit le type ou le mode de stockage de l'objet
### création d'un objet modèle linéaire lm() 
mod1 <- lm(reg[,1] ~ reg[,2] + reg[,3], data=reg) # noter l'équation et le jeu de données du modèle == l'objet reg créé plus haut
### résultats plus détaillés: oui le modèle est pourri !
summary(mod1) # cas intéressant de R²bar négatif !
### affichage du contenu du modèle
mod1
### graphiques de diagnostic du modèle:
plot(mod1)


fviz_pca_var(res.pca)
fviz_pca_var(res.pca, axes=c(2,3))
fviz_pca_var(res.pca, axes=c(1,3))





### affichons les valeurs propres:
res.pca$eig
### son mode:
mode(res.pca$eig)
actives$walim
plot(x = factomineR$Année[37:58], y = factomineR$walim[37:58], type="b")

### les coordonnées des variables sur les axes principaux:
res.pca$var
### mais on peut très bien n'afficher que les contributions:
res.pca$var$contrib
### idem donc pour les individus mais cette fois dans la variable la partie individus == "ind":
res.pca$ind
### remarquons que pour les individus, nous avons une données en plus: la distance à l'origine, $dist:
res.pca$ind$dist
### on peut afficher les cosinus carrés des individus:
res.pca$ind$cos2

### les cosinus carrés s'apprécient relativement à la distance à l'origine.
### à l'aide de ces éléments nous pouvons construire un objet list contenant l'année la distance à l'origine et 
illustratives[,1]
res.pca$ind$coord[,1:3]
res.pca$ind$coord.ellipse[,1:3]
res.pca$quanti.sup$coord[,1:3]

### cette partie de l'objet nous pouvons créer des graphiques par exemple des nuages de points:
### les années dans le plan principal (1,2)
axe1 <- res.pca$ind$coord[,1]
axe2 <- res.pca$ind$coord[,2]
axe3 <- res.pca$ind$coord[,3]
year <- illustratives[,1]

### obtention des v-test et des c-test pour les variables actives et illustratives
### attention !!! ne pas mettre le signe "-" dans un nom de variable ==>  cause erreur ! préférer le trait de soulignement
### c-test des variables quantitatives actives
c_test <- sqrt(nrow(year)-2)*res.pca$var$cor/sqrt(1-res.pca$var$cor)
c_test
### c-test des variables quantitatives illustratives
c_test_quanti_sup <- sqrt(nrow(year)-2)*res.pca$quanti.sup$cor/sqrt(1-res.pca$quanti.sup$cor)
c_test_quanti_sup

### calcul des c-test sur les cos² coordonnées des années
c_test_cos2_ind <- sqrt(nrow(year)-2)*res.pca$ind$cos2/sqrt(1-res.pca$ind$cos2)
c_test_cos2_ind
c_test_cos2_ind[,1:3]
### calcul de c-test pour plusieurs axes simultanéments:
cos2_12 <- res.pca$ind$cos2[,1]+res.pca$ind$cos2[,2]
cos2_23 <- res.pca$ind$cos2[,2]+res.pca$ind$cos2[,3]
cos2_123 <- cos2_12+res.pca$ind$cos2[,3]
c_test_cos12_ind <- sqrt(nrow(year)-2)*cos2_12/sqrt(1-cos2_12)
c_test_cos12_ind
c_test_cos123_ind <- sqrt(nrow(year)-2)*cos2_123/sqrt(1-cos2_123)
c_test_cos123_ind

### creation de variables qualitatives de périodisation des séries.
### périodisation axe 1--2
period12 <- cut(factomineR$Année, c(1959,1972,1983,2001,Inf), right=FALSE,labels=c("happy days","lyric days","hard days","hard times"))
period12

### autre procédé
peraxe12 <- categorize(factomineR$Année, breaks=c(1972,1982,2001),quantile=FALSE, )
peraxe12

### graphiques standards: plans des années.
### nuage de point du plan principal, via un graphique par la commande plot de R:
plot(x = res.pca$ind$coord[,1],y=res.pca$ind$coord[,2],type="b", main="Consommation des ménages dans le plan 1--2",
     xlab="axe 1", ylab="axe 2", panel.first=grid(), col=period12)
text(res.pca$ind$coord[,1],res.pca$ind$coord[,2],labels=illustratives$Année, pos = 1, cex = 0.5, col="red")
# nota bene: pos = 1 == en dessous des points ; 
### plan 1--3
plot(x = res.pca$ind$coord[,1],y=res.pca$ind$coord[,3],type="b", main="Consommation des ménages dans le plan 1--3",
     xlab="axe 1", ylab="axe 3", panel.first=grid())
text(res.pca$ind$coord[,1],res.pca$ind$coord[,3],labels=illustratives$Année, pos = 1, cex = 0.5, col="red")
### le plan 1--3 semble capter l'effet du choc pétrolier de 1973 et la décennie qui suit jusqu'au contrechoc de 1986
### plan 2--3
plot(x = res.pca$ind$coord[,2],y=res.pca$ind$coord[,3],type="b", main="Consommation des ménages dans le plan 2--3",
     xlab="axe 2", ylab="axe 3", panel.first=grid())
text(res.pca$ind$coord[,2],res.pca$ind$coord[,3],labels=illustratives$Année, pos = 1, cex = 0.5, col="red")

### (1) graphiques spécifiques factomineR
### cercles de correlations
### plan principal: sans les variables supplémentaires
plot.PCA(res.pca, choix="var",invisible = "quanti.sup", cex=0.7)
### avec supplémentaires
plot.PCA(res.pca, choix="var", cex=0.7)
### tracé d'ellipses de confiance, pour les nominales supplémentaires (s'il y en a !):
#plotellipses(res.pca)

### plans 1--3 et 2--3 sans les illustratives
plot.PCA(res.pca,axes=c(1,3),choix="var",invisible = "quanti.sup",cex=0.7)
plot.PCA(res.pca,axes=c(2,3),choix="var",invisible = "quanti.sup",cex=0.7)
### avec les illustratives
plot.PCA(res.pca,axes=c(1,3),choix="var",cex=0.8)
plot.PCA(res.pca,axes=c(2,3),choix="var",cex=0.8)
### cela montre la nécessité de mettre un filtre pour sélectionné les élements actifs et illustratifs représentés


### (2) Graphiques génériques de R utilisant les résultats de factomineR
###graphiques factoriels des années par commande plot.
### plan principal
plot(x = res.pca$ind$coord[,1],y = res.pca$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan principal",
     xlab = "axe 1 (61.7%)", ylab = "axe 2 (23.6%)", panel.first = grid())
text(res.pca$ind$coord[,1], res.pca$ind$coord[,2], illustratives$Année, cex= 0.6, pos=2,col="red")
plot(x = res.pca$ind$coord[,1],y = res.pca$ind$coord[,3], type="b", main = "Trajectoire des années dans le plan 1--3",
     xlab = "axe 1 (61.7%)", ylab = "axe 3 (8%)", panel.first = grid())
text(res.pca$ind$coord[,1], res.pca$ind$coord[,3], illustratives$Année, cex= 0.5, pos=2,col="red")
plot(x = res.pca$ind$coord[,2],y = res.pca$ind$coord[,3], type="b", main = "Trajectoire des années dans le plan 2--3",
     xlab = "axe 2 (23.6%)", ylab = "axe 3 (8%)", panel.first = grid())
text(res.pca$ind$coord[,2], res.pca$ind$coord[,3], illustratives$Année, cex= 0.5, pos=2,col="red")
plot(x = res.pca$ind$coord[,3],y = res.pca$ind$coord[,4], type="b", main = "Trajectoire des années dans le plan 3--4",
     xlab = "axe 3 (8%)", ylab = "axe 4 (3%)", panel.first = grid())
text(res.pca$ind$coord[,3], res.pca$ind$coord[,4], illustratives$Année, cex= 0.5, pos=2,col="red")


### on peut créer un facteur et utiliser scatterplot:
### axes et contrib
plot(x = res.pca$ind$coord[,1],y = res.pca$ind$contrib[,1], type="b", main = "Plan coordonnées - CTR axe 1",
     xlab = "axe 1 (61.7%)", ylab = "CTR axe 1", panel.first = grid())
text(res.pca$ind$coord[,1], res.pca$ind$contrib[,1], illustratives$Année, cex= 0.6, pos=2,col="red")
plot(x = res.pca$ind$coord[,2],y = res.pca$ind$contrib[,2], type="b", main = "Plan coordonnées - CTR axe 2",
     xlab = "axe 1 (23.6%)", ylab = "CTR axe 2", panel.first = grid())
text(res.pca$ind$coord[,2], res.pca$ind$contrib[,2], illustratives$Année, cex= 0.6, pos=2,col="red")
plot(x = res.pca$ind$coord[,3],y = res.pca$ind$contrib[,3], type="b", main = "Plan coordonnées - CTR axe 2",
     xlab = "axe 3 (8%)", ylab = "CTR axe 3", panel.first = grid())
text(res.pca$ind$coord[,3], res.pca$ind$contrib[,3], illustratives$Année, cex= 0.6, pos=2,col="red")
### axes et cos2
plot(x = res.pca$ind$coord[,1],y = res.pca$ind$cos2[,1], type="b", main = "Plan coordonnées - COS² axe 1",
     xlab = "axe 1 (61.7%)", ylab = "COS2 axe 1", panel.first = grid())
text(res.pca$ind$coord[,1], res.pca$ind$cos2[,1], illustratives$Année, cex= 0.6, pos=2,col="red")
plot(x = res.pca$ind$coord[,2],y = res.pca$ind$cos2[,2], type="b", main = "Plan coordonnées - COS² axe 2",
     xlab = "axe 1 (23.6%)", ylab = "COS2 axe 2", panel.first = grid())
text(res.pca$ind$coord[,2], res.pca$ind$cos2[,2], illustratives$Année, cex= 0.6, pos=2,col="red")
plot(x = res.pca$ind$coord[,3],y = res.pca$ind$cos2[,3], type="b", main = "Plan coordonnées - COS² axe 2",
     xlab = "axe 3 (8%)", ylab = "COS2 axe 3", panel.first = grid())
text(res.pca$ind$coord[,3], res.pca$ind$cos2[,3], illustratives$Année, cex= 0.6, pos=2,col="red")

### (3) expérimentation: avec scatterplot3D
### on peut même le faire en nuage de point 3D des axes 1 à 3
## coordonnées
scatterplot3d(res.pca$ind$coord[,1],y = res.pca$ind$coord[,2],z=res.pca$ind$coord[,3],
              hihlight3d=TRUE,type="h")
### contributions
scatterplot3d(res.pca$ind$contrib[,1],y = res.pca$ind$contrib[,2],z=res.pca$ind$contrib[,3],
              hihlight3d=TRUE,type="h")
### cos2
scatterplot3d(res.pca$ind$cos2[,1],y = res.pca$ind$cos2[,2],z=res.pca$ind$cos2[,3],
              hihlight3d=TRUE,type="h")
### et l'ensemble par axe !
scatterplot3d(res.pca$ind$coord[,1],y = res.pca$ind$contrib[,1],z=res.pca$ind$cos2[,1],
              hihlight3d=TRUE,type="h")
scatterplot3d(res.pca$ind$coord[,2],y = res.pca$ind$contrib[,2],z=res.pca$ind$cos2[,2],
              hihlight3d=TRUE,type="h")
scatterplot3d(res.pca$ind$coord[,3],y = res.pca$ind$contrib[,3],z=res.pca$ind$cos2[,3],
              hihlight3d=TRUE,type="h")



### essayons un diagramme ternaire pour la trajectoire du plan 1-2-3:
### attention, il faut renormaliser les données sur [0,1]
#require(vcd)
## Rescale each column to range between 0 and 1
#axes1_2_3 <-res.pca$ind$coord[,1:3]
#axes123 <-as.matrix(axes1_2_3)
### normalisation sur intervalle unité des données de la matrice axes123
#axes123 <- apply(axes123, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
#ternaryplot(axes123, dimnames = c("axe 1","axe 2","axe 3"))







