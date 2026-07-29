############################################################################################################
#########                                                                                          ######### 
#########                         Script atelier bricole 'R 2018.2                                 ######### 
#########                                                                                          ######### 
############################################################################################################

#reference
# advanced R:
# https://adv-r.hadley.nz/


### chargement des librairies
library(FactoMineR)
library(ggplot2)
# à explorer d'urgence:
# https://github.com/kassambara/factoextra/issues/27
library(factoextra)
library(kernlab)
library(FactoInvestigate)
attach(donnees_2018_2)


#rm(acp1)
## première ACP
## voir sur le site factomineR sur l'ACP:
# http://factominer.free.fr/factomethods/analyse-en-composantes-principales.html
acp1 <- PCA(donnees_2018_2, quanti.sup = 13:74, quali.sup = 75:82)
## commande spécifique factomineR permettant d'enregistrer des objets factomineR en csv:
write.infile(acp1,"c:/bricoleR/acp1.csv")
## l'objet acp1 contient tous les résutlats de l'ACP, c'est une liste d'objets complexe que factomineR peut exporter
## sous forme de csv avec write.infile. Attention, ne nombre d'axes factoriels est donné par l'option ncp dans PCA (défaut = 5)
## si vous voulez 10 ou 20 axes il faut donc mettre ncp = 10 ou 20 dans les options de PCA(mes_donnees, ncp = 10, etc.)
descap1 <- dimdesc(acp1, axes = 1:5, proba = 0.01)
descap1
write.infile(descap1, "c:/bricoleR/descap1.csv")
# Investigate ne fonctionne pas: il produit une erreur le tri sur la description des composantes.
Investigate(acp1)

# histogramme des valeurs propres
# https://www.r-graph-gallery.com/209-the-options-of-barplot/
# https://www.statmethods.net/graphs/bar.html
barplot(acp1$eig[,1], main="Histogramme des valeurs propres de l'ACP", xlab = "axes d'inertie", ylab = "valeur propre", ylim = c(0,10),col=rgb(0.2,0.4,0.6,0.6),axis.lty=1,cex.names = 0.8)
# on voit très nettement l'importance des axes 1 et 2.
# ylim permet d'ajuster l'échelle des ordonnées, ici de 0 à 10.
# col = rgb(0.2,0.4,0.6,0.6) permet de modifier la couleur des barres en modifiant la part de rouge, vert, bleu et la saturation (alpha)
# axis.lty=1 ajoute le trait de l'axe des abscisses qui n'est pas dessiné par défaut dans barplot.
# cex.names correspond à cex dans plot, cela fait varier la taille d'affichage des caractères.
ts.plot(acp1$ind$coord[,1], acp1$ind$coord[,2], acp1$ind$coord[,3], acp1$ind$coord[,4])

### on peut faire des graphiques en appelant les objets de ACP1 avec les commandes spécifiques factomineR
plot.PCA(acp1, axes = c(1,2), choix = "ind", label="ind", invisible = "quali", cex=0.6)
plot.PCA(acp1, axes = c(1,3), choix = "ind", label="ind", invisible = "quali", cex=0.6)
plot.PCA(acp1, axes = c(2,3), choix = "ind", label="ind", invisible = "quali", cex=0.6)
### on peut faire des graphiques en appelant les objets de ACP1 avec les commandes spécifiques factomineR
plot.PCA(acp1, axes = c(1,2), choix = "var", label="var", invisible = "quanti.sup", cex=0.7)
plot.PCA(acp1, axes = c(1,3), choix = "var", label="var", invisible = "quanti.sup", cex=0.7)
plot.PCA(acp1, axes = c(2,3), choix = "var", label="var", invisible = "quanti.sup", cex=0.7)


### ou avec les commandes génériques de plot. 
# j'ai trouvé ici comment afficher les étiquettes d'années à côté des points:
#https://www.statmethods.net/advgraphs/axes.html
# plan 1--2
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], Année,cex=0.6,col="red",pos=1)
#grid()
# c'est donc la fonction text(...) qui exécutée avec le plot(...) permet d'ajouter des textes supplémentaires dans un graphique
# on peut même ajouter des commentaires dans le graphique, voir le lien.
# SUGGESTION: modifiez Année pour "Année" afin de voir la différence.
# plan 1--3
plot(acp1$ind$coord[,1], acp1$ind$coord[,3], type="o", main ="Trajectoire des années dans le plan 1--3", xlab = "axe 1", ylab = "axe 3",cex=0.7)
text(acp1$ind$coord[,1], acp1$ind$coord[,3], Année,cex=0.6, col="red", pos=1)
#grid()
#plan 2--3
plot(acp1$ind$coord[,2], acp1$ind$coord[,3], type="b",main ="Trajectoire des années dans le plan 2--3", xlab = "axe 2", ylab = "axe 3",cex=0.7)
text(acp1$ind$coord[,2], acp1$ind$coord[,3], Année,cex=0.6, col="red", pos=1)
#grid()
# executez l'option grid()suite à texte.
# options de grid: https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/grid.html

typo.acp1.1 <- HCPC(acp1, nb.clust=-1)
typo.acp1.2 <- HCPC(acp1, nb.clust=2)
typo.acp1.2
typo.acp1.2$desc.var
typo.acp1.2$desc.axes
typo.acp1.2$desc.ind
typo.acp1.2$desc.var$quanti.var
write.infile(typo.acp1.2,"c:/bricoleR/typo.acp1.2.csv")
### graphiques des typologies
plot(typo.acp1.2, choice="map", draw.tree=FALSE, tree.barplot=TRUE)
plot(typo.acp1.2, choice="3D.map", ind.names=FALSE, angle=45)


### jouons avec kernlab... une classifcation spectrale
typospec1 <- specc(acp1$ind$coord, centers = 2)
typospec1
class(typospec1)
mode(typospec1)
### accéder à un objet de classe S4:
#https://stackoverflow.com/questions/13099780/how-to-access-the-slots-of-an-s4-object-in-rplot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7)
#par l'opérateur @ !
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], typospec1@.Data,cex=0.6,col="red",pos=1)
### cf: advanced R chap 16: # https://adv-r.hadley.nz/
### essayons le laplacien normé:
typospec2 <- specc(acp1$ind$coord, centers = 2, kernel = "laplacedot")
typospec2
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], typospec2@.Data,cex=0.6,col="red",pos=1)

### avec couleur variable:
# voir paragraphe: Using R's built in plot functionality dans:
###  https://stackoverflow.com/questions/7721262/colouring-plot-by-factor-in-r
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7, col=typospec2@.Data)
legend(x = 'bottomright', legend = levels(as.factor(typospec2@.Data)),col=1:2,pch=1)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], Année,cex=0.5,col="blue",pos=1)
### le plus simple en fait:
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7, col=typospec2@.Data)
legend(x = 'bottomright', legend = levels(as.factor(typospec2@.Data)),col=1:2,text.col=1:2, pch=1)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], Année,cex=0.5,col="blue",pos=1)
### ou alors aussi:
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="b", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7, col=typospec2@.Data)
legend(x = 'bottomright', legend = c("cluster 1", "cluster 2"),col=1:2,text.col=1:2, pch=1)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], Année,cex=0.5,col="blue",pos=1)
### see also:
# https://www.harding.edu/fmccown/r/
## o = both avec sur dessin
plot(acp1$ind$coord[,1], acp1$ind$coord[,2], type="o", main = "Trajectoire des années dans le plan 1--2", xlab = "axe 1", ylab = "axe 2", cex=0.7, col=typospec2@.Data)
legend(x = 'bottomright', legend = c("cluster 1", "cluster 2"),col=1:2,text.col=1:2, pch=1)
text(acp1$ind$coord[,1], acp1$ind$coord[,2], Année,cex=0.5,col="blue",pos=1)
grid()

# see also:
# https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/plot

### copié de:https://www.stat.ubc.ca/~jenny/STAT545A/block15_colorMappingBase.html
legend(x = 'bottomright',
       legend = as.character(jColors$continent),
       col = jColors$color, pch = par("pch"), bty = 'n', xjust = 1)

### je tente:
write.infile(typospec1,"c:/bricoleR/typospectrale.1.csv")



### dans le plan principal 1--2, on voit nettement une rupture de tendance au milieu des années 70: la courbe se stablise de 1976 à 1982.
### choissons donc deux périodes: 1959-1981 et 1982-2016.
### estimons un modèle linéaire sur l'axe 1 fonction du RDB réel des ménages et de l'IPC
modele1 <-lm(acp1$ind$coord[,1] ~ RDB2010 + ipc2010)
### on créé un objet modele1 qui est une liste de 12 séries:
mode(modele1)
### la fonction summary permet d'en afficher les résultats classiques:
summary(modele1)
### par contre la commande plot génère quatre graphiques de diagnotic des résidus intéressants:
plot(modele1)
### ce modèle s'ajuste pas mal, essayons de l'améliorer en ajoutant un terme quadratique du revenu réel:
modele2 <-lm(acp1$ind$coord[,1] ~ RDB2010 + I(RDB2010^2) + ipc2010)
# REMARQUEZ Le terme I(RDB2010^2) c'est une commande méconnue mais très importante qui permet de traiter 
# une instabilité numérique, on peut se passer du I() mais à ses risques et périls.
# on aurait pu aussi utiliser poly(RDB2010,2)
summary(modele2)
# de toutes évidence l'ajustement a été amélioré ;
plot(modele2)
# mais le graphique résidu, ajusté est nettement meilleur ici. 
# on note une légère asymétrie et d'autres points influants cette fois-ci...

### retirons les I() protecteur pour le fun...
modele3 <-lm(acp1$ind$coord[,1] ~ RDB2010 + RDB2010^2 + ipc2010)
summary(modele3)
### il se produit un truc intéressant: il vire le carré de RDB2010 en raison de la colinéarité:
cor(RDB2010,RDB2010^2)
### une correlation presque parfaite donc !
plot(modele3)

### essayons le modele 4 avec les polynômes.
### retirons les I() protecteur pour le fun...
modele4 <-lm(acp1$ind$coord[,1] ~ poly(RDB2010,2) + ipc2010)
summary(modele4)
plot(modele4)
#résultat intéressant à comparer à modèle 2:
summary(modele2)
### entre I(), poly(x,k) et le défaut, on ne sait plus quoi faire... de toute évidence il faut lire les instructions sur ces options
## en fait poly() calcul un polynome orthogonal des variables données !
### Et voila pour cette fois les aminchettes !!!

#https://www.statmethods.net/graphs/scatterplot.html

# essayons différents visions 3D spécifiques
library(scatterplot3d)
scatterplot3d(acp1$ind$coord[,1], acp1$ind$coord[,2], acp1$ind$coord[,3])


# suppléments factomineR à consulter
#http://factominer.free.fr/graphs/index.html
#http://factominer.free.fr/reporting/index.html

# nous testerons plus tard plotly qui permettra de représenter les trois premiers axes en perspective.
# graph 3D avec plotly. installer le paquetage et voir.
#https://plot.ly/r/3d-scatter-plots/


### reference links for tips on graphs
# https://www.harding.edu/fmccown/r/
# http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/grid.html
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plot.html
# https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/plot
# http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/legend.html
# https://www.r-graph-gallery.com/119-add-a-legend-to-a-plot/
# https://www.r-bloggers.com/mastering-r-plot-part-1-colors-legends-and-lines/
# https://datascienceplus.com/mastering-r-plot-part-1-colors-legends-and-lines/
# avec plot, ggplot et lattice:
# https://stackoverflow.com/questions/7721262/colouring-plot-by-factor-in-r
# https://www.stat.ubc.ca/~jenny/STAT545A/block15_colorMappingBase.html