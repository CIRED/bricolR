################################################################################
#       test des commandes facto extra sur les ACP de l'exemple bricole'R      #
################################################################################
################################################################################

### chargement des librairies
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(AER)
library(olsrr)
library(lmtest)

###attachement des données
attach(actives)
attach(illustratives)
attach(donnees_2018_2)
attach(indices_prix_2010)
attach(year)


### resultats partiels de l'ACP
summary(res.pca)
### traitements avec factoextra
get_eig(res.pca)
# visualisation des variables 
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 65))
## second test ave choix
fviz_screeplot(res.pca, ncp = 7, choice = "eigenvalue", addlabels = TRUE, ylim = c(0, 10))
## diagramme ligne ou barre 
fviz_screeplot(res.pca, ncp = 7, geom = "line", addlabels = TRUE, ylim = c(0, 65))
fviz_screeplot(res.pca, ncp = 7, geom = "bar", addlabels = TRUE, ylim = c(0, 65))
### conclusion: le défaut est meilleur

### analyse des variables:
var.pca <- get_pca_var(res.pca)
var.pca
## la fonction head permet d'afficher les en-têtes de l'objet var.pca
head(var.pca$coord)
head(var.pca$contrib)
head(var.pca$cos2)
## representation des variables par défaut
fviz_pca_var(res.pca, col.var = "black")
### toutes les fonctions factomineR sont disponibles:
fviz_pca_var(res.pca, col.var = "black", invisible = "quanti.sup", repel = TRUE,
             title = "Cercle des correlations du plan principal")

### toutes les variables sont représentées yc les supplémentaires.
### filtre des supplementaires
fviz_pca_var(res.pca, col.var = "black")
fviz_pca_var(res.pca, col.var = "black", invisible = "quanti.sup", repel = TRUE)

# coloration selon les CTR
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# coloration selon les cos2
fviz_pca_var(res.pca, col.var="cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# norme x^2+y^2
fviz_pca_var(res.pca, col.var="coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# coordonnées x
fviz_pca_var(res.pca, col.var="x",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)
# coordonnées y
fviz_pca_var(res.pca, col.var="y",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)
# vtest ?
fviz_pca_var(res.pca, col.var="y",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)
### les colorations acceptent des codes rcolorbrewer aussi:
# coloration selon les CTR
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = "BrBG",
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# coloration selon les cos2
fviz_pca_var(res.pca, col.var="cos2",
             gradient.cols = "Spectral",
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)


# coloration selon les CTR
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("darkblue", "gold2", "darkred"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# coloration selon les cos2
fviz_pca_var(res.pca, col.var="cos2",
             gradient.cols = c("darkblue", "gold2", "darkred"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)
# norme x^2+y^2
fviz_pca_var(res.pca, col.var="coord",
             gradient.cols = c("darkblue", "gold2", "darkred"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# norme x
fviz_pca_var(res.pca, col.var="x",
             gradient.cols = c("darkblue", "gold2", "darkred"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

# norme y
fviz_pca_var(res.pca, col.var="y",
             gradient.cols = c("darkblue", "gold2", "darkred"),
             invisible = "quanti.sup",
             repel = TRUE # Avoid text overlapping
)

### représentation des individus et des valeurs test.
mode(c_test)


### les palettes prédéfinies de Rcolorbrewer sont trop claires avec des données.
# il vaut mieux mettre des vecteurs de couleurs:



pca.var.coord <- var.pca$coord
pca.var.coord
ggplot(pca.var.coord, aes(x = Dim.1, y = Dim.2))

# les diagrammes en x et y sont intéressants pour faire apparaître les fortes coordonnées sur chaque axe.
# mais on peut aussi examiner contributions et cos2
fviz_contrib(res.pca, choice = "var", axes = 1, sort.val = "desc")
fviz_contrib(res.pca, choice = "var", axes = 2, sort.val = "desc")
fviz_contrib(res.pca, choice = "var", axes = 3, sort.val = "desc")
fviz_contrib(res.pca, choice = "var", axes = 4, sort.val = "desc")
fviz_contrib(res.pca, choice = "var", axes = 5, sort.val = "desc")
fviz_cos2(res.pca, choice = "var", axes = 1, sort.val = "desc")
fviz_cos2(res.pca, choice = "var", axes = 2, sort.val = "desc")
fviz_cos2(res.pca, choice = "var", axes = 3, sort.val = "desc")
fviz_cos2(res.pca, choice = "var", axes = 4, sort.val = "desc")
fviz_cos2(res.pca, choice = "var", axes = 5, sort.val = "desc")
### try individus
fviz_contrib(res.pca, choice = "ind", axes = 1, sort.val = "none")
fviz_contrib(res.pca, choice = "ind", axes = 2, sort.val = "none")
fviz_contrib(res.pca, choice = "ind", axes = 3, sort.val = "none")
fviz_contrib(res.pca, choice = "ind", axes = 4, sort.val = "none")
fviz_contrib(res.pca, choice = "ind", axes = 5, sort.val = "none")
fviz_cos2(res.pca, choice = "ind", axes = 1, sort.val = "none")
fviz_cos2(res.pca, choice = "ind", axes = 2, sort.val = "none")
fviz_cos2(res.pca, choice = "ind", axes = 3, sort.val = "none")
fviz_cos2(res.pca, choice = "ind", axes = 4, sort.val = "none")
fviz_cos2(res.pca, choice = "ind", axes = 5, sort.val = "none")
### test avec get dist sur les données actives
dis.act <- get_dist(actives, stand = TRUE, method = "pearson")
fviz_dist(dis.act, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dis.act.spear <- get_dist(actives, stand = TRUE, method = "spearman")
fviz_dist(dis.act.spear, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dis.act.kend <- get_dist(actives, stand = TRUE, method = "kendall")
fviz_dist(dis.act.kend, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
### idem sur illustratives
dis.ill <- get_dist(illustratives, stand = TRUE, method = "pearson")
fviz_dist(dis.ill, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dis.ill.spear <- get_dist(illustratives, stand = TRUE, method = "spearman")
fviz_dist(dis.ill.spear, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dis.ill.kend <- get_dist(illustratives, stand = TRUE, method = "kendall")
fviz_dist(dis.ill.kend, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))

### travail sur les distances entre composantes
ind.pca <- get_pca_ind(res.pca)
## la fonction head permet d'afficher les en-têtes de l'objet var.pca
head(ind.pca$coord)
head(ind.pca$contrib)
head(ind.pca$cos2)
dist.ind <- get_dist(ind.pca$coord, stand = FALSE, method ="pearson")
fviz_dist(dist.ind, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dist.ind.spear <- get_dist(ind.pca$coord, stand = FALSE, method ="spearman")
fviz_dist(dist.ind.spear, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
dist.ind.kend <- get_dist(ind.pca$coord, stand = FALSE, method ="kendall")
fviz_dist(dist.ind.kend, gradient= list(low = "darkblue", mid = "gold2", high = "darkred"))
### travaux articulés avec les c-tests
c_test_1
### voir comment accorder aux graphes associés à une ACP dans factoextra.
fviz_pca_ind(res.pca, repel = TRUE, col.ind = "#00AFBB")
# coloration par le cos2 ou les contrib
fviz_pca_ind(res.pca, repel = TRUE, col.ind = "cos2")
fviz_pca_ind(res.pca, repel = TRUE, col.ind = "contrib")


