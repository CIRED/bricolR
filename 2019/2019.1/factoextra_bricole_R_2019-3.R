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

# les diagrammes en x et y sont intéressants pour faire apparaître les fortes coordonnées sur chaque axe.




