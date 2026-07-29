################################################################################
####               Script de la séance 2019-1                              ####
####   /0\   /0\     spécial aminchettes de noel   /0\   /0\               ####
################################################################################

# il convient d'ouvrir les donnees de exemple_factomineR et de donnees_2018_2
## car la session fait appel à toutes les sessions précédentes et leurs objets associés.
#

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

### un modèle tout simple de courbe d'Engel part budgtaire, revenu réel prix
# bon faire graphique correct de quelques dépenses.
#Line plot
# creation d'un objet plot1 ggplot
ggplot(data = actives, aes(x = Année, y = wloisirs)) +
 geom_line() +
 labs(x = "Année", y = "Part budgétaire des loisirs") +
 theme_bw()

### attention aux confusions entre fichiers ouverts ! 
# `Revenu disponible 2010` provient de la base des illustratives de l'ACP des consommations...
summary(`Revenu disponible 2010`)
summary(RDB2010)
#### un modèle tout simple que l'on peut envisager
#rm(E.loisirs)
E.loisirs <- lm(wloisirs ~ Année + I(log(RDB2010)) + I(log(IP10C09)))
summary(E.loisirs)
vif(E.loisirs)
### graphiques associés à lm()
plot(E.loisirs)
### exploration de la dépendante:
ols_plot_response(E.loisirs)
### calcul des élasticités
el.inc.loisirs <- 1 + E.loisirs$coefficients[[3]]/E.loisirs$fitted.values
el.prix.loisirs <- E.loisirs$coefficients[[4]] / E.loisirs$fitted.values - (1+E.loisirs$coefficients[[3]])
summary(el.inc.loisirs)
summary(el.prix.loisirs)
### diagnostics de colinéarité:
### notice: les diagnostics proposés sont de trois types:
### la tolérance, le VIF, et les indices de conditionnement.
### tol(k) = 1-R²(k) avec R²(k) le R² de la régression de la variable k sur les autres variables
### VIF(k) = 1(1-R²(k)) = 1/tol(k)
### soit X'X, la matrice des moments, les indices de conditionnements sont 
### Racine(lembda_max / lembda_min) construits avec les valeurs propres de la matrice X'X
### Règle: on s'équiète un peu entre 30 et 100 et beaucoup au delà de 100 et plus...
### En fait, plus on s'approche d'une relation algébrique entre les variables et plus l'indice
### de conditionnement croîtra
ols_coll_diag(E.loisirs)

### diagnostics de corrélations:
### tableau des corrélations entre la variables dépendante et les indépendantes
### soit : corrélation simple (zero), part = indique de combien le R² baisse si
### on retire la variable, partielle: la corrélation due à la variable seule, nette des autres
ols_correlations(E.loisirs)

### mesures d'influence de Hadi et graphique
ols_hadi(E.loisirs)
ols_plot_hadi(E.loisirs)
### calcul du levier: créer objet et le représenter dans un ggplot
ols_leverage(E.loisirs)
lev.loisirs <- ols_leverage(E.loisirs)
ggplot(data = E.loisirs) +  
  geom_point(mapping = aes(x = lev.loisirs, y = E.loisirs$fitted.values)) +
  geom_text(mapping = aes(x = lev.loisirs, y = E.loisirs$fitted.values, label = Année, 
  hjust = 0), color = "blue", size = 3) +
  labs(x = "levier", y = "part ajustée des dépenses de loisirs") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")
### ajouter à ce graphe les années en lable des points !
ols_plot_comp_plus_resid(E.loisirs)
ols_plot_resid_fit_spread(E.loisirs)
ols_plot_resid_lev(E.loisirs)

### plus élaboré: une suite de graphiques de diagnostics prédéfinis:
ols_plot_diagnostics(E.loisirs)


### 
### 
### plans des élasticités:
### graphiques facettes années ; niveau de vie --- part budgetaire
ggplot(data = actives) +  
  geom_point(mapping = aes(x = log(`Revenu disponible 2010`), y = el.inc.loisirs)) +
  labs(x = "log du revenu disponible réel", y = "élasticité revenu") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")
ggplot(data = actives) +  
  geom_line(mapping = aes(x = log(`Revenu disponible 2010`), y = el.prix.loisirs)) +
  labs(x = "log du revenu disponible réel", y = "élasticité prix") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")
### un second modèle avec facteurs d'ACP
#### examen des facteurs d'ACP
res.pca$eig
ggplot(data = res.pca$eig) +
  geom_point(mapping = aes(x = eigenvalue, y = `percentage of variance`)) +
  labs(x = "valeur propres de l'ACP", "pourcentage d'inertie") +
  theme_bw() +

View(res.pca$ind$coord)
res.pca$ind$coord[,1]
pca.1 <- res.pca$ind$coord[,1]
pca.2 <- res.pca$ind$coord[,2]
pca.3 <- res.pca$ind$coord[,3]

E.loisirs.pca <- lm(wloisirs ~ Année + I(log(`Revenu disponible 2010`)) + I(log(IP10C09)) + 
                    pca.1 + pca.2 + pca.3 )
summary(E.loisirs.pca)
vif(E.loisirs.pca)
plot(E.loisirs.pca)
el.inc.loisirs.pca <- 1 + E.loisirs.pca$coefficients[[3]]/E.loisirs.pca$fitted.values
el.prix.loisirs.pca <- E.loisirs.pca$coefficients[[4]] / E.loisirs.pca$fitted.values - (1+E.loisirs.pca$coefficients[[3]])
summary(el.inc.loisirs.pca)
summary(el.prix.loisirs.pca)
### plans des élasticités:
### graphiques facettes années ; niveau de vie --- part budgetaire
ggplot(data = actives) +  
  geom_point(mapping = aes(x = log(`Revenu disponible 2010`), y = el.inc.loisirs.pca)) +
  labs(x = "log du revenu disponible réel", y = "élasticité revenu ACP") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")
ggplot(data = actives) +  
  geom_line(mapping = aes(x = log(`Revenu disponible 2010`), y = el.prix.loisirs.pca)) +
  labs(x = "log du revenu disponible réel", y = "élasticité prix ACP") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")