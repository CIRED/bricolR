################################################################################
####               Script de la séance 2019-2                               ####
####          une séance consacrée à la présentation de olsrr               ####
################################################################################

# liens utiles:
# https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
# https://datascienceplus.com/multicollinearity-in-r/

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


### Partie 1 : bricole'R 2019.1 ----
### un modèle tout simple de courbe d'Engel part budgtaire, revenu réel prix
# bon faire graphique correct de quelques dépenses.
#Line plot
# creation d'un objet plot1 ggplot
ggplot(data = actives, aes(x = Année, y = wloisirs)) +
 geom_line() +
 labs(x = "Année", y = "Part budgétaire des loisirs") +
 theme_bw()
summary(wloisirs)

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
el.prix.loisirs <- E.loisirs$coefficients[[4]] / E.loisirs$fitted.values - (1 + E.loisirs$coefficients[[3]])
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
  scale_color_brewer(palette = "Paired")

View(res.pca$ind$coord)
res.pca$ind$coord[,1]
pca.1 <- res.pca$ind$coord[,1]
pca.2 <- res.pca$ind$coord[,2]
pca.3 <- res.pca$ind$coord[,3]

### Partie 2 : bricole'R 2019.2 ----
# des liens utiles:
# https://bookdown.org/ndphillips/YaRrr/linear-regression-with-lm.html
### regression avec facteurs d'ACP
E.loisirs.pca <- lm(wloisirs ~ Année + I(log(RDB2010)) + I(log(IP10C09)) + 
                    pca.1 + pca.2 + pca.3 )
summary(E.loisirs.pca)
## qu'y a-t-il dans l'objet E.loisirs.pca ?
attributes(E.loisirs.pca)
names(E.loisirs.pca)
### c'est pareil sauf que cela n'affiche pas la classe d'objet "lm".
# ce qui est contenu dans l'objet lm et qu'on peut appeler:
E.loisirs.pca$coefficients
E.loisirs.pca$residuals
E.loisirs.pca$df.residual
### par contre ce sont là des concepts élaborés:
# https://stackoverflow.com/questions/40228117/what-is-the-effects-returned-by-aov-and-lm
E.loisirs.pca$effects
E.loisirs.pca$qr
## descriptions du contenu:
E.loisirs.pca$xlevels
E.loisirs.pca$terms
E.loisirs.pca$model
### tous ces termes sont utilisables
### ce qui est moins connu, c'est qu'on peut accéder aux éléments des résultats comme 
# dans un tableau usuel:
#https://stackoverflow.com/questions/45943674/extracting-t-stat-p-values-from-lm-in-r
summary(E.loisirs.pca, diagnostics = TRUE)
# extrayons les t-stats:
tstat.E.loisirs.pca <- summary(E.loisirs.pca)$coef[, 3]
tstat.E.loisirs.pca
cyx.zz <- (tstat.E.loisirs.pca^2)/(tstat.E.loisirs.pca^2 + E.loisirs.pca$df.residual)
cyx.zz


# affichage des résultats de régression:
summary(E.loisirs.pca, diagnostics = TRUE)
### calcul des facteurs d'inflation de la variance pour diagnostic de colinéarité:
vif(E.loisirs.pca)
### calcul des diagnostics complets:
ols_coll_diag(E.loisirs.pca)
# graphiques par défaut:
plot(E.loisirs.pca)
# diagnostics complets:
ols_plot_diagnostics(E.loisirs.pca)
ols_correlations(E.loisirs.pca)
### calcul des élasticités
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

### calcul du levier: créer objet et le représenter dans un ggplot
ols_leverage(E.loisirs.pca)
lev.loisirs.pca <- ols_leverage(E.loisirs.pca)
ggplot(data = E.loisirs) +  
  geom_point(mapping = aes(x = lev.loisirs, y = E.loisirs.pca$fitted.values)) +
  geom_text(mapping = aes(x = lev.loisirs, y = E.loisirs.pca$fitted.values, label = Année, 
                          hjust = 0), color = "blue", size = 3) +
  labs(x = "levier", y = "part ajustée des dépenses de loisirs") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### ajouter à ce graphe les années en lable des points !
ols_plot_comp_plus_resid(E.loisirs.pca)
ols_plot_resid_fit_spread(E.loisirs.pca)
ols_plot_resid_lev(E.loisirs.pca)
### plus élaboré: une suite de graphiques de diagnostics prédéfinis:
ols_plot_diagnostics(E.loisirs.pca)


### les tests des hypothèses économétriques du modèle linéaire général.
# test d'hétéroscédasticité:
ols_test_breusch_pagan(E.loisirs.pca)
ols_test_f(E.loisirs.pca)
ols_test_score(E.loisirs.pca)
# test de corrélations résidus sous hyp de normalité:
ols_test_correlation(E.loisirs.pca)
### normalité:
ols_test_normality(E.loisirs.pca)
E.loisirs.pca.aov <- aov(E.loisirs.pca)
E.loisirs.pca.aov
## utiliser lmtest pour la suite:
#https://cran.r-project.org/web/packages/lmtest/lmtest.pdf
# autocorrélation
dwtest(E.loisirs.pca)
bgtest(E.loisirs.pca)
harvtest(E.loisirs.pca)
hmctest(E.loisirs.pca)
raintest(E.loisirs.pca)
resettest(E.loisirs.pca)
# traitement de l'hétéroscédasticité:
### log RDB
ggplot(data = E.loisirs.pca, aes(x = log(RDB2010), y = E.loisirs.pca$residuals)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "log RDB 2010", y = "residus") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### log prix
ggplot(data = E.loisirs.pca, aes(x = log(IP10C09), y = E.loisirs.pca$residuals)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "log prix 2010", y = "residus") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### log prix
ggplot(data = E.loisirs.pca, aes(x = Année, y = E.loisirs.pca$residuals)) +
  geom_line() +
  geom_point(size = 3) +
  labs(x = "log prix 2010", y = "residus") +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
# estimation avec repondération par une variables 
E.loisirs.pca.wls <- lm(wloisirs ~ Année + I(log(RDB2010)) + I(log(IP10C09)) + 
                    pca.1 + pca.2 + pca.3, weights = 1/deptot)
summary(E.loisirs.pca.wls)
plot(E.loisirs.pca.wls)
ols_plot_diagnostics(E.loisirs.pca.wls)
ols_test_breusch_pagan(E.loisirs.pca.wls)
ols_test_f(E.loisirs.pca.wls)
ols_test_score(E.loisirs.pca.wls)

E.loisirs.pca.wls.2 <- wls(wloisirs ~ Année + I(log(RDB2010)) + I(log(IP10C09)) + 
                          pca.1 + pca.2 + pca.3)
summary(E.loisirs.pca.wls.2)