################################################################################
####               Script de la séance 2018-7                              ####
####   /0\   /0\     spécial aminchettes de noel   /0\   /0\               ####
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
  
#### un modèle tout simple que l'on peut envisager
E.loisirs <- lm(wloisirs ~ Année + I(log(`Revenu disponible 2010`)) + I(log(IP10C09)))
summary(E.loisirs)
vif(E.loisirs)
plot(E.loisirs)
el.inc.loisirs <- 1 + E.loisirs$coefficients[[3]]/E.loisirs$fitted.values
el.prix.loisirs <- E.loisirs$coefficients[[4]] / E.loisirs$fitted.values - (1+E.loisirs$coefficients[[3]])
summary(el.inc.loisirs)
summary(el.prix.loisirs)
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