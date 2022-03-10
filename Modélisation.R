### Modélisation !
library(magrittr) # paquetage nécessaire pour utiliser le tuyau ou pipe de programmation
library(gapminder)
library(tidyverse)
library(SmartEDA)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(knitr) 
library(rmarkdown) 
library(markdown) 
### définition du répertoire de travail ----
setwd("c:/Cours_R") ### changez cela dans votre environnement
getwd() # afficher le répertoire de travail.
theme_set(theme_bw()) # 
(.packages())

### modéle sur les données gapmind 2007 2019 ------
# lancement de paquetages
library(estimatr) # procédures d'estimation optimisées
library(AER) # paquetage d'économétrie complet
library(olsrr) # suite complète d'estimations et tes avec ols 
# https://olsrr.rsquaredacademy.com/index.html
library(mctest) # paquetage complet de diagnostics de colinéarité
(.packages())

### un premier modéle ols...
mco1 <- lm(log(GDI_2019) ~ log(GNI_2019/pop_2019), data = gapmind_2007_19)
mco1
smco1 <- summary(mco1)
smco1
#### observons l'objet modéle:
class(mco1)
mode(mco1)
str(mco1)
View(mco1)
plot(mco1)
### explication des graphiques de diagnostic de regression:
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

### afficher les noms d'une liste ou d'un objet:
names(mco1)
names(gapmind_2007_19)
names(smco1)
View(smco1)

### éa marche pour presque tout:
names(gapmind_2007)
names(gdignilog)

### on va tenter de prédire GDI_2019 avec gdpPercap 2007 qui n'a pas de NA !
summary(gapmind_2007_19$GDI_2019)
summary(gapmind_2007_19$gdpPercap)
### modèle 2: ajustement de GDI_2019 par gdppc 2007 (sans NA)
mco2 <- (log(GDI_2019) ~ log(gdpPercap),
           data = filter(gapmind_2007_19, !is.na(gapmind_2007_19$GDI_2019)))
smco2 <- summary(mco2)
smco2
smco1
plot(mco2)
### utilisons les valeurs de gdpPercap pour prédire les NAs de GDI_2019
gdipred2  <-  filter(gapmind_2007_19$GDI_2019, is.na(gapmind_2007_19$GDI_2019))
## ou bien ce que nous avions calculé:
gdipred2 <- gapmind_07_19_na
names(gdipred2)
glimpse(gdipred2)
yhat.mco2 <- predict(mco2, gdipred2)
yhat.mco2
### en fait on peut tout faire en ggplot ! 
# https://ggplot2.tidyverse.org/reference/fortify..html
head(fortify(mco2), 25) ### on va utiliser fortify mais broom devrait étre bien mieux !
### rappeler les graphiques de diagnostic
plot(mco2, which = 1:6)

##### modèle MCO 4
mco4 <- lm(log(GDI_2019) ~ log(gdpPercap), 
           weights = pop, 
           data = filter(gapmind_2007_19, !is.na(gapmind_2007_19$GDI_2019)))
smco4 <- summary(mco4)
smco4
plot(mco4, which = 1:6)

### on peut regarder le modéle avec les ggplots via fortify:
ggplot(mco2, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(mco2, aes(.fitted, .stdresid)) +
  geom_point() +
  geom_hline(yintercept = c(-2,0,2), colour = "red") +
  geom_smooth(se = FALSE)

ggplot(mco4, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(mco4, aes(.fitted, .stdresid)) +
  geom_point() +
  geom_hline(yintercept = c(-2,0,2)) +
  geom_smooth(se = FALSE)

#### en fait on avait un tibble filtré sans na sur gdi_2019: 
mco3 <- lm(log(GDI_2019) ~ log(gdpPercap), data = gapmind_07_19_gdi)
smco3 <- summary(mco3)
smco3
plot(mco3) ### c'est le méme modéle mais les données sont compatibles !
ggplot(mco3, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.fitted, .stdresid)) +
  geom_point(aes(color = HDI_Class, shape = continent)) +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Graphique ajustement vs. résidus",
       subtitle = "Résidus studentisés",
       x = "Valeurs ajustées", y = "Résidus studentisés",
       caption = "Modèle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Très haute")) +
  scale_shape_discrete(name = "Continent", labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))

### graphe quantile quantile des résidus
ggplot(mco3) +
  stat_qq(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  geom_hline(yintercept = 0)

### mais on peut aussi le calculer selon les facteurs actifs:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(color = HDI_Class, shape = continent)) +
  stat_qq(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  labs(title = "Graphique quantiles-quantiles des résidus",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modèle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Très haute")) +
  scale_shape_discrete(name = "Continent", 
                       labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))


### on peut appliquer notre esthétique sur les données du qq plot:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(color = HDI_Class)) +
  stat_qq(aes(sample = .stdresid)) +
  stat_qq_line(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  labs(title = "Graphique quantiles-quantiles des résidus selon l'HDI",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Très haute"))

### mais on peut aussi le calculer selon les facteurs actifs:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(color = continent)) +
  stat_qq(aes(sample = .stdresid)) +
  stat_qq_line(aes(sample = .stdresid)) +
  geom_abline(color = "red", linetype = 3) + # attention, l'esthétique doit s'appliquer é *._line
  geom_vline(xintercept = c(-2, 2), color = "blue", linetype = 2) +
  labs(title = "Graphique quantiles-quantiles des résidus selon le continent",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Continent") +
  scale_color_discrete(name = "Continent", labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))

### on peut regarder les résidus absolus: ----
plot(mco3, which = 3)
# same ggplot:
ggplot(mco3, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_smooth(se = FALSE)
### soit:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(aes(color = HDI_Class)) +
  geom_hline(color = "blue", linetype = 3, yintercept = 2.0) +
  geom_smooth(se = FALSE) +
  labs(title = "Graphique échelle-localisation",
       subtitle = "Racine carrée des résidus standardisés",
       x = "Valeurs ajustées", y = "Racine carrée des résidus standardisés",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Très haute"))


### distances de Cook ----
plot(mco3, which = 4)
### en ggplot frustre:
ggplot(mco3, aes(seq_along(.cooksd), .cooksd)) +
  geom_col()
smco3

### effet intéressant quand on applique l'esthétiue par collage de la commande:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(seq_along(.cooksd), .cooksd)) +
  geom_col(aes(color = HDI_Class)) # collage de color !
# rectification par fill.
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(seq_along(.cooksd), .cooksd)) +
  geom_col(aes(fill = HDI_Class)) + # collage de color !
  geom_hline(color = "blue", linetype = 2, yintercept = 0.1) +
labs(title = "Graphique des distances de Cook",
     subtitle = "Indicateur d'influence des observations",
     x = "Index", y = "Distances de Cook",
     caption = "Modèle 3, lnGDI_2019 ~ lngdpPercap_2007",
     color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_fill_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Très haute"))


### Graphiques Résidus vs levier ----
plot(mco3, which = 5)
### ggplot simple:
ggplot(mco3, aes(.hat, .stdresid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + geom_smooth(se = FALSE)
### ggplot amélioré:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.hat, .stdresid)) +
  geom_vline(size = 0.5, colour = "red", xintercept = 0) +
  geom_hline(size = 0.5, colour = "red", yintercept = 0) +
  geom_point(aes(color = HDI_Class)) + 
  geom_smooth(se = FALSE) +
  labs(title = "Graphique des résidus vs leviers",
     subtitle = "Indicateur d'influence des observations sur les résidus",
     x = "Leviers", y = "Distances de Cook",
     caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
     color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Très haute"))

### on peut jouer sur la taille avec size dans geom point !
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.hat, .stdresid)) +
  geom_vline(size = 0.5, xintercept = 0) +
  geom_hline(size = 0.5, yintercept = 0) +
  geom_point(aes(color = HDI_Class, size = .cooksd)) + 
  geom_smooth(se = FALSE, size = 0.5) +
  labs(title = "Graphique des résidus vs leviers",
       subtitle = "Indicateur d'influence des observations sur les résidus",
       x = "Leviers", y = "Résidus standardisés",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Très haute"))

### graphique Cook -- Leviers
plot(mco3, which = 6) # le sixiéme graphique de diagnostic !
### ggplot basic
ggplot(mco3, aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
  geom_smooth(se = FALSE) +
  geom_point()
### ggplot amélioré
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "red", linetype = 2) + # esthétiques de geom_line()
  geom_smooth(se = FALSE) +
  geom_point(aes(color = HDI_Class)) +
  labs(title = "Graphique des leviers vs distances de Cook",
       subtitle = "Levier = hii / (1 - hii)",
       x = "Leviers", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI")

### on peut aussi modifier l'esthétique en faisant le ratio cook/hat:
ggplot(mco3, aes(.hat, .cooksd)) +
  geom_point(aes(size = .cooksd / .hat)) +
  scale_size_area()
# amélioré:
ggplot(fortify(mco3, gapmind_07_19_gdi), aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "red", linetype = 2) + # esthétiques de geom_line()
  geom_smooth(se = FALSE) +
  geom_point(aes(color = HDI_Class, size = .cooksd / .hat)) +
  scale_size_area() +
  labs(title = "Graphique des leviers vs distances de Cook",
       subtitle = "Levier = hii / (1 - hii)",
       x = "Leviers", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI")

### on voit donc la richesse des graphiques de diagnostic de régression !

#### regardons avec broom les mises en formes de modéles... ----
install.packages("broom")
install.packages("broomhelpers")
install.packages("tidymodels")
install.packages("finalfit")
library(broom)
library(broom.helpers)
library(tidymodels)
library(finalfit)
### https://broom.tidymodels.org/
### https://www.tidymodels.org/
### https://finalfit.org/reference/finalfit.html

### la base: le modéle 3:
mco3 # le plus compact: l'objet associé au modéle...
summary(mco3) # un peu plus complet... mais...
# regardons le bazar:
str(mco3)

mco3$coefficients
mco3$residuals
mco3$fitted.values
names(mco3)

### avec broom on transforme les modéles bazars en beaux tibbles !
t.mco3 <- tidy(mco3) # ah c'est plus beau !
glimpse(t.mco3)
# noter la colonne terme accessible avec $ !
aug.mco3 <- augment(mco3) # on a un équivalent élargi du modéle
View(aug.mco3)
g.mco3 <- glance(mco3) # coup d'oeil aux diagnostics de régression !
### accéder aux objets modéle ----
names(mco3)
modele_3 <- c(t.mco3, aug.mco3, g.mco3)
modele_3
str(modele_3)

#### modele mco 4 -----
plot(mco4) ### c'est le méme modéle mais les données sont compatibles !
ggplot(mco4, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.fitted, .stdresid)) +
  geom_point(aes(color = HDI_Class, shape = continent)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2.5, 2.5), linetype = 2, color = "red") +
  geom_smooth(se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Graphique ajustement vs. résidus",
       subtitle = "Résidus studentisés",
       x = "Valeurs ajustées", y = "Résidus studentisés",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Trés haute")) +
  scale_shape_discrete(name = "Continent", labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))

### graphe quantile quantile des résidus
ggplot(mco4) +
  stat_qq(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  geom_hline(yintercept = 0)

### mais on peut aussi le calculer selon les facteurs actifs:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(color = HDI_Class, shape = continent)) +
  stat_qq(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  geom_hline(yintercept = c(-2.5, 2.5), linetype = 2, color = "blue") +
  labs(title = "Graphique quantiles-quantiles des résidus",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute")) +
  scale_shape_discrete(name = "Continent", 
                       labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))


### on peut appliquer notre esthétique sur les données du qq plot:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(color = HDI_Class)) +
  stat_qq(aes(sample = .stdresid)) +
  stat_qq_line(aes(sample = .stdresid)) +
  geom_abline(color = "red") +
  geom_vline(xintercept = c(-2, 2), color = "blue") +
  labs(title = "Graphique quantiles-quantiles des résidus selon l'HDI",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute"))

### mais on peut aussi le calculer selon les facteurs actifs:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(color = continent)) +
  stat_qq(aes(sample = .stdresid)) +
  stat_qq_line(aes(sample = .stdresid)) +
  geom_abline(color = "red", linetype = 3) + # attention, l'esthétique doit s'appliquer é *._line
  geom_vline(xintercept = c(-2, 2), color = "blue", linetype = 2) +
  geom_hline(yintercept = c(-2.5, 2.5), linetype = 2, color = "blue") +
  labs(title = "Graphique quantiles-quantiles des résidus selon le continent",
       subtitle = "Résidus studentisés",
       x = "Quantile théoriques", y = "Quantiles empiriques",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Continent") +
  scale_color_discrete(name = "Continent", labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))

### on peut regarder les résidus absolus: ----
plot(mco4, which = 3)
# same ggplot:
ggplot(mco4, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_smooth(se = FALSE)
### soit:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(aes(color = HDI_Class)) +
  geom_hline(color = "blue", linetype = 3, yintercept = 2.0) +
  geom_smooth(se = FALSE) +
  labs(title = "Graphique échelle-localisation",
       subtitle = "Racine carrée des résidus standardisés",
       x = "Valeurs ajustées", y = "Racine carrée des résidus standardisés",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Trés haute"))


### distances de Cook ----
plot(mco4, which = 4)
### en ggplot frustre:
ggplot(mco4, aes(seq_along(.cooksd), .cooksd)) +
  geom_col()
### effet intéressant quand on applique l'esthétiue par collage de la commande:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(seq_along(.cooksd), .cooksd)) +
  geom_col(aes(color = HDI_Class)) # collage de color !
# rectification par fill.
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(seq_along(.cooksd), .cooksd)) +
  geom_col(aes(fill = HDI_Class)) + # collage de color !
  geom_hline(color = "blue", linetype = 2, yintercept = 0.1) +
  labs(title = "Graphique des distances de Cook",
       subtitle = "Indicateur d'influence des observations",
       x = "Index", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_fill_discrete(name = "Continent", labels = c("Afrique", "Amérique", "Asie", "Europe", "Océanie"))

# rectification par fill.
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(seq_along(.cooksd), .cooksd)) +
  geom_col(aes(fill = continent)) + # collage de color !
  geom_hline(color = "blue", linetype = 2, yintercept = 0.1) +
  labs(title = "Graphique des distances de Cook",
       subtitle = "Indicateur d'influence des observations",
       x = "Index", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_fill_discrete(name = "Classe d'HDI", labels = c("Haute", "Basse", "Moyenne", "Trés haute"))



### Graphiques Résidus vs levier ----
plot(mco4, which = 5)
### ggplot simple:
ggplot(mco4, aes(.hat, .stdresid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + geom_smooth(se = FALSE)
### ggplot amélioré:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.hat, .stdresid)) +
  geom_vline(size = 0.5, colour = "red", xintercept = 0) +
  geom_hline(size = 0.5, colour = "red", yintercept = 0) +
  geom_hline(size = 0.5, colour = "blue", yintercept = c(-2.5, 2.5), linetype = 2) +
  geom_point(aes(color = HDI_Class)) + 
  geom_smooth(se = FALSE) +
  labs(title = "Graphique des résidus vs leviers",
       subtitle = "Indicateur d'influence des observations sur les résidus",
       x = "Leviers", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute"))
### on peut jouer sur la taille avec size dans geom point !
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.hat, .stdresid)) +
  geom_vline(size = 0.5, xintercept = 0) +
  geom_hline(size = 0.5, yintercept = 0) +
  geom_hline(size = 0.5, colour = "blue", yintercept = c(-2.5, 2.5), linetype = 2) +
  geom_point(aes(color = HDI_Class, size = .cooksd)) + 
  geom_smooth(se = FALSE, size = 0.5) +
  labs(title = "Graphique des résidus vs leviers",
       subtitle = "Indicateur d'influence des observations sur les résidus",
       x = "Leviers", y = "Résidus standardisés",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") + # pourrait ajouter la palette Rcolorbrewer
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute"))

### graphique Cook -- Leviers
plot(mco4, which = 6) # le sixiéme graphique de diagnostic !
### ggplot basic
ggplot(mco4, aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
  geom_smooth(se = FALSE) +
  geom_point()
### ggplot amélioré
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "red", linetype = 2) + # esthétiques de geom_line()
  geom_smooth(se = FALSE) +
  geom_point(aes(color = HDI_Class)) +
  labs(title = "Graphique des leviers vs distances de Cook",
       subtitle = "Levier = hii / (1 - hii)",
       x = "Leviers", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute"))

### on peut aussi modifier l'esthétique en faisant le ratio cook/hat:
ggplot(mco4, aes(.hat, .cooksd)) +
  geom_point(aes(size = .cooksd / .hat)) +
  scale_size_area()
# amélioré:
ggplot(fortify(mco4, gapmind_07_19_gdi), aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "red", linetype = 2) + # esthétiques de geom_line()
  geom_smooth(se = FALSE) +
  geom_point(aes(color = HDI_Class, size = .cooksd / .hat)) +
  scale_size_area() +
  labs(title = "Graphique des leviers vs distances de Cook",
       subtitle = "Levier = hii / (1 - hii)",
       x = "Leviers", y = "Distances de Cook",
       caption = "Modéle 3, lnGDI_2019 ~ lngdpPercap_2007",
       color = "Classe d'HDI") +
  scale_color_discrete(name = "Classe d'HDI", 
                       labels = c("Haute", "Basse", "Moyenne", "Trés haute"))
smco4

### on voit donc la richesse des graphiques de diagnostic de régression !

#### regardons avec broom les mises en formes de modéles... ----
install.packages("broom")
install.packages("broomhelpers")
install.packages("tidymodels")
install.packages("finalfit")
library(broom)
library(broom.helpers)
library(tidymodels)
library(finalfit)
(.packages())
### https://broom.tidymodels.org/
### https://www.tidymodels.org/

### la base: le modéle 3:
mco4 # le plus compact: l'objet associé au modéle...
summary(mco4) # un peu plus complet... mais...
# regardons le bazar:
str(mco4)

### avec broom on transforme les modéles bazars en beaux tibbles !
t.mco4 <- tidy(mco4) # ah c'est plus beau !
t.mco4
# noter la colonne terme accessible avec $ !
aug.mco4 <- augment(mco4) # on a un équivalent élargi du modéle
g.mco4 <- glance(mco4) # coup d'oeil aux diagnostics de régression !
### accéder aux objets modéle ----
names(mco4)

modele4 <- c(t.mco4, aug.mco4, g.mco4)
modele4

write.csv2(modele4, "modele4.csv")
write.csv2(t.mco4, "t.mco4.csv")
write.csv2(aug.mco4, "aug.mco4.csv")
saveRDS(t.mco4, "t.mco4.rds")

#### Exporter des résultats... 
modele.4 <- c(glance(mco4), tidy(mco4), augment(mco4))
glimpse(modele.4) # on a le modéle complet avec les données.
View(modele.4)
modele.4

### explorer avec finalfit
# https://finalfit.org/reference/finalfit.html
library(finalfit)
gapmind_2007_19 %>% missing_glimpse()
### utilisation missing compare à partir de :
# https://finalfit.org/reference/missing_compare.html
dependent <- "Stadev"
explicatives <- c("lifeExp", "pop", "gdpPercap", "urban_pop_2019", "age_median_2019", "GDI_2019", "GDP_GUSDPPP_2017")
gapmind_2007_19 %>% finalfit(dependent, explanatory)
# multi(.data, dependent, explanatory, ...)
multi(.data, dependent, explanatory, ...) # ...Other arguments to pass to 

# https://cran.r-project.org/web/packages/broom.helpers/vignettes/tidy.html
ibrary(broom.helpers)
# https://larmarange.github.io/broom.helpers/
library(gtsummary)

### méthode 1: force brute par factomineR:
install.packages("FactoMineR")
library(FactoMineR)

write.infile(modele.4, file = "modele.4.xls", sep="\t")

### méthode 2: plus subtile, plusieurs modéles dans un classeur xlsx.
modele.2 <- c(glance(mco2), tidy(mco2), augment(mco2))
write.infile(modele.2, file = "modele.2.xls", sep="\t") # comme en 1
modele.3 <- c(glance(mco3), tidy(mco3), augment(mco3))
write.infile(modele.3, file = "modele.3.xls", sep="\t") # comme en 1
### avec le paquetage openxlsx beaucoup plus souple:
install.packages("openxlsx")
library(openxlsx)
diagajust.4 <- glance(mco4)
tidy4 <- tidy(mco4)
augm4 <- augment(mco4)
modeles.4.gdi <- createWorkbook(creator = "Formation R Cired/IEDES",
                              title = "Exemple d'exportation d'objets en xlsx",
                              subject = "Exportation des modéles estimés",
                              category = "Résultats d'estimations")
addWorksheet(modeles.4.gdi, "diagnostics ajustement")
addWorksheet(modeles.4.gdi, "tidy_modele")
addWorksheet(modeles.4.gdi, "augment_modele")
writeData(modeles.4.gdi, "diagnostics ajustement", diagajust.4)
writeData(modeles.4.gdi, "tidy_modele", tidy4)
writeData(modeles.4.gdi, "augment_modele", augm4)
saveWorkbook(modeles.4.gdi, file = "Modeles.4.gdi.estimes.xlsx")

### méthode 3: méme paquetage commande write.xlsx:
write.xlsx(modele.2, "modele.2.xlsx")
write.xlsx(modele.3, "modele.3.xlsx")
write.xlsx(modele.4, "modele.4.xlsx")

### méthode 4: bloc note markdown
write.table(t.mco4, "modele.4.txt", sep="\t")
t.mco4
write.table(aug.mco4, "aug.mco4.txt", sep="\t")
aug.mco4

### exemple de compilation d'un script via appel de commande markdown
rmarkdown::render("Exemple_compilation_de_script.R", "pdf_document")


#### olsrr ----
library(olsrr)
t.mco4
aug.mco4
summary(mco4)

### ols_regress === ressemble à stata
ols_regress(log(GDI_2019) ~ log(gdpPercap),
            weights = pop, 
            data = filter(gapmind_2007_19, !is.na(gapmind_2007_19$GDI_2019)))
### mais c'est du stata dans R !!!
mco4.stata <- ols_regress(log(GDI_2019) ~ log(gdpPercap),
                          weights = pop, 
                          data = filter(gapmind_2007_19, !is.na(gapmind_2007_19$GDI_2019)))
mco4.stata
### nous pouvons utiliser les commandes olsrr
# https://olsrr.rsquaredacademy.com/
ols_plot_resid_fit(mco4)
ols_plot_dfbetas(mco4)
ols_plot_resid_fit_spread(mco4)
ols_plot_resid_qq(mco4)
ols_test_normality(mco4)
ols_plot_resid_hist(mco4)
### tests d'hétéroscédasticité
ols_test_breusch_pagan(mco4)
ols_test_breusch_pagan(mco4, rhs = TRUE, multiple = TRUE)
ols_test_breusch_pagan(mco4, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')
ols_test_score(mco4)
ols_test_f(mco4)
# mesures d'influence
ols_plot_cooksd_bar(model)
require(ggpacman)
library(ggpacman)
animate_pacman(
  pacman = pacman,
  ghosts = list(blinky, pinky, inky, clyde),
  font_family = "xkcd")
