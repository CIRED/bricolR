### Script de la sÈance bricole'R 2019.6, premiËre sÈance d'ÈtÈ
### pratique de l'ÈconomÈtrie interactive avec AER, jtools et olsrr
### lancement des packages utiles
#library(MASS)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(bookdown)
library(FactoMineR)
library(car)
#library(plm)
library(dummies)
### Colonescu:
library(AER)
library(knitr)
library(xtable)
library(broom)
library(stargazer)
library(lmtest)
### autres
library(olsrr)

# useful links:
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# http://www.ggplot2-exts.org/gallery/
# pour changer les labels des facettes:
# https://github.com/tidyverse/ggplot2/wiki/labeller
# d√©taille sur les facettes et notamment labels et mises en forme:
# http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
attach(dataphebus)

### synthse datafphebus
### Attention ‡ l'importation !!! Il n'aime pas les accents !!!
summary(dataphebus)
### les variables qualitatives sont en caracteres

### modalit√©s des variables qualitatives:
ggplot(data = dataphebus) +
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
  facet_wrap(~ decuc, nrow = 2) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Tissu urbain") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### double facettage 
### modalit√©s des variables qualitatives:
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
  facet_grid(Urbain ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Tissu urbain") + 
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### idem par type de logement:
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
  facet_grid(typeloge ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Tissu urbain") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")


### same as fossil fuel heating:
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
  facet_grid(chauffossile ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Tissu urbain") +  
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### comment pr√©c√©dent invers√©
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(chauffossile))) +
  facet_grid(Urbain ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Energie de chauffage fossile ") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### creation des muettes de pauvret√©:
pauvre_50 <- as.numeric(revtotuc <= median(revtotuc)*0.5)
pauvre_60 <- as.numeric(revtotuc <= median(revtotuc)*0.6)
summary(pauvre_50, diagnostics = TRUE)
summary(pauvre_60, diagnostics = TRUE)

### energie principale - decile - urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
  facet_grid(enerprinc ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Tissu urbain") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

###urbain - decile - energie principale 
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(enerprinc))) +
  facet_grid(Urbain ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Energie principale du logement") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")


### energie principale - decile - chauffage fossile
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(chauffossile))) +
  facet_grid(enerprinc ~ decuc) +
  labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes", color = "Energie de chauffage fossile ") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### energie principale - decile - pauvrete 60
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(pauvre_60))) +
  facet_grid(enerprinc ~ decuc) +
  labs(x = "Niveau de vie par dÈcile", y = "Emissions de CO2 en tonnes") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

### chauffage fossile - decile - pauvrete 60
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(pauvre_60))) +
  facet_grid(chauffossile ~ decuc) +
  labs(x = "Niveau de vie par decile", y = "Emissions de CO2 en tonnes") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
### urbain - decile - pauvrete 60
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = tCO2, colour = factor(pauvre_60))) +
  facet_grid(Urbain ~ decuc) +
  labs(x = "Niveau de vie par decile", y = "Emissions de CO2 en tonnes") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")





### examen et explorations des donnees PHEBUS
### box plots des Èmissions par dÈcile selon tissu urbain
ggplot(data = dataphebus) +
  geom_boxplot(mapping = aes(x = factor(decuc), y = tCO2, colour = factor(Urbain))) +
  facet_grid() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

### examen et explorations des donnees PHEBUS
### niveau de vie -- Èmissions selon tissu urbain
  ggplot(data = dataphebus, aes(x = revtotuc, y = tCO2, colour = factor(Urbain))) +
    geom_point() + 
    labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes") +
    theme_bw() +
  scale_color_brewer(palette = "Dark2")
### examen et explorations des donnees PHEBUS
### niveau de vie -- Èmissions selon type de logement
  ggplot(data = dataphebus, aes(x = revtotuc, y = tCO2, colour = as.factor(typeloge))) +
    geom_point() + 
    labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes") +
    theme_bw() +
  scale_color_brewer(palette = "Set1")
### examen et explorations des donnees PHEBUS
### niveau de vie -- Èmissions selon Ènergie de chauffage
  ggplot(data = dataphebus, aes(x = revtotuc, y = tCO2, colour = as.factor(enerprinc))) +
    geom_point() + 
    labs(x = "Niveau de vie", y = "Emissions de CO2 en tonnes") +
    theme_bw() +
    scale_color_brewer(palette = "Set1")  

### statistiques   
summary(tCO2)
summary(revtotuc)
summary(Surface)


d <- data.frame(tCO2, revtotuc, Surface)
cor(d)
rm(d)

### WLS model
model.1 <- lm(I(log(tCO2)) ~ I(log(revtotuc)), weights = Pondmen)
summary(model.1)
plot(model.1)
ols_plot_diagnostics(model.1)
ols_plot_response(model.1)

### all this seems badly going...

### compute logs of quantitaves:
l_tCO2 <- log(tCO2)
l_revtotuc <- log(revtotuc)
l_surf <- log(Surface)

### try model 2 without calling I()
### https://cran.rstudio.com/web/packages/olsrr/vignettes/influence_measures.html 
model.2 <- lm(l_tCO2 ~ l_revtotuc, weights = Pondmen)
summary(model.2)
plot(model.2)
#ols_plot_diagnostics(model.2)
ols_plot_response(model.2)
ols_plot_comp_plus_resid(model.2)
ols_plot_resid_fit_spread(model.2)
ols_plot_resid_lev(model.2)
ols_plot_cooksd_bar(model.2)
ols_plot_hadi(model.2)
## ols plot for normality and symmetry of residuals
ols_plot_resid_box(model.2)
ols_plot_resid_hist(model.2)
ols_plot_resid_qq(model.2)
## ols pot for residuals vs fitted
ols_plot_resid_fit(model.2)
## studentized residuals: threshol == 3*sigma
ols_plot_resid_stud(model.2)
ols_plot_resid_stud_fit(model.2)
### het test with iid assumptions:
ols_test_score(model.2)
### Test from residuals
### Heteroskedasticity
### BP test : normal errors
ols_test_breusch_pagan(model.2)
### iid errors
ols_test_f(model.2)
### normality
ols_test_correlation(model.2)
### four tests of normality:
ols_test_normality(model.2)


### Let us try model 3 now adding surface to the model
model.3 <- lm(l_tCO2 ~ l_revtotuc + l_surf, weights = Pondmen)
summary(model.3)
summary(model.2)
plot(model.3)
### colinearity checks
vif(model.3)
ols_coll_diag(model.3)
#ols_plot_diagnostics(model.3)
ols_plot_response(model.3)
ols_plot_comp_plus_resid(model.3)
ols_plot_resid_fit_spread(model.3)
ols_plot_resid_lev(model.3)
ols_plot_cooksd_bar(model.3)
ols_plot_hadi(model.3)
## ols plot for normality and symmetry of residuals
ols_plot_resid_box(model.3)
ols_plot_resid_hist(model.3)
ols_plot_resid_qq(model.3)
## ols pot for residuals vs fitted
ols_plot_resid_fit(model.3)
## studentized residuals: threshol == 3*sigma
ols_plot_resid_stud(model.3)
ols_plot_resid_stud_fit(model.3)
### het test with iid assumptions:
ols_test_score(model.3)
### Test from residuals
### Heteroskedasticity
### BP test : normal errors
ols_test_breusch_pagan(model.3)
### iid errors
ols_test_f(model.3)
### normality
ols_test_correlation(model.3)
### four tests of normality:
ols_test_normality(model.3)


### avec ajouts de variables muettes.
urb <- dummy(dataphebus$urbain, sep = "_")
###‚ô• urb est une matrice cf dummies p.2
Rural <- as.numeric(Urbain == "Rural")
Periurbain <- as.numeric(Urbain == "Periurbain")
Urbain <- as.numeric(Urbain == "Urbain")

### modele 3 : avec ref√©rence == urbain
model.3 <- lm(l_tCO2 ~ l_revtotuc + l_surf + Rural + Periurbain, weights = Pondmen)
summary(model.3)
plot(model.3)
vif(model.3)
ols_coll_diag(model.3)

### mode√®le (4) avec chauffosile
fossile <- as.numeric(chauffossile == "fossile")
non_fossile <- as.numeric(chauffossile == "non fossile")
model.4 <- lm(l_tCO2 ~ l_revtotuc + l_surf + Rural + Periurbain +
                fossile, weights = Pondmen)
summary(model.4)
vif(model.4)
plot(model.4)
ols_coll_diag(model.4)

d <- data.frame(tCO2, revtotuc, Surface, Rural, Periurbain, fossile)
cor(d)
rm(d)

###‚ô¶ calcul de type logement
maison <- as.numeric(typeloge == 1)
appart <- as.numeric(typeloge == 2)
### mode√®le (5) avec chauffosile + logement, ref == appart
model.5 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + urbain_1 + urbain_2 +
                chauffossile + maison, weights = pondmen)
summary(model.5, diagnostics = TRUE)
ols_coll_diag(model.5)
## calcul de enerprinc dummies
elec <- as.numeric(enerprinc == 1)
gaznat <- as.numeric(enerprinc == 2)
fioul <- as.numeric(enerprinc == 3)
bois <- as.numeric(enerprinc == 4)
chal <- as.numeric(enerprinc == 5)
### mode√®le (5) avec chauffosile + logement, ref == appart + enerprinc ref = elec
model.6 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + urbain_1 + urbain_2 +
                maison + gaznat + fioul + bois + chal, weights = pondmen)
summary(model.6, diagnostics = TRUE)
ols_coll_diag(model.6)
plot(model.6)

### mode√®le (5) avec chauffosile + logement, ref == appart + enerprinc ref = elec
model.7 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + urbain_1 + urbain_2 +
                maison + gaznat + fioul + bois + chal + pauvr_60, weights = pondmen)
summary(model.7, diagnostics = TRUE)
ols_coll_diag(model.7)
plot(model.7)

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot()


### A01 : 
agriciv <-ivreg(wA01 ~ I(lnA114St) + I(log(IP2010A01)) + I(Coupe) + soceco.1 + soceco.2 + soceco.3 + soceco.4 + soceco.5 |
                  I(lnrevtotucnatA115St) + I(log(IP2010A01)) + I(Coupe) + soceco.1 + soceco.2 + soceco.3 + soceco.4 + soceco.5, weights = pondmen)
summary(agriciv, diagnostics = TRUE)
bptest(agriciv)
vif(agriciv)
### calcul des elasticites
## cas simplifie
e_agriciv_inc <- 1 + agriciv$coefficients[[2]] / agriciv$fitted.values
### expression simplifiee:
e_agriciv_p01 <- agriciv$coefficients[[3]] / agriciv$fitted.values - agriciv$coefficients[[2]]
### expression exacte:
e_agriciv_p01_t <- agriciv$coefficients[[3]] / agriciv$fitted.values - (agriciv$coefficients[[2]] + agriciv$coefficients[[3]])
e_agriciv_p01_tc <- e_agriciv_p01_t - e_agriciv_inc * agriciv$fitted.values
summary(e_agriciv_inc)
summary(e_agriciv_p01)
summary(e_agriciv_p01_t)
summary(e_agriciv_p01_tc)
### graphiques facettes annees ; Revenu -- elasticite prix
ggplot(data = ADEME114_NAT) +  
  geom_point(mapping = aes(x = revtotucnat, y = e_agriciv_inc, color = factor(decuc))) +
  facet_wrap(~ ANNEE, nrow = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### graphiques facettes annees ; part budgetaire -- elasticite prix
ggplot(data = ADEME114_NAT) +  
  geom_point(mapping = aes(x = revtotucnat, y = e_agriciv_p01_t, color = factor(decuc))) +
  facet_wrap(~ ANNEE, nrow = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### graphiques facettes annees ; part budgetaire -- elasticite prix
ggplot(data = ADEME114_NAT) +  
  geom_point(mapping = aes(x = wA01, y = e_agriciv_p01_t, color = factor(decuc))) +
  facet_wrap(~ ANNEE, nrow = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
### graphiques facettes annees ; part budgetaire -- elasticite prix compensee
ggplot(data = ADEME114_NAT) +  
  geom_point(mapping = aes(x = wA01, y = e_agriciv_p01_tc, color = factor(decuc))) +
  facet_wrap(~ ANNEE, nrow = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")
