### Script de la sÈance bricole'R 2019.7, seconde sÈance d'ÈtÈ
### pratique de l'ÈconomÈtrie interactive avec AER, jtools et olsrr
### lancement des packages utiles
library(MASS)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(bookdown)
#library(FactoMineR)
#library(car)
#library(plm)
library(dummies)
### Colonescu:
library(AER)
library(car)
library(knitr)
library(xtable)
library(broom)
library(stargazer)
library(lmtest)
### autres
library(olsrr)
library(mfx)
library(margins)

# useful links:
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# http://www.ggplot2-exts.org/gallery/
# pour changer les labels des facettes:
# https://github.com/tidyverse/ggplot2/wiki/labeller
# d√©taille sur les facettes et notamment labels et mises en forme:
# http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
attach(dataphebus)

### sÈance 2019.6 ----
### synthese datafphebus
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

### diagramme violon des revenus selon les qualitatives
# https://ggplot2.tidyverse.org/reference/geom_violin.html
ggplot(data = dataphebus, aes(factor(enerprinc), revtotuc)) +
  geom_violin(aes(fill = factor(enerprinc))) + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2")
## localite
ggplot(data = dataphebus, aes(factor(Urbain), revtotuc)) +
  geom_violin(aes(fill = factor(Urbain))) + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2")
### diagramme violon des emissions selon les qualitatives
ggplot(data = dataphebus, aes(factor(enerprinc), tCO2)) +
  geom_violin(aes(fill = factor(enerprinc))) + 
  theme_bw() +
  scale_color_brewer(palette = "Set1")
## localite
ggplot(data = dataphebus, aes(factor(Urbain), tCO2)) +
  geom_violin(aes(fill = factor(Urbain))) + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

#### Modelisation de la sÈance 2019.7 ----

### WLS model, ponderation nationale, sans filtrage des 
# https://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf
# sur I() : https://stat.ethz.ch/R-manual/R-devel/library/base/html/AsIs.html
model.1 <- lm(I(log(tCO2)) ~ I(log(revtotuc)), weights = Pondmen)
summary(model.1)
plot(model.1)
### notez que la commande plot(model.1) ne marche pas lorsque l'on dÈclare des
### variables comme des objets de mode AsIs() avec la commande I()
### essayons sans I()
model.1 <- lm(log(tCO2) ~ log(revtotuc), weights = Pondmen)
summary(model.1)
plot(model.1)
### cette fois cela marche: on a donc accËs ‡ plot(objet_lm) avec les commandes
### de transformation directes mais pas avec I(log())

### WLS model == avec filtre des niveaux de vie supÈrieurs ‡ 0
###  WLS with filtering of non negative living standards (revtotuc)
model.1 <- lm(log(tCO2) ~ log(revtotuc), weights = Pondmen, subset = (revtotuc > 0))
summary(model.1)
plot(model.1)
### notice that the plot(model.1) command works normally: because model.1 does not
### contain AsIs I() objects ; same for ols_plots_diags
ols_plot_diagnostics(model.1)
ols_plot_response(model.1)

### compute logs of quantitaves:
l_tCO2 <- log(tCO2)
l_revtotuc <- log(revtotuc)
l_surf <- log(Surface)

### https://cran.rstudio.com/web/packages/olsrr/vignettes/influence_measures.html 
model.2 <- lm(l_tCO2 ~ l_revtotuc, weights = Pondmen)
summary(model.2)
plot(model.2)
#ols_plot_diagnostics(model.2)
ols_plot_comp_plus_resid(model.2)
ols_plot_resid_fit_spread(model.2)
ols_plot_resid_lev(model.2)
ols_plot_cooksd_bar(model.2)
## ols plot for normality and symmetry of residuals
ols_plot_resid_box(model.2)
ols_plot_resid_hist(model.2)
ols_plot_resid_qq(model.2)
## ols pot for residuals vs fitted
ols_plot_resid_fit(model.2)
ols_plot_resid_fit
## studentized residuals: threshol == 3*sigma
ols_plot_resid_stud(model.2)
### displaying a function by calling its name:
ols_plot_resid_stud_fit(model.2)
ols_plot_resid_stud_fit
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

### Donc un modËle loglinÈaire n'est pas toujours je meilleur choix.
### So, a loglinear model is not always the best choice !
model.3 <- lm(l_tCO2 ~ revtotuc, weights = Pondmen)
summary(model.3)
plot(model.3)
### we still have some assymmetry and leverage problems
### let us check the standard diagnostics for model 2
#ols_plot_diagnostics(model.2)
ols_plot_comp_plus_resid(model.3)
ols_plot_resid_fit_spread(model.3)
ols_plot_resid_lev(model.3)
### many outiliers: 1696, 2123, 1266, 2001, 2099, 1265 (outlier + leverage)
### there is a very high leverage point: 1696
### let us test by calling the car outlierTest() command:
outlierTest(model.3, cutoff = 0.05, digits = 4)
# the test detects 5 outliers at 5% cutoff. Notice they are not the same as from 
# the ols_plots commands because these tests distinguish leverage and outliers.
plot(model.3$fitted.values)
text(model.3$fitted.values, labels = row.names(dataphebus), cex = 0.7, pos = 4)
### graphique
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = model.3$fitted.values, y = tCO2, 
                           colour = factor(pauvre_60))) +
  facet_grid(decuc) +
  labs(x = "Niveau de vie par decile", y = "Emissions de CO2 en tonnes", color = "Pauvrete 60%") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
### autre possibilitÈ
scatterplot(model.3$fitted.values ~ revtotuc | decuc, data = dataphebus,
            xlab = "Niveau de vie", ylab = "Emissions prÈdites",
            main = "Graphique des Èmissions prÈdites selon le niveau de vie")
### on voit clairement que les gros leviers sont les mÈnages du decile 10
### high leverage points are from decile 10.
ols_plot_cooksd_bar(model.3)
## we notice high Cook's distances for 1696 and 1266, perhaps priority outiers
## ols plot for normality and symmetry of residuals
ols_plot_resid_box(model.3)
ols_plot_resid_hist(model.3)
ols_plot_resid_qq(model.3)
# l'asymÈtrie ‡ droite apparaÓt clairement sur le grahique.
### notice that the strongest asymmetry is now on the right positive side.
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
### clearly, the model needs improvement.
### let us draw the graph of predicted vs observed values acoording to decuc
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = tCO2, y = model.3$fitted.values, 
                           colour = factor(decuc))) +
  labs(x = "Emissions observees", y = "Emissions ajustees", color = "Decile") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
### les us compute some elasticities and display them
### first compute the DyDx and exy quantities from model 3.

revtotucx <- (l_tCO2 - coef(model.3)[[1]])/coef(model.3)[[2]]
summary(revtotucx)
### plot compputed vs observerd
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = revtotucx, 
                           colour = factor(decuc))) +
  labs(x = "niveau de vie observe", y = "Niveau de vie ajuste", color = "Decile") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
### Computing elasticities
Eyx.3 <- revtotucx*coef(model.3)[[2]]
summary(Eyx.3)
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = Eyx.3, 
                           colour = factor(decuc))) +
  labs(x = "Niveau de vie", y = "elasticites des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")

### so a real challenge is this: what credit can we put to the different points ?
# let us plot the indexes of elasticities:
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = Eyx.3, y = tCO2, 
                           colour = factor(pauvre_60))) +
  facet_grid(decuc) +
  labs(x = "Elasticite des emissions", y = "Emissions de CO2 en tonnes") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
plot(Eyx.3)

###??? let us create an index plot with ggplot geom_point
#https://stackoverflow.com/questions/13837565/how-to-plot-one-variable-in-ggplot
# colored with appart
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = seq(1, length(Eyx.3)), y = Eyx.3, colour = factor(appart))) +
  labs(x = "indice", y = "Elasticite des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
# colored with fossile
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = seq(1, length(Eyx.3)), y = Eyx.3, colour = factor(fossile))) +
  labs(x = "indice", y = "Elasticite des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
# colored with urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = seq(1, length(Eyx.3)), y = Eyx.3, colour = factor(Urbain))) +
  labs(x = "indice", y = "Elasticite des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")
### WELL FOLKS ! THERE IS CLEARLY A FOSSILE NON FOSSIL DEVIDE !!!
# let us compute the elasticities by groups for different categories:
with(dataphebus, aggregate(Eyx.3 , by = list(fossile) , FUN = summary))
with(dataphebus, aggregate(Eyx.3 , by = list(appart) , FUN = summary))
with(dataphebus, aggregate(Eyx.3 , by = list(Urbain) , FUN = summary))
with(dataphebus, aggregate(Eyx.3, by = list(decuc), FUN = summary))

### let us compute the medians of the chevances by decile:
with(dataphebus, aggregate(revtotuc, by = list(decuc), FUN = summary))
revtotucmed <- c(aggregate(. ~ decuc, dataphebus[, 4], median))
### one can thus map an acreggregate command into a vector:
revtotucmed
### compute emissions by fitted values to revtotucmed:
CO2_3 <- exp(model.3$fitted.values)
summary(CO2_3)
# colored with urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = CO2_3, colour = factor(decuc))) +
  labs(x = "Niveau de vie", y = "Emissions ajustees", color = "Decile") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
### computing elasticities and then summing up:
El_CO2_revtotuc.3 <- a3.2 * (log(CO2_3) - a3.1) / a3.2
summary(El_CO2_revtotuc.3)
### the final results seem quite low on average...
#Let us plot them:
# colored with urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = El_CO2_revtotuc.3, y = CO2_3, colour = factor(decuc))) +
  labs(x = "Niveau de vie", y = "Elasticites des emissions ajustees", color = "Decile") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
# colored with urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = seq(1, length(El_CO2_revtotuc.3)), y = El_CO2_revtotuc.3, colour = factor(decuc))) +
  labs(x = "indice", y = "Elasticite des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
### let us see what goes for different variables:
# let us compute the elasticities by groups for different categories:
with(dataphebus, aggregate(El_CO2_revtotuc.3 , by = list(fossile) , FUN = summary))
with(dataphebus, aggregate(El_CO2_revtotuc.3 , by = list(appart) , FUN = summary))
with(dataphebus, aggregate(El_CO2_revtotuc.3 , by = list(Urbain) , FUN = summary))
with(dataphebus, aggregate(El_CO2_revtotuc.3, by = list(decuc), FUN = summary))
### we see large differences by deciles but not much for the other variables.
### however, we know the fit is very bad


### cleaning model 3 from outliers and comparing the restults. ----
## first exclude the outliers and influent points from the regression.
exclude.3.1 <- c(1696)
# recall model 3:
summary(model.3)
rm(model.3.1)
### expeditive way:
dataphebus.2 <- dataphebus[-c(1696),]
attach(dataphebus.2)
l_tCO2 = log(tCO2)

### So, a loglinear model is not always the best choice !
model.3.1 <- lm(l_tCO2 ~ revtotuc, weights = Pondmen, data = dataphebus.2)
summary(model.3.1)
plot(model.3.1)

### it is not better and we get stuck into never ending deleting... and ressetting:
### WARNING: we cant subset easily observations in lm():
# https://stackoverflow.com/questions/32545143/can-the-subset-function-within-the-lm-r-function-can-be-used-to-remove-obser
# the most expeditious way is to create new dataframes and drop the old but this is rather inefficient.
# OK for small data set but not for large ones !
summary(l_tCO2)
sd(l_tCO2)
sd(model.3.1$residuals)
model.3.1$coefficients

### the residual sd is very bad... clearly, it not a good bargain to continue, but let us see:
 
CO2_3.1 <- exp(model.3.1$fitted.values)
summary(CO2_3.1)

### extract coefficients from model 3.1 and compute elasticity
a1.31 <- model.3.1$coefficients[[1]]
a2.31 <- model.3.1$coefficients[[2]]
El_CO2_revtotuc.3.1 <- a2.31 * (log(CO2_3.1) - a1.31)/ a2.31
summary(El_CO2_revtotuc.3.1)
### NOTICE THAT THE ELASTICITIES CHANGED QUITE A LOT WITH JSUT ONE DATA POINT LESS !!!
median(El_CO2_revtotuc.3)/median(El_CO2_revtotuc.3.1) - 1
sd(El_CO2_revtotuc.3)/sd(El_CO2_revtotuc.3.1) - 1

### Let us now just try to exclude a few less observations:
dataphebus.3.2 <- dataphebus[-c(1696, 2123, 1266, 2001, 2099, 1265),]
### So, a loglinear model is not always the best choice !
model.3.2 <- lm(log(tCO2) ~ revtotuc, weights = Pondmen, data = dataphebus.3.2)
summary(model.3.2)
plot(model.3.2)
CO2_3.2 <- exp(model.3.2$fitted.values)
summary(CO2_3.2)
### extract coefficients from model 3.2 and compute elasticity
a1.32 <- model.3.2$coefficients[[1]]
a2.32 <- model.3.2$coefficients[[2]]
El_CO2_revtotuc.3.2 <- a2.32 * (log(CO2_3.2) - a1.32)/ a2.32
summary(El_CO2_revtotuc.3.2)
### NOTICE THAT THE ELASTICITIES CHANGED QUITE A LOT WITH JSUT ONE DATA POINT LESS !!!
median(El_CO2_revtotuc.3)/median(El_CO2_revtotuc.3.2) - 1
sd(El_CO2_revtotuc.3)/sd(El_CO2_revtotuc.3.2) - 1
# colored with decile
ggplot(data = dataphebus.3.2) + 
  geom_point(mapping = aes(x = seq(1, length(El_CO2_revtotuc.3.2)), 
                           y = El_CO2_revtotuc.3.2, colour = factor(decuc))) +
  labs(x = "indice", y = "Elasticite des emissions") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")


### All that does not seem quite practical...
### Let us try model 3 now adding surface to the model
model.4 <- lm(log(tCO2) ~ revtotuc + Surface, weights = Pondmen, data = dataphebus)
summary(model.4)
plot(model.4)
### colinearity checks
vif(model.4)
ols_coll_diag(model.4)
#ols_plot_diagnostics(model.3)
ols_plot_response(model.4)
ols_plot_comp_plus_resid(model.4)
ols_plot_resid_fit_spread(model.4)
ols_plot_resid_lev(model.4)
ols_plot_cooksd_bar(model.4)
ols_plot_hadi(model.4)
## ols plot for normality and symmetry of residuals
ols_plot_resid_box(model.4)
ols_plot_resid_hist(model.4)
ols_plot_resid_qq(model.4)
## ols pot for residuals vs fitted
ols_plot_resid_fit(model.4)
## studentized residuals: threshol == 3*sigma
ols_plot_resid_stud(model.4)
ols_plot_resid_stud_fit(model.4)
### het test with iid assumptions:
ols_test_score(model.4)
### Test from residuals
### Heteroskedasticity
### BP test : normal errors
ols_test_breusch_pagan(model.4)
### iid errors
ols_test_f(model.4)
### normality
ols_test_correlation(model.4)
### four tests of normality:
ols_test_normality(model.4)

### computing elasticities
### compute emissions by fitted values to revtotucmed:
CO2_4 <- exp(model.4$fitted.values)
summary(CO2_4)
# colored with urbain
ggplot(data = dataphebus) + 
  geom_point(mapping = aes(x = revtotuc, y = CO2_4, colour = factor(decuc))) +
  labs(x = "Niveau de vie", y = "Emissions ajustees", color = "Decile") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Spectral")
### computing elasticities and then summing up:
a1.4 <- model.4$coefficients[[1]] # intercept coefficient
a2.4 <- model.4$coefficients[[2]] # income coefficient
a3.4 <- model.4$coefficients[[3]] # surface coefficient
### emissions to living standard:
El_CO2_revtotuc.4 <- a2.4 * (log(CO2_4) - a1.4 - a3.4 * Surface) / a2.4
summary(El_CO2_revtotuc.4)
### emissions to surface:
El_CO2_surf.4 <- a3.4 * (log(CO2_4) - a1.4 - a2.4 * revtotuc) / a3.4
summary(El_CO2_surf.4)
### clearly surface effect is far more important.





### location effect
### urb est une matrice cf dummies p.2
Rural <- as.numeric(Urbain == "Rural")
Periurbain <- as.numeric(Urbain == "Periurbain")
Urbain <- as.numeric(Urbain == "Urbain")


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

### calcul de type logement
maison <- as.numeric(typeloge == "Maison individuelle")
appart <- as.numeric(typeloge == "Logement collectif")
### mode√®le (5) avec chauffosile + logement, ref == appart
model.5 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + Rural + Periurbain +
                fossile + maison, weights = Pondmen)
summary(model.5, diagnostics = TRUE)
plot(model.5)
vif(model.5)
ols_coll_diag(model.5)
## calcul de enerprinc dummies
elec <- as.numeric(enerprinc == "Electricite")
gaznat <- as.numeric(enerprinc == "Gaz")
fioul <- as.numeric(enerprinc == "fioul")
bois <- as.numeric(enerprinc == "Bois")
chal <- as.numeric(enerprinc == "Chaleur")
summary(enerprinc)
### mode√®le (5) avec chauffosile + logement, ref == appart + enerprinc ref = elec
model.6 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + Rural + Periurbain +
                maison + gaznat + fioul + bois + chal, weights = Pondmen)
summary(model.6, diagnostics = TRUE)
ols_coll_diag(model.6)
plot(model.6)

### mode√®le (5) avec chauffosile + logement, ref == appart + enerprinc ref = elec
model.7 <- lm(I(log(tCO2)) ~ I(log(revtotuc)) + I(log(Surface)) + urbain_1 + urbain_2 +
                maison + gaznat + fioul + bois + chal + pauvr_60, weights = pondmen)
summary(model.7, diagnostics = TRUE)
ols_coll_diag(model.7)
plot(model.7)

### seance 2019.7 on realise que la specification n'est pas des plus heureuses:
### un modele semi log en y est plus adapte et clair car on peut:
### calculer des vraies elasticites pour tous les coefficients
### calculer des effets globaux pour toutes les variables qualitatives
### 
### modËles 8 : log(E) avec niveau de vie et surface
model.8 <- lm(l_tCO2 ~ revtotuc + Surface, weights = Pondmen)
summary(model.8)
plot(model.8)
el_revtotuc <- coef(model.8)[[2]] * revtotuc
summary(el_revtotuc)
el_surf <- coef(model.8)[[3]] * Surface
summary(el_surf)
### cela impacte durement les elasticites qui sont de signes et magnitudes irrealistes
### niveau de vie -- Èmissions selon Ènergie de chauffage
ggplot(data = dataphebus, aes(x = el_revtotuc, y = el_surf, 
                              colour = as.factor(Urbain))) +
  geom_point() + 
  labs(x = "ElasticitÈ niveau de vie des Èmissions", 
       y = "ElasticitÈ surface des Èmissions") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")
### niveau de vie -- Èmissions selon Ènergie de chauffage
ggplot(data = dataphebus, aes(x = el_revtotuc, y = el_surf, 
                              colour = as.factor(Urbain))) +
  geom_point() + 
  facet_grid(Urbain ~ decuc) +
  labs(x = "ElasticitÈ niveau de vie des Èmissions", 
       y = "ElasticitÈ surface des Èmissions") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

### le mÍme mais avec les niveaux de vie nÈgatifs exclus
model.8.1 <- lm(l_tCO2 ~ revtotuc + Surface, weights = Pondmen, subset = (revtotuc > 0))
## notez l'exclusion des niveaux de vie negatifs
summary(model.8.1)
plot(model.8.1)
el_revtotuc.8.1 <- coef(model.8.1)[[2]] * revtotuc
summary(el_revtotuc.8.1)
el_surf.8.1 <- coef(model.8.1)[[3]] * Surface
summary(el_surf.8.1)

### niveau de vie -- Èmissions selon Ènergie de chauffage
ggplot(data = dataphebus, aes(x = el_revtotuc, y = tCO2, 
                              colour = as.factor(enerprinc))) +
  geom_point() + 
  labs(x = "ElasticitÈ niveau de vie des Èmissions", y = "Emissions de CO2") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

### niveau de vie -- Èmissions selon Ènergie de chauffage
ggplot(data = dataphebus, aes(x = el_surf, y = tCO2, 
                              colour = as.factor(enerprinc))) +
  geom_point() + 
  labs(x = "ElasticitÈ surface des Èmissions", y = "Emissions de CO2") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

### niveau de vie -- Èmissions selon Ènergie de chauffage
ggplot(data = dataphebus, aes(x = el_surf, y = tCO2, 
                              colour = as.factor(Urbain))) +
  geom_point() + 
  labs(x = "ElasticitÈ surface des Èmissions", y = "Emissions de CO2") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")
