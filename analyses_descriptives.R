###############################################################################
####  Analyses statistiques et exploratoires des donn?es gamind_2007_19   ####
###############################################################################

### chargement des paquetages ----
library(magrittr) # paquetage n?cessaire pour utiliser le tuyau ou pipe de programmation
library(gapminder)
library(tidyverse)
library(SmartEDA)
### d?finition du r?pertoire de travail ----
setwd("D:/cours_R")
# https://thinkr.fr/debuter-avec-r-et-rstudio/
#setwd("c:/Users/user/desktop/Cours_R") ### changez cela dans votre environnement
getwd() # afficher le r?pertoire de travail.
theme_set(theme_bw())  # pre-set the bw theme.


### on a un choix tr?s vaste de paquetages d'analyse.
# https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/
# https://towardsdatascience.com/eda-in-r-with-smarteda-eae12f2c6094

### les r?sum?s classiques: ----
# summary, fivenum, by(,summary)
summary(gapmind.2007.19[,"lifeExp"])


summary(gapmind.2007.19$lifeExp) #notez la diff?rence avec la pr?c?dente commande

summary(gapmind.2007.19)

### on peut faire des r?sum?s selon les cat?gories d'un facteur
by(gapmind.2007.19, gapmind.2007.19$continent, summary)

by(gapmind.2007.19,
   list(gapmind.2007.19$continent, 
   gapmind.2007.19$HDI_Class),
   summary)

by(gapmind.2007.19, gapmind.2007.19$HDI_Class, summary)

### on peut donc ?galement demander le r?sum? d'une seule variable:
by(gapmind.2007.19$lifeExp, gapmind.2007.19$continent, summary)
by(gapmind.2007.19$HDI_Class, gapmind.2007.19$continent, summary)
by(gapmind.2007.19[,"lifeExp"], gapmind.2007.19$continent, summary)
by(gapmind.2007.19[,"HDI_Class"], gapmind.2007.19$continent, summary)
### le dernier facteur, niveau de d?veloppement est peu pratique ? manier !
# on peut le renommer pour l'utiliser:
gapmind.2007.19 <- rename(gapmind.2007.19, "Stadev" = "Developed / Developing Countries")
# attention ? la syntaxe: nouveau nom = ancien nom !
# ne pas oublier l'affectation gapmind_2007_19 <-

#  https://dplyr.tidyverse.org/reference/rename.html

# de m?me HDI_Class devrait ?tre red?fini en facteur:
gapmind.2007.19$HDI.Class <- as.factor(gapmind.2007.19$HDI_Class) # on maintenant un facteur

by(gapmind.2007.19[,"HDI_Class"], gapmind.2007.19$Stadev, summary)
by(gapmind.2007.19[,"lifeExp"], gapmind.2007.19$Stadev, summary)


### via smart eda ----
library(SmartEDA) # une suite de fonctions d'analyse descriptive pour le big data

# https://www.rdocumentation.org/packages/SmartEDA/versions/0.3.8
# https://github.com/daya6489/SmartEDA
# https://joss.theoj.org/papers/10.21105/joss.01509


# Overview of the data - Type = 1
ExpData(data = gapmind.2007.19, type = 1) # r?sum? g?n?ral des donn?es

# Structure of the data - Type = 2
ExpData(data = gapmind.2007.19, type = 2) # r?sum? plus d?taill?

### r?sum?s des variables -----
### ajouter exporter fichiers pour voir !
# statistiques sur les variables num?riques
ExpNumStat(gapmind.2007.19, 
           by = "A", # r?sum? statistique pour toutes les variables Group = all
           gp = NULL, # pas de variables pr?cis?e
           Qnt = seq(0,1, 0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10) # limite de valeurs num?riques diff?rentes
# mais on peut calculer selon un facteur:
ExpNumStat(gapmind.2007.19, 
           by = "G", # r?sum? statistique par Groupe
           gp = "continent", # facteur = continent
           Qnt = seq(0, 1, 0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10) # limite de valeurs num?riques diff?rentes
# et on peut m?me faire les deux: par une variable et globalement:
ExpNumStat(gapmind.2007.19, 
           by = "GA", # r?sum? statistique pour toutes les variables Group + All
           gp = "HDI_Class", # facteur == HDI_Class
           Qnt = seq(0,1, 0.1), # quantiles ici quartiles 1 et 3 
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10) # limite de valeurs num?riques diff?rentes
# En fait il faudrait enlever les NA en amont de la commande... 


# statistiques des fr?quences sur les facteurs
ExpCTable(gapmind.2007.19, 
          Target = NULL, 
          margin = 1, 
          clim = 10, 
          nlim = 3, 
          round = 2, 
          bin = NULL, 
          per = T) # resultats en % des cat?gories du facteur per = TRUE
# l? aussi on peut choisir un facteur qui sera crois? avec les deux autres
# continent crois? avec hdi et stadev:
ExpCTable(gapmind.2007.19, Target = "continent", margin = 1, clim = 10, nlim = 3, round = 2, per = F)
# continent crois? avec hdi et stadev totaux en pourcentage colonne:
ExpCTable(gapmind.2007.19, Target = "continent", margin = 1, clim = 10, nlim = 3, round = 2, per = T)
# HDI_Class crois? avec hdi et stadev en % colonne:
ExpCTable(gapmind.2007.19, Target = "HDI_Class", margin = 1, clim = 10, nlim = 3, round = 2, per = T)
# HDI_Class crois? avec hdi et stadev en % ligne (margin = 2):
ExpCTable(gapmind.2007.19, Target = "HDI_Class", margin = 2, clim = 10, nlim = 3, round = 2, per = T)


### une analyse du degr? d'association des variables ? chaque facteur:
catstat <- ExpCatStat(gapmind.2007.19, Target = "continent", result = "Stat", clim = 10, nlim = 10)
class(catstat)

# idem avec result = IV (information value)
ExpCatStat(gapmind.2007.19, Target = "continent", result = "IV", clim = 10, nlim = 10)
# de mani?re int?ressante il a red?coup? toutes les variables continues en classes !
# de toute ?vidence, les autres variables n'ont pas de pouvoir pr?dictif de continent...

### changeons de variables cible:
ExpCatStat(gapmind.2007.19, Target = "HDI_Class", result = "Stat", clim = 10, nlim = 10)
ExpCatStat(gapmind.2007.19, Target = "Stadev", result = "Stat", clim = 10, nlim = 10) 

### probl?me ! SmartEDA semble ne pas aimer les NAs !
summary(gapmind.2007.19)
# on pourrait filter la base de travail pour retirer les valeurs manquantes:
gapmind.07.19.hdi <- filter(gapmind.2007.19, !is.na(gapmind.2007.19$HDI_Class)) # noter !

glimpse(gapmind.07.19.hdi)
summary(gapmind.07.19.hdi)
str(gapmind.07.19.hdi)

## exemple replace_na() : https://tidyr.tidyverse.org/reference/replace_na.html
(df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b")))
(df %>% replace_na(list(x = 0, y = "unknown")))

gapmind.na.omit <- na.omit(gapmind.2007.19)
summary(gapmind.na.omit) # gapmind sans aucune donnée manquante.

gapmind.2007.19.replace_na <- gapmind.2007.19
summary(gapmind.07.19.hdi$Stadev)

gapmind.07.19.hdi %>% 
   mutate(Stadev = replace_na(Stadev, "missing"))
summary(gapmind.07.19.hdi$Stadev)

gapmind.2007.19$Stadev <- replace_na("missing")
gapmind.2007.19$Stadev
View(gapmind.2007.19)

### différences d'objets listes et vecteurs.
# un vecteur numérique:
v <- c(2, 4, 3.2, 5.9, 6.0001)
v
class(v)
mode(v)

(3*v)

(w <- c(2, 6, TRUE, "Alice", NA))
class(w)

(z <- c(2, 6, TRUE, 1e6, NA))
class(z)

(list.1 <- list(4, 6, TRUE, "Alice", NA))

class(gapmind.2007.19$Stadev)
mode(gapmind.2007.19$Stadev)
summary(gapmind.2007.19$Stadev)

by(gapmind.07.19.hdi, gapmind.07.19.hdi$HDI.Class, summary)

# on retir? les NA dans le facteur hdi.class:
ExpCatStat(gapmind.07.19.hdi, Target = "HDI.Class", result = "Stat", clim = 10, nlim = 10)
# cela devient plus convaincant et int?ressant !
ExpCatStat(gapmind.07.19.hdi, Target = "HDI.Class", result = "IV", bins = 5, clim = 5, nlim = 5)
ExpCatStat(gapmind.07.19.hdi, Target = "HDI.Class", result = "IV", Pclass = 2, bins = 5, clim = 5, nlim = 5)

summary(gapmind.07.19.hdi$HDI.Class)

### r?sum?s statistiques des variables quantitatives avec ExpNumStat
hdi.exp.num <- ExpNumStat(gapmind.07.19.hdi, 
           by = "A", # r?sum? statistique pour toutes les variables Group = all
           gp = "GDI_2019", # pas de variables pr?cis?e
           Qnt = seq(0,1,0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10) # limite de valeurs num?riques diff?rentes
View(hdi.exp.num)
### lorsque la variable cible est continue, il calcule des corr?lations
gapmind.07.19.gdi <- filter(gapmind.2007.19, !is.na(gapmind.2007.19$GDI_2019))

gapmind.07.19.na <- filter(gapmind.2007.19, is.na(gapmind.2007.19$GDI_2019))

summary(gapmind.07.19.gdi$GDI_2019)
summary(gapmind.07.19.na)
#changement du nom de Stadev
rename(gapmind.07.19.gdi, "Stadev" = "Developed / Developing Countries")
rename(gapmind.07.19.na, "Stadev" = "Developed / Developing Countries")

by(gapmind.07.19.na, gapmind.07.19.na$continent, summary)
by(gapmind.07.19.na, gapmind.07.19.na$HDI_Class, summary)

ExpNumStat(gapmind.07.19.gdi, 
           by = "A", # r?sum? statistique pour toutes les variables Group = all
           gp = "GDI_2019", # pas de variables pr?cis?e
           Qnt = seq(0,1,0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10) # limite de valeurs num?riques diff?rentes
### il faut donc retirer les NAs pour obtenir des r?sultats probants


# Remarquez les corr?latons finales sont NA pour les variables comptant des NA ==> 
# le plus direct serait de retirer tous les NA...
# rm(gampind_07_19_nona)

gapmind.07.19.nona <- na.omit(gapmind.2007.19) # sauf que nous perdons 32 pays sur 142 !

32/142 # soit 23% !

summary(gapmind.07.19.nona)

# reprenons 
ExpData(data = gapmind.07.19.nona, type = 1) # on voit qu'il n'y a plus de NA
ExpData(data = gapmind.07.19.nona, type = 2) # on voit qu'il n'y a plus de NA

### résumé de type A avec comme variable cible == GDI_2019
cor.GDI.2019 <- ExpNumStat(gapmind.07.19.nona, 
           by = "A", # r?sum? statistique pour toutes les variables Group = all
           gp = "GDI_2019", # pas de variables pr?cis?e
           Qnt = seq(0,1,0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10)
# on note la correlation n?gative avec l'indice d'in?galit?s de genre et nulle avec le pib !
(ExpNumStat(gapmind.07.19.nona, 
           by = "A", # r?sum? statistique pour toutes les variables Group = all
           gp = "GNI_2019", # pas de variables pr?cis?e
           Qnt = seq(0,1,0.1), # quantiles ici d?ciles
           MesofShape = 2, # mesures de forme == sym?trie et applatissement
           Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
           round = 2, # arrondi ? deux d?cimales
           Nlim = 10))

(ExpNumStat(gapmind.07.19.nona, 
by = "A", # r?sum? statistique pour toutes les variables Group = all
gp = "Gender_Inequality_2019", # pas de variables pr?cis?e
Qnt = seq(0,1,0.1), # quantiles ici d?ciles
MesofShape = 2, # mesures de forme == sym?trie et applatissement
Outlier = TRUE, # calcul des pivots de box plot et outresitu?s
round = 2, # arrondi ? deux d?cimales
Nlim = 10))

### Calculs de coefficients d'applatissment:
ExpKurtosis(gapmind.2007.19$GDI_2019, type = "moment")
ExpKurtosis(gapmind.2007.19$GDI_2019, type = "excess")# kurtosis = moment de degr? 4
ExpKurtosis(gapmind.2007.19$gdpPercap, type = "excess") # moment - 4

### exploration d'outresitu?s:
boxout.gapmind <- ExpOutliers(gapmind.2007.19,
                              varlist = c("GDI_2019", "gdpPercap", "lifeExp", "Gender_Inequality_2019"),
                              method = 'Boxplot',
                              capping = c(0.05, 0.95)) # d?finitions du boxplot
View(boxout.gapmind)
boxout.gapmind

stdout.gapmind <- ExpOutliers(gapmind.2007.19,
                              varlist = c("GDI_2019", "gdpPercap", "lifeExp", "Gender_Inequality_2019"),
                              method = '2xStDev',
                              capping = c(0.05, 0.95)) # d?finition de nombre d'?carts-types
stdout.gapmind
str(stdout.gapmind)
class(stdout.gapmind)
mode(stdout.gapmind)

stdout.gapmind$outlier.index$lower.out.index$lifeExp
stdout.gapmind$outlier.index$upper.out.index

### ExpReport ==> cr?er rapport avec markdown ----
### ExpReport 
install.packages("markdown")
library(markdown)
### rapport sur la base compl?te !
ExpReport(gapmind.2007.19, op_file = "rapport.gapminder.html")
# idem mais avec une variable continue cible : GDI_2019
ExpReport(gapmind.2007.19, Target = "GDI_2019", op_file = "rapp.GDI_2019.html")
# idem avec facteur : HDI_Class
ExpReport(gapmind_2007_19, Target = "Stadev", theme = "stata", op_file = "rapp.stadev.html")

# exploration de donn?es manquantes:
#  https://cran.r-project.org/web/packages/finalfit/index.html
#  https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
install.packages("finalfit")
install.packages("visdat")
# paquetage naniar
# https://cran.r-project.org/web/packages/naniar/index.html
# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html
# https://cran.r-project.org/web/views/MissingData.html
# https://cran.r-project.org/web/views/MissingData.html#exploration
# https://cran.r-project.org/web/views/MissingData.html
# 
library(naniar)
library(visdat)
library(finalfit)
(.packages())

gapmind.2007.19 %>%
  missing_plot()

explanatory <-  c("gdpPercap", "urban_pop_2019", "lifeExp", "age_median_2019")
dependent  <-  "GDI_2019"

gapmind.2007.19 %>% 
  missing_pattern(dependent, explanatory)

comb <- c(gapmind.2007.19, boxout.gapmind)
comb

View(comb)
gapmind.data <- as.data.frame(comb[1:17])
class(gapmind.data)
View(gapmind.data)


comb$continent
glimpse(comb)



class(comb)
mode(comb)
str(comb)

library(visdat)
vis_dat(gapmind.2007.19)
gapmindat <- gapminder
vis_dat(gapmindat)
vis_dat(UNSD_Regions)

