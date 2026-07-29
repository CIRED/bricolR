################################################################################
###                   Script bricole'R session 2022.2                        ###
###                     introduction à crosstable                            ###
###                   Session  1
################################################################################


#::::::::::::::::::::::::: initialisation ::::::::::::::::::::::::::::::::::----
# lancement des paquetages
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(AER)
library(broom, broomExtra)
library(tidymodels)
library(tidypredict)
library(RColorBrewer)
library(coop) # calculs efficients de correlations et cosinus-carrés
# libary(lsa) 
#calcul des cosinus carrés existe aussi dans LSA mais seulement  sur des matrices
library(crosstable) # le paquetage de la session

setwd("C:/bricoleR/2022.2_crosstable")
getwd()

# https://danchaltiel.github.io/crosstable/
# https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html
# https://danchaltiel.github.io/crosstable/articles/crosstable-selection.html
# https://danchaltiel.github.io/crosstable/reference/crosstable_options.html
# https://danchaltiel.github.io/crosstable/reference/as_flextable.html

#::::::::::::::::::::::::__ premières manipulations __::::::::::::::::::::::----

# ouverture des données, transformation en tibble: 
spam.hcpc <- as_tibble(readRDS("spam.hcpc.RDS"))
# https://tibble.tidyverse.org/
glimpse(spam.hcpc)
mode(spam.hcpc)
class(spam.hcpc)

# le plus simple:
crosstable(data = spam.hcpc, clust, by = spam)

# mais peu lisible en console... utiliser as_flextable:
crosstable(data = spam.hcpc, clust, by = spam) %>% as_flextable() #noter les options

# avec les totaux ligne et colonne:
crosstable(spam.hcpc, c(money, project, meeting, spam), by = clust, total = "both") %>% 
  as_flextable()
crosstable(spam.hcpc, c(money, project, meeting, clust), by = spam, total = "both") %>% 
  as_flextable()



### tableau croisés selon variable catégorielle ----
### spécifier l'affichage des totaux par percent_pattern, et les décimales
crosstable(spam.hcpc,
           clust, by = spam, total= "both", 
           percent_pattern = "{n} ({p_row}/{p_col})",
           percent_digits = 0) %>%
  as_flextable()

### croisement de plusieurs variables catégorielles
crosstable(spam.hcpc, c(money, project, meeting),
           by = c(spam, clust),
           total="both",
           percent_pattern = "{n} ({p_row}/{p_col})",
           percent_digits = 0) %>% 
  as_flextable()
### notez l'ordre d'affichage des catégories: clust puis spam

# contrôler les options d'affichage
# https://danchaltiel.github.io/crosstable/reference/crosstable_options.html

# statistiques sur des variables continues:
crosstable(spam.hcpc, c(money, project, meeting),
           funs = c(median, mean, "std dev" = sd)) %>% 
  as_flextable()
# autre possibilité avec les fonctions internes de crosstable:
# https://danchaltiel.github.io/crosstable/reference/summaryFunctions.html
crosstable(spam.hcpc, c(money, project, meeting),
           funs = c(minmax, mediqr, meansd, meanCI),
           by = c(clust, spam)) %>% 
  as_flextable()

### calcul de correlations si by spécifie des variables numériques:
crosstable(spam.hcpc, c(money, project, meeting),
           by = c(CapLtot)) %>% 
  as_flextable()
# on peut faire plus sûr: 
crosstable(spam.hcpc, c(money, project, meeting, where(is.numeric)),
           cor_method = "pearson",
           by = c(CapLtot)) %>% 
  as_flextable()
## nb: bonne syntaxe == where(...) dans le vecteur c(...)


# selection de variables :::::::::::::::::::::::::::::::::::::::::::::::::----
# documentation tidyselect: https://tidyselect.r-lib.org/reference/language.html
# doc tech: https://tidyselect.r-lib.org/articles/syntax.html

# afficher tout
crosstable(iris2)


# ou tidyselect everything()
ct = crosstable(iris2, everything()) #or simply `crosstable(iris2)`
ct %>% 
  as_flextable(keep_id = TRUE)

# selection par nom de variables 
crosstable(spam.hcpc, c(CapLtot, "CapLsup", CapLM), by = spam) %>% 
  as_flextable() # noter: entre guillemets ou non !

### ? l'aide d'un vecteur externe:
cap <- c("CapLtot", "CapLsup", "CapLM") # vecteur capitales entre guillemets
crosstable(spam.hcpc, cap, by = spam) %>% 
  as_flextable() # noter: entre guillemets ou non !

# il faut toujours employer un tidyselect dans l'appel de vecteur externe:
crosstable(spam.hcpc, any_of(cap), by = spam) %>% 
  as_flextable() # noter: entre guillemets ou non !

# negation:
crosstable(spam.hcpc, -c(2:55), by = spam) %>% 
  as_flextable()
#???
car <- seq(49:58)
crosstable(spam.hcpc, -c(2:49), by = spam) %>% 
  as_flextable()

### exclusions avec les selecteurs tidyverse
crosstable(spam.hcpc, -c(starts_with("C", ignore.case = FALSE)), by = spam) %>% 
  as_flextable()

# indice de colonne:
crosstable(spam.hcpc, 50:58, by = spam) %>% 
  as_flextable()


### avec les selecteurs tidyselect:
# https://tidyselect.r-lib.org/reference/language.html
# les principaux sont : starts_with(), ends_with(), contains() and matches()

crosstable(spam.hcpc, starts_with("C"), by = spam) %>% 
  as_flextable()
# noter qu'il ne distingue pas la casse par d?faut ! Il pr?ciser:
crosstable(spam.hcpc, starts_with("C", ignore.case = FALSE), by = spam) %>% 
  as_flextable()
# cette fois on r?cup?re bien les variables qui commencent pas C majuscule !
crosstable(spam.hcpc, ends_with("t"), by = spam) %>% 
  as_flextable()

### avec regexs : selects all columns which name 
#starts with "d" or "g", followed by exactly 3 characters
crosstable(spam.hcpc, matches("^d|g.{3}$"), by = spam) %>% 
  as_flextable()

# ajouter avec une autre regex


### avec pr?dicats tidyverse 
# mais il faut utiliser purr:

# https://purrr.tidyverse.org/
# https://purrr.tidyverse.org/reference/index.html

# https://www.youtube.com/watch?v=xkBFyNys1LI
# il est préférable d'utiliser where():
crosstable(spam.hcpc, c(where(is.integer), where(is.factor), -spam)) %>% 
  as_flextable()
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/integer

# avec des fonctions:
glimpse(spam.hcpc)
crosstable(spam.hcpc, where(function(x) is.numeric(x) && mean(x) > 1.0)) %>% 
  as_flextable()  
# ici les variables num?riques dont la moyenne est sup?rieure ? 1.0

# avec des formules:
crosstable(spam.hcpc, CapLM + CapLsup + CapLtot ~ spam) %>% 
  as_flextable()
# cela fonctionne comme avec lm() on peut donc sp?cifier une liste d'index:
crosstable(spam.hcpc, c(56:58) ~ spam) %>% 
  as_flextable()
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/integer

# l'avantage de formules est de permettre des combinaisons complexes:
crosstable(spam.hcpc, sqrt(CapLM) + I(CapLsup + CapLtot^2) ~ ifelse(CapLtot > 2.0,"CapLtot>2.0","CapLtot<2.0"),
           label=FALSE) %>% 
  as_flextable()

### un dernier exemple:
# I want all numeric variables that do not start by “d” or “w”, but I still want drat in the end.
crosstable(spam.hcpc,
           c(where(is.numeric), -matches("^d|w"), starts_with("C", ignore.case = FALSE)),
           label=FALSE) %>% 
  as_flextable()




### partie II avec textes et options de mise en forme...




