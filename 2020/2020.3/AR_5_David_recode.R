#### 
####  bricole'R 2019.7 
###  exemples tidyverse sur données IPCC
####
#### documentation et présentation:
### https://www.tidyverse.org/packages/
### 
### tutoriel R en français:
### https://juba.github.io/tidyverse/06-tidyverse.html

### lancement des paquetages necessaires:
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(AER)
library(rjags)
library(corrplot)
library(openxlsx)
library(officer)
library(RColorBrewer)
library(wesanderson)
library(ggsci)
library(gganimate)
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

### attachement des données
attach(ar5_sres_models)


### specification du répertoire de travail:
setwd("c:/bricoleR/AR5_David_Franck")

### resumé statistique de la nouvelle base de données:
summary(ar5_sres_models)
#rm(ar5_sres_models)
### curieusement il considère les 2055 -- 2100 comme des valeurs logiques vrai-faux !
### so we have some work on these data before we proceed...
### let us convert those columns to numeric with the mutate function.
# useful stack on mutate to change variables class and mode:
# https://stackoverflow.com/questions/3796266/change-the-class-from-factor-to-numeric-of-many-columns-in-a-data-frame

mode(ar5_sres_models)
class(ar5_sres_models)

### we proceed to apply all the commands to the whole data.
# first, we proceed to change all the variables coded as logical to numeric in the
# whole database:
ar5_sres_models <- ar5_sres_models %>% mutate_all( ~ as.numeric(is.logical(.)))
summary(ar5_sres_models)

# oops ! it really works
rm(ar5_sres_models)

# lets do it the right way...
#  using readxl, and modifying the variable modes from logical to numeric, and executing the equivalent script:
ar5_sres_models <- read_excel("C:/bricoleR/2019.8_AR5/ar5_sres_models.xlsx", 
                              +     col_types = c("text", "text", "text", 
                                                  +         "text", "text", "numeric", "numeric", 
                                                  +         "text", "text", "text", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "text", "text", "text", "text", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric"))
View(ar5_sres_models)

### ceci permet d'éviter l'importation des données projetées par année en mode logique 
### mais nous ne sommes pas au bout de nos difficultés: il faut recoder les variables caractères en facteurs.

### on utilise pour cela mutate:

ar5_sres_models <- mutate_if(ar5_sres_models, is.character, as.factor)
summary(ar5_sres_models)
### on a transformé toutes les variables en mode caractère en facteurs.
# so we have transformed all our data into factors.
# we can thus compute statistics and graphs by factors ; example:
# first attach data:

by(`2005`, MODEL, summary)
# use altGr-7 for the anti-quotes in order to designate variables 
# we can also use a vector of variables:
# les années les mieux renseignées sont 2005 : 2050
# pour calculer les résumés par année on doit faire une table spécifique:
# on créée un vecteurs d'indices des colonnes à lire dans ar5_sres
sres2050 <- c("2005","2010","2015","2020", "2025","2030", "2035", "2040", "2045", "2050")
# on créée ensuite le cadre de données avec pour réféfences de colonnes le vecteur sres2050
data2050 <- ar5_sres_models[, sres2050]
# résumé des données de data2050 par MODELE dans ar5_sres
by(data2050, MODEL, summary)

# on a dû dupliquer la base pour faire les synthèses, c'est assez pu efficient.
# cela peut être utile pour exporter les seules données

# Quelques variables ont été recondées en caractères. 
summary(CO2_only)
summary(Climate) 
summary(`Land use`)

### pour climate, Nogent ! we have a problem !
# on va recoder la valeur de damages en damage pour le facteur climate
Climate <- recode_factor(Climate, damages = "damage")
summary(Climate) 
### de même pour land-use on a une erreur avec deux modalités au lieu d'une sur bioenergy and food ;
# on reconde bioenergy food en bioenergy and food:
`Land use` <- recode_factor(`Land use`, `bioenergy food` = "bioenergy and food")
summary(`Land use`)

### calcul de résumés des années selon différentes facteurs
by(data2050, CO2_only, summary)
by(data2050, `Land use`, summary)
by(data2050, Coverage, summary)
by(data2050, Anticipations, summary)
by(data2050, SCENARIO, summary)
by(data2050, REGION, summary)

by(data2050, ar5_sres_models[ar5_sres_models$VARIABLE %in% 	"Emissions|CO2", CO2_only], summary)

### construction d'une base pour ACP et AFM. ----
ar5_mod_2050 <- select(ar5_sres_models, c(1:31))
View(ar5_mod_2050)
####
saveRDS(ar5_mod_2050, file = "ar5_mod_2050.rds")
saveRDS(ar5_sres_models, file = "ar5_sres.rds")
