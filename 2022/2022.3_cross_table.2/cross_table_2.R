#### script cross_table_2 : export et formattage de cross_table


#### chargement des libraries -----------
library(tidyverse)
library(tidyselect)
library(tidymodels)
library(broom, broomExtra)
library(RColorBrewer)
library(readr)
library(coop)
library(crosstable)
### liens utiles sur tidyverse:
# https://principles.tidyverse.org/names-attribute.html
# https://dominicroye.github.io/en/2020/a-very-short-introduction-to-tidyverse/
# https://juba.github.io/tidyverse/index.html
# https://rodrigue.rhodatascience.com/posts/r/encoding/


## importation des données énergie ----------
## source : ORCAE 
## https://www.orcae-auvergne-rhone-alpes.fr/carte-interactive?celink=8&tx_ciminteractivemap_displayinteractivemap%5Bfieldset_8%5D%5Bcategories_parent%5D%5Bcategories.uid%5D=92&cHash=0e99381541f8a4e951d03f67a26831aa
# cliquer sur les trois intercos == ccvd, c3ps et diois sur la carte interactive!

CCVD <- read.csv2("C:/bricoleR/2022.3_cross_table.2/orcae_conso_communes_242600252_2022-06-27.csv",
                  stringsAsFactors = TRUE, dec = ",", na.strings = "NA")
summary(CCVD)
C3PS <- read.csv2("C:/bricoleR/2022.3_cross_table.2/orcae_conso_communes_200040509_2021-03-25.csv",
                  stringsAsFactors = TRUE, dec = ",", na.strings = "NA")
summary(C3PS)
Diois <- read.csv2("C:/bricoleR/2022.3_cross_table.2/orcae_conso_communes_242600534_2021-03-25.csv",
                   stringsAsFactors = TRUE, dec = ",", na.strings = "NA")
summary(Diois)

### résumés après importation
glimpse(Diois)
glimpse(C3PS)
glimpse(CCVD)

### problème de format de variable !
CCVD$valeur..GWh. <- as.numeric(CCVD$valeur..GWh.) # hard coding mais ça marche
### on peut retirer colonne 9 de CCVD
CCVD %>% select(-9) # retirer colonne 9 de CCVD
glimpse(CCVD)

### fusion des intercos par concaténation des lignes
Biovallee <- bind_rows(CCVD[1:8], C3PS, Diois)
# on a une table longue Biovallee complète avec les facteurs #
glimpse(Biovallee)
summary(Biovallee)

### enregistrement de biovallee:
saveRDS(Biovallee, "Biovallee.RDS") # enregistrer le fichier énergie biovallée 

### Seconde partie : traitements crosstable ----

# à partir de Biovalee on va créer fichier de consommations par usages résidentiels
# un tableau par usages selon les années et les intercos

# tableau intercos x années par usages domestiques
crosstable(Biovallee, c(usage, where(secteur == "Résidentiel")),
           by = c(nom.territoire)) %>% as_flextable()

crosstable(Biovallee, c(valeur..GWh.), by = c(nom.territoire, usage)) %>% as_flextable() # peu pratique il faudrait filtrer

crosstable(filter(Biovallee, secteur == "Résidentiel"),
           cols = c(énergie, usage), funs(sum(valeur..GWh.)),
           by = c(nom.territoire)) %>% as_flextable() 
# par énergie, somme par nom territoire
crosstable(filter(Biovallee, secteur == "Résidentiel"),
           cols = usage,
           by = c(nom.territoire)) %>% as_flextable()
# https://statisticsglobe.com/r-sum-by-group-example
# https://dplyr.tidyverse.org/articles/colwise.html
# https://www.statology.org/sum-by-group-in-r/


### extraction des données du secteur résidentiel: -----
residentiel <- filter(Biovallee, secteur == "Résidentiel") 
summary(residentiel)
residentiel$année <- as.factor(residentiel$année)


# construction d'une table des consommations d'électricité par usages
# le plus simple est de filtrer sur ce qu'on veut représenter:
residentiel.2020 <- filter(Biovallee,
                           année == "2020" & 
                           secteur == "Résidentiel" &
                           énergie == "Toutes énergies",
                           preserve = TRUE) %>% droplevels()
summary(residentiel.2020)
glimpse(residentiel.2020)

# autre possibilité:
residentiel.2020 <- filter(Biovallee,
                           année == "2020" & 
                           secteur == "Résidentiel" &
                           énergie == "Toutes énergies" &
                           usage %in% c("Autre électricité spécifique",
                                         "Chauffage",
                                         "Cuisson",
                                         "Eclairage",
                                         "ECS",
                                         "Froid",
                                         "Lavage",
                                         "Loisirs",
                                         "Tous usages"),
                           preserve = TRUE) %>% droplevels()

str(residentiel.2020) # ça marche !!!
# discussion intéressante sur le stacks suivant:
# https://stackoverflow.com/questions/1195826/drop-unused-factor-levels-in-a-subsetted-data-frame
# notons que Hadley suggère de ne pas utiliser l'option string.as.factor dans read.csv

# dans tous les cas, les caractéristiques sont préservées : choix le plus consistant.
# il faut donc filtrer en sur les facteurs qui posent problèmes.

# https://sscc.wisc.edu/sscc/pubs/dwr/index.html

residentiel.2020 <- rename(residentiel.2020, GWh = valeur..GWh.)
# attention nouveau nom = ancien nom !!! 


## https://stackoverflow.com/questions/26826865/how-to-drop-unused-levels-after-filtering-by-factor
crosstable(residentiel.2020,
           c(GWh),
           showNA = "no",
           by = c(usage, nom.territoire)) %>% as_flextable() 

crosstable(residentiel.2020,
           c(GWh, nom.territoire),
           by = c(usage),
           funs = c(sum)) %>% 
  as_flextable()

crosstable(residentiel.2020,
           c(GWh), by = c(usage),
           funs = c(sum, mean, "std dev"= sd)) %>% 
  as_flextable()


# somme seulee
crosstable(residentiel.2020, c(GWh), by = c(usage),
           funs = c(sum)) %>% 
  as_flextable()

### residentiel pour toutes les années par interco
residentiel.2018 <- filter(Biovallee,
                           année == "2018" & 
                             secteur == "Résidentiel" &
                             énergie == "Toutes énergies" &
                             usage %in% c("Autre électricité spécifique",
                                          "Chauffage",
                                          "Cuisson",
                                          "Eclairage",
                                          "ECS",
                                          "Froid",
                                          "Lavage",
                                          "Loisirs",
                                          "Tous usages"),
                           preserve = TRUE) %>% droplevels()
str(residentiel.2018)
summary(residentiel.2018)
residentiel.2018 <- rename(residentiel.2018, GWh = valeur..GWh.)
## apprendre code tidyverse across ici hard
residentiel.2018$année   <- as.factor(residentiel.2018$année)
residentiel.2018$code.insee  <- as.factor(residentiel.2018$code.insee)

### table sur 2018 somme par usages
crosstable(residentiel.2018,
           c(GWh),
           by = c(usage),
           funs = c(sum)) %>% 
  as_flextable()
### table sur 2018 somme par usages et territoires
crosstable(residentiel.2018,
           c(GWh),
           by = c(nom.territoire, usage),
           funs = c(sum)) %>% 
  as_flextable()
## idem mais avec la fonction uniquement sur variables numériques where()
crosstable(residentiel.2018,
           where(function(x) is.numeric(x)),
           by = c(nom.territoire, usage),
           funs = c(sum)) %>% 
  as_flextable()



residentiel.ter <- pivot_wider(residentiel.2018,
                              names_from = usage,
                              values_from = GWh,
                              values_fill = 0) %>% 
  group_by(nom.territoire) %>%
  summarize_at(vars("Autre électricité spécifique":"Tous usages"), list(name = sum)) # on voit ainsi comment construire les tables par le tuyau

# mais on aurait pu le faire à partir du tableau par usages 
residentiel.2018.usages <- pivot_wider(residentiel.2018,
                                       names_from = usage,
                                       values_from = GWh,
                                       values_fill = 0)

# afin de construire un tableau croisé par sommation sur les territoires:
crosstable(residentiel.2018.usages,
           where(function(x) is.numeric(x)),
           by = c(nom.territoire),
           funs = c(sum),
           total = "both") %>% 
  as_flextable() 
# mais attention ! il a considéré année et code insee comme des nombres !!
glimpse(residentiel.2018.usages)


### residentiel par énergie pour toutes les années par interco
Biovallee$code.insee <- as.factor(Biovallee$code.insee)
Biovallee$année <- as.factor(Biovallee$année)
residentiel.2018.energies <- filter(Biovallee,
                                    année == "2018" & 
                                    secteur == "Résidentiel" &
                                    usage %in% c("Autre électricité
                                                 spécifique",
                                                 "Chauffage",
                                                 "Cuisson",
                                                 "Eclairage",
                                                 "ECS",
                                                 "Froid",
                                                 "Lavage",
                                                 "Loisirs",
                                                 "Tous usages"),
                                    preserve = TRUE) %>% droplevels()
residentiel.2018.energies <- rename(residentiel.2018.energies,
                                    GWh = valeur..GWh.)
residentiel.2018.énergie <- pivot_wider(residentiel.2018.energies,
                                       names_from = énergie,
                                       values_from = GWh,
                                       values_fill = 0)
glimpse(residentiel.2018.énergie)
###
crosstable(residentiel.2018.énergie,
           where(function(x) is.numeric(x)),
           by = c(nom.territoire),
           funs = c(sum),
           total = "both") %>% 
  as_flextable()






### essais anciens :                           
resid.2018.inteco <- residentiel.2018 %>% 
                      group_by(nom.territoire) %>% 
                      summarize_at(vars("2020":"2010"), list(name = sum))

# toutes les fonctions pour obtenir résumés stats:
# https://stackoverflow.com/questions/9847054/how-to-get-summary-statistics-by-group
# très complet !

















# paquetage à voir:
# https://rpkgs.datanovia.com/rstatix/






# construction d'une table des consommations d'énergie par usages
# on utilisera pivot_wider pour déplier une table.

energies.annees <- pivot_wider(Biovallee,
                               names_from = c(année),
                               values_from = valeur..GWh.,
                               values_fn = sum,
                               values_fill = 0)

## grouper par intercommunalité
energies.annees.inteco <- energies.annees %>% 
  group_by(nom.territoire) %>% 
  summarize_at(vars("2020":"2010"), list(name = sum))

energies.annees.loc <- pivot_wider(Biovallee, 
                                    names_from = c(année, énergie),
                                    values_from = valeur..GWh.,
                                    values_fn = sum,
                                    values_fill = 0)


energies.annees.loc %>% group_by(c(secteur, nom.commune))
                                   
## on pourra produire des tables d'usages x énergies par interco pour 
## différentes années.
 
### travail sur les tables séparées par interco
glimpse(C3PS)
summary(C3PS)

resid.C3PS <- filter(C3PS,
                       secteur == "Résidentiel" &
                       énergie == "Toutes énergies" &
                       usage %in% c("Autre électricité spécifique",
                                    "Chauffage",
                                    "Cuisson",
                                    "Eclairage",
                                    "ECS",
                                    "Froid",
                                    "Lavage",
                                    "Loisirs",
                                    "Tous usages"),
                     preserve = TRUE) %>% droplevels()
str(resid.C3PS)
summary(resid.C3PS)
