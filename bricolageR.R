######## premier script exemple de script
# juste p?ur voir...

#### section 1 -----


# c'est du commentaire

#### section 2 ----

 
### chargement des paquetages ----
library(magrittr) # paquetage n?cessaire pour utiliser le tuyau ou pipe de programmation
library(gapminder)
library(tidyverse)
install.packages("gapminder")
install.packages("data.table")

(.packages())

### d?finition du r?pertoire de travail ----
setwd("D:/cours_R")
setwd("c:/Users/user/desktop/Cours_R") ### changez cela dans votre environnement
getwd() # afficher le r?pertoire de travail.
theme_set(theme_bw())  # pre-set the bw theme.

theme_set(theme_bw())
theme_get()

# afficher les paquetages charg?s
(.packages()) # noter le . qui est une variable d'environnement R
# afficher tous les paquetages (notez la diff?rence)
library()
(.Rdata)


### premiers pas avec gapminder ----
# un extrait des wdi de la banque mondiale:  https://www.gapminder.org/data/
? gapminder
? library
str(gapminder) # examiner la structure d'un objet R arbitraire
class(gapminder)
? tibble
mode(gapminder)

head(gapminder) # visualiser les quelques premi?res lignes d'un cadre de donn?es
tail(gapminder) # on peut regarder les derni?res lignes par tail(df)
head(gapminder, 20) # visualiser les 20 premi?res lignes d'un cadre de donn?es
tail(gapminder, 20) # les vingt derni?res lignes
head(gapminder, -6L) # on n?glige les six derni?res lignes
head(gapminder, n = c(20, 2)) # on peut visualiser des blocs en pr?cisant par un vecteur
# ici c(20,2)
tail(gapminder, n = c(20, -2)) # en pr?cisant -2 dans le vecteur, la fonction comprend
# qu'il faut n?gliger les deux premi?res colonnes mais cela nous g?ne un peu ici:

# pour des ?l?ments sur l'indexation des R, voir:
# # http://www.cookbook-r.com/Basics/Indexing_into_a_data_structure/

### fonction glimpse (aper?u)
glimpse(gapminder) # glimpse == aper?u des donn?es ! Noter la transposition comme dans str()
glimpse(gapminder::continent_colors) # une variable du paquetage gapminder
View(gapminder) # ouvrir la visionneuse de donn?es
### c'est done un panel de s?ries temporelles par pays dans l'ordre alphab?tique
?View
View(gapminder)
glimpse(gapminder$pop)

### synth?se des donn?es -----
### premiers r?sum?s:
summary(gapminder) # r?sum?s de base

class(gapminder$continent)
mode(gapminder$continent)
table(gapminder$continent) # continent == d?coupage ONU
table(gapminder$year)
table(gapminder$country)


### creation d'un objet tableau r?sumant les donn?es par continent pour 2007
continent <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>%
  summarize(lifeExp = median(lifeExp))
continent
View(continent)
str(continent)

## variance de l'espérance de vie
continent.V.LE <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>%
  summarize(lifeExp = var(lifeExp))
continent.V.LE

### on peut regrouper selon l'ann?e et le continent:
#library(magrittr)
(.packages())

continent.yr <- gapminder %>%
  group_by(year, continent) %>%
  summarize(lifeExp = median(lifeExp))
continent.yr
str(continent.yr) # il est en format long mais cela pourra ?tre utile pour les graphiques

### resum? inverse:
continent.ct <- gapminder %>%
  group_by(continent, year) %>%
  summarize(lifeExp = median(lifeExp))
continent.ct
str(continent.ct) 

### on peut aussi grouper plusieurs r?sum?s:
continent.median <- gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>%
  summarize(lifeExp = median(lifeExp), 
            gdp_pc = median(gdpPercap), 
            pop = median(pop), 
            countries = n()) # il suffit donc de pr?ciser les r?sum?s voulus
continent.median
str(continent.median)

### on finit sur le resum? m?dian en format long avec ann?es et 4 indicateurs
continent.median.ct <- gapminder %>% 
  group_by(continent, year) %>%
  summarize(lifeExp = median(lifeExp), 
            gdp.pc = median(gdpPercap), 
            pop = median(pop), 
            countries = n()) # il suffit donc de pr?ciser les r?sum?s voulus
continent.median.ct
str(continent.median.ct)# structure plus complexe du tibble
summary(continent.median.ct)





### comment ensuite joindre gapminder avec les indicateurs de genr?s de l'ONU
(gapmind.2007 <- gapminder %>% 
  filter(year == 2007))

gapmind.2007
head(gapmind.2007, 20)
glimpse(gapmind.2007)
str(gapmind.2007) # tout est ok !
glimpse(gapmind.2007)

### importation des indicateurs du d?veloppement humain ONU

#  http://hdr.undp.org/en/content/download-data

#rm(ONU_HDI)
View(ONU_HDI)
head(ONU_HDI)
glimpse(ONU_HDI)
str(ONU_HDI)
summary(ONU_HDI) 
### nous voici tr?s embarass?s: les variables sont cod?es en caract?res !
# que s'est-il pass? ? l'importation ? 
# Rstudio a consid?r? les variables avec des caract?res textes comme textes !
# les caract?res ".." ne sont pas conformes dans R. Ce sont des donn?es manquantes
#on les remplace par une valeur valide: NA.
install.packages("naniar")
(R.version)
library(naniar)
# https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
ONU_HDI %>% replace_with_na_all(condition = ~. == "..")
str(ONU_HDI)
str(gapmind.2007)
summary(ONU_HDI)
### nous allons, changer les caract?ristiques des colonnes du tibble ONU_HDI
ONU_HDI$Country <- as.factor(ONU_HDI$Country)
ONU_HDI$HDI_Class <- as.factor(ONU_HDI$HDI_Class)
ONU_HDI$Rang_HDI_2019 <- as.numeric(ONU_HDI$Rang_HDI_2019)
ONU_HDI$age_median_2019 <- as.numeric(ONU_HDI$age_median_2019)
ONU_HDI$Gender_Inequality_2019 <- as.numeric(ONU_HDI$Gender_Inequality_2019)
ONU_HDI$GDI_2019 <- as.numeric(ONU_HDI$GDI_2019)
ONU_HDI$GDI_Group_2019 <- as.numeric(ONU_HDI$GDI_Group_2019)
ONU_HDI$GNI_2019 <- as.numeric(ONU_HDI$GNI_2019)
ONU_HDI$GDP_GUSDPPP_2017 <- as.numeric(ONU_HDI$GDP_GUSDPPP_2017)


### En fait on peut importer correctement tout cela avec readxl
# recommen?ons
View(ONU_HDI)
head(ONU_HDI)
glimpse(ONU_HDI)
str(ONU_HDI)
summary(ONU_HDI) 
# tout est ok sauf les deux premi?res colonnes qui sont en caract?res...
#https://cran.r-project.org/web/packages/WDI/index.html

### fichiers ONU r?gions:
# https://unstats.un.org/unsd/methodology/m49/overview/
View(UNSD_Regions)
head(UNSD_Regions)
glimpse(UNSD_Regions)
str(UNSD_Regions)
summary(UNSD_Regions) 
(.packages())
### on apparie les bases ONU_HDI et UNSD_Regions
# tout d'abord on vire les huit premi?res colonnes de UNSD_Regions
UNSD_Regions <- select(UNSD_Regions, -c(1:8)) # force brute mais ?a marche!
write.csv2(UNSD_Regions, "UNSD_Regions.csv")
# on fait ensuite la jointure ? gauche de

# https://dplyr.tidyverse.org/reference/mutate-joins.html

ONU <- left_join(ONU_HDI, UNSD_Regions, by = c("Country" = "Country or Area"))
View(ONU)
head(ONU, 20)
glimpse(ONU)
str(ONU)
# tout a parfaitement march? et on peut donc faire une jointure gauche sur ONU
summary(ONU) # avant cela on va se d?barasser des colonnes 13--17
ONU <- select(ONU, -c(13:17)) # attention ! bien distinguer c(13,17) de c(13:17)
glimpse(ONU)
ONU <- select(ONU, -c(12))
# on transforme la derni?re colonne en facteur:
ONU$`Developed / Developing Countries` <- as.factor(ONU$`Developed / Developing Countries`)


### on peut maintenant op?rer la jointure ? gauche sur gapminder_2007:
gapmind.2007.19 <- left_join(gapmind.2007, ONU, by = c("country" = "Country"))
View(gapmind.2007.19)
head(gapmind.2007.19, 20)
tail(gapmind.2007.19, 25)
glimpse(gapmind.2007.19)
str(gapmind.2007.19)
summary(gapmind.2007.19)
str(ONU)

gapmind.2007.19$HDI_Class <- as.factor(gapmind.2007.19$HDI_Class)
(.packages())
### exercices:
# 1. construire un r?sum? de gapmind_2007_19 regroup? par : continents, classes d'hdi et niveau  de developpement
# 2. examiner la jointure ? droite et compl?te de gapmind_2007 avec ONU : que constatez vous ?
# 3. sur la modification de ONU tester la difference entre c(13,17) et c(13:17) comment corrigeriez-vous ?
# 4. trouver quelques questions.

#changement du nom de Stadev
rename(gapmind.2007.19, "Stadev" = "Developed / Developing Countries")
