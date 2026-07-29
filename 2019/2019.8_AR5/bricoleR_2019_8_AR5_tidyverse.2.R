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

### specification du répertoire de travail:
setwd("c:/bricoleR/2019.8_AR5")

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
# sample code to do the job:
# by column type:
df %>% 
  mutate_if(is.factor, ~as.numeric(as.character(.)))

# by specific columns:
df %>% 
  mutate_at(vars(x, y, z), ~as.numeric(as.character(.))) 

# all columns:
df %>% 
  mutate_all(~as.numeric(as.character(.)))

### we proceed to apply all the commands to the whole data.
# first, we proceed to change all the variables coded as logical to numeric in the
# whole database:
ar5_sres_models <- ar5_sres_models %>% mutate_all( ~ as.numeric(is.logical(.)))
summary(ar5_sres_models)

# oops ! it really works
rm(ar5_sres_models)

# lets do it the right way...
  



### partie pour la séance 2018.7 ----
### partie du script de David: importation directe des données SRES AR5 
TAB <- read.csv("ar5_public_version102_compare_compare_20150629-130000.csv", sep=",", header=T)
head(TAB)
summary(ar5_sres_models)

### on va se consacrer au total modial:
DATA <- TAB %>% filter(REGION == "World")
### on extrait ensuite les colonnes modele, scenario, region, variable et les années par
### pas de 10 ans
DATA <- DATA %>% select(MODEL, SCENARIO, REGION, VARIABLE,
                        X2010, X2020, X2030, X2040, X2050)
### quelle est la nature de la variable modèle ?
mode(DATA$MODEL)
class(DATA$MODEL)

### on transforme la variable modele en caracteres:
DATA$MODEL <- as.character(DATA$MODEL)

### Maintenant, on extrait les données voulues:
Emissions <- DATA %>% filter(VARIABLE == "Emissions|CO2")
## creation de la table emissions on rassemble les valeurs X1 par années consécutives:
Emissions <- Emissions %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = X1)

### idem pour la table coûts:
### on extrait d'abord les données de coût en pertes de PIB:
Cost <- DATA %>% filter(VARIABLE == "Policy Cost|GDP Loss")
### Puis on le rassemble:
Cost <- Cost %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = Cost)

Cost <- arrange(Cost, MODEL, SCENARIO)

### pareil pour le GDP, on extrait les PIBs:
GDP <- DATA %>% filter(VARIABLE == "GDP|MER")
### puis on les rassemble par la commande "gather":
GDP <- GDP %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = GDP)

### idem population:
Population <- DATA %>% filter(VARIABLE == "Population")
Population <- Population %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = Population)

### Prenons maintenant l'énergie primaire:
PrimEner <- DATA %>% filter(VARIABLE == "Primary Energy")
PrimEner <- PrimEner %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = PrimaryEner)

### idem pour l'énergie finale:
FinalEner <- DATA %>% filter(VARIABLE == "Final Energy")
FinalEner <- FinalEner %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = FinalEner)

### idem pour carbon price:
Carbprice <- DATA %>% filter(VARIABLE == "Price|Carbon")
Carbprice <- Carbprice %>% gather('X2010', 'X2020', 'X2030', 'X2040', 'X2050', key = Year, value = Carbprice)


### seance 2019.8 ----
### Pour garder plus d'observations on va finalement composer une table
### construction récursive de la base par concaténation de colonnes par jointure gauche:
DATA_left <- left_join(Emissions, GDP, by = c("MODEL","SCENARIO","Year"))
DATA_left <- left_join(DATA_left, Population, by = c("MODEL","SCENARIO","Year"))
DATA_left <- left_join(DATA_left, PrimEner, by = c("MODEL","SCENARIO","Year"))
DATA_left <- left_join(DATA_left, FinalEner, by = c("MODEL","SCENARIO","Year"))
DATA_left <- left_join(DATA_left, Carbprice, by = c("MODEL","SCENARIO","Year"))

head(DATA_left)
summary(DATA_left)
names(DATA_left)

### recalling DATAcol (notice name)
head(DATAcol)
summary(DATAcol)
names(DATAcol)

##### selction des colonnes utiles dans la table DATA_left
DATA_left_sel <- DATA_left %>% select("MODEL","SCENARIO","Year","X1","GDP",
                                      "Population","PrimEner", "FinalEner", "Carbprice") head(DATA_left_sel)
head(DATA_left_sel)
#####New variables####

DATAsel <- mutate(DATAsel, Y = DATAsel$"Cost"/(DATAsel$"Cost" + DATAsel$"GDP"))
head(DATAsel)
summary(DATAsel)

### on voit que les données sont empilées par scénario. On aimerait faire autrement.
DATAsel <- arrange(DATAsel, SCENARIO)
### ça marche pas ! effet garcimore !
DATAsel <- arrange(DATAsel, MODEL, SCENARIO)
### on peut donc trier selon plusieurs critères. Ici deux suffisent: modele-scenario semble plus logique.
# A ce stade, donc, nous avons un fichier trié par modèle puis scenario



#####Dataset with two years of data
DATAmod_1 <- DATAsel %>% filter(Year == "X2050")
DATAmod_2 <- DATAsel %>% filter(Year == "X2030") %>% select("MODEL","SCENARIO","X1")
names(DATAmod_2)[3] <- "X2"
DATAmod <- left_join(DATAmod_1, DATAmod_2, by = c("MODEL","SCENARIO"))

#remove NAs
DATAmod<-na.omit(DATAmod)