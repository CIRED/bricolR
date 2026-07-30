library(tidyverse)
library(vroom)
library(sqldf)
library(bread)
libary(bigmemory) # big memory
library(sqlparseR)

setwd("c:/__Natalia")


# utilisation de bread
# from : 
# https://github.com/MagicHead99/bread/
# https://rdrr.io/github/MagicHead99/bread/man/bfilter.html
getwd()

# pour mémoire, différentes options ----
# RSQL : 
# https://github.com/rOpenStats/RSQL


# RSQLite:
# https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html


# conseils pour traiter des grosses sources de données :
# https://inbo.github.io/tutorials/tutorials/r_large_data_files_handling/
# https://waterdata.usgs.gov/blog/formats/
# https://stackoverflow.com/questions/38536226/r-reading-a-huge-csv
# 

# utiliser file.info
# https://stackoverflow.com/questions/30580798/how-to-check-file-size-before-opening

file.info("US_TradeMatrix_Part5_ST202209201450_v1.csv")
file.info("US_IntraTrade_ST202210061409_v1.csv")
# utilisation de bdfilter:
oil_trade_2016_2021 <- bread("US_TradeMatrix_Part5_ST202209201450_v1.csv",
                             patterns = "33",
                             filtered_columns = 'SitcRev3Product')


unctad_names <- names(US_IntraTrade_ST202210061409_v1)
(unctad_names)
saveRDS(US_IntraTrade_ST202210061409_v1, "US_Intra_Trade.rds")



# importation par filtrage sql avec sqldf

oil_trade_2016_2021 <- read.csv.sql("US_TradeMatrix_Part5_ST202209201450_v1.csv", 
                      sql = "select * from file where SitcRev3Product == '33' ")
length(vroom_lines("US_TradeMatrix_Part5_ST202209201450_v1.csv", altrep_opts = TRUE, progress = FALSE)) - 1L
oil_trade_nrow <- length(count.fields("US_TradeMatrix_Part5_ST202209201450_v1.csv"))


oil_intratrade_nrow <- length(count.fields("US_IntraTrade_ST202210061409_v1.csv"))

closeAllConnections()
oil_intratrade_2016_2021 <- read.csv.sql("US_IntraTrade_ST202210061409_v1.csv", 
                      sql = "select * from file where SitcRev3Product == '33' ",
                      sep = ",")
# 12522069 de lignes !!!
length(vroom_lines("US_IntraTrade_ST202210061409_v1.csv", altrep_opts = TRUE, progress = FALSE)) - 1L
# https://stackoverflow.com/questions/48161683/r-sqldf-returning-dataframe-with-zero-rows
# https://github.com/ggrothendieck/sqldf
# retrouver ici, dans ce site : 
# https://www.rdocumentation.org/packages/sqldf/versions/0.4-11
# https://community.rstudio.com/t/filter-data-before-data-import-into-a-data-frame/122474/14

top_few <- read.delim("US_IntraTrade_ST202210061409_v1.csv", sep = ",", nrows = 10)

top_few_trade <- read.delim("US_TradeMatrix_Part5_ST202209201450_v1.csv",sep = ",", nrows = 1000)

# possible que cela marche avec :
# https://stackoverflow.com/questions/61990541/how-do-read-file-by-filtering-rows-based-on-a-condition-in-r

fread(file=file_name, select=col_names)[grep(pattern, specific_col_name, ignore.case = TRUE)]

fread(file=file_name, select=col_names)[specific_col_name %in% ID_name] 

# pas du tout la bonne solution : 
# https://stackoverflow.com/questions/70520536/error-loading-20gb-csv-file-in-r-using-sqldf


# from : https://community.rstudio.com/t/filter-data-before-data-import-into-a-data-frame/122474/9
# Import des donn?es




# Installation et chargement du package d?di? ? l'import

install.packages("readr")
library(readr)

# Fonction pour cr?er un sous-ensemble du jeu de donn?es d'origine (ici nomm? x)

f = function(x, pos) 
  subset(x, 
         REGION == "93" & NAF08 !="ZZZZZ", 
         select = c(REGION, IPONDI,DIPL,EMPL,NAF08,SEXE,STAT,TACTD16,TP,AGED))

# Application de la fonction f() et stockage du sous-ensemble g?n?r? dans une table R (rp2018PacaV2)

## On va appliquer la fonction sur des portions (chunks) du jeu de donn?es d'origine, petit ? petit (par lot de 10 000 lignes)
## avant d'importer les donn?es filtr?es.

rp2018PacaV2 = read_csv2_chunked(
  file ="C:/Users/fabien/Desktop/Import_parties/sources/FD_INDREG_2018.csv", # Chemin d'acc?s aux donn?es sources
  DataFrameCallback$new(f), # Appel de la fonction cr??e plus haut 
  chunk_size = 10000, # Nombre de lignes ? traiter pour chaque r?p?tition de la fonction f()
  col_names = TRUE) # La premi?re ligne contient les noms des colonnes


# Export des donn?es de sortie au format Excel

## Installation et chargement du paquet
install.packages("openxlsx")
library(openxlsx) 

## Cr?ation d'un fichier et d'un classeur Excel vide
fileName = "rp2018PacaV2.xlsx"
excel <- createWorkbook(fileName)

## Cr?ation d'une feuille Excel vide
firstSheet <- "Feuille1"

## Ajout de la feuille dans le classeur
addWorksheet(excel, firstSheet)

## Ajout des donn?es de la table R dans le classeur
writeData(excel, sheet = 1, rp2018PacaV2)

## Sauvegarde des donn?es ajout?es dans le fichier Excel
saveWorkbook(excel, file = fileName, overwrite = TRUE)


# application de ce script :
# from : https://community.rstudio.com/t/filter-data-before-data-import-into-a-data-frame/122474/9
# Import des donn?es

# Installation et chargement du package d?di? ? l'import

library(tidyverse)

# Fonction pour cr?er un sous-ensemble du jeu de donn?es d'origine (ici nomm? x)

# sélection gaz et pétrole
oil_gas_sict3 <- c("33", "333", "334", "335", "34", "342", "343", "344", "345")

f = function(x, pos) 
  subset(x, 
         SitcRev3Product == "33" & SitcRev3Product == "34", 
         select = c(Year,
                    Economy,
                    `Economy Label`,
                    Partner,
                    `Partner Label`,
                    Flow,
                    `Flow Label`,
                    SitcRev3Product,
                    `SitcRev3Product Label`,
                    `US dollars at current prices in thousands`,
                    `US dollars at current prices in thousands Footnote`))

# Application de la fonction f() et stockage du sous-ensemble g?n?r? dans une table R (rp2018PacaV2)

## On va appliquer la fonction sur des portions (chunks) du jeu de donn?es d'origine, petit ? petit (par lot de 10 000 lignes)
## avant d'importer les donn?es filtr?es.

oil_gas_trade_2016_2021 = read_csv2_chunked(
  file ="US_TradeMatrix_Part5_ST202209201450_v1.csv", # Chemin d'acc?s aux donn?es sources
  DataFrameCallback$new(f), # Appel de la fonction cr??e plus haut 
  chunk_size = 10000, # Nombre de lignes ? traiter pour chaque r?p?tition de la fonction f()
  col_names = TRUE)  # La premi?re ligne contient les noms des colonnes


# Export des donn?es de sortie au format Excel

## Installation et chargement du paquet
install.packages("openxlsx")
library(openxlsx) 

## Cr?ation d'un fichier et d'un classeur Excel vide
fileName = "rp2018PacaV2.xlsx"
excel <- createWorkbook(fileName)

## Cr?ation d'une feuille Excel vide
firstSheet <- "Feuille1"

## Ajout de la feuille dans le classeur
addWorksheet(excel, firstSheet)

## Ajout des donn?es de la table R dans le classeur
writeData(excel, sheet = 1, rp2018PacaV2)

## Sauvegarde des donn?es ajout?es dans le fichier Excel
saveWorkbook(excel, file = fileName, overwrite = TRUE)