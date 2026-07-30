################################################################################
## S?ance du 24 mai 2023 sur l'import de donn?es ? partir de fichiers massifs ##
################################################################################

#' ceci est un YAML
#+ ceci est un chunk rmarkdown , echo = FALSE
# ceci est un commantaire R standard


#+ le probl?me : importer par lecture d'un fichier trop grand, echo = FALSE
#' on se heurte tout de suite ? la m?moire.
#' on devrait le traiter ? l'ext?rieur de R mais ce n'est pas toujours possible
#' Pour autant R offre de nombreuses possibilit?s avec tidyverse pour 
#' extraire des donn?es de fichiers massifs avec une relative facilit?

#' les sources de données :
#' https://unctadstat.unctad.org/7zip/US_IntraTrade.csv.7z
#' 

#+ Solution 1: fonctions read_chunked, echo = FALSE
#' ce sont des fonctions qui permettent d'importer les donn?es ? partir
#' de fichiers externes de trops grande taille

library(tidyverse) # l'indispensable tidyverse
library(vroom) # vroom est une librairie de lecture ultra rapide de fichiers
search() # afficher l'espace des noms complet
gc()


# read :
# https://readr.tidyverse.org/articles/readr.html

#' 1 -- pr?liminaire: s'informer sur les fichiers
#+ echo = FALSE
start = Sys.time()
file.info("US_IntraTrade_ST202210061409_v1.csv")
file.size("US_IntraTrade_ST202210061409_v1.csv")
end = Sys.time()
(time = end - start)
gc()

#' compter le nombre de lignes
#+ echo = FALSE
start = Sys.time()
length(readLines("US_IntraTrade_ST202210061409_v1.csv"))
end = Sys.time()
(time = end - start)
gc()
closeAllConnections() # tr?s utile pour gagner en m?moire !

# utils
start = Sys.time()
length(count.fields("US_IntraTrade_ST202210061409_v1.csv")) 
end = Sys.time()
(time = end - start)
gc()


# readr
start = Sys.time()
length(count_fields("US_IntraTrade_ST202210061409_v1.csv", tokenizer_csv()))
end = Sys.time()
(time = end - start)
gc()
# ?a va plus vite mais c'est long !!!
#' Attention 

# vroom
start = Sys.time()
length(vroom_lines("US_IntraTrade_ST202210061409_v1.csv", altrep = FALSE, progress = TRUE)) - 1L
end = Sys.time()
(time = end - start)
gc()

# bread
library(bread)
start = Sys.time()
bnrow(file = "US_IntraTrade_ST202210061409_v1.csv")
end = Sys.time()
(time = end - start)
gc()


closeAllConnections() # tr?s utile pour gagner en m?moire !


# https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r

# R.utils
library(R.utils)
start = Sys.time()
sapply("US_IntraTrade_ST202210061409_v1.csv", countLines)
end = Sys.time()
(time = end - start)
gc()
closeAllConnections()


# fonction R:
start = Sys.time()
fichier <- file("US_IntraTrade_ST202210061409_v1.csv", open="rb")
nlines <- 0L
while (length(chunk <- readBin(fichier, "raw", 65536)) > 0) {
  nlines <- nlines + sum(chunk == as.raw(10L))
}
print(nlines)
close(fichier)
end = Sys.time()
(time = end - start)
gc()
closeAllConnections()

#' 2 -- extraire les donn?es de fichiers externes
#' on va importer ? partir de US_Intratrade l'ann?e 2016:
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2016)
intratrade_2016 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2016, "intratrade_2016.rds")
write.csv2(intratrade_2016, "intratrade_2016.csv")
rm(oil_gas_intratrade_2016_2021)
# tout fonctionne parfaitement il a mis 26 secondes.
 
#' comme ce sont des fonctions tidyverse et base R, le subset admet le pipe:
#' on importe l'ann?e 2020 et 2021:
#+ echo = FALSE
start = Sys.time()
f <- function(x, pos) subset(x, Year %in% c(2020, 2021))
intratrade_2020_2021 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2020_2021, "intratrade_2020_2021.rds")
write.csv2(intratrade_2020_2021, "intratrade_2020_2021.csv")
# noter que le pipe am?liore la lecture de quelques secondes...

#' on peut aussi jouer sur la taille des morceaux lus : ici 10 000 ? 100 0000
#' importons 2018 par morceaux de 100 000 lignes
#' d'abord 10 000
#+ echo = FALSE
start = Sys.time()
f <- function(x, pos) subset(x, Year %in% c(2018))
intratrade_2018 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2018, "intratrade_2018.rds")
write.csv2(intratrade_2018, "intratrade_2018.csv")
#' puis 100 000
#+ echo = FALSE
start = Sys.time()
f <- function(x, pos) subset(x, Year %in% c(2018))
intratrade_2018 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2018, "intratrade_2018.rds")
write.csv2(intratrade_2018, "intratrade_2018.csv")
#' puis 1 000 000
#+ echo = FALSE
start = Sys.time()
f <- function(x, pos) subset(x, Year %in% c(2018))
intratrade_2018 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2018, "intratrade_2018.rds")
write.csv2(intratrade_2018, "intratrade_2018.csv")
#' finalement la taille des morceaux n'am?liore pas la vitesse cf. ci-dessous

#' 3 --- autres m?thodes d'extraction :
#+ echo = FALSE
# sqldf
# https://cran.r-project.org/web/packages/sqldf/readme/README.html
# https://stackoverflow.com/questions/tagged/sqldf
# https://stackoverflow.com/questions/41067275/filter-by-column-values-while-reading-using-read-csv-in-r
library(sqldf)

### visiblement un problème de traitement des doubles quotes == guillemets
### apparemment des fichiers non conformes au format csv ==> 
# https://stackoverflow.com/questions/32103639/read-csv-file-in-r-with-double-quotes
# il faut donc faire un parsing spécifique.
# cependant fread fait le job sans problèmes donc pas un obstacle....
start = Sys.time()
intratrade_2016 <- read.csv.sql("US_IntraTrade_ST202210061409_v1.csv", 
                                sql = "select * from file where Year = '2016' ",
                                sep = ",",
                                quote = "")
end = Sys.time()
(time = end - start)
rm(intratrade_2016)
closeAllConnections()
gc()

start = Sys.time()
World <- read.csv.sql("intratrade_2016.csv", 
                      sql = "select * from file where Economy = '0' ",
                      header = TRUE,
                      sep = ";",
                      quote = "")
end = Sys.time()
(time = end - start)

system.time(France <- read.csv2.sql("intratrade_2016.csv", 
                       sql = "select * from file where Economy Label = 'France' ",
                       sep = ";",
                       quote = ""))

#' exemple jouet de l'aide
write.csv(iris, "iris.csv", quote = FALSE, row.names = FALSE)
iris2 <- read.csv.sql("iris.csv", 
                      sql = "select * from file where Species = 'setosa' ")
(mtcars)
write.csv(mtcars, "mtcars.csv", quote = FALSE, row.names = FALSE)
start = Sys.time()
mtcars2 <- read.csv.sql("mtcars.csv",
                        sql = "select * from file where cyl > '4' ")
end = Sys.time()
(time = end - start) # ok ça marche parfaitement

#' exemple biovallée
# https://stackoverflow.com/questions/73972162/trying-to-read-20gb-of-data-read-csv-sql-produces-errors
start = Sys.time()
enedis_diois <- read.csv.sql("conso_biovallee_2011_2021.csv", 
                              sql = "select * from file where annee = '2021' ",
                              header = TRUE,
                              sep = ";",
                              eol = "\n")
end = Sys.time()
(time = end - start)
glimpse(enedis_diois) # intéressant résultat ! problème de format !

#' expertise vins AFM factomineR
start = Sys.time()
sauvignon <- read.csv.sql("data_PCA_ExpertWine.csv", 
                            sql = "select * from file where Label = 'Sauvignon' ",
                            header = TRUE,
                            sep = ";",
                            eol = "\n")
end = Sys.time()
(time = end - start)

closeAllConnections()
gc()




# https://stackoverflow.com/questions/48161683/r-sqldf-returning-dataframe-with-zero-rows

# tr?s complet :
# https://www.datacamp.com/tutorial/importing-data-r-part-two


# https://www.r-bloggers.com/2019/06/how-data-tables-fread-can-save-you-a-lot-of-time-and-memory-and-take-input-from-shell-commands/
# https://rdrr.io/github/MagicHead99/bread/man/bfilter.html
# bread !!! 
# https://cran.r-project.org/web/packages/bread/index.html
# https://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes/1820610#1820610



# avec bread :
library(bread)
start = Sys.time()
intratrade_2016 <- bfilter(file = "US_IntraTrade_ST202210061409_v1.csv",
                           patterns = '2016',
                           filtered_columns = 'Year',
                           sep = ',')
end = Sys.time()
(time = end - start)
glimpse(intratrade_2016)


# check later :
# https://www.book.utilitr.org/03_fiches_thematiques/fiche_import_fichiers_plats

# 4' ----