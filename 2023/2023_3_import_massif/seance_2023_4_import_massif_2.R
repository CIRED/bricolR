################################################################################
## Séance du  juin 2023 sur l'import de données à partir de fichiers massifs ##
##                        seconde partie                                      ##
################################################################################

#' ceci est un YAML
#+ ceci est un chunk rmarkdown , echo = FALSE
# ceci est un commantaire R standard

#' on va faire des comparaisons de temps d'execution sur des imports externes
#' à partir de fichiers existants.

#+ Lancement des paquetages nécessaires, echo = FALSE
library(tidyverse) # l'indispensable tidyverse
library(vroom) # vroom est une librairie de lecture ultra rapide de fichiers
library(microbenchmark) # paquetage de benchmarking d'execution dans R
library(sqldf) # sqldf
library(bread) # bread
library(R.utils)

search() # afficher l'espace des noms complet
gc()


closeAllConnections() # tr?s utile pour gagner en m?moire !


#' comparons les comptage des lignes avant importation
#+ comptage R.utils
start = Sys.time()
sapply("US_IntraTrade_ST202210061409_v1.csv", countLines)
end = Sys.time()
(time = end - start)
#' comparaison sur un fichier jouet : fichier mtcars en csv
microbenchmark(sapply("mtcars.csv", countLines)) # R.utils
microbenchmark(length(readLines("mtcars.csv"))) # base R
microbenchmark(length(count.fields("mtcars.csv"))) # utils
microbenchmark(length(count_fields("mtcars.csv", tokenizer_csv()))) # readr
microbenchmark(length(vroom_lines("mtcars.csv", altrep = FALSE, progress = TRUE)) - 1L) # vroom
microbenchmark(bnrow(file = "mtcars.csv")) # bread


#' on peut faire tout cela en une seule expression:
library(genio)
library(microbenchmark)
compteligne <- microbenchmark(
  R.utils = sapply("mtcars.csv", countLines),
  base_R = length(readLines("mtcars.csv")),
  utils = length(count.fields("mtcars.csv")),
  readr = length(count_fields("mtcars.csv", tokenizer_csv())),
  vroom = length(vroom_lines("mtcars.csv", altrep = FALSE, progress = TRUE)) - 1L,
  bread = bnrow(file = "mtcars.csv"),
  genio = count_lines("mtcars.csv"))

boxplot(compteligne, names = c('R.utils', 'base_R', 'utils', 'readr', 'vroom', 'bread', 'genio'))

#' https://www.r-bloggers.com/2015/01/using-the-microbenchmark-package-to-compare-the-execution-time-of-r-expressions/
#' https://www.statology.org/r-microbenchmark/
#' autre possibilité : genio == https://search.r-project.org/CRAN/refmans/genio/html/count_lines.html

#' verdict : 
#' 1 = base R, 2 = utils, 3 = R.utils, 4 = genio, 5 = readr, 6 = vroom, 7 = bread

compteligne <- microbenchmark(
  R.utils = sapply("mtcars.csv", countLines),
  base_R = length(readLines("mtcars.csv")),
  utils = length(count.fields("mtcars.csv")),
  readr = length(count_fields("mtcars.csv", tokenizer_csv())),
  vroom = length(vroom_lines("mtcars.csv", altrep = FALSE, progress = TRUE)) - 1L,
  bread = bnrow(file = "mtcars.csv"),
  genio = count_lines("mtcars.csv"),
  n)
print(compteligne)
boxplot(compteligne, names = c('R.utils', 'base_R', 'utils', 'readr', 'vroom', 'bread', 'genio'))
autoplot(compteligne)

#' doubler la précision == quadrupler le nombre de réplications == 400
#+ double précision, echo = FALSE
compteligne.double <- microbenchmark(times = 400L,
  R.utils = sapply("mtcars.csv", countLines),
  base_R = length(readLines("mtcars.csv")),
  utils = length(count.fields("mtcars.csv")),
  readr = length(count_fields("mtcars.csv", tokenizer_csv())),
  vroom = length(vroom_lines("mtcars.csv", altrep = FALSE, progress = TRUE)) - 1L,
  bread = bnrow(file = "mtcars.csv"),
  genio = count_lines("mtcars.csv"))

print(compteligne.double)
boxplot(compteligne.double, names = c('R.utils', 'base_R', 'utils', 'readr', 'vroom', 'bread', 'genio'))
autoplot(compteligne.double)

gc()
showConnections(all = TRUE)
closeAllConnections()


# fonction R:
start = Sys.time()
fichier <- file("US_IntraTrade_ST202210061409_v1.csv", open ="rb")
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

#' 2 -- évaluation du temps d'execution d'une fonction sur mtcars
#' exemple de lecture importation d'un fichier 
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

#' benchmarking des fonctions 
library(bench)
start = Sys.time()
f <- function(x, pos) subset(x, cyl == 4) 
     mtcars_read = read_delim_chunked(
     file = "mtcars.csv",
     delim = ",",
     DataFrameCallback$new(f),
     chunk_size = 5)
end = Sys.time()
(time = end - start) #ok it works

# csv
start = Sys.time()
f <- function(x, pos) subset(x, cyl == 4) 
      mtcars_read = read_csv_chunked(
      file = "mtcars.csv",
      DataFrameCallback$new(f),
      chunk_size = 5)
end = Sys.time()
(time = end - start) # petite différence....

# avec le pipe sur deux valeurs de cyl
start = Sys.time()
f <- function(x, pos) subset(x, cyl %in% c(4, 6))
       mtcars_read = read_delim_chunked(
       file = "mtcars.csv",
       delim = ",",
       DataFrameCallback$new(f),
       chunk_size = 5)
end = Sys.time()
(time = end - start)

start = Sys.time()
f <- function(x, pos) subset(x, cyl %in% c(4, 6))
      mtcars_read = read_csv_chunked(
      file = "mtcars.csv",
      DataFrameCallback$new(f),
      chunk_size = 5)
end = Sys.time()
(time = end - start)


### spécifier valeur exacte
start = Sys.time()
f <- function(x, pos) subset(x, cyl == 4)
      mtcars_read = read_delim_chunked(
      file = "mtcars.csv",
      delim = ",",
      DataFrameCallback$new(f),
      chunk_size = 10)
end = Sys.time()
(time = end - start)

start = Sys.time()
f <- function(x, pos) subset(x, cyl %in% c(4))
mtcars_read = read_delim_chunked(
  file = "mtcars.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10)
end = Sys.time()
(time = end - start)
# notez l'écart sensible du pipe même pour une valeur unique !

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
### année
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2018)
intratrade_2018 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()

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
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2018)
intratrade_2018 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()


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


compte_sqldf <- microbenchmark(
                cyl_eq_4 = read.csv.sql("mtcars.csv", 
                                        sql = "select * from file where cyl = '4' ",
                                        sep = ","),
                cyl_eq_4_or_6 = read.csv.sql("mtcars.csv", 
                                             sql = "select * from file where cyl = '4' OR cyl = '6' ",
                                             sep = ","),
                cyl_sup_4 = read.csv.sql("mtcars.csv",
                                         sql = "select * from file where cyl > '4' "))

autoplot(compte_sqldf)
glimpse(compte_sqldf)
ggplot(compte_sqldf) +
geom_violin(aes(x = time, y = expr))



iris_filter <- microbenchmark(
                sqldf = read.csv.sql("iris.csv", 
                            sql = "select * from file where Species = 'setosa' "),
                bread = bfilter("iris.csv",
                            patterns = 'setosa',
                            filtered_columns = 'Species'))
closeAllConnections()
gc()
autoplot(iris_filter)
glimpse(iris_filter)
ggplot(iris_filter) +
  geom_violin(aes(x = time, y = expr))

# bread = importation de l'année 2016 à parti r de intratrade :
library(bread)
start = Sys.time()
intratrade_2016 <- bfilter(file = "US_IntraTrade_ST202210061409_v1.csv",
                           patterns = '2016',
                           filtered_columns = 'Year',
                           sep = ',')
end = Sys.time()
(time = end - start)
glimpse(intratrade_2016)
gc()

# check later :
# https://www.book.utilitr.org/03_fiches_thematiques/fiche_import_fichiers_plats

# 4' ----
#' méthodes ultra rapides : = parallélisation multicoeur
library(tidyverse)
library(furrr)
glimpse(iris)
str(iris)
mode(iris)

# Make a CSV file out of the NASA stock dataset for demo purposes
raw_data_path <- tempfile(fileext = ".csv")
nasa %>% as_tibble() %>% write_csv(raw_data_path)

# Get the row count of the raw data, incl. header row, without loading the
# actual data
raw_data_nrow <- length(count.fields("iris.csv"))

# Hard-code the largest batch size you can, given your RAM in relation to the
# data size per row
batch_size    <- 1e2 

# Set up parallel processing of multiple chunks at a time, leaving one virtual
# core, as usual
plan(multisession, workers = availableCores() - 1)

filtered_data <- 
  # Define the sequence of start-point row numbers for each chunk (each number
  # is actually the start point minus 1 since we're using the seq. no. as the
  # no. of rows to skip)
  seq(from = 0, 
      # Add the batch size to ensure that the last chunk is large enough to grab
      # all the remainder rows
      to = raw_data_nrow + batch_size, 
      by = batch_size) %>% 
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip      = .x,
      n_max     = batch_size, 
      # Can't read in col. names in each chunk since they're only present in the
      # 1st chunk
      col_names = FALSE,
      # This reads in each column as character, which is safest but slowest and
      # most memory-intensive. If you're sure that each batch will contain
      # enough values in each column so that the type detection in each batch
      # will come to the same conclusions, then comment this out and leave just
      # the guess_max ==
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>% 
      print(.) %>%
      str(.) %>% 
      # This is where you'd insert your filter condition(s)
      filter(.$X5 == "setosa"),
    # Progress bar! So you know how many chunks you have left to go
    .progress = TRUE
  ) %>% 
  # The first row will be the header values, so set the column names to equal
  # that first row, and then drop it
  set_names(slice(., 1)) %>% 
  slice(-1)

names(filtered_data) <- names(iris)
glimpse(filtered_data)

### study this:
# https://community.rstudio.com/t/from-do-to-future-map-dfr-with-tidyeval/21829/5
# https://www.youtube.com/watch?v=U7ynho78-vA
# https://www.schlosslab.org/mikropml/articles/parallel.html
# https://www.youtube.com/watch?v=K7rXergzO_Q
# https://www.r-bloggers.com/2021/09/tidy-parallel-processing-in-r-with-furrr/
# https://github.com/tidyverse/purrr
# https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html
# https://www.zevross.com/blog/2019/02/12/dramatically-speed-up-your-r-purrr-functions-with-the-furrr-package/
# 

help("future_map2_dfr")

availableCores()
# https://furrr.futureverse.org/reference/future_map.html

# test du code help
plan(multisession, workers = 2)

1:10 %>%
  future_map(rnorm, n = 10, .options = furrr_options(seed = 123)) %>%
  future_map_dbl(mean)

# https://stackoverflow.com/questions/48161683/r-sqldf-returning-dataframe-with-zero-rows

# tr?s complet :
# https://www.datacamp.com/tutorial/importing-data-r-part-two


# https://www.r-bloggers.com/2019/06/how-data-tables-fread-can-save-you-a-lot-of-time-and-memory-and-take-input-from-shell-commands/
# https://rdrr.io/github/MagicHead99/bread/man/bfilter.html
# bread !!! 
# https://cran.r-project.org/web/packages/bread/index.html
# https://stackoverflow.com/questions/1727772/quickly-reading-very-large-tables-as-dataframes/1820610#1820610
# https://stackoverflow.com/questions/73972162/trying-to-read-20gb-of-data-read-csv-sql-produces-errors
 