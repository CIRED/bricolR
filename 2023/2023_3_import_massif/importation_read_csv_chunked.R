library(tidyverse)
library(vroom)
library(sqldf)
library(bread)
library(sqlparseR)
setwd("c:/__Natalia")


# sur le sujet readr en général:
# https://rstudio-pubs-static.s3.amazonaws.com/267110_e68eaa23db6e4e29bd846c770ca35254.html

### avec read csv / delim chunked
# exemple de tidyverse :
# https://readr.tidyverse.org/reference/callback.html
# labels from xl import :
Year	Economy	Economy Label	Partner	Partner Label	Flow	Flow Label	SitcRev3Product	SitcRev3Product Label	US dollars at current prices in thousands	US dollars at current prices in thousands Footnote
# https://cran.r-project.org/web/packages/bread/bread.pdf

# Cars with 3 gears
f <- function(x, pos) subset(x, gear == 3)
read_csv_chunked(readr_example("mtcars.csv"),
                 DataFrameCallback$new(f),
                 chunk_size = 100000)


# de l'exemple ici :
# https://community.rstudio.com/t/filter-data-before-data-import-into-a-data-frame/122474/9f = function(x, pos) 
f <- function(x, pos) subset(x, 
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
oil_gas_trade_2016_2021 = read_csv2_chunked(
  file ="US_TradeMatrix_Part5_ST202209201450_v1.csv", # Chemin d'acc?s aux donn?es sources
  DataFrameCallback$new(f), # Appel de la fonction cr??e plus haut 
  chunk_size = 10000, # Nombre de lignes ? traiter pour chaque r?p?tition de la fonction f()
  col_names = TRUE)  # La premi?re ligne contient les noms des colonnes




read_lines_chunked("US_TradeMatrix_Part5_ST202209201450_v1.csv", str, chunk_size = 5)


f <- function(x, pos) subset(x, gear == 3)
read_csv_chunked(readr_example("mtcars.csv"), DataFrameCallback$new(f), chunk_size = 5)



oil_gas_trade_2016_2021 <- vroom("US_TradeMatrix_Part5_ST202209201450_v1.csv",
                                 filter(`SitcRev3Product` %in% c("33", "34")))

### test de la fonction  avec code == 33 == petrole
gc()
start = Sys.time()
f <- function(x) subset(x, "SitcRev3Product" == "33")
oil_gas_trade_2016_2021 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201450_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 20000000,
  col_names = TRUE)
end = Sys.time()
(time = start - end)
gc()


## essayer ceci :

# https://github.com/tidyverse/readr/issues/706

read_csv_chunked(rt_raw_file_bz2, DataFrameCallback$new(pf1), chunk_size = 10000)

pf1 == filter()

### ici : 
# https://stackoverflow.com/questions/55016426/read-csv-chunked-not-working-with-filter-in-list-or-is-element-in-list-in-r


f <- function(x, pos) filter(x, x[,1] %in% HCPlist$NPI)
NPPESinfo_list <- read_csv_chunked("npidata_pfile_20050523-20190210.csv", 
                                   DataFrameCallback$new(f), chunk_size = 10000)

# cela pourrait être dû à la mention du nom de la colonne dans l'appel de commande ???

### clarifications des classes callback:
# https://github.com/tidyverse/readr/issues/510


### Filtrer niveau 2 == code deux digits !
### filtrer ensuite produits puis pays etc.
# test 1 sur intratrade
### test de la fonction  avec code == 33 == petrole
gc()
start = Sys.time()
f <- function(x) filter(x, SitcRev3Product == "33")
oil_gas_trade_2016_2021 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000,
  col_names = TRUE)
end = Sys.time()
(time = start - end)
gc()
### avec subset
### test de la fonction  avec code == 33 == petrole
gc()
start = Sys.time()
f <- function(x, pos) subset(x, SitcRev3Product == "33")
oil_intratrade_2016_2021 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000)
end = Sys.time()
(time = start - end)
gc() ### il manquait l'argument pos pour repérer les chunks !!!

saveRDS(oil_intratrade_2016_2021, "oil_intratrade_2016_2021.rds")

### maintenant sur le gas et le pétrole en même temps !
gc()
start = Sys.time()
f <- function(x, pos) subset(x, SitcRev3Product %in% c("33", "34"))
oil_gas_intratrade_2016_2021 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 10000)
end = Sys.time()
(time = start - end)
gc()
saveRDS(oil_gas_intratrade_2016_2021, "oil_gas_intratrade_2016_2021.rds")


# correct
read_lines_chunked("US_IntraTrade_ST202210061409_v1.csv", str, chunk_size = 5)
# Print starting line of each chunk
f <- function(x, pos) print(pos)
read_lines_chunked("US_IntraTrade_ST202210061409_v1.csv",
                   SideEffectChunkCallback$new(f),
                   chunk_size = 10)
# Print starting line of each chunk
f <- function(x, pos) print(pos)
read_lines_chunked(readr_example("mtcars.csv"),
                   SideEffectChunkCallback$new(f),
                   chunk_size = 5)

# intéressant:
# https://github.com/tidyverse/readr/issues/1143

read_csv_chunked(readr_example("mtcars.csv"),
                 callback = ListCallback$new(function(x, pos) problems(x)),
                 chunk_size = 20,
                 col_types = paste0(rep("i", 11), collapse = ""))
read_csv_chunked(readr_example("mtcars.csv"),
                 callback = ListCallback$new(function(x, pos) class(x)),
                 chunk_size = 20)
