library(tidyverse)
library(vroom)
setwd("c:/__Natalia")


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
(time = end - start)
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
(time = end - start)
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
(time = end - start)
gc()
saveRDS(oil_gas_intratrade_2016_2021, "oil_gas_intratrade_2016_2021.rds")

### extraction complète sur le fichier intratrade.
### par années 2016 -- 2021
gc()
### 2016
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
saveRDS(oil_gas_intratrade_2016_2021, "intratrade_2016.rds")
write.csv2(oil_gas_intratrade_2016_2021, "intratrade_2016.csv")
### 2017
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2017)
intratrade_2017 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2017, "intratrade_2017.rds")
write.csv2(intratrade_2017, "intratrade_2017.csv")
rm(intratrade_2017)

### 2018
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
saveRDS(intratrade_2018, "intratrade_2018.rds")
write.csv2(intratrade_2018, "intratrade_2018.csv")
rm(intratrade_2018)

### 2019
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2019)
intratrade_2019 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2019, "intratrade_2019.rds")
write.csv2(intratrade_2019, "intratrade_2019.csv")
rm(intratrade_2019)

### 2020
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2020)
intratrade_2020 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2020, "intratrade_2020.rds")
write.csv2(intratrade_2020, "intratrade_2020.csv")
rm(intratrade_2020)

### 2021
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2021)
intratrade_2021 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2021, "intratrade_2021.rds")
write.csv2(intratrade_2021, "intratrade_2021.csv")
rm(intratrade_2021)

## 2015
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2015)
intratrade_2015 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2015, "intratrade_2015.rds")
write.csv2(intratrade_2015, "intratrade_2015.csv")
rm(intratrade_2015)

## 2013
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2013)
intratrade_2013 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2013, "intratrade_2013.rds")
write.csv2(intratrade_2013, "intratrade_2013.csv")
rm(intratrade_2013)

## 2013
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2013)
intratrade_2013 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2013, "intratrade_2013.rds")
write.csv2(intratrade_2013, "intratrade_2013.csv")
rm(intratrade_2013)

## 2012
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2012)
intratrade_2012 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2012, "intratrade_2012.rds")
write.csv2(intratrade_2012, "intratrade_2012.csv")
rm(intratrade_2012)

## 2011
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2011)
intratrade_2011 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2011, "intratrade_2011.rds")
write.csv2(intratrade_2011, "intratrade_2011.csv")
rm(intratrade_2011)

## 2010
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2010)
intratrade_2010 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2010, "intratrade_2010.rds")
write.csv2(intratrade_2010, "intratrade_2010.csv")
rm(intratrade_2010)


## 2009
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2009)
intratrade_2009 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2009, "intratrade_2009.rds")
write.csv2(intratrade_2009, "intratrade_2009.csv")
rm(intratrade_2009)

## 2008
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2008)
intratrade_2008 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2008, "intratrade_2008.rds")
write.csv2(intratrade_2008, "intratrade_2008.csv")
rm(intratrade_2008)

## 2007
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2007)
intratrade_2007 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2007, "intratrade_2007.rds")
write.csv2(intratrade_2007, "intratrade_2007.csv")
rm(intratrade_2007)

## 2006
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2006)
intratrade_2006 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2006, "intratrade_2006.rds")
write.csv2(intratrade_2006, "intratrade_2006.csv")
rm(intratrade_2006)


## 2005
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2005)
intratrade_2005 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2005, "intratrade_2005.rds")
write.csv2(intratrade_2005, "intratrade_2005.csv")
rm(intratrade_2005)

## 2004
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2004)
intratrade_2004 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2004, "intratrade_2004.rds")
write.csv2(intratrade_2004, "intratrade_2004.csv")
rm(intratrade_2004)

## 2003
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2003)
intratrade_2003 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2003, "intratrade_2003.rds")
write.csv2(intratrade_2003, "intratrade_2003.csv")
rm(intratrade_2003)


## 2002
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2002)
intratrade_2002 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2002, "intratrade_2002.rds")
write.csv2(intratrade_2002, "intratrade_2002.csv")
rm(intratrade_2002)

## 2001
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2001)
intratrade_2001 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2001, "intratrade_2001.rds")
write.csv2(intratrade_2001, "intratrade_2001.csv")
rm(intratrade_2001)


### extraction des matrices bilatérales complètes ----
# filtrages sur années et codes SITC


# test sur intratrade OK
## 2001
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2001 & nchar(SitcRev3Product) < 3)
intratrade_2_2001 = read_delim_chunked(
  file = "US_IntraTrade_ST202210061409_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(intratrade_2_2001, "intratrade_2_2001.rds")
write.csv2(intratrade_2_2001, "intratrade_2_2001.csv")
rm(intratrade_2_2001)
### extraction par niveau d'agrégation !

### 2021 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2021 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2021 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 100000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2021, "trade_sitc_1_2021.rds")
write.csv2(trade_sitc_1_2021, "trade_sitc_1_2021.csv")
rm(trade_sitc_1_2021)


### 2020 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2020 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2020 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2020, "trade_sitc_1_2020.rds")
write.csv2(trade_sitc_1_2020, "trade_sitc_1_2020.csv")
rm(trade_sitc_1_2020)

### test de purr str_lenght() vs nchar() sur microbenchmark

library(microbenchmark)
# enregistrement de oil_intratrade
write.csv(oil_intratrade_2016_2021, "oil_intratrade_2016_2021.csv")
microbenchmark(f <- function(x, pos) subset(x, Year == 2020 & nchar(SitcRev3Product) == 1),
               trade_sitc_1_2020 = read_delim_chunked(
                 file = "oil_intratrade_2016_2021.csv",
                 delim = ",",
                 DataFrameCallback$new(f),
                 chunk_size = 1000000))

microbenchmark(f <- function(x, pos) subset(x, Year == 2020 & str_length(SitcRev3Product) == 1),
               trade_sitc_1_2020 = read_delim_chunked(
                 file = "oil_intratrade_2016_2021.csv",
                 delim = ",",
                 DataFrameCallback$new(f),
                 chunk_size = 1000000))

search()

### 2019 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2019 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2019 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2019, "trade_sitc_1_2019.rds")
write.csv2(trade_sitc_1_2019, "trade_sitc_1_2019.csv")
rm(trade_sitc_1_2019)


### 2018 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2018 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2018 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2018, "trade_sitc_1_2018.rds")
write.csv2(trade_sitc_1_2018, "trade_sitc_1_2018.csv")
rm(trade_sitc_1_2018)


### 2017 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2017 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2017 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2017, "trade_sitc_1_2017.rds")
write.csv2(trade_sitc_1_2017, "trade_sitc_1_2017.csv")
rm(trade_sitc_1_2017)


### 2016 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2016 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2016 = read_delim_chunked(
  file = "US_TradeMatrix_Part5_ST202209201350_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2016, "trade_sitc_1_2016.rds")
write.csv2(trade_sitc_1_2016, "trade_sitc_1_2016.csv")
rm(trade_sitc_1_2016)

### 2015 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2015 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2015 = read_delim_chunked(
  file = "US_TradeMatrix_Part4_ST202209201344_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2015, "trade_sitc_1_2015.rds")
write.csv2(trade_sitc_1_2015, "trade_sitc_1_2015.csv")
rm(trade_sitc_1_2015)

### 2013 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2013 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2013 = read_delim_chunked(
  file = "US_TradeMatrix_Part4_ST202209201444_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2013, "trade_sitc_1_2013.rds")
write.csv2(trade_sitc_1_2013, "trade_sitc_1_2013.csv")
rm(trade_sitc_1_2013)

### 2011 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2011 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2011 = read_delim_chunked(
  file = "US_TradeMatrix_Part4_ST202209201444_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2011, "trade_sitc_1_2011.rds")
write.csv2(trade_sitc_1_2011, "trade_sitc_1_2011.csv")
rm(trade_sitc_1_2011)

### 2010 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2010 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2010 = read_delim_chunked(
  file = "US_TradeMatrix_Part3_ST202209201438_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2010, "trade_sitc_1_2010.rds")
write.csv2(trade_sitc_1_2010, "trade_sitc_1_2010.csv")
rm(trade_sitc_1_2010)

### 2009 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2009 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2009 = read_delim_chunked(
  file = "US_TradeMatrix_Part3_ST202209201438_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2009, "trade_sitc_1_2009.rds")
write.csv2(trade_sitc_1_2009, "trade_sitc_1_2009.csv")
rm(trade_sitc_1_2009)

### 2008 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2008 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2008 = read_delim_chunked(
  file = "US_TradeMatrix_Part3_ST202209201438_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2008, "trade_sitc_1_2008.rds")
write.csv2(trade_sitc_1_2008, "trade_sitc_1_2008.csv")
rm(trade_sitc_1_2008)

### 2007 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2007 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2007 = read_delim_chunked(
  file = "US_TradeMatrix_Part3_ST202209201438_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2007, "trade_sitc_1_2007.rds")
write.csv2(trade_sitc_1_2007, "trade_sitc_1_2007.csv")
rm(trade_sitc_1_2007)


### 2006 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2006 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2006 = read_delim_chunked(
  file = "US_TradeMatrix_Part3_ST202209201438_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2006, "trade_sitc_1_2006.rds")
write.csv2(trade_sitc_1_2006, "trade_sitc_1_2006.csv")
rm(trade_sitc_1_2006)


### 2005 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2005 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2005 = read_delim_chunked(
  file = "US_TradeMatrix_Part2_ST202209201432_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2005, "trade_sitc_1_2005.rds")
write.csv2(trade_sitc_1_2005, "trade_sitc_1_2005.csv")
rm(trade_sitc_1_2005)

### 2004 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2004 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2004 = read_delim_chunked(
  file = "US_TradeMatrix_Part2_ST202209201432_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2004, "trade_sitc_1_2004.rds")
write.csv2(trade_sitc_1_2004, "trade_sitc_1_2004.csv")
rm(trade_sitc_1_2004)


### 2003 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2003 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2003 = read_delim_chunked(
  file = "US_TradeMatrix_Part2_ST202209201432_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2003, "trade_sitc_1_2003.rds")
write.csv2(trade_sitc_1_2003, "trade_sitc_1_2003.csv")
rm(trade_sitc_1_2003)


### 2002 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2002 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2002 = read_delim_chunked(
  file = "US_TradeMatrix_Part2_ST202209201432_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2002, "trade_sitc_1_2002.rds")
write.csv2(trade_sitc_1_2002, "trade_sitc_1_2002.csv")
rm(trade_sitc_1_2002)


### 2001 -- niveau 1
start = Sys.time()
f <- function(x, pos) subset(x, Year == 2001 & nchar(SitcRev3Product) == 1)
trade_sitc_1_2001 = read_delim_chunked(
  file = "US_TradeMatrix_Part2_ST202209201432_v1.csv",
  delim = ",",
  DataFrameCallback$new(f),
  chunk_size = 1000000)
end = Sys.time()
(time = end - start)
gc()
saveRDS(trade_sitc_1_2001, "trade_sitc_1_2001.rds")
write.csv2(trade_sitc_1_2001, "trade_sitc_1_2001.csv")
rm(trade_sitc_1_2001)