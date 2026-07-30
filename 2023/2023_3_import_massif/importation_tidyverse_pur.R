# from : https://stackoverflow.com/questions/59482431/how-to-filter-a-very-large-csv-in-r-prior-to-opening-it
# solution tidyverse :


library(tidyverse)
library(furrr)

# Make a CSV file out of the NASA stock dataset for demo purposes
raw_data_path <- tempfile(fileext = ".csv")
nasa %>% as_tibble() %>% write_csv(raw_data_path)

# Get the row count of the raw data, incl. header row, without loading the
# actual data
raw_data_nrow <- length(count.fields(raw_data_path))

# Hard-code the largest batch size you can, given your RAM in relation to the
# data size per row
batch_size    <- 1e3 

# Set up parallel processing of multiple chunks at a time, leaving one virtual
# core, as usual
plan(multiprocess, workers = availableCores() - 1)

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
      raw_data_path,
      skip      = .x,
      n_max     = batch_size, 
      # Can't read in col. names in each chunk since they're only present in the
      # 1st chunk
      col_names = FALSE,
      # This reads in each column as character, which is safest but slowest and
      # most memory-intensive. If you're sure that each batch will contain
      # enough values in each column so that the type detection in each batch
      # will come to the same conclusions, then comment this out and leave just
      # the guess_max
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>% 
      # This is where you'd insert your filter condition(s)
      filter(TRUE),
    # Progress bar! So you know how many chunks you have left to go
    .progress = TRUE
  ) %>% 
  # The first row will be the header values, so set the column names to equal
  # that first row, and then drop it
  set_names(slice(., 1)) %>% 
  slice(-1)


#### adapter ceci:
### from : https://community.rstudio.com/t/filter-data-before-data-import-into-a-data-frame/122474/4

library(duckdb)
#> Loading required package: DBI
# original file (note long filename split over two lines)
# https://www.dnb.nl/statistieken/data-zoeken/#/details/aandelenbeursindices/dataset/
#  71497a3a-391b-41e3-a858-0d1cc6475208/resource/64131a7e-3eaa-45f1-a915-b47dbf05b517
# in local file the column "Periode " was changed to "Periode"
filename <- r'(D:\data\R\RStudio_Community\Aandelenbeursindices per dag 2021H2.csv)'

con <- dbConnect(duckdb())
str(dbGetInfo(con))
#> List of 5
#>  $ dbname    : chr ":memory:"
#>  $ db.version: chr "0.3.2-dev1"
#>  $ username  : logi NA
#>  $ host      : logi NA
#>  $ port      : logi NA

sql <- glue::glue(
  "CREATE TABLE WW AS SELECT \"Soort Index\", \"Periode\", \"waarde\"  FROM \'{filename}\' a WHERE a.\"Periode\" = '2021-07-01'  ;") 
print(sql)
#> CREATE TABLE WW AS SELECT "Soort Index", "Periode", "waarde"  FROM 'D:\data\R\RStudio_Community\Aandelenbeursindices per dag 2021H2.csv' a WHERE a."Periode" = '2021-07-01'  ;
dbExecute(con,sql) 
#> [1] 9

DBI::dbListTables(con)
#> [1] "ww"
df1<-DBI::dbReadTable(con,"WW")
str(df1)
#> 'data.frame':    9 obs. of  3 variables:
#>  $ Soort.index: chr  "AEX-index " "Midkap-index " "All-share index " "FinanciÃ«le instellingen " ...
#>  $ Periode    : Date, format: "2021-07-01" "2021-07-01" ...
#>  $ waarde     : num  731 1050 1042 308 34634 ...
dbDisconnect(con)
Created on 2021-11-27 by the reprex package (v2.0.0)

### discussion python
# https://www.reddit.com/r/learnpython/comments/23x1o9/filtering_data_from_a_huge_csv_file150200gb/


### proposition de chatGPT:

library(furrr)
library(dplyr)
library(readr)

# Define the batch size and read the total number of rows in the file
batch_size <- 100
raw_data_nrow <- nrow(read_csv("iris.csv"))

filtered_data <- seq(from = 0,
                     to = raw_data_nrow + batch_size,
                     by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip = .x,
      n_max = batch_size,
      col_names = FALSE,
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>%
      dplyr::filter(Species == "Setosa"), # Filter the data using dplyr::filter
    .progress = TRUE
  ) %>%
  set_names(slice(., 1)) %>%
  slice(-1)

# Output the filtered data
print(filtered_data)


### second essai chatGPT

library(furrr)
library(dplyr)
library(readr)

batch_size <- 100
raw_data_nrow <- nrow(read_csv("iris.csv"))

filtered_data <- seq(from = 0,
                     to = raw_data_nrow + batch_size,
                     by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip = .x,
      n_max = batch_size,
      col_names = if (.x == 0) col_names(read_csv("iris.csv")) else FALSE,  # Add column names for the first chunk
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>%
      dplyr::filter(Species == "Setosa"),  # Filter the data using dplyr::filter
    .progress = TRUE
  ) %>%
  set_names(slice(., 1)) %>%
  slice(-1)

print(filtered_data)


### correction

library(furrr)
library(dplyr)
library(readr)

batch_size <- 100
raw_data_nrow <- nrow(read_csv("iris.csv"))

filtered_data <- seq(from = 0,
                     to = raw_data_nrow + batch_size,
                     by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip = .x,
      n_max = batch_size,
      col_names = if (.x == 0) colnames(read_csv("iris.csv")) else FALSE,  # Corrected colnames() function
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>%
      dplyr::filter(Species == "Setosa"),  # Filter the data using dplyr::filter
    .progress = TRUE
  ) %>%
  set_names(slice(., 1)) %>%
  slice(-1)

print(filtered_data)

# troisième essai

library(furrr)
library(dplyr)
library(readr)

batch_size <- 100
raw_data_nrow <- nrow(read_csv("iris.csv"))

filtered_data <- seq(from = 0,
                     to = raw_data_nrow + batch_size,
                     by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip = .x,
      n_max = batch_size,
      col_names = if (.x == 0) colnames(read_csv("iris.csv")) else FALSE,
      col_types = cols(Species = col_character(),  # Specify column types explicitly
                       .default = col_character()),  # Set default column type
      guess_max = batch_size
    ) %>%
      dplyr::filter(Species == "Setosa"),
    .progress = TRUE
  ) %>%
  set_names(slice(., 1)) %>%
  slice(-1)

print(filtered_data)

### quatrième essai


library(furrr)
library(dplyr)
library(readr)

batch_size <- 100
raw_data_nrow <- nrow(read_csv("iris.csv"))

filtered_data <- seq(from = 0,
                     to = raw_data_nrow + batch_size,
                     by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      "iris.csv",
      skip = .x,
      n_max = batch_size,
      col_names = if (.x == 0) colnames(read_csv("iris.csv")) else FALSE,
      col_types = cols(Sepal.Length = col_double(),
                       Sepal.Width = col_double(),
                       Petal.Length = col_double(),
                       Petal.Width = col_double(),
                       Species = col_character()),  # Specify the column types explicitly
      guess_max = batch_size
    ) %>%
      dplyr::filter(Species == "Setosa"),
    .progress = TRUE
  ) %>%
  set_names(slice(., 1)) %>%
  slice(-1)

print(filtered_data)