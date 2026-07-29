library(tidyverse)
library(readxl)




# Read data ---------------------------------------------------------------

df <- read_delim(file = "inputs/emissions/env_ac_ainah_r2.tsv", 
                 delim = "\t",
                 comment = "#",
                 na = c("", "NA", ": "))


# Separate -------------------------------------------------------------

df2 <- df %>% 
  separate(1, into = c("air_pol", "sector", "unit", "geo"), sep = ",")


# Gather ------------------------------------------------------------------

df3 <- df2 %>% 
  gather(key = year, value = emissions, -c(1:4))

df4 <- df3 %>% 
  filter(unit == "KG_HAB" & sector == "TOTAL" & 
           air_pol %in% c("CO2", "N2O_CO2E", "CH4_CO2E")) 


# STRINGR -----------------------------------------------------------------

#Generic
str_detect(c("coucou", "hello", "ahem"), "h")

#Début de caractère
str_detect(c("coucou", "hello", "ahem"), "^h")

#Fin de caractère
str_detect(c("coucou", "hello", "ahem"), "o$")

#Lettres
str_detect(c("nombre", "1nom", "1234"), "[a-z]")

#Chiffres
str_detect(c("nombre", "1nom", "1234"), "[3-9]")

#Nombre d'occurenes
str_detect(c("123", "1234"), "[0-9]{4}")

df5 <- df4 %>% 
  mutate(emissions = str_replace_all(emissions, 
                                     pattern = " [a-z]",
                                     replacement = "")) %>% 
  mutate(emissions = as.numeric(emissions))


# Fusion ! ----------------------------------------------------------------

country_code <- read_excel("inputs/country_codes.xlsx") %>% 
  select(Name, ISO2)

df6 <- left_join(df4, country_code, by = c("geo" = "ISO2"))


results <- saveRDS(df6, "outputs/results.rds")

#To read the RDS file:
#readRDS("results.rds")

#Other commands
#bind_cols
#bind_rows
#colnames(df4)[4]