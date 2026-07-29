library(tidyverse)
library(nycflights13)

# pour exécuter du code : Ctrl + Entrée

flights <- flights

str(flights) #overview of the table



# DPLYR -------------------------------------------------------------------

# Filter

vols_janvier <- filter(flights, month == 1)
filter(flights, month == 1 & day == 1) # Logique : "et"
filter(flights, month == 1 | month  == 4) # logique : "ou"
vols_janvier <- filter(flights, month != 1)

sort(unique(vols_janvier$month))

a <- 0/0 #NA

is.na(a)


# Select : choisir les colones 

tmp <- select(flights, c("origin", "dest"))

select(flights, origin, dest)

select(flights, flight:dest)

select(flights, -flight)
select(flights, starts_with("d"))


# Mutate

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)



# Section 2: with the pipe ---------------------------------------------------------------

subset_flight <- 
  flights %>% 
  filter(month == 1) %>% 
  select(dep_time, dep_delay) %>% 
  mutate(gain = dep_time - dep_delay)

#

# Group by and Summarise --------------------------------------------------

delays <- flights %>% 
  group_by(month) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE)) 

#Issue with group? ungroup

#Need help? 
?mean


# Ifelse

subset_flight <- 
  flights %>% 
  filter(month %in% c(1,2,4,5)) %>% 
  select(month, dep_time, dep_delay) %>% 
  mutate(gain = ifelse(month == 1, dep_time - dep_delay, NA))


### script d'apprentissage tidyverse addenda
### créé le 26/04/2019
## in french:
## https://juba.github.io/tidyverse/10-dplyr.html
## english
## http://www.storybench.org/getting-started-with-tidyverse-in-r/
## 
### chargement des librairies.
library(tidyverse)
### attention ! pour utiliser le pipe il faut charger magrittr :
library(magrittr)
### comprendre les tibbles:
library(tibble)

key_value <- tribble(
  ~row, ~key1, ~key2, ~key3, # These are the names of the columns (indicated with ~)
  "1", "1_value_1","1_value_2","1_value_3", # Row 1
  "2", "2_value_1", "2_value_2", "2_value_3", # Row 2
  "3", "3_value_1", "3_value_2", "3_value_3" # Row 3
)
key_value
## aboute tibbles: https://tibble.tidyverse.org/

# gather command:
kv_gathered <- key_value %>% 
  gather(key, # this will be the new column for the 3 key columns
         value, # this will contain the 9 distinct values
         key1:key3, # this is the range of columns we want gathered
         na.rm = TRUE # handles missing
  )
kv_gathered
## spread back:
kv_spreaded <- kv_gathered %>% 
  spread(
    key, 
    value
  )
kv_spreaded

### so ok for creation of a tibble by spread / gather

