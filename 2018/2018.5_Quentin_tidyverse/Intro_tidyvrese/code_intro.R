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
