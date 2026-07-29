### reproduction à partir du code de l'auteur de la carte animée covid19 GE
### https://heads0rtai1s.github.io/2020/04/30/animate-map-covid/

### lancement des paquetages.
libs <- c('dplyr', 'tibble',      # wrangling
          'stringr', 'readr',     # strings, input
          'lubridate', 'tidyr',   # time, wrangling
          'knitr', 'kableExtra',  # table styling
          'ggplot2', 'viridis',   # visuals
          'gganimate', 'sf',      # animations, maps
          'ggthemes')             # visuals
### extraction des données:
infile <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
covid_de <- read_csv(infile, col_types = cols())
### visualisations des données:
covid_de %>% 
  head(5) %>% 
  glimpse()
### transformation des données:
covid_de <- covid_de %>% 
  select(state = Bundesland,
         county = Landkreis,
         age_group = Altersgruppe,
         gender = Geschlecht,
         cases = AnzahlFall,
         deaths = AnzahlTodesfall,
         recovered = AnzahlGenesen,
         date = Meldedatum) %>% 
  mutate(date = date(date)) %>% 
  mutate(age_group = str_remove_all(age_group, "A")) %>% 
  mutate(age_group = case_when(
    age_group == "unbekannt" ~ NA_character_,
    age_group == "80+" ~ "80-99",
    TRUE ~ age_group
  )) %>% 
  mutate(gender = case_when(
    gender == "W" ~ "F",
    gender == "unbekannt" ~ NA_character_,
    TRUE ~ gender
  )) %>% 
  group_by(state, county, age_group, gender, date) %>% 
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            recovered = sum(recovered)) %>% 
  ungroup() %>% 
  filter(cases >= 0 & deaths >= 0) %>%
  filter(date < today()) %>% 
  mutate(state = str_replace_all(state, "ü", "ue")) %>% 
  mutate(state = str_replace_all(state, "ä", "ae")) %>% 
  mutate(state = str_replace_all(state, "ö", "oe")) %>% 
  mutate(state = str_replace_all(state, "ß", "ss")) %>% 
  mutate(county = str_replace_all(county, "ü", "ue")) %>% 
  mutate(county = str_replace_all(county, "ä", "ae")) %>% 
  mutate(county = str_replace_all(county, "ö", "oe")) %>% 
  mutate(county = str_replace_all(county, "ß", "ss")) %>% 
  mutate(county = str_remove(county, "\\(.+\\)")) %>% 
  mutate(county = str_trim(county)) 
### filtrage des données de nouveaux cas pour représentation carto
covid_de %>%
  filter(state == "Sachsen") %>% 
  select(-deaths, -recovered) %>% 
  head(5) %>% 
  kable() %>% 
  column_spec(1:6, width = c("15%", "25%", "15%", "10%", "25%", "10%")) %>% 
  kable_styling()
### préparation des cartes pour jointure:
shape_county <- st_read(str_c("../../static/files/", "de_county.shp"), quiet = TRUE) %>% 
  rename(county = GEN) %>% 
  select(county, BEZ, geometry) %>% 
  mutate(county = as.character(county)) %>% 
  mutate(county = str_replace_all(county, "ü", "ue")) %>% 
  mutate(county = str_replace_all(county, "ä", "ae")) %>% 
  mutate(county = str_replace_all(county, "ö", "oe")) %>% 
  mutate(county = str_replace_all(county, "ß", "ss")) %>% 
  mutate(county = str_remove(county, "\\(.+\\)")) %>% 
  mutate(county = str_trim(county)) %>% 
  mutate(BEZ = case_when(
    BEZ == "Kreis" ~ "LK",
    BEZ == "Landkreis" ~ "LK",
    BEZ == "Stadtkreis" ~ "SK",
    BEZ == "Kreisfreie Stadt" ~ "SK"
  )) %>% 
  unite(county, BEZ, county, sep = " ", remove = TRUE)
### transformation des données covid GE:
foo <- covid_de %>% 
  mutate(county = case_when(
    county == "Region Hannover" ~ "LK Region Hannover",
    county == "SK Muelheim a.d.Ruhr" ~ "SK Muelheim an der Ruhr",
    county == "StadtRegion Aachen" ~ "LK Staedteregion Aachen",
    county == "SK Offenbach" ~ "SK Offenbach am Main",
    county == "LK Bitburg-Pruem" ~ "LK Eifelkreis Bitburg-Pruem",
    county == "SK Landau i.d.Pfalz" ~ "SK Landau in der Pfalz",
    county == "SK Ludwigshafen" ~ "SK Ludwigshafen am Rhein",
    county == "SK Neustadt a.d.Weinstrasse" ~ "SK Neustadt an der Weinstrasse",
    county == "SK Freiburg i.Breisgau" ~ "SK Freiburg im Breisgau",
    county == "LK Landsberg a.Lech" ~ "LK Landsberg am Lech",
    county == "LK Muehldorf a.Inn" ~ "LK Muehldorf a. Inn",
    county == "LK Pfaffenhofen a.d.Ilm" ~ "LK Pfaffenhofen a.d. Ilm",
    county == "SK Weiden i.d.OPf." ~ "SK Weiden i.d. OPf.",
    county == "LK Neumarkt i.d.OPf." ~ "LK Neumarkt i.d. OPf.",
    county == "LK Neustadt a.d.Waldnaab" ~ "LK Neustadt a.d. Waldnaab",
    county == "LK Wunsiedel i.Fichtelgebirge" ~ "LK Wunsiedel i. Fichtelgebirge",
    county == "LK Neustadt a.d.Aisch-Bad Windsheim" ~ "LK Neustadt a.d. Aisch-Bad Windsheim",
    county == "LK Dillingen a.d.Donau" ~ "LK Dillingen a.d. Donau",
    county == "LK Stadtverband Saarbruecken" ~ "LK Regionalverband Saarbruecken",
    county == "LK Saar-Pfalz-Kreis" ~ "LK Saarpfalz-Kreis",
    county == "LK Sankt Wendel" ~ "LK St. Wendel",
    county == "SK Brandenburg a.d.Havel" ~ "SK Brandenburg an der Havel",
    str_detect(county, "Berlin") ~ "SK Berlin",
    TRUE ~ county
  )) %>% 
  group_by(county, date) %>% 
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  ungroup() %>% 
  complete(county, date, fill = list(cases = 0, deaths = 0)) %>% 
  group_by(county) %>% 
  mutate(cumul_cases = cumsum(cases),
         cumul_deaths = cumsum(deaths)) %>% 
  ungroup() %>% 
  filter(between(date, date("2020-03-01"), date("2020-03-31")))
# The animation parameters are provided in the animate function, 
#such as the transition style from one day to the next (cubic-in-out), 
#the animation speed (10 frames per s), or the size of the plot. 
#For cumulative animations like this, it’s always a good idea 
# to include an end_pause freeze-frame, so that the reader 
#can have a closer look at the final state before the loop begins anew:
gg <- shape_county %>% 
  right_join(foo, by = "county") %>% 
  ggplot(aes(fill = cumul_cases)) +
  geom_sf() +
  scale_fill_viridis(trans = "log1p", breaks = c(0, 10, 100, 1000)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  labs(title = "Total COVID-19 cases in Germany: {frame_time}", fill = "Cases") +
  transition_time(date)

animate(gg + ease_aes('cubic-in-out'), fps = 10, end_pause = 25, height = 800, width = round(800/1.61803398875))

