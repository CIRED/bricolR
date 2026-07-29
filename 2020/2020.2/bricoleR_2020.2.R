################################################################################
#####          script bricol'R 2020.2 graphiques gganimate                  ####
#####          Exemples sur les données COVID 19                            ####
################################################################################

# lien connection pour les donnéess:
# https://www.data.gouv.fr/fr/
# https://github.com/etalab/covid19-dashboard
# https://dashboard.covid19.data.gouv.fr/
# https://thibautfabacher.shinyapps.io/covid-19/

### idées à reprendre ici:
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# liens à conserver et lire:
# https://thinkr.fr/manipuler-ses-donnees-avec-tidyr-ou-tout-ce-que-vous-voulez-savoir-sur-le-pivot/

# good ggplot book:
#https://danmaclean.github.io/ggplotbook/using-factors-to-subset-data-and-plots.html

#gganimate: 
# https://github.com/ropenscilabs/learngganimate
# https://gganimate.com/articles/gganimate.html
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# https://twitter.com/hashtag/gganimate
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# https://www.data-imaginist.com/2019/gganimate-has-transitioned-to-a-state-of-release/
# https://www.r-pkg.org/pkg/gganimate

#  tutos interessants:
# https://suzan.rbind.io/2018/02/dplyr-tutorial-3/

# chargement des paquetages nécessaires aux traitements 
library(tidyverse)
library(xlsx)
library(openxlsx)
library(officer)
library(RColorBrewer)
library(ggsci)
library(gganimate)
theme_set(theme_bw())  # pre-set the bw theme.

# repertoire de travail de l'environnement
setwd("c:/bricoleR/2020.2")
saveRDS(ar5_sres_2050, "ar5_sres_2050.rds")
#rm(ar5_sres_2050)
### importation du fichier de données IPCC 2050 ----
data2050 <- readRDS("ar5_sres_2050.rds")
data2050 <- select(data2050, -c(32:41))
View(data2050)
### construction du ficher en version longue par pivot_wider:
data2050_long <- pivot_longer(data2050, c(22:31), names_to = "year", names_prefix = NULL,
                              names_sep = NULL, names_pattern = NULL, names_ptypes = list(),
                              names_repair = "check_unique", values_to = "value",
                              values_drop_na = FALSE, values_ptypes = list())
### attention à bien spécifier les bonnes colonnes pour cols ici 22:31 et non pas 21:31 !
head(data2050_long,25)
View(data2050_long) # la base est donc bien vectorisée.
###pourquoi ne pas virer les NA des valeurs dans data2050_long:
data2050_long <- drop_na(data2050_long, value)
view(data2050_long)
summary(data2050_long$value) # il n'y a plus de NA
# excellent c'est le plus direct ! un peu long mais cela se fait...
saveRDS(data2050_long, "data_2050_long.rds")
### pivotement pour creer les données larges.
data_2050_wide <- pivot_wider(data2050_long,
                              id_cols = c(MODEL, SCENARIO, REGION, year),
                              names_from = VARIABLE,
                              names_prefix = "", names_sep = "_", names_repair = "check_unique",
                              values_from = value, values_fill = NULL, values_fn = NULL) 
## en fait il manquait un identifiant indivuel des observations !!!
head(data_2050_wide, 25) ## it works but many NAs ; carbon price inutile...
View(data_2050_wide)
write.xlsx(data_2050_wide, "data_2050_wide.xlsx") # enregistrement xlsx
saveRDS(data_2050_wide, "data_2050_wide.rds") # enregistrement rds
data_2050_co2 <- filter(data2050_long, VARIABLE == "Emissions_CO2")

### génération des graphiques pour le bricole'R ----
### co2--pib en niveaux statique --- by model 
co2_pib_reg <- ggplot(data_2050_wide) +
                aes(x = GDP_MER, y = Emissions_CO2, colour = REGION) +
                geom_point(alpha = 0.4) +
                  labs(x = "PIB 2005",
                        y = "Emissions de CO2",
                        title = "Emisions de CO2 SRES AR5 jusqu'à 2050",
                        subtitle = "selon la région", 
                        caption = "Source: IPCC",
                        Colour = "Région")
co2_pib_reg

### co2_pc / pib_pc statique --- by model 
co2_pib_pc_mod <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Emissions_CO2/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Emissions de CO2 per capita",
       title = "Emisions de CO2 et PIB 2005 per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle", 
       caption = "Source: IPCC",
       Colour = "Modèle")
co2_pib_pc_mod
### EF_pc / PIB_pc statique --- by model
EF_pib_pc_mod <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle", 
       caption = "Source: IPCC",
       Colour = "Modèle")
EF_pib_pc_mod
### EF_pc / PIB_pc statique --- transition - manual sur region
EF_pib_pc_mod_reg <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle et la région: {current_frame}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  transition_manual(REGION)
EF_pib_pc_mod_reg
### EF_pc / PIB_pc statique --- transition - manual sur region
EF_pib_pc_mod_year <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle et l'année: {current_frame}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  transition_manual(year)
EF_pib_pc_mod_year

### animation par modèle sur 100 cadres avec 10 fps:
EF_pib_pc_mod_anim <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle : {current_frame}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  transition_manual(MODEL) + #ici il répartit les 30 modèles sur 100 cadres ce qui est équivalent à transition_stestes par défaut
  ease_aes('linear') 
EF_pib_pc_mod_anim
anim_save("EF_pib_pc_mod_anim.gif", animation = last_animation())
### refaisons le même graphique mais en ralentissant le rythme de défilement des modèles
EF_pib_pc_mod_anim_slow <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle : {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  transition_states(MODEL,
                    transition_length = 1,
                    state_length = 2) +
  ease_aes('linear') 
animate(EF_pib_pc_mod_anim_slow, nframes = 600) # attention ! 18 minutes de temps de calcul ! 6 pour 600
anim_save("EF_pib_pc_mod_anim_very_slow.2.gif", animation = last_animation()) # seconde version sauvegardée (600)

### carbon and energy intensity statique --- by model
CO2_PIB_EF_PIB <- ggplot(data_2050_wide) +
  aes(x = Final_Energy/GDP_MER, y = Emissions_CO2/GDP_MER, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "Intensité énergétique finale",
       y = "Intensité CO2",
       title = "Intensités énergétique finale et CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle", 
       caption = "Source: IPCC",
       Colour = "Modèle")
CO2_PIB_EF_PIB

### animation Intentité finale et co2 par modèle (avec effets entrée et sortie)
CO2_PIB_EF_PIB_anim_MODEL <- ggplot(data_2050_wide) +
  aes(x = Final_Energy/GDP_MER, y = Emissions_CO2/GDP_MER, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "Intensité énergétique finale",
       y = "Intensité CO2",
       title = "Intensités énergétique finale et CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle : {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") + 
  transition_states(MODEL,
                    transition_length = 1,
                    state_length = 2) +
  ease_aes('linear') + # cycle linéaire
  enter_fade() +  # entrée pâlissement
  exit_shrink()  # sortie par réduction
animate(CO2_PIB_EF_PIB_anim_MODEL, nframes = 600) # 11 minutes de calcul !
anim_save("CO2_PIB_EF_PIB_MODEL.gif", animation = last_animation())

### animation Intentité finale et co2 par année
CO2_PIB_EF_PIB_anim_year <- ggplot(data_2050_wide) +
  aes(x = Final_Energy/GDP_MER, y = Emissions_CO2/GDP_MER, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "Intensité énergétique finale",
       y = "Intensité CO2",
       title = "Intensités énergétique finale et CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle et par année: {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") + 
  transition_states(year,
                    transition_length = 3,
                    state_length = 1) +
  ease_aes('linear') +
  enter_fade() +  # entrée pâlissement
  exit_shrink()  # sortie par réduction
CO2_PIB_EF_PIB_anim_year
anim_save("CO2_PIB_EF_PIB_year.gif", animation = last_animation())

###graphiques en lignes avec nom des moèdles par années.
# creation d'un idenficateur des données
data_2050_wide <- mutate(data_2050_wide, id = rownames(data_2050_wide))
data_2050_wide$id <- as.integer(data_2050_wide$id)
summary(data_2050_wide$id)
data_2050_wide$year <- as.factor(data_2050_wide$year)
summary(data_2050_wide$year)
p <- ggplot(data_2050_wide, aes(year,
                                Emissions_CO2,
                                group = id,
                                color = MODEL)) +
  geom_line() +
  geom_text(aes(label = MODEL)) +
  transition_reveal(along = year, keep_last = F)
animate(p,nframes = 50) 
# résultat intéressant mais pas attendu mais cela marche
## il faudrait définir une autre variable et ne retenir qu'une seule trajectoire par modèle.
# ou alors en suivre une seule et mettre les autres en grisé ? voir cela.
# travailler sur les scenarii...
data_2050_wide$SCENARIO <- as.factor(data_2050_wide$SCENARIO)
summary(data_2050_wide$SCENARIO)


### ou bien selectionner un scenario et cycler sur les scenarii ?
### il faudra créer des variables de scenarii avec des groupes d'études par des regex 
## ===> une séance sur les strings dans R et le data wrangling ...



### ça marche mais c'est un peu rapide ! 
# cela s'explique par animate qui répartit l'animation sur 100 cadres affichés par 10 / seconde
# lorsque l'on a beaucoup de catégories comme ici cela va trop vite.
# on va donc allonger le temps d'animation avec animate.
CO2_PIB_EF_PIB_anim_2 <- ggplot(data_2050_wide) +
  aes(x = Final_Energy/GDP_MER, y = Emissions_CO2/GDP_MER, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "Intensité énergétique finale",
       y = "Intensité CO2",
       title = "Intensités énergétique finale et CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle : {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") + 
  transition_states(MODEL,
                    transition_length = 1,
                    state_length = 3) +
  ease_aes('linear') +
  enter_fade() +  # entrée pâlissement
  exit_shrink()  # sortie par réduction
animate(CO2_PIB_EF_PIB_anim_2, nframes = 300)
# si on rappelle de ggplot animé il est recalculé ! on peut donc le sauvegarder directement:
anim_save("CO2_PIB_EF_PIB_anim_2.gif", animation = last_animation())

### On peut changer de variable du cycle par exemple le temps, les années de 2005 à 2050
### reprenons le graphique per capita
EF_pib_pc_mod_anim_slow_year <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle et l'année: {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  transition_states(year,
                    transition_length = 1,
                    state_length = 2) +
  ease_aes('linear') 
animate(EF_pib_pc_mod_anim_slow_year, nframes = 400, fps = 20) # effet pas inintéressant !
anim_save("EF_pib_pc_mod_anim_very_slow_year.gif", animation = last_animation())
### on peut aussi utliser les grilles par région
EF_pib_pc_mod_anim_slow_year_reg <- ggplot(data_2050_wide) +
  aes(x = GDP_MER/Population, y = Final_Energy/Population, colour = MODEL) +
  geom_point(alpha = 0.4) +
  labs(x = "PIB 2005 per capita",
       y = "Energie finale per capita",
       title = "Energie finale et PIB per capita SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle et l'année: {closest_state}", 
       caption = "Source: IPCC",
       Colour = "Modèle") +
  facet_grid(~ REGION) +
  transition_states(year,
                    transition_length = 1,
                    state_length = 2) +
  ease_aes('linear') 
animate(EF_pib_pc_mod_anim_slow_year_reg, nframes = 400, fps = 20) # effet pas inintéressant !
anim_save("EF_pib_pc_mod_anim_very_slow_year_reg.gif", animation = last_animation())


### données covid19 -----
# phase 1 importer hosp et indid
datahosp <- read.csv2("donnees-hospitalieres-covid19-2020-05-13-19h00.csv",
                      stringsAsFactors = FALSE)
datahosp$jour <- as.Date(datahosp$jour, format = "%Y-%m-%d")
# mais problème: comme on importe sans transformer les caractères en facteurs, il faut le faire ensuite !
datahosp$dep <- as.factor(datahosp$dep) # dep transformé en facteur
## je décide de virer l'ajustement des données au 24/03/2020
datahosp <- filter(datahosp, dep != "")
head(datahosp, 25)
summary(datahosp)
summary(datahosp$dep)
levels(datahosp$dep)
View(datahosp)
saveRDS(datahosp, file = "datahosp-05-13.rds") # sauvegarde du fichier de données

# phase 2 importer pop2020 avec les regions
# importation fichiers codes dépeartements et regions
pop2020 <- read.xlsx("pop_depreg.xlsx") # cf manuel openxlsx page 48: pas d'option character as factors
format(pop2020$lib_dep, justify = "left")
format(pop2020$lib_reg, justify = "left")
head(pop2020, 20)
View(pop2020)
# phase 3 : mise en forme des donnes en format large
# en fait il faudrait réorganiser la base en empilant les quatre colonnes de données avec pivot_longer()
datahosp_long <- pivot_longer(datahosp, cols = hosp:dc, names_to = "patients", values_to = "nombres")
head(datahosp_long, 25)
# appariement de la base pop2020 sur datahosp_long
# recodage dans pop2020
pop2020$dep <- as.factor(pop2020$dep)
pop2020$reg <- as.factor(pop2020$reg)
pop2020$lib_dep <- as.factor(pop2020$lib_dep)
pop2020$lib_reg <- as.factor(pop2020$lib_reg)
levels(pop2020$dep)
# appariement par jointure à gauche sur
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/join
datahosp_long <- left_join(datahosp_long, pop2020, by = "dep") # tout simplement !
# transformer patients en facteur
datahosp_long$patients <- as.factor(datahosp_long$patients)
head(datahosp_long, 25)
summary(datahosp_long)
### calul des taux d'incidence pour 100 000 habutants
datahosp_long <- mutate(datahosp_long, taux_100 = 100000*nombres/popjanv2020) 
head(datahosp_long, 25)
summary(datahosp_long)
saveRDS(datahosp_long, file = "datahosp_long.rds")
#read_rds("datahosp_long.rds") # cas de fausse manip !

### filtrage sur le total.
datahosp_tot <- filter(datahosp_long, sexe == 0)
head(datahosp_tot, 25)
summary(datahosp_tot) # fichier extrait des totaux pour premier graphique
# mise en format large:
datahosptot.w <- pivot_wider(datahosp_tot, id_cols = NULL, names_from = patients,
                             names_prefix = "", names_sep = "_", names_repair = "check_unique",
                             values_from = c(nombres, taux_100, popjanv2020), values_fill = NULL, values_fn = NULL)
datahosptot.w <- select(datahosptot.w, -c(15:18))
head(datahosptot.w)
View(datahosptot.w) #: tout est ok. On peut retrouver poptot par mutate inverse des taux.


## faire un premier test avec les effectifs par jours.
jour <- datahosp_tot$jour[datahosp_tot$lib_dep == "Ain" & datahosp_tot$patients == "hosp"]
jour <- as.Date(jour, format = "%Y-%m-%d")
head(jour)
summary(jour)
View(jour)
nb.jour = nrow(jour)
nb.jour

### travail sur gganimate ----
# graphiques animés de nuages de points animés par jour avec tous les départements,cf;
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# animated bubble chart
library(gganimate)
### courbe animées par département: hospitalisations
datahosptot.w$jour <- as.Date(datahosptot.w$jour, format = "%Y-%m-%d")
summary(datahosptot.w$jour)
hosp_line_dep <- ggplot(datahosptot.w, aes(jour,
                               nombres_hosp,
                                group = dep,
                                color = nombres_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "hospitalisations",
       title = "Evolution des hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "décès cumulés") +
  transition_reveal(along = jour, keep_last = F)
animate(hosp_line_dep, nframes = 57) ## ça marche parfaitement
animate(hosp_line_dep, nframes = 57, end_pause = 25) # décale l'affichage des jours !!!
anim_save("hosp_line_dep.gif", last_animation())
### décès
dec_line_dep <- ggplot(datahosptot.w, 
                              aes(jour,
                                 nombres_dc,
                                 group = dep,
                                 color = nombres_rea)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "décès cumulés",
       title = "Evolution des décès COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "patients en réanimation") +
  transition_reveal(along = jour, keep_last = F)
animate(dec_line_dep, nframes = 57) ## très intéressant
anim_save("dec_line_dep.gif", last_animation())
### réanimation
rea_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           nombres_rea,
                           group = dep,
                           color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "patients en réanimation",
       title = "Evolution patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(rea_line_dep, nframes = 57) ## très intéressant
anim_save("rea_line_dep.gif", last_animation())
### retours à domicile
rad_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           nombres_rad,
                           group = dep,
                           color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "patients retournés au domicile",
       title = "Evolution patients COVID 19 retournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(rad_line_dep, nframes = 57) ## très intéressant
anim_save("rad_line_dep.gif", last_animation())

### A faire: à partir du fichier long facet wrap des quatre courbes animées.
# ou bien tableau avec quatre graphiques séparés de chaque courbe.
## améliorer les effets.

### courbes en taux pour 100 000
### courbe animées par département: hospitalisations
t100_hosp_line_dep <- ggplot(datahosptot.w, aes(jour,
                                           taux_100_hosp,
                                           group = dep,
                                           color = taux_100_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "hospitalisations pour 100 000 habitants",
       title = "Evolution des hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_hosp_line_dep, nframes = 57) ## ça marche parfaitement
anim_save("t100_hosp_line_dep.gif", last_animation())
### décès
t100_dec_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           taux_100_dc,
                           group = dep,
                           color = taux_100_rea)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "décès cumulés pour 100 000 habitants",
       title = "Evolution des décès COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "patients en réanimation \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_dec_line_dep, nframes = 57) ## très intéressant
anim_save("t100_dec_line_dep.gif", last_animation())
### réanimation
t100_rea_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           taux_100_rea,
                           group = dep,
                           color = taux_100_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "patients en réanimation pour 100 000 habitants",
       title = "Evolution des patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés \n pour 100 000 habitants",
       colour = "hospitalisations \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rea_line_dep, nframes = 57, end_pause = 25) # c'est bien cela ! il faut une pause au 18/4
animate(t100_rea_line_dep, nframes = 57)
anim_save("t100_rea_line_dep.gif", last_animation())
### retours au domicile
t100_rad_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           taux_100_rad,
                           group = dep,
                           color = taux_100_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "patients retournés au domicile pour 100 000 habitants",
       title = "Evolution des patients COVID 19 rtournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "hospitalisations \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rad_line_dep, nframes = 57) ## très intéressant
anim_save("t100_rad_line_dep.gif", last_animation())

### tentons les graphiques nuages de points avec étiquettes de département
### courbe animées par département: hospitalisations
hosp_t100_hosp_line_dep <- ggplot(datahosptot.w, aes(nombres_hosp,
                                                taux_100_hosp,
                                                group = dep,
                                                color = taux_100_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "hospitalisations pour 100 000 habitants",
       title = "Evolution des hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(hosp_t100_hosp_line_dep, nframes = 57) ## ça marche !!!
anim_save("hosp_t100_hosp_line_dep.gif", last_animation())
### décès
dc_t100_dec_line_dep <- ggplot(datahosptot.w, 
                            aes(nombres_dc,
                                taux_100_dc,
                                group = dep,
                                color = taux_100_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés", y = "décès cumulés pour 100 000 habitants",
       title = "Evolution des décès COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(dc_t100_dec_line_dep, nframes = 57)
anim_save("dc_t100_dec_line_dep.gif", last_animation())
### réanimation
rea_t100_rea_line_dep <- ggplot(datahosptot.w, 
                            aes(nombres_rea,
                                taux_100_rea,
                                group = dep,
                                color = taux_100_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation", y = "patients en réanimation pour 100 000 habitants",
       title = "Evolution des patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés \n pour 100 000 habitants",
       colour = "hospitalisations \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(rea_t100_rea_line_dep, nframes = 57)
anim_save("rea_t100_rea_line_dep.gif", last_animation())
### retours au domicile
rad_t100_rad_line_dep <- ggplot(datahosptot.w, 
                            aes(nombres_rad,
                                taux_100_rad,
                                group = dep,
                                color = taux_100_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = taux_100_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "retours au domicile", y = "patients retournés au domicile pour 100 000 habitants",
       title = "Evolution des patients COVID 19 retournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "hospitalisations \n pour 100 000 habitants") +
  transition_reveal(along = jour, keep_last = F)
animate(rad_t100_rad_line_dep, nframes = 57) ## très intéressant
anim_save("rad_t100_rad_line_dep.gif", last_animation())

### combinaisons par séries permutées
### courbe animées par département: hospitalisations
dec_hosp_line_dep <- ggplot(datahosptot.w, aes(nombres_hosp,
                                               nombres_dc,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "décès cumulés",
       title = "Evolution des hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "hospitalisations",
       colour = "décès cumulés") +
  transition_reveal(along = jour, keep_last = F)
animate(dec_hosp_line_dep, nframes = 57)
anim_save("dec_hosp_line_dep.gif", last_animation())
## décès -- réa
dec_rea_line_dep <- ggplot(datahosptot.w, aes(nombres_dc,
                                              nombres_rea,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés", y = "patients en réanimation",
       title = "Evolution des décès et patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(dec_rea_line_dep, nframes = 57)
anim_save("dec_rea_line_dep.gif", last_animation())
### hosp -- rea
rea_hosp_line_dep <- ggplot(datahosptot.w, aes(nombres_hosp,
                                               nombres_rea,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "patients en réanimation",
       title = "Evolution des hospitalisations et patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(rea_hosp_line_dep, nframes = 57) ### très intéressant noter le tournant de pâque mi avril
anim_save("rea_hosp_line_dep.gif", last_animation())
### rad_hosp
rad_hosp_line_dep <- ggplot(datahosptot.w, aes(nombres_hosp,
                                               nombres_rad,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "retours au domicile",
       title = "Evolution des hospitalisations et retours au domicile de patients COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(rad_hosp_line_dep, nframes = 57)
anim_save("rad_hosp_line_dep.gif", last_animation())

### rad_rea
rad_rea_line_dep <- ggplot(datahosptot.w, aes(nombres_rea,
                                              nombres_rad,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation", y = "retours au domicile",
       title = "Evolution des patients COVID 19 en réanimation et retournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(rad_rea_line_dep, nframes = 57)
anim_save("rad_rea_line_dep.gif", last_animation())

### courbes permutées en taux permutés
### courbe animées par département: hospitalisations -- décès
t100_dec_hosp_line_dep <- ggplot(datahosptot.w, aes(taux_100_hosp,
                                               taux_100_dc,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 habitants", y = "décès cumulés pour 100 000 habitants",
       title = "Evolution des hospitalisations et décès COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "hospitalisations",
       colour = "décès cumulés") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_dec_hosp_line_dep, nframes = 57)
anim_save("t100_dec_hosp_line_dep.gif", last_animation())
## décès -- réa
t100_dec_rea_line_dep <- ggplot(datahosptot.w, aes(taux_100_dc,
                                              taux_100_rea,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés pour 100 000 habitants", y = "patients en réanimation pour 100 000 habitants",
       title = "Evolution des décès et patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_dec_rea_line_dep, nframes = 57)
anim_save("t100_dec_rea_line_dep.gif", last_animation())
### hosp -- rea
t100_rea_hosp_line_dep <- ggplot(datahosptot.w, aes(taux_100_hosp,
                                               taux_100_rea,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 habitants", y = "patients en réanimation pour 100 000 habitants",
       title = "Evolution des hospitalisations et patients COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rea_hosp_line_dep, nframes = 57) ### très intéressant noter le tournant de pâque mi avril
anim_save("t100_rea_hosp_line_dep.gif", last_animation())
### rad_hosp
t100_rad_hosp_line_dep <- ggplot(datahosptot.w, aes(taux_100_hosp,
                                                    taux_100_rad,
                                               group = dep,
                                               color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 habitants", y = "retours au domicile pour 100 000 habitants",
       title = "Hospitalisations et retours au domicile de patients COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rad_hosp_line_dep, nframes = 57)
anim_save("t100_rad_hosp_line_dep.gif", last_animation())
### dec_rad
t100_rad_dec_line_dep <- ggplot(datahosptot.w, aes(taux_100_dc,
                                                   taux_100_rad,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés pour 100 000 habitants", y = "retours au domicile cumulés pour 100 000 habitants",
       title = "Décès et patients COVID 19 reournés au domicilepar département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rad_dec_line_dep, nframes = 57)
anim_save("t100_rad_dec_line_dep.gif", last_animation())
### rad_rea
t100_rad_rea_line_dep <- ggplot(datahosptot.w, aes(taux_100_rea,
                                                   taux_100_rad,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation pour 100 000 habitants", y = "retours au domicile pour 100 000 habitants",
       title = "Patients COVID 19 en réanimation et retournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "hospitalisations") +
  transition_reveal(along = jour, keep_last = F)
animate(t100_rad_rea_line_dep, nframes = 57)
anim_save("t100_rad_rea_line_dep.gif", last_animation()) #2 effet intéressant..
### faire des cartes ---



#### older code ----
# partons de nombres de patients hospitalisés  + décès cumulés. == points
hospdecanim <- ggplot(datahosptot.w,
                      aes(x = nombres_hosp,
                          y = nombres_dc,
                          size = nombres_hosp,
                          colour = nombres_dc)) +
  geom_point() +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "décès cumulés",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "entre le 18 mars et le 23 avril 2020 ; date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') 
hospdecanim
anim_save("hospdecanim.gif", animation = last_animation())
### animation avec des numéros de département
#rm(hosp.dec.gt.anim)
hosp.dec.gtext.anim <- ggplot(datahosptot.w,
                           aes(x = nombres_hosp,
                               y = nombres_dc,
                               size = nombres_hosp,
                               colour = nombres_dc)) +
  geom_text(aes(nombres_hosp, nombres_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "décès",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "entre le 18 mars et le 20 avril 2020 ; date :{closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  #shadow_wake(wake_length = 0.1, alpha = FALSE) + ## traces de défilement
  shadow_mark(alpha = 0.8, size = 0.5) 
  # sentier de données passées
  #view_follow(fixed_y = TRUE) + # suivi en y
  #view_follow(fixed_x = TRUE) + # suivi en x
hosp.dec.gtext.anim
anim_save("hosp.dec.gtext.anim.gif", animation = last_animation())

### animation avec des numéros de département
hosp.rea.gt.anim <- ggplot(datahosptot.w,
                           aes(x = nombres_hosp,
                               y = nombres_dc,
                               size = nombres_rea,
                               colour = nombres_dc)) +
  geom_text(aes(nombres_hosp, nombres_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "décès cumulés",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
hosp.rea.gt.anim
anim_save("hosp.rea.gt.anim.gif", animation = last_animation())

### animation avec des département hosp-rea-rea-dc
hosp.rea.dc.gt.anim <- ggplot(datahosptot.w,
                              aes(x = nombres_hosp,
                                  y = nombres_rea,
                                  size = nombres_rea,
                                  colour = nombres_dc)) +
  geom_text(aes(nombres_hosp, nombres_rea,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "patients en réanimation",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
hosp.rea.dc.gt.anim
anim_save("hosp.rea.dc.gt.anim.gif", animation = last_animation())
### animation avec des département hosp-rea-rad-dc
hosp.rea.rad.gt.anim <- ggplot(datahosptot.w,
                               aes(x = nombres_hosp,
                                   y = nombres_rea,
                                   size = nombres_rad,
                                   colour = nombres_dc)) +
  geom_text(aes(nombres_hosp, nombres_rea,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "patients en réanimation",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients retournés au domicile cumulés",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
hosp.rea.rad.gt.anim
anim_save("hosp.rea.rad.gt.anim.gif", animation = last_animation())
### animation avec des département hosp--rad-rea-dc
hosp.rad.rea.gt.anim <- ggplot(datahosptot.w,
                               aes(x = nombres_hosp,
                                   y = nombres_rad,
                                   size = nombres_rea,
                                   colour = nombres_dc)) +
  geom_text(aes(nombres_hosp, nombres_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations", y = "patients retournés au domicile cumulés",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
hosp.rad.rea.gt.anim
anim_save("hosp.rad.rea.gt.anim.gif", animation = last_animation())
### animation avec des département rad--dec-hosp-rea
rad.dec.rea.gt.anim <- ggplot(datahosptot.w,
                              aes(x = nombres_rad,
                                  y = nombres_dc,
                                  size = nombres_hosp,
                                  colour = nombres_rea)) +
  geom_text(aes(nombres_rad, nombres_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients retournés au domicile cumulés",
       y = "décès cumulés",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés",
       colour = "patients en réanimation") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
rad.dec.rea.gt.anim
anim_save("rad.dec.rea.gt.anim.gif", animation = last_animation())
### animation avec des département rad--dec-hosp-rea
rea.rad.dec.gt.anim <- ggplot(datahosptot.w,
                              aes(x = nombres_rea,
                                  y = nombres_rad,
                                  size = nombres_hosp,
                                  colour = nombres_dc)) +
  geom_text(aes(nombres_rea, nombres_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation",
       y = "patients retournés au domicile cumulés",
       title = "Evolution des hospitalisations et des décès COVID 19 par département",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés",
       colour = "décès cumulés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
rea.rad.dec.gt.anim
anim_save("rea.rad.dec.gt.anim.gif", animation = last_animation())

### graphique sur les sur les variables en taux pour 100 000 habitants: ----
# partons de nombres de patients hospitalisés  + décès cumulés.
t100.hosp.dec.anim <- ggplot(datahosptot.w,
                             aes(x = taux_100_hosp,
                                 y = taux_100_dc,
                                 size = taux_100_hosp,
                                 colour = taux_100_dc)) +
  geom_text(aes(taux_100_hosp, taux_100_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "Hospitalisations pour 100 000 hab", y = "décès cumulés pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
#shadow_wake(wake_length = 0.1, alpha = FALSE) ## traces de défilement
#shadow_mark(alpha = 0.5, size = 0.8) # sentier de données passées
view_follow(fixed_y = TRUE) # suivi en y
#view_follow(fixed_x = TRUE) # suivi en x
t100.hosp.dec.anim
anim_save("t100.hosp.dec.anim.gif", animation = last_animation())
### animation avec des numéros de département
t100.hosp.dec.gt.anim <- ggplot(datahosptot.w,
                                aes(x = taux_100_hosp,
                                    y = taux_100_dc,
                                    size = taux_100_dc,
                                    colour = taux_100_hosp)) +
  geom_text(aes(taux_100_hosp, taux_100_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 hab", y = "décès cumulés pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
t100.hosp.dec.gt.anim
anim_save("t100.hosp.dec.gt.anim.gif", animation = last_animation())

### animation avec des numéros de département
taux_100.hosp.rea.gt.anim <- ggplot(datahosptot.w,
                                    aes(x = taux_100_hosp,
                                        y = taux_100_dc,
                                        size = taux_100_rea,
                                        colour = taux_100_dc)) +
  geom_text(aes(taux_100_hosp, taux_100_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 hab", y = "décès cumulés pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.hosp.rea.gt.anim
anim_save("taux_100.hosp.rea.gt.anim.gif", animation = last_animation())

### animation avec des département hosp-rea-rea-dc
taux_100.hosp.rea.dc.gt.anim <- ggplot(datahosptot.w,
                                       aes(x = taux_100_hosp,
                                           y = taux_100_rea,
                                           size = taux_100_rea,
                                           colour = taux_100_dc)) +
  geom_text(aes(taux_100_hosp, taux_100_rea,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 hab", y = "patients en réanimation pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.hosp.rea.dc.gt.anim
anim_save("taux_100.hosp.rea.dc.gt.anim.gif", animation = last_animation())
### animation avec des département hosp-rea-rad-dc
taux_100.hosp.rea.rad.gt.anim <- ggplot(datahosptot.w,
                                        aes(x = taux_100_hosp,
                                            y = taux_100_rea,
                                            size = taux_100_rad,
                                            colour = taux_100_dc)) +
  geom_text(aes(taux_100_hosp, taux_100_rea,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 hab", y = "patients en réanimation pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients retournés au domicile \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.hosp.rea.rad.gt.anim
anim_save("taux_100.hosp.rea.rad.gt.anim.gif", animation = last_animation())
### animation avec des département hosp--rad-rea-dc
taux_100.hosp.rad.rea.gt.anim <- ggplot(datahosptot.w,
                                        aes(x = taux_100_hosp,
                                            y = taux_100_rad,
                                            size = taux_100_rea,
                                            colour = taux_100_dc)) +
  geom_text(aes(taux_100_hosp, taux_100_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "hospitalisations pour 100 000 hab", y = "cumul de patients retournés au domicile pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients retournés au domicile \n pour 100 000 habitants",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.hosp.rad.rea.gt.anim
anim_save("taux_100.hosp.rad.rea.gt.anim.gif", animation = last_animation())
### animation avec des département rad--dec-hosp-rea
taux_100.rad.dec.rea.gt.anim <- ggplot(datahosptot.w,
                                       aes(x = taux_100_rad,
                                           y = taux_100_dc,
                                           size = taux_100_hosp,
                                           colour = taux_100_rea)) +
  geom_text(aes(nombres_rad, nombres_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "cumul des patients retournés au domicile (pour 100 000 hab)",
       y = "cumul des décès (pour 100 000 hab)",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients retournés \n au domicile cumulés",
       colour = "patients en réanimation") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.rad.dec.rea.gt.anim
anim_save("taux_100.rad.dec.rea.gt.anim.gif", animation = last_animation())
### animation avec des département rad--dec-hosp-rea
taux_100.rea.rad.dec.gt.anim <- ggplot(datahosptot.w,
                                       aes(x = taux_100_rea,
                                           y = taux_100_rad,
                                           size = taux_100_hosp,
                                           colour = taux_100_dc)) +
  geom_text(aes(taux_100_rea, taux_100_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation (pour 100 000 hab)",
       y = "cumul des patients retournés au domicile (pour 100 000 hab)",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés \n (pour 100 000 hab)",
       colour = "décès cumulés \n (pour 100 000 hab)") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.rea.rad.dec.gt.anim
anim_save("taux_100.rea.rad.dec.gt.anim.gif", animation = last_animation())
# idem axes permutés et couleurs / tailles inversés...
taux_100.rad.rea.dec.gt.anim <- ggplot(datahosptot.w,
                                       aes(x = taux_100_rad,
                                           y = taux_100_rea,
                                           size = taux_100_dc,
                                           colour = taux_100_hosp)) +
  geom_text(aes(taux_100_rea, taux_100_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "cumul des patients retournés au domicile (pour 100 000 hab)",
       y = "patients en réanimation (pour 100 000 hab)",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés \n (pour 100 000 hab)",
       colour = "patients hospitalisés \n (pour 100 000 hab)") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear')
taux_100.rad.rea.dec.gt.anim
anim_save("taux_100.rad.rea.dec.gt.anim.gif", animation = last_animation())

### representations taux ---effectifs ----
# hospitalisations
hosp.t100.hosp.anim <- ggplot(datahosptot.w,
                              aes(x = nombres_hosp,
                                  y = taux_100_hosp,
                                  size = taux_100_hosp,
                                  colour = nombres_hosp)) +
  geom_text(aes(nombres_hosp, taux_100_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "Hospitalisations", y = "Hospitalisations pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients hospitalisés \n pour 100 000 habitants",
       colour = "patients hospitalisés") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') 
hosp.t100.hosp.anim
anim_save("hosp.t100.hosp.anim.gif", animation = last_animation())
# décès cumulés
dec.t100.dec.anim <- ggplot(datahosptot.w,
                            aes(x = nombres_dc,
                                y = taux_100_dc,
                                size = nombres_dc,
                                colour = taux_100_dc)) +
  geom_text(aes(nombres_dc, taux_100_dc,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés", y = "décès cumulés pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "décès cumulés",
       colour = "décès cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') 
dec.t100.dec.anim
anim_save("dec.t100.hosp.anim.gif", animation = last_animation())
# patients en réanimation
rea.t100.rea.anim <- ggplot(datahosptot.w,
                            aes(x = nombres_rea,
                                y = taux_100_rea,
                                size = nombres_rea,
                                colour = taux_100_rea)) +
  geom_text(aes(nombres_rea, taux_100_rea,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "patients en réanimation", y = "patients en réanimation pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "patients en réanimation",
       colour = "patients en réanimation \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') 
rea.t100.rea.anim
anim_save("rea.t100.rea.anim.gif", animation = last_animation())
# retours domicile réanimation
rad.t100.rad.anim <- ggplot(datahosptot.w,
                            aes(x = nombres_rad,
                                y = taux_100_rad,
                                size = nombres_rad,
                                colour = taux_100_rad)) +
  geom_text(aes(nombres_rad, taux_100_rad,label = dep)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "retours au domicile cumulés", y = "retours au domicile cumulés pour 100 000 hab",
       title = "Evolution de l'épidémie COVID 19 par département (pour 100 000 habitants)",
       subtitle = "date : {closest_state}", 
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       size = "retours au domicile cumulés",
       colour = "retours au domicile cumulés \n pour 100 000 habitants") +
  transition_states(jour, transition_length = 2, state_length = 1) +
  ease_aes('linear') 
rad.t100.rad.anim
anim_save("rad.t100.rad.anim.gif", animation = last_animation())
### cela marche très bien ==> voir pour représenter d'autres données et changer
# les paramètres d'affichage de l'animation, vitesse, transitions etc.
