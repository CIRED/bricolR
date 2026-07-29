################################################################################
###       script de traitement des cartes animees                            ###
###   https://heads0rtai1s.github.io/2020/04/30/animate-map-covid/           ###
###       script de traitement des cartes animees                            ###
################################################################################


### très important !
## pour pouvoir faire des animations avec sf il faut installer transformmr:
# https://github.com/thomasp85/gganimate/issues/190
# à cause de l'erreur:
#Error in transform_sf(all_frames, states[[i]], ease, nframes[i], !!id,  : 
##                        transformr is required to tween sf layers
# https://github.com/thomasp85/transformr/blob/master/R/tween_sf.R
# https://community.rstudio.com/t/error-dependency-transformr-is-not-available-for-package-gganimate/11134
# # install.packages("devtools")
# devtools::install_github("thomasp85/transformr")
# 


### REproduire l'exemple suivant du twitter gganimate:
### https://heads0rtai1s.github.io/2020/04/30/animate-map-covid/

# liste de liens utilesporuc ce script des Rladies:
# https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
# http://d4tagirl.com/2017/05/how-to-deal-with-ggplotly-huge-maps
# https://d4tagirl.com/2017/05/how-to-fetch-twitter-users-with-r

# https://github.com/opencovid19-fr/data/blob/master/data-sources/sante-publique-france/covid_hospit.csv

### chargeent des paquetage cartograhiques
library(maps)
library(cartography)
library(ggmap)
library(maptools)
library(ggthemes)
library(sf)
theme_set(theme_bw())
# liens carto dans R avec ggplot:
# https://ggplot2-book.org/maps.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://www.datanovia.com/en/fr/blog/comment-creer-une-carte-avec-ggplot2/
# plus sûr d'utiliser sf ici:
# https://community.rstudio.com/t/plotting-shapefiles-with-attributes-using-ggplot/41290
# vignettes de sf en six pages: 
# https://cran.r-project.org/web/packages/sf/vignettes/sf1.html
# https://unconj.ca/blog/choropleth-maps-with-r-and-ggplot2.html
# avec ggmap:
# http://www.milanor.net/blog/maps-in-r-choropleth-maps/
### attention aux fonds de carte départementaux !
### le fond de carte france de maps est départemental
france <- map('france', fill = TRUE, col = 1:10) # page 6 de la doc maps
france$names # carte bizarre avec enclaves et exclaves
### comme conseillé dans la notice d'initiation, on renomme les enclaves et exclaves:
summary(france$names)
france$names[4] <- "Nord"
france$names[16] <- "Val-d'Oise"
france$names[25] <- "Côtes-d'Armor"
france$names[27] <- "Côtes-d'Armor"
france$names[29] <- "Finistère"
france$names[30] <- "Finistère"
france$names[37] <- "Finistère"
france$names[43] <- "Côte-d'Or"
france$names[44] <- "Haute-Saône"
france$names[49] <- "Morbihan"
france$names[51] <- "Nièvre"
france$names[53] <- "Morbihan"
france$names[54] <- "Morbihan"
france$names[55] <- "Morbihan"
france$names[59] <- "Saône-et-Loire"
france$names[61] <- "Vendée"
france$names[62] <- "Vendée"
france$names[64] <- "Vendée"
france$names[71] <- "Rhône"
france$names[71] <- "Puy-de-Dôme"
france$names[72] <- "Charente-Maritime"
france$names[75] <- "Charente-Maritime"
france$names[77] <- "Isère"
france$names[78] <- "Corrèze"
france$names[83] <- "Drôme"
france$names[84] <- "Ardèche"
france$names[87] <- "Lozère"
france$names[93] <- "Vaucluse"
france$names[93] <- "Hérault"
france$names[99] <- "Hérault"
france$names[100] <- "Bouches-du-Rhône"
france$names[103] <- "Hautes-Pyrénées"
france$names[104] <- "Pyrénées-Atlantiques"
france$names[106] <- "Hautes-Pyrénées"
france$names[107] <- "Ariège"
france$names[108] <- "Hautes-Pyrénées"
france$names[109] <- "Var"
france$names[110] <- "Var"
france$names[111] <- "Var"
france$names[113] <- "Pyrénées-Orientales"
france$names[114] <- "Corse-du-Sud"
summary(france$names)
france$names
francedep <- ggplot() +
  borders("france", colour = "gray85", fill = "gray80") +
  theme_map() 
francedep
cartedep <- francedep
cartedep
### appariement entre carte France de maps et fichier de données covid
appari <- match.map(france, datahosptot.w$lib_dep, exact = TRUE)
appari
### test sur un extrait des données soit dans le map soit en filtrage.
cartedep + 
  geom_polygon(
  aes(nombres_hosp, ),
  data = datahosptot.w,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

france_map <- map_data("france")
hosp_dep <- ggplot(datahosptot.w, 
                   aes(map_id = lib_dep)) +
            geom_map(aes(fill = nombres_hosp), 
                     map = france_map)
hosp_dep


### construction d'une carte ggplot à partir de ces blocs:
states_map <- map_data("state")
ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)

last_plot() + coord_map()
ggplot(crimes_long, aes(map_id = state)) +
  geom_map(aes(fill = value), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  facet_wrap( ~ variable)
  

##https://ggplot2.tidyverse.org/reference/geom_map.html
geom_map(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  ...,
  map,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

### la cartographie de la france avec cartography est plus complète:
fr_dep <- st_read("departements-20140306-100m.shp")
ggplot(data = filter(datahosptot.w, jour == "2020-03-18")) +
  geom_sf(aes(fill = nombres_hosp)) +
  theme_map() +
  labs(title = "Total COVID-19 cases in France: {frame_time}", fill = "Cases")
# https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
fr_dep %>% rename(fr_dep, code_insee = dep) # comprendre pourquoi cela ne marche pas ?
head(fr_dep, 5)

names(fr_dep)[names(fr_dep) == "code_insee"] <- "dep" # ici ça marche ?
head(fr_dep, 5)
data_map_fr_dep <- right_join(fr_dep, datahosptot.w, by = "dep") # c'est bien jointure droite
head(data_map_fr_dep, 5)

### tracé de la carte !!! transition reveal non supporté !
hosp_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_hosp)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Hospitalisations COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "hospitalisations",
       caption = "Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(hosp_dep_map)
anim_save("hosp_dep_map.gif", last_animation()) # ça marche mais il s'arrête au 24 / 04 /2020 ???
### a noter, transition_time ne fonctionnait pas correctement ? reproduire et voir pourquoi.
### tracé de la carte !!! transition reveal non supporté !
dec_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_dc)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis", breaks = c(400, 800, 1200, 1600)) +
  labs(title = "Décès cumulés COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "Décès cumulés",
       caption = "Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(dec_dep_map, nframes = 57)
anim_save("dec_dep_map.gif", last_animation())
# réanimation
rea_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_rea)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Patients COVID-19 en réanimation par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       fill = "Patients en réanimation") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(rea_dep_map, nframes = 57)
anim_save("rea_dep_map.gif", last_animation())
# retours domicile
rad_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_rad)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Patients COVID-19 retournés au domicile par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
       fill = "Patients retournés \n au domicile") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(rad_dep_map, nframes = 57)
anim_save("rad_dep_map.gif", last_animation())

### cartes en taux pour 100 000 habitants
### tracé de la carte !!! transition reveal non supporté !
t100_hosp_dep_map <- ggplot(data_map_fr_dep, aes(fill = taux_100_hosp)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Hospitalisations COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "hospitalisations pour \n 100 000 habitants",
       caption = "Source: Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(t100_hosp_dep_map, nframes = 57)
anim_save("t100_hosp_dep_map.gif", last_animation())
### incidence des décès cumulés
### tracé de la carte !!! transition reveal non supporté !
t100_dec_dep_map <- ggplot(data_map_fr_dep, aes(fill = taux_100_dc)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Décès cumulés COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "Décès cumulés pour \n 100 000 habitants",
       caption = "Source: Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(t100_dec_dep_map, nframes = 57)
anim_save("t100_dec_dep_map.gif", last_animation())
# réanimation
t100_rea_dep_map <- ggplot(data_map_fr_dep, aes(fill = taux_100_rea)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Patients COVID-19 en réanimation par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : Santé publique France",
       fill = "Patients en réanimation \n pour 100 000 habitants") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(rea_dep_map, nframes = 57)
anim_save("rea_dep_map.gif", last_animation())
# retours domicile
t100_rad_dep_map <- ggplot(data_map_fr_dep, aes(fill = taux_100_rad)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Patients COVID-19 retournés au domicile par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : Santé publique France",
       fill = "Patients retournés \n au domicile \n pour 100 000 habitants") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(t100_rad_dep_map, nframes = 57)
anim_save("t100_rad_dep_map.gif", last_animation())

### hospitalisations échelle log
log_hosp_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_hosp)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis", trans = "log1p", breaks = c(0, 10, 100, 1000)) +
  labs(title = "Hospitalisations COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "hospitalisations \n (échelle logarithmique)",
       caption = "Source: Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(log_hosp_dep_map, nframes = 57)
anim_save("log_hosp_dep_map.gif", last_animation())
### décès cumulés échelle log
log_dec_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_dc)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis", trans = "log1p", breaks = c(0, 10, 100, 1000)) +
  labs(title = "Décès cumulés COVID-19 par département", 
       subtitle = "date : {jour[frame]}",
       fill = "Décès cumulés \n (échelle logarithmique)",
       caption = "Source: Santé publique France") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(log_dec_dep_map, nframes = 57)
anim_save("log_dec_dep_map.gif", last_animation())
### réanimations échelle log
# réanimation
log_rea_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_rea)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis", trans = "log1p", breaks = c(0, 10, 100, 1000)) +
  labs(title = "Patients COVID-19 en réanimation par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : Santé publique France",
       fill = "Patients en réanimation \n (échelle logarithmique)") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(log_rea_dep_map, nframes = 57)
anim_save("log_rea_dep_map.gif", last_animation())
# retours domicile
log_rad_dep_map <- ggplot(data_map_fr_dep, aes(fill = nombres_rad)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_continuous(type = "viridis", trans = "log1p", breaks = c(0, 10, 100, 1000)) +
  labs(title = "Patients COVID-19 retournés au domicile par département", 
       subtitle = "date : {jour[frame]}",
       caption = "Source : Santé publique France",
       fill = "Patients retournés \n au domicile \n (échelle logarithmique)") +
  transition_states(jour) + # on peut utiliser transition_state
  ease_aes('linear')
animate(log_rad_dep_map, nframes = 57)
anim_save("log_rad_dep_map.gif", last_animation())

# lire second commentaire ici:
# https://github.com/tidyverse/ggplot2/issues/2872
# ici travail sur données ouragans:
# https://github.com/r-spatial/sf/issues/88

synth_dep <- read.csv2("donnees-carte-synthese-tricolore.csv", header = TRUE,
                       stringsAsFactors = FALSE, sep = ",")
synth_dep$date <- as.Date(synth_dep$date, format = "%Y-%m-%d")
synth_dep$indicateur_synthese <- as.factor(synth_dep$indicateur_synthese)
head(synth_dep, 5)
names(synth_dep)[names(synth_dep) == "code_departement"] <- "dep" # ici ça marche ?
head(fr_dep, 5)
data_map_fr_dep_syn <- right_join(fr_dep, synth_dep, by = "dep") # c'est bien jointure droite
head(data_map_fr_dep_syn, 5) # ok pour la carte jointure effectuée.

# carte de l'indicateur de synthèse
cols <- c("rouge" = "red", "orange" = "orange", "vert" = "green")
synthese_dep_map <- ggplot(data_map_fr_dep_syn, aes(fill = indicateur_synthese)) + ### c'est ok mais on perd les doms
  geom_sf() +
  coord_sf(xlim = c(-7, 10), ylim = c(41.5, 51)) +
  theme_map() +
  theme(title = element_text(size = 15), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) +
  scale_fill_manual(values = cols) +
  labs(title = "Indicateur de synthèse COVID-19 par département", 
       subtitle = "date : {date[frame]}",
       caption = "Source : Santé publique France",
       fill = "Indicateur de synthèse") +
  transition_states(date) + # on peut utiliser transition_state
  ease_aes('linear')
animate(synthese_dep_map, nframes = 8) # attention il n'y a que 8 jours ! 
anim_save("synthese_dep_map.gif", last_animation())

summary(data_map_fr_dep_syn) 

write.csv2(data_map_fr_dep_syn, "data_map_fr_dep_syn.csv")

# voir ici: il faut faire une jointure pour amener sur la carte les données.
#https://www.datanovia.com/en/fr/blog/comment-creer-une-carte-avec-ggplot2/

### voir s'il ne faut pas utiliser le sélecteur manuel que j'avais utilisé pour la carto 
### communale énergie.


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

animate(gg + ease_aes('cubic-in-out'), fps = 10, end_pause = 25, 
        height = 800, width = round(800/1.61803398875))

