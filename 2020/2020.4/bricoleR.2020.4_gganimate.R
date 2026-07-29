###############################################################################
######            script de la séance bricole'R 2020.4                   ######
######                  vendredi 11 / 12/ 2020                           ######
######                  séance gganimate  n°2                            ######
###############################################################################


##chargement des paquetages
#### 1)  lancer les paquetages utiles ----
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
library(ggsci)
library(ggpubr)
library(gganimate) # le paquetage essentiel de l'atelier
library(ggfittext)
library(ggimage)
library(gt)
### paquetages utiles pour produire des fichiers xl
library(openxlsx)
library(xlsx)
library(haven) # importations de données format propriétaire
library(EDA) # analyses de données exploratoires 
theme_set(theme_bw())  # pre-set the bw theme.

### définition du répertoire de travail
setwd("c:/bricoleR/2020.4") ### changez cela dans votre environnement
getwd() # afficher le répertoire de travail.

### liens utiles pour explorer et progresser:
# https://github.com/ropenscilabs/learngganimate
# https://stt4230.rbind.io/tutoriels_etudiants/hiver_2020/gganimate/
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# funny :
# https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#32
# https://declaredesign.org/r/estimatr/articles/getting-started.html
### gganimate par état dans une coupe ?
# sur les données AR 5


# extraction et construction des bases larges ----
### test sur extraction 2010 et 2050 !
data.co2.long.wld.2010.w <- pivot_wider(data.co2.long.wld.2010,
                                        names_from = "VARIABLE",
                                        values_from = c("VARIABLE","value"))
View(data.co2.long.wld.2010.w)
by(data.co2.long.wld.w, data.co2.long.wld.w$SCENARIO, summary)

### test pour comprendre les problèmes de scenarii:
data.co2.long.wld.2010.sce <- pivot_wider(data.co2.long.wld.2010,
                                          names_from = "SCENARIO",
                                          values_from = "value",
                                          values_fn = mean)
View(data.co2.long.wld.2010.sce) ### peut-être pas toutes les années ?
summary(data.co2.long.wld.2010.sce)

### séries non homogènes en raison des différents modèles:
View(data.co2.long.wld.w) ### la base monde étendue en croisant variable--valeurs

### plan pib / Emissions
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_GDP|MER`/`value_Population`,
           y = `value_Emissions|CO2`/`value_Population`,
           colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "PIB per capita (k$2005/cap)", y = "Emissions per capita(tCO2pc)",
       title = "Emissions per capita selon le PIB per capita entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")
    
### plan pib / Emissions
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_Final Energy`/`value_Population`,
           y = `value_Emissions|CO2`/`value_Population`,
           colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "Final Energy per capita (PJ/cap)", y = "Emissions per capita(tCO2pc)",
       title = "Emissions per capita selon l'énergie finale per capita entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")
### gganimate sur modèles en per capita 
transition.modele.CO2pc <- ggplot(data = data.co2.long.wld.w, 
                            aes(x = `value_GDP|MER`/`value_Population`,
                                y = `value_Emissions|CO2`/`value_Population`,
                                colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "PIB per capita (k$2005/cap)", y = "Emissions per capita(tCO2pc)",
       title = "Emissions per capita selon le PIB per capita entre 2010 et 2100", 
       subtitle = "Modèle: {closest_state}", 
       caption = "Source : IPCC AR5",
       colour = "modèle") +
  transition_states(MODEL, transition_length = 2, state_length = 10)
animate(transition.modele.CO2pc, nframes = 150, fps = 4)
anim_save("transition.modele.CO2pc.gif", last_animation())
### comment sélectionner dans le ggplot les données utiles ? == sans NA ?

### chargement du fichier pour animation IPCC -----
saveRDS(data.co2.long.wld.w, file = "data.co2.long.wld.w.rds")
attach(data.co2.long.wld.w)
data.co2.long.wld.w <- readRDS("data.co2.long.wld.w.rds")
View(data.co2.long.wld.w)
### premiers graphiques dans le plan Emissions / population selon le modèle ----
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_Population`, y = `value_Emissions|CO2`, colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "Population (millions)", y = "Emissions (MtCO2)",
       title = "Emissions en fonction de la population mondiale entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")
### plan pib / Emissions
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_GDP|MER`, y = `value_Emissions|CO2`, colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "PIB", y = "Emissions (MtCO2)",
       title = "Emissions en fonction du entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")
### plan pib / Emissions
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_GDP|MER`, y = `value_Emissions|CO2`, colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "PIB", y = "Emissions (MtCO2)",
       title = "Emissions en fonction du entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")
### pibpc et emissions pc
### plan pib / Emissions per capita
ggplot(data = data.co2.long.wld.w, 
       aes(x = `value_GDP|MER`/`value_Population`,
           y = `value_Emissions|CO2`/`value_Population`,
           colour = factor(MODEL))) +
  #scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  #geom_line() + # ajouter un autre niveau de groupe ?
  labs(x = "PIB per capita", y = "Emissions per capita(tCO2pc)",
       title = "Emissions per capita selon le PIB per capita entre 2010 et 2100", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "modèle")

#### graphiques animés avec GGANIMATE IPCC ----------
### Animation sur les scenarios plan (PIB,CO2)
transition.modele <- ggplot(data = data.co2.long.wld.w, 
                          aes(x = `value_GDP|MER`, 
                              y = `value_Emissions|CO2`,
                              colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "PIB", y = "Emissions",
       title = "Emissions dans le plan PIB / CO2 selon le scenario des scenarii SRES de l'AR 5.", 
       subtitle = "Modèle: {closest_state}", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_states(MODEL, transition_length = 2, state_length = 10)
animate(transition.modele, nframes = 150, fps = 4)
anim_save("transition.modele.gif", last_animation())
### Une première animation sur les scenarios plan (EF,CO2)
transition.modele.E <- ggplot(data = data.co2.long.wld.w, 
                            aes(x = `value_Final Energy`, 
                                y = `value_Emissions|CO2`,
                                colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "Energy Finale (EJ/yr)", y = "Emissions (MtCO2)",
       title = "Emissions dans le plan Energie finale / CO2 selon le scenario des scenarii SRES de l'AR 5.", 
       subtitle = "Modèle: {closest_state} ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_states(MODEL, transition_length = 2, state_length = 10)
animate(transition.modele.E, nframes = 150, fps = 5)
anim_save("transition.modele.E.gif", last_animation())
### Une première animation sur les scenarios plan (Pop,CO2)
transition.modele.P <- ggplot(data = data.co2.long.wld.w, 
                              aes(x = `value_Population`, 
                                  y = `value_Emissions|CO2`,
                                  colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "Population (Millions)", y = "Emissions (MtCO2)",
       title = "Emissions dans le plan Population / CO2 selon le scenario des scenarii SRES de l'AR 5.", 
       subtitle = "Modèle: {closest_state} ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_states(MODEL, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() + exit_fade()
animate(transition.modele.P, fps = 3)
anim_save("transition.modele.P.gif", last_animation())
### transition sur les années par modèle dans CO2 / PIB
### 150 cadres, 4 cadres / seconde, pas d'effets...
transition.year <- ggplot(data = data.co2.long.wld.w, 
                            aes(x = `value_GDP|MER`, 
                                y = `value_Emissions|CO2`,
                                colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "PIB", y = "Emissions",
       title = "Emissions dans le plan PIB / CO2 selon le scenario des scenarii SRES de l'AR 5.", 
       subtitle = "Modèle: {closest_state} ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_states(year, transition_length = 2, state_length = 10)
animate(transition.year, nframes = 150, fps = 4)
anim_save("transition.modele.year.gif", last_animation())
### transition sur les années par modèle dans CO2 / Pop
### 150 cadres, 4 cadres / seconde, pas d'effets...
transition.year.P <- ggplot(data = data.co2.long.wld.w, 
                          aes(x = `value_Population`, 
                              y = `value_Emissions|CO2`,
                              colour = MODEL)) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  labs(x = "Population (Millions)", y = "Emissions (MtCO2)",
       title = "Emissions dans le plan population / CO2 selon le scenario des scenarii SRES de l'AR 5.", 
       subtitle = "Modèle: {closest_state} ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() + exit_fade()
animate(transition.year.P, nframes = 150, fps = 4)
anim_save("transition.modele.P.year.gif", last_animation())


## junk code:
#p <- ggplot(data = data.co2.long.wld.w, aes(x = `value_GDP|MER`, y = `value_Emissions|CO2`, colour = MODEL)) +
#  geom_point()
#plot(p)
#attach(data.co2.long.wld.w)
#anim <- p +  transition_states(MODEL, transition_length = 2, state_length = 1)
#anim
#rm(anim)

#### partie 2 : graphiques animés covid 19 !
### courbe animées par département: hospitalisations
library(ggplot2)
library(transformr)
library(gganimate)
hosp_line_dep <- ggplot(datahosptot.w,
                        aes(jour,
                            nombres_hosp,
                            group = dep,
                            color = nombres_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "hospitalisations",
       title = "Hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "décès cumulés") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F)
animate(hosp_line_dep, nframes = ndays)
anim_save("hosp_line_dep.gif", last_animation())
rm(hosp_line_dep )
### décès cumulés
dec_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           nombres_dc,
                           group = dep,
                           color = nombres_rea)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "décèss cumulés",
       title = "Evolution des décès cumulés COVID 19 par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "hospitatlisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F)
animate(dec_line_dep, nframes = ndays)
anim_save("dec_line_dep.gif", last_animation())
rm(dec_line_dep)
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
       caption = "Source : Santé publique France",
       size = "décès cumulés",
       colour = "hospitalisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F)
animate(rea_line_dep, nframes = ndays)
anim_save("rea_line_dep.gif", last_animation())
rm(rea_line_dep)
### retours au domicile
rad_line_dep <- ggplot(datahosptot.w, 
                       aes(jour,
                           nombres_rad,
                           group = dep,
                           color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "patients retournés au domicile",
       title = "Patients retournés au domicile par département",
       subtitle = "date : {jour[frame]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "hospitalisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F)
animate(rad_line_dep, nframes = ndays) ## trés intéressant
anim_save("rad_line_dep.gif", last_animation())
rm(rad_line_dep) # soulager la mémoire !

### quelques graphiques animés covid intéressants -----
# pour alléger le temps de calcul on va extraire les données sur les trois mois passés:
postconf <-  filter(datahosptot.w, datahosptot.w$jour > "2020-09-12") # attention au format date !!!
view(postconf)
View(jour)
jourpostconf <- filter(jour, jour$V1 > "2020-09-13") # noter qu'il n'y a pas de filtre tidyverse pour des ojets date.
View(jourpostconf)
postconf <- na.omit(postconf) # nécessaire pour gganimate
summary(postconf)
# diagrammes de phases (x,x')
hosp_line_dep <- ggplot(postconf,
                        aes(jour,
                            nombres_hosp,
                            group = dep,
                            color = nombres_dc)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "hospitalisations",
       title = "Hospitalisations COVID 19 par département",
       subtitle = "date : {jour[frame+178]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "décès cumulés") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F) +
  view_follow()
animate(hosp_line_dep, nframes = 90)
anim_save("hosp_line_dep_follow.gif", last_animation())
#  décès cumulés post confinement
dec_line_dep <- ggplot(postconf,
                        aes(jour,
                            nombres_dc,
                            group = dep,
                            color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_rea)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "décès cumulés",
       title = "Décès cumulés COVID 19 par département",
       subtitle = "date : {jour[frame+178]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "hospitalisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F) +
  view_follow()
animate(dec_line_dep, nframes = 90)
anim_save("dec_line_dep_follow_postconf.gif", last_animation())
#  reanimation  post confinement
rea_line_dep <- ggplot(postconf,
                       aes(jour,
                           nombres_rea,
                           group = dep,
                           color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "date", y = "décès cumulés",
       title = "Pations COVID 19 en réanimation par département",
       subtitle = "date : {jour[frame+178]}", 
       caption = "Source : Santé publique France",
       size = "patients en \n réanimation",
       colour = "hospitalisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F) +
  view_follow()
animate(rea_line_dep, nframes = 90)
anim_save("rea_line_dep_follow_postconf.gif", last_animation())

### diagrammes de phase (x,y)
## deces --- reanimations postconf
dec_rea_line_dep <- ggplot(postconf, aes(nombres_dc,
                                              nombres_rea,
                                              group = dep,
                                              color = nombres_hosp)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés", y = "patients en réanimation",
       title = "Décès et patients en réanimation par département",
       subtitle = "date : {jour[frame+178]}", 
       caption = "Source : Santé publique France",
       size = "décès cumulés",
       colour = "hospitalisations") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F) +
  view_follow()
animate(dec_rea_line_dep, nframes = 90)
anim_save("dec_rea_line_dep_follow.gif", last_animation())
## deces --- hosp
dec_hosp_line_dep <- ggplot(postconf, aes(nombres_dc,
                                         nombres_hosp,
                                         group = dep,
                                         color = nombres_rea)) +
  geom_line() +
  geom_text(aes(label = dep, size = nombres_dc)) +
  scale_colour_gradient(low = "orange", high = "purple4") +
  labs(x = "décès cumulés", y = "patients hospitalisés",
       title = "Décès et patients hospitalisés par département",
       subtitle = "date : {jour[frame+178]}", 
       caption = "Source : Santé publique France",
       size = "décès cumulés",
       colour = "réanimation") +
  theme(plot.title = element_text(family = "dejavu-serif", face = "bold", size = 14)) +
  theme(plot.subtitle = element_text(family = "dejavu-serif", face = "bold", size = 12, hjust = 0.5)) +
  transition_reveal(along = jour, keep_last = F) +
  view_follow()
animate(dec_hosp_line_dep, nframes = 90)
anim_save("dec_hosp_line_dep_follow.gif", last_animation())
### faire des cartes animées ?


