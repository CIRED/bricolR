###############################################################################
####             Script bricole R 2020.3 sur gganimate                     ####
####                  séance du 21 juillet 2020                            ####
###############################################################################

##chargement des paquetages
### lancement des paquetages necessaires:
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(openxlsx)
library(officer)
library(RColorBrewer)
library(ggsci)
library(gganimate)
library(ggfittext)
library(ggimage)
theme_set(theme_bw())  # pre-set the bw theme.

### définition du répertoire de travail
setwd("c:/bricoleR/2020.3") ### changez cela dans votre environnement

### traitement des données: on importe tout le fichier AR 5 recodé ----
# on va travailler sur le PIB, le CO2 et la population.
data.co2 <- filter(datar5, VARIABLE == c("Emissions|CO2", "GDP|MER", "Population", "Final Energy"))
View(data.co2) #Il a bien filtré mais on obtient un avertissement obscur ...
summary(data.co2) # on a hélas beaucoup de NAs : on aura pas des séries continues mais tant pis !
# cependant ce n'est pas utilisable comme cela. 
# rarrangeons les séries en les empilant les années, pour cela on va utiliser
# la commandes pivot.longer: https://tidyr.tidyverse.org/reference/pivot_longer.html
data.co2.long <- pivot_longer(data.co2, 
                              cols = starts_with("2"),
                              names_to = "year",
                              values_to = "value",
                              values_drop_na = TRUE) # on débarasse les NAs
View(data.co2.long) # les séries sont maintenant empilées dans la variable year et value.
# maintenant nous pouvons demander des résumés qui ont du sens:
data.co2.long$year <- as.factor(data.co2.long$year)
View(data.co2.long) # c'est converti en facteur
attach(data.co2.long)

## on peut aussi extraire les données relatives au monde
data.co2.long <- filter(data.co2.long, REGION == "World")

# résumé par observations par année selon ls modèles
by(data.co2.long$year, MODEL, summary)
by(data.co2.long$year, REGION, summary) # problème j'aurais dû renommer le fichier filtré sur monde autrement



## on pourrait utiliser des fonctions de dplyr tidyr pour faire des totaux plus intéressants
## ==> on laisse cela pour la prochaine séance 2020.4 où on fera du tidyverse
## ceci clot la phase de préparation des données.


### premiers graphiques classiques non animés ----
# on travaille à partir du fichier long créé par pivot_longer():
View(data.co2.long) # ça marche !

# prenons ggplot classique dans le plan des trajectoires, années -- indicateur
ggplot(data = data.co2.long, aes(x = year, y = value, group = MODEL, color = value)) +
  facet_wrap(~ VARIABLE, scales = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
  scale_colour_gradient(low = "orange", high = "purple4") + # avec quatre indicateurs c'est peu parlant
  theme(axis.text.x = element_text(size = 9, angle = 45),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires des émissions, du PIB, de l'énergie finale et de la population", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "Indicateur") 
### on peut aussi changer la variable de groupe:
ggplot(data = data.co2.long, aes(x = year, y = value, group = SCENARIO, color = MODEL)) +
  facet_wrap(~ VARIABLE, scales = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
  theme(axis.text.x = element_text(size = 9, angle = 45),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires des émissions, du PIB, de l'énergie finale et de la population", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "Indicateur")
# on pourra aussi faire des facettes selon le modèle:
ggplot(data = data.co2.long, aes(x = year, y = value, group = SCENARIO, color = MODEL)) +
  facet_wrap(~ MODEL, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 9, angle = 45),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires des émissions, du PIB, de l'énergie finale et de la population", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "Modèle") # c'est difficilement lisible et on préfère une seul indicateur
## on pourra aussi faire des facettes par indicateur et MODEL:
ggplot(data = data.co2.long, aes(x = year, y = value, group = SCENARIO, color = MODEL)) +
  facet_grid(MODEL ~ VARIABLE, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 9, angle = 45),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires des émissions, du PIB, de l'énergie finale et de la population", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "Modèle")
### mais cela devient difficile à lire ! C'est là que les animations trouvent toute leur utilité !

#### graphiques animés avec GGANIMATE ----------
### Une première animation sur les années.
transition.year <- ggplot(data = data.co2.long, 
                          aes(x = year,
                              y = value,
                              group = SCENARIO,
                              color = MODEL)) +
  facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
  theme(axis.text.x = element_text(size = 9, angle = 45),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires mondiales par modèle selon l'année", 
       subtitle = "des scenarii SRES de l'AR 5 ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_reveal(along = year, keep_last = F)
  animate(transition.year)
  anim_save("transition.year.gif", last_animation())

### transition sur la couverture
  transition.coverage <- ggplot(data = data.co2.long, 
                            aes(x = year,
                                y = value,
                                group = SCENARIO,
                                color = MODEL)) +
    facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
    theme(axis.text.x = element_text(size = 9, angle = 45),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_point() +
    geom_line() +
    labs(x = "year", y = "valeurs",
         title = "Trajectoires mondiales par modèle selon: {closest_state}", 
         subtitle = "des scenarii SRES de l'AR 5 ", 
         caption = "Source : IPCC",
         colour = "Modèle") +
    transition_states(Coverage, transition_length = 2, state_length = 4, wrap = TRUE)
  animate(transition.coverage)
  anim_save("transition.coverage.gif", last_animation())
### à valider sur d'autres variables mais il semble que l'unique modèle économétrique
### n'a pas de données pour les émissions et l'énergie finale ==> cela fait planter
### le cycle d'animation sur les modèles d'où les messages d'erreur lorsque l'on fait la # transition sur modèle...
  ### transition sur le nombre de substances prises en compte :
  transition.substances <- ggplot(data = data.co2.long, 
                                aes(x = year,
                                    y = value,
                                    group = SCENARIO,
                                    color = MODEL)) +
    facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
    theme(axis.text.x = element_text(size = 9, angle = 45),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_point() +
    geom_line() +
    labs(x = "year", y = "valeurs",
         title = "Trajectoires mondiales par modèle selon: {closest_state}", 
         subtitle = "des scenarii SRES de l'AR 5 ", 
         caption = "Source : IPCC",
         colour = "Modèle") +
    transition_states(Substances, transition_length = 2, state_length = 4, wrap = TRUE)
  animate(transition.substances)
  anim_save("transition.substances.gif", last_animation())
  ### les modèles avec 10 substances n'ont que la population comme variable projetée !
  

### liens utiles pour approfondir:
#  https://gganimate.com/articles/gganimate.html
# https://github.com/ropenscilabs/learngganimate
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# funny :
# https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#32

  


