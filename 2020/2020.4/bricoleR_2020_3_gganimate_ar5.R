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
glimpse(data.co2)
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
by(data.co2.long$year, data.co2.long$MODEL, summary)
by(data.co2.long$year, data.co2.long$REGION, summary) # problème j'aurais dû renommer le fichier filtré sur monde autrement

annees <- c("2005", "2010", "2015", "2020", "2025", "2030", "2035", "2040",
            "2045", "2050", "2055", "2060", "2065", "2070", "2075", "2080",
            "2085", "2090", "2095", "2100")
annees.num <- c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 
                2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
## attention nombre d'années et de palier diffèrent : c'est pour cela que cela ne s'affiche pas !

mode(data.co2.long$year)
class(data.co2.long$year)

### remplacer les varleurs de year avec mutate:
data.co2.long$year <- as.factor(data.co2.long$year) # recondage des années en facteurs
factor_year <- factor(annees)
# https://dplyr.tidyverse.org/reference/recode.html
data.co2.long$year <- recode(data.co2.long$year, `1` = 2005, `2` = 2010, `3` = 2015,
                             `4` = 2020, `5` = 2025, `6` = 2030, `7` = 2035, `8` = 2040,
                             `9` = 2045, `10` = 2050, `11` = 2055, `12` = 2060, `13` = 2065,
                             `14` = 2070, `15` = 2075, `16` = 2080, `17` = 2085, `18` = 2090,
                             `19` = 2095, `20` = 2100) # attention, pour les données numériques: 
# inversion du sens de recodage == old = new !


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
  theme(axis.text.x = element_text(size = 9, angle = 90),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() + # ajouter un autre niveau de groupe ?
  scale_x_discrete(label = annees.num) +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires des émissions, du PIB, de l'énergie finale et de la population", 
       subtitle = "des scenarii SRES de l'AR 5 selon le modèle", 
       caption = "Source : IPCC",
       colour = "Indicateur") 
### on peut aussi changer la variable de groupe:
ggplot(data = data.co2.long, aes(x = year, y = value, group = SCENARIO, color = MODEL)) +
  facet_wrap(~ VARIABLE, scales = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
  theme(axis.text.x = element_text(size = 9, angle = 90),
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
  theme(axis.text.x = element_text(size = 9, angle = 90),
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
# dans tous les cas il faut filter les séries représentées == faire des extraits des données 
# de la base...

#### graphiques animés avec GGANIMATE ----------
### Une première animation sur les années.
transition.year <- ggplot(data = data.co2.long, 
                          aes(x = year,
                              y = value,
                              group = SCENARIO,
                              color = MODEL)) +
  facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
  theme(axis.text.x = element_text(size = 9, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "valeurs",
       title = "Trajectoires mondiales par modèle selon l'année", 
       subtitle = "des scenarii SRES de l'AR 5. Année: {year[frame]} ", 
       caption = "Source : IPCC",
       colour = "Modèle") +
  transition_reveal(along = year, keep_last = F)
  animate(transition.year)
  anim_save("transition.year.gif", last_animation())
### oops ! pas de transition possible sur un facteur !!! attention ! 
mode(year)
class(year)
## on doit modifier year pour en faire un integer ou du date !
data.co2.long$year <- as.integer(data.co2.long$year) # year recodé en un variable tweenable

### transition sur la couverture
  transition.coverage <- ggplot(data = data.co2.long, 
                            aes(x = year,
                                y = value,
                                group = SCENARIO,
                                color = MODEL)) +
    facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
    theme(axis.text.x = element_text(size = 9, angle = 90),
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
  
  ### transition sur les scenarii ! ooops ! attention pas une variable valide ! car texte !
  transition.scenario <- ggplot(data = data.co2.long, 
                                  aes(x = year,
                                      y = value,
                                      group = SCENARIO,
                                      color = MODEL)) +
    facet_wrap(~ VARIABLE, scale = "free_y") + # attention ! ordonnées différentes dans chaque panneau !
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_point() +
    geom_line() +
    labs(x = "year", y = "valeurs",
         title = "Trajectoires mondiales par scénario: {closest_state}", 
         subtitle = "des scenarii SRES de l'AR 5 ", 
         caption = "Source : IPCC",
         colour = "Modèle") +
    transition_reveal(along = SCENARIO, keep_last = F)
  animate(transition.scenario)
  anim_save("transition.scenario.gif", last_animation())
  
### une possibilité serait de faire un cycle sur les scenarii
  
### idées pour bricole R:
# extraire données monde, recoder scenario et regions puis faire cyclesd de transition sur cela.
# extraits de données et dans le plan pib pc, emissions pc ou autre.
  
  
glimpse(data.co2.long)  

### liens utiles pour approfondir:
#  https://gganimate.com/articles/gganimate.html
# https://github.com/ropenscilabs/learngganimate
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# funny :
# https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#32

  


