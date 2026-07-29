# extra code to keep:





# problème des palettes sur facteurs avec beaucoup de modalités:
# https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
# Define the number of colors you want
#♥
nb.cols <- 30
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
spectral <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

# https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html


CO2_modele.s <- ggplot(subset(data2050_long, VARIABLE == "Emissions|CO2")) +
  aes(x = year, y = value) +
  geom_point(aes(colour = MODEL), alpha = 0.7) +
  scale_color_manual(values = spectral) +
  labs(x = "année de projection",
       y = "Emissions de CO2",
       title = "Emisions de CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle", 
       caption = "Source: IPCC",
       colour = "Modèle")
CO2_modele.s # ça marche mais il faudra revoir la palette de couleurs et les tailles !

CO2_modele.l <- ggplot(subset(data2050_long, VARIABLE == "Emissions|CO2")) +
  aes(x = year, y = value) +
  geom_line(aes(colour = MODEL)) +
  #scale_color_manual(values = spectral) +
  labs(x = "année de projection",
       y = "Emissions de CO2",
       title = "Emisions de CO2 SRES AR5 jusqu'à 2050",
       subtitle = "selon le modèle", 
       caption = "Source: IPCC",
       colour = "Modèle")
CO2_modele.l



### en fait, le mieux serait de créer un df en utilsant pivot wider de manière
# à distribuer les variables en colonne par values to:
# ensuite je pourrait utiliser les fonctions de ggplot2 comme dans les bouquisn !
# on garde peut-être plus de possibilités à voir.

# extracton des variables, Energie Finale totale PIB 2005, CO2 et population
# dans ce contexte il faut employer filter == on doit filtrer des observations
# qui satisfont une condition précise !
#rm(data2050.gdp)
### prendre ce cas d'école pour une séance suivante sur tidyverse ! ----
data2050.gdp <- filter(data2050_long, VARIABLE %in% c("Emissions|CO2", 
                                                      "Final Energy",
                                                      "GDP|MER",
                                                      "Population"))
summary(data2050.gdp$value)
levels(data2050.gdp$VARIABLE) # 148 catégories au lieu de 4 !!!!
levels(data2050.gdp$SCENARIO)
### bien garder cela car c'est un pur cas d'école !!!
### il faut modifier le fichier en amont en filtrant par une regex pour retirer les signes |
head(data2050.gdp) ### ça marche !
View(data2050.gdp)
saveRDS(data2050.gdp, "data2050_gdp.rds")
### prendre ce cas d'école pour une séance suivante sur tidyverse !
# pourtant, dans un certain contexte il a réussi à le faire:
View(data.kaya.2050)
### retrouver comment j'ai réussi à extraire exactement les quatre variables demandées ???
### il y a un problème de contexte !!! ? refaire toute la séquence des traitements
# dans un autre fichier ?








# ensuite on le pivote en format large pour obtenir les variables en colonnes !
data.kaya.2050 <- pivot_wider(data2050.gdp, id_cols = NULL, names_from = VARIABLE,
                              names_prefix = "", names_sep = "_", names_repair = "check_unique",
                              values_from = value, values_fill = NULL, values_fn = NULL)
head(data.kaya.2050, 25) ## it works but many NAs
View(data.kaya.2050)

##il faut absolument bannir les noms de variable IPCC en xyz|yyy ! 


## cas intéressant à faire: 
GDP.CO2.2050 <- ggplot(data = data.kaya.2050) +
  aes(x = GDP|MER, y = Emissions|CO2) +
  geom_point(aes(colour = MODEL, size = "Population"), alpha = 0.7) +
  scale_color_manual(values = spectral) +
  labs(x = "PIB en G$2005",
       y = "Emissions de CO2",
       title = "Emisions de CO2 SRES AR5 jusqu'à 2050",
       subtitle = "Couleurs selon le modèle", 
       caption = "Source: IPCC",
       colour = "Modèle",
       size = "Population \n (millions)")
GDP.CO2.2050 # il va planter à cause des |


#### CAS d'ECOLE a CONSERVER pour le soumettre aux AMINCHETTES ! -----
## renommber les colonnes:
rename(data.kaya.2050, Emissions\\|CO2 = Emissions_CO2)
rename(data.kaya.2050, GDP\\|MER = GDP_2005)
rename(data.kaya.2050, Final Energy = Final_Energy)

summary(data.kaya.2050)
View(data.kaya.2050)

### cas très intéressant car on voit l'effet catastrophique des conventions entre
# plateformes et producteurs de données: 
# les noms de colonnes des la table pivotée finale sont totalement impossibles à modifier !
# ==> on doit donc modifier les | en amont pour éviter la cata !

# il y a peut-être une solution avec stringr

# rename !!!! TROUVE !!! 
rename(data.kaya.2050, Emissions_CO2 = Emissions|CO2)
rename(data.kaya.2050, Final_Energy = GDP|MER)
rename(data.kaya.2050, GDP_2005 = Final Energy)
summary(data.kaya.2050)

View(data.kaya.2050)

#https://www.r-bloggers.com/rename-columns-r/
#"Using Base r:

colnames(cars)[2] <-"Stopping Distance (ft)"

[1] "speed"                  "Stopping Distance (ft)"

colnames(data.kaya.2050)[22] <- "Emissions_CO2"
colnames(data.kaya.2050)[23] <- "Final_Energy"
colnames(data.kaya.2050)[24] <- "GDP_2005"
### enfin ! ça marche !!!! tydiverse KO techno par colnames de Base R !!! 
# un puzzle intéressant à garder !

### ==> cas d'école super intéressant à soumettre en une séance de puzzles et 
# autour du traitement de données ! une galère de deux heures pour un dimanche confiné !

### reprise du ggplot avec la base data.kaya.2050. ----

summary(data.kaya.2050)
View(data.kaya.2050)

GDP.CO2.2050 <- ggplot(data = data.kaya.2050) +
  geom_point(aes(x = GDP_2005,
                 y = Emissions_CO2,
                 colour = MODEL,
                 size = Population),
             alpha = 0.7) +
  scale_color_manual(values = spectral) +
  labs(x = "PIB en G$2005",
       y = "Emissions de CO2",
       title = "Emisions de CO2 SRES AR5 jusqu'à 2050",
       subtitle = "Couleurs selon le modèle", 
       caption = "Source: IPCC",
       colour = "Modèle",
       size = "Population \n (millions)")
GDP.CO2.2050 # il va planter à cause des |
rm(GDP.CO2.2050)

### interessant problème: il a en effet que ds données manquantes puisque toutes
# séries ont des NA en correspondance !!! donc tout est NA !

### il faut réarranger labase data.kaya.2050 une fois de plus avec filter !
# virer la colonne Unit inutile:
data.kaya.2050 <- select(data.kaya.2050, -20)
head(data.kaya.2050, 25)

# on doit tenter un filtrage par appariement des scenario et années non NA.
### je commence juste quelques colonnes
rm(iltre_Energie)
rm(filtre_GDP)
rm(filre_co2)

CO2 <- drop_na(select(data.kaya.2050, c(1,2,18,20,21)))
head(CO2, 25) ### excellent !
PIB <- drop_na(select(data.kaya.2050, c(1,2,18,20,23)))
head(PIB, 25) ### excellent !
EF <- drop_na(select(data.kaya.2050, c(1,2,18,20,24)))
head(EF, 25) ### excellent !

# creation du fichier de donnees pour ggplot2 nuages de points par jointures successives
### on commence par population qui a le plus d'observations
rm(data.kaya.2050.4)
data.kaya.2050.4 <- left_join(filtre_Population, CO2, by = c("year"))
head(data.kaya.2050.4, 25)
View(data.kaya.2050.4)
data.kaya.2050.4 <- left_join(filtre_Population, CO2)
head(data.kaya.2050.4, 25)

# affichage de l'allocation de mémoire:
gc()
.rs.restartR()

### bon stacks à lire attentiviement 
# https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot
# code proposé:
library(tidyr)
library(dplyr)
library(ggplot2)
df %>% select(id, num1, num2, cat) %>%
  pivot_longer(., cols = c(num1,num2), names_to = "Var", values_to = "Val") %>%
  ggplot(aes(x = Var, y = Val, fill = cat)) +
  geom_boxplot()
# on approche ?
rm(gg.pib.co2)
data.kaya.2050.4 <- filter(.data = data.kaya.2050,
                           Emissions_CO2 > 0 | Final_Energy > 0 | GDP_2005 > 0 | Population > 0)
head(data.kaya.2050.4, 25)
summary(data.kaya.2050.4) # excellent ça marche !

pib2005.co2 <- ggplot(data = data.kaya.2050.4) +
  geom_point(aes(x = GDP_2005,
                 y = Emissions_CO2,
                 colour = MODEL,
                 size = Population),
             alpha = 0.7) +
  scale_color_manual(values = spectral)
pib2005.co2