###############################################################################
######                   graphismes en ggplot                     ############
###############################################################################

### chargement des paquetages ----
library(magrittr) # paquetage n?cessaire pour utiliser le tuyau ou pipe de programmation
library(gapminder)
library(tidyverse)
library(SmartEDA)
(.packages())
### d?finition du r?pertoire de travail ----
setwd("D:/cours_R")
#setwd("c:/Users/user/desktop/Cours_R") ### changez cela dans votre environnement
getwd() # afficher le r?pertoire de travail.
theme_set(theme_bw())  # pre-set the bw theme.

### pr?lude ? ggpplot: fonctions graphiques de smarteda ----------
# https://towardsdatascience.com/eda-in-r-with-smarteda-eae12f2c6094
# les fonctions de SmartEDA encapsulent des commandes ggplot2 pr?d?finies

#### graphiques basiques : Variables quantitatives ---

## Par d?faut, ExpNumViz calcule des distributions par noyaux des variables quantitatives
densites_gapmind <- ExpNumViz(gapmind.2007.19,
                              target = NULL,
                              nlim = 10,
                              Page = c(3,3))
densites_gapmind[[1]] # notez la subtilit? de l'indexation double crochet:
class(densites_gapmind)
double <- densites_gapmind[[1]]
class(double)
mode(double)

simple <- densites_gapmind[1]
class(simple)
mode(simple)

# https://cran.r-project.org/doc/manuals/R-lang.html#Indexing
mode(densites_gapmind) # c'est une liste d'objets
class(densites_gapmind) # de classe liste
str(densites_gapmind) 
# c'est donc une liste compos?e de 1 objet contenant 3 gtable


### variables qualitatives:
ciredium <- rgb(67, 135, 135, max = 255)
class(ciredium)
library(RColorBrewer)
(.packages())
display.brewer.all()

barres_gapmind <- ExpCatViz(gapmind.2007.19,
                            target = NULL,
                            col = ciredium,
                            clim = 10,
                            margin = 2,
                            Page = c(2,1))
barres_gapmind[[1]]
barres_gapmind
class(barres_gapmind)
mode(barres_gapmind)
str(barres_gapmind)

### En fait, lorsque target = NULL, les autres param?tres sont ignor?s, 
# il g?n?re les courbes de densit? de toutes les variables selon page etc.

# https://www.datamentor.io/r-programming/color/
rgb(67,135,135, max = 255)
ciredium <- "#438787"

barres_HDI_Class <- ExpCatViz(gapmind.2007.19,
                              target = "HDI_Class",
                              col = NULL,
                              clim = 10,
                              margin = 2,
                              Page = c(2,1))
barres_HDI_Class[[1]]



# choisissons une variables quantitative :
graph_GDI_2019 <- ExpNumViz(gapmind_2007_19[,-2], target = "GDI_2019", nlim = 10, Page = c(2,2))
graph_GDI_2019[[1]] # on obtient une liste de nuages de points entre le GDI_2019.

# si on execute la commande sans l'affecter ? un objet on obtient ceci:
ExpNumViz(gapmind_2007_19[,-2], target = "GDI_2019", nlim = 10, Page = c(2,2)) #notez qu'il ignore l'indice -2 !

### on peut aussi des pr?ciser des variables cibles, toujours en indexant:
ExpNumViz(gapmind_2007_19, target = "GDI_2019", nlim = 10)[4:6]
ExpNumViz(gapmind_2007_19, target = "GDI_2019", nlim = 10, 
          gtitle = "Indicateur de d?veloppement genr?", theme = "Default")
titre_GDI_2019 <- ExpNumViz(gapmind_2007_19, target = "GDI_2019", nlim = 10, 
                            gtitle = "Indicateur de d?veloppement genr?", theme = "Default", Page = c(2,2))
titre_GDI_2019[[1]]
ExpNumViz(gapmind_2007_19, scatter = TRUE) # noter l'index relatif

### cible == variable qualitative
ExpNumViz(gapmind_2007_19, target = "GDI_Group_2019", nlim = 10, 
          gtitle = "Indicateur de d?veloppement genr?", theme = "Default")
box_hdi_class <- ExpNumViz(gapmind_2007_19, target = "HDI_Class", nlim = 10, 
                           gtitle = "Variables quantitatives par groupe HDI", theme = "Default")
box_hdi_class[[4]]
rm(box_hdi_class)
ExpNumViz(gapmind_2007_19, target = "HDI_Class", nlim = 10, 
          gtitle = "Variables quantitatives par groupe HDI", theme = "Default")

# http://adv-r.had.co.nz/Subsetting.html
# https://bookdown.org/rdpeng/rprogdatascience/subsetting-r-objects.html

### graphiques quantiles-quantiles ----
gapmindQQ <- ExpOutQQ(gapmind.2007.19,
                      nlim = 10,
                      Page = c(2,2))
gapmindQQ
ExpOutQQ(gapmind.2007.19, nlim = 10)

### coordonn?es parall?les -----
ExpParcoord(gapmind.2007.19, Nvar = c("GDI_2019", "gdpPercap", "lifeExp", "Gender_Inequality_2019"))
ExpParcoord(gapmind.2007.19,
            Group = "continent",
            Nvar = c("GDI_2019", "gdpPercap", "lifeExp", "Gender_Inequality_2019"))
ExpParcoord(gapmind.2007.19,
            Group = "Stadev",
            Nvar = c("GDI_2019", "Gender_Inequality_2019", "gdpPercap", "lifeExp"))
ExpParcoord(gapmind.2007.19,
            Group = "HDI_Class",
            Nvar = c("GDI_2019", "Gender_Inequality_2019", "gdpPercap", "lifeExp", "urban_pop_2019"))


### ajouter passage sur corrplot.
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# http://www.sthda.com/french/wiki/visualiser-une-matrice-de-correlation-par-un-correlogramme

### visualisons les corr?lations avec le paquetage corrplot
# on doit va utiliser la m?thode corrplot... 
library(corrplot)
# Elle n'admet que des donn?es num?riques en entr?e et g?re les NA
# pour cela il faut calculer une matrice de corr?lation ds donn?es avec cor()
gapcor <- cor(gapmind.2007.19[,c(4:6,8:16)], use = "complete.obs") # on a vir? les NAs
round(gapcor,2) # on demande d'afficher la matrice avec deux d?cimales
class(gapcor)
View(gapcor)
# il pourrait ?tre int?ressant d'utiliser les corr?lations de rangs (sur des classements)
gapcor.kendall <- cor(gapmind.2007.19[,c(4:6,8:16)], use = "complete.obs", method = "kendall")
round(gapcor.kendall,2) ### notez la diff?rence entre les deux matrices !
gapcor.spearman <- cor(gapmind.2007.19[,c(4:6,8:16)], use = "complete.obs", method = "spearman")
round(gapcor.spearman,2) ### notez la diff?rence entre les deux matrices !

### corrplot admet une matrice de correlation entr?e
corrplot(gapcor, method = "circle") # corrplot admet en entr?e une matrice corr?lation
corrplot(gapcor.kendall, method = "circle") # noter les diff?rences avec la premi?re matrice
corrplot(gapcor.spearman, method = "circle") # noter les diff?rences avec la premi?re matrice

### on peut changer de m?thodes de visualisation:
corrplot(gapcor, method = "pie") # des secteurs... noter les corr?lations tr?s faibles de pop
corrplot(gapcor, method = "color") # des cases color?es 
corrplot(gapcor, method = "number") # le coefficient
corrplot(gapcor, method = "ellipse") # cercle allong? dans la direction de corr?lation
corrplot(gapcor, method = "shade") # la case color?e avec une hachure pour les n?gatives
### ou encore l'implantation
corrplot(gapcor, method = "ellipse", type = "upper") # aussi "lower", defaut = "full"
# changer l'aspect et les couleurs:
col.cor <- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(gapcor, method = "color", col = col.cor) # une ?chelle ? 21 ?chelons
corrplot(gapcor, method = "circle", col = c("black", "white"), bg = "#438787") # noir blanc, fond ciredium !

### l'int?r?t de corrplot est qu'il propose une selection d'options int?ressantes
# l'analyse exploratoire: la plus int?ressante est de r?ordonner les variables de la matrice:
corrplot(gapcor, method = "ellipse", order = "hclust")
corrplot(gapcor, method = "circle", col = col.cor, order = "hclust")

### utilisons des palettes pr?d?finies
library(RColorBrewer) # tr?s pratique, on l'utilisera intensivement dans la partie ggplot !
corrplot(gapcor, method = "ellipse", order = "hclust", col = brewer.pal(n = 8, name = "RdBu"), bg = "#438787")
corrplot(gapcor, method = "ellipse", order = "hclust", 
         col = brewer.pal(n = 8, name = "PuOr"), bg = "#438787", tl.col="black")
### on peut personnaliser fortement l'affichage:
corrplot(gapcor, method = "color", col = NULL,  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Ajout du coefficient de corr?lation
         tl.col = "black", tl.srt = 45, #Rotation des etiquettes de textes
         # Cacher les coefficients de corr?lation sur la diagonale
         diag=FALSE 
) # r?sultat int?ressant mais pas tr?s beau
corrplot(gapcor, method = "ellipse", order = "hclust", tl.srt = 45,
         col = brewer.pal(n = 11, name = "PuOr"), bg = "#438787", tl.col="black",
         title = "Corr?lations des donn?es ONU")
# on peut par contre faire varier l'ordre du trac? des corr?lations.
# m?thode d'?cart angulaire aux valeurs propres:
corrplot(gapcor, method = "ellipse", order = "AOE", tl.srt = 45,
         col = brewer.pal(n = 11, name = "PuOr"), bg = "#438787", tl.col="black",
         title = "Corr?lations des donn?es quantitatives ONU") # remarquez l'agencement diff?rent des corr?lations !
# classement par coordonn?es sur la premi?re composante principale:
corrplot(gapcor, method = "ellipse", order = "FPC", tl.srt = 45,
         col = brewer.pal(n = 11, name = "PuOr"), bg = "#438787", tl.col="black",
         title = "Corr?lations des donn?es quantitatives ONU") # remarquez l'agencement diff?rent des corr?lations !

### on peut aussi varier la m?thode de classification dans hclust:
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "complete",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black",
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en lien complet")

# lien simple
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "single",
         col = brewer.pal(n = 11, name = "PuOr"), bg = "#438787", 
         tl.col = "black", tl.cex = 0.8, tl.srt = 45, 
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en lien simple")

# lien moyen
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "average",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col = "black", tl.cex = 0.8, 
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en lien moyen",
         line = -2) 

# lien centroid
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "centroid",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.8,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI centro?de",
         line = -2) 

# lien m?dian
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "median",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.8,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI m?dian",
         line = -2)

# ward
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "ward",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.8,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en distance de ward", line = -2)
# ward.d
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "ward.D",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.9,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en distance de ward.D", line = -2)

# ward.d2
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "ward.D2",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.9,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en distance de ward.D2", line = -2)

#mcquitty
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "mcquitty",
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"),
         bg = "#438787", tl.col="black", tl.cex = 0.9,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en distance de MacQuitty", line = -2)

#mcquitty rect.col = 2 ou 6 ou 5
corrplot(gapcor, method = "ellipse", order = "hclust", hclust.method = "mcquitty", addrect = 3,
         tl.srt = 45, col = brewer.pal(n = 11, name = "PuOr"), rect.col = 2, rect.lwd = 2,
         bg = "#438787", tl.col="black", tl.cex = 0.9,
         title = "Corr?lations des donn?es ONU, r?ordonn?es par CAHI en distance de MacQuitty", line = -2)
### il existe une version mixte:
corrplot.mixed(gapcor) # plus limit?e et autres options


### ajouter: exercices == faire de m?me sur les matrices de corr?lations de rangs et comparer !
### d?cision ! 



### Commencer en GGplot ----
# installer ggthemes:
install.packages("ggthemes")

(.packages())
### les fondamentaux de ggplot  sur les donn?es gapmind_2007_19
# https://www.datanovia.com/en/fr/lessons/introduction-a-ggplot2/
# 
# appliquer les esth?tiques aux variables:
# boxplot: en x OU y

ggplot(gapmind.2007.19,
       aes(y = GDP_GUSDPPP_2017)) +
  geom_boxplot()


ggplot(gapmind.2007.19,
       aes(y = GDI_2019)) +
  geom_boxplot()

ggplot(gapmind.2007.19,
       aes(x = GDP_GUSDPPP_2017)) +
  geom_boxplot()

ggplot(gapmind.2007.19, aes(x = GDI_2019)) +
  geom_boxplot()

### on peut afficher les points sur un boxplot en cr?ant un facteur fictif:
ggplot(gapmind.2007.19,
       aes(x = factor(1),
           y = GDI_2019)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = 0.1) # jitter = d?placer les points l?g?rement de 10 %

### nuage de points:
ggplot(gapmind.2007.19,
       aes(x = GDP_GUSDPPP_2017,
           y = GDI_2019)) +
  geom_point()
## on peut utiliser des fonctions sur des variables dans l'esth?tique:
ggplot(gapmind.2007.19,
       aes(x = log(GDP_GUSDPPP_2017),
           y = GDI_2019)) +
  geom_point()

ggplot(gapmind.2007.19,
       aes(x = log(GDP_GUSDPPP_2017),
           y = Gender_Inequality_2019)) +
  geom_point()

ggplot(gapmind.2007.19,
       aes(x = log(GNI_2019),
           y = Gender_Inequality_2019)) +
  geom_point() +
  geom_smooth()

### noter la diff?rence de position de l'esth?tique dans les commandes:
# Utilisez ceci
attach(gapmind.2007.19) # attacher le tibble nomm? aux commandes
detach(gapmind.2007.19) # d?tacher le tibble nomm? aux commandes
ggplot(gapmind.2007.19,
       aes(log(gdpPercap), urban_pop_2019)) +
  geom_point() +
  geom_smooth()

# ou ceci
ggplot(gapmind.2007.19) +
  geom_point(aes(log(gdpPercap), urban_pop_2019))

## diff?rence entre couleur et remplissage:
# Couleur
ggplot(gapmind.2007.19, aes(GDI_2019, HDI_Class)) +
  geom_boxplot(aes(color = HDI_Class))
class(gapmind.2007.19$HDI_Class)

# Remplir
ggplot(gapmind.2007.19, aes(HDI_Class, GDI_2019)) +
  geom_boxplot(aes(fill = HDI_Class)) 
# on a juste un petit probl?me de chevauchement des ?tiquettes d'axes.
ggplot(gapmind.2007.19, aes(HDI_Class, GDI_2019)) +
  geom_boxplot(aes(fill = HDI_Class)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# notez comment il vous avertis de l'?viction des NA. 
# On pourrait gagner un peu de place dans l'espace de tra?age en changeant l'angle:

# on peut affecter un objet:
boxplot_hdi_class <- ggplot(gapmind.2007.19,
                            aes(HDI_Class, GDI_2019)) +
  geom_boxplot(aes(fill = HDI_Class)) +
  labs(title = "Indicateur d'inégalité de genre selon l'HDI",
       subtitle = "HDI = indicateur synthétique de développement humain ONU",
       caption = "Source: ONU",
       x = "Niveau HDI",
       y = "Indicateur d'inégalités genré (2019)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
(boxplot_hdi_class)
##sauvegarder l'objet ggplot:
ggsave("boxplot_hdi_class.png", device = "png")
#rm(xplot_hdi_class)

# il faut afficher l'objet:
boxplot_hdi_class # l'objet peut ?tre modifi?, puis enregistr? etc...
str(boxplot_hdi_class) # noter la liste de param?tres ajustables !
class(boxplot_hdi_class)
mode(boxplot_hdi_class)


### changer theme 
#https://ggplot2.tidyverse.org/reference/
theme_set(theme_grey())

boxplot_hdi_class_wrap <- ggplot(gapmind.2007.19,
                            aes(HDI_Class, GDI_2019)) +
  geom_boxplot(aes(fill = HDI_Class)) +
  facet_wrap(vars(continent)) +
  labs(title = "Indicateur d'inégalité de genre selon l'HDI",
       subtitle = "HDI = indicateur synthétique de développement humain ONU",
       caption = "Source: ONU",
       x = "Niveau HDI",
       y = "Indicateur d'inégalités genré (2019)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
(boxplot_hdi_class_wrap)



