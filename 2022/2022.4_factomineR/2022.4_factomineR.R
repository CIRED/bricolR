################################################################################
#########  """"script bricole'R 2022.4 factomineR  du 25 juillet 2022  #########
################################################################################

### chargement des paquetages 
library(FactoMineR)
library(factoextra) # indispensable pour les graphiques ggplot2
library(FactoInvestigate )
### chargement des librairies 
library(tidyverse)
library(magrittr) # paquetage n?cessaire pour utiliser le tuyau ou pipe de programmation
library(crosstable)
library(RColorBrewer)
theme_set(theme_bw())
library(ggthemes)
library(ggpubr)
#library(ggcorr)
### mod?le sur les donn?es gapmind 2007 2019 ------
# lancement de paquetages
library(estimatr) # procédures d'estimation optimisées
library(AER) # paquetage d'économétrie complet
library(olsrr) # suite complète d'estimations et tes avec ols 
# https://olsrr.rsquaredacademy.com/index.html
library(mctest) # paquetage complet de diagnostics de colinéarité
### mises en forme améliorées des modèles
library(broom)
library(tidymodels)
library(MASS)
# listage des paquetages
getwd() # afficher le dossier de travail.
(.packages())

# sur les formats C dans les l'affichage R:
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf


### description de la base importée
glimpse(data.aura)
data.aura <- mutate(data.aura, across(83:137, as.factor)) # ça marche avec across
# ici quand on importe, toutes qualitatives sont en integer, on pourrait donc aussi faire:
# mutate_if(is.integer,as.factor)  :=> cf https://stackoverflow.com/questions/33180058/coerce-multiple-columns-to-factors-at-once
# https://willhipson.netlify.app/post/dplyr_across/dplyr_across/


#### ACP AURA avec poids nationaux  == pondmen -------------
ACP.nat <- PCA(data.aura,
               quanti.sup = c(12:82),
               quali.sup = c(83:137),
               row.w = data.aura$pondmen, ncp = 11)
# attention ! ne pas oublier : spécifier la colonne des poids et non pas seulement son index
summary(ACP.nat)
summary.PCA(ACP.nat) # deux commandes identiques
glimpse(ACP.nat$ind)
glimpse(data.aura)
# exportation résultats
write.infile(ACP.nat, "ACP.nat.csv", nb.dec = 3)
### on peut utiliser les graphiques par défaut de factomineR
plot.PCA(ACP.nat, axes = c(1, 2), choix = "var")
plot.PCA(ACP.nat, axes = c(1, 2), choix = "var", invisible = "quanti.sup")
plot.PCA(ACP.nat, axes = c(1, 2), choix = "ind", habillage = 89) # décile
plot.PCA(ACP.nat, axes = c(1, 2), choix = "ind", lim.cos2.var = 0.4) # enlever labels
plot.PCA(ACP.nat, axes = c(1, 2), choix = "ind",
         lim.cos2.var = 0.4,
         label = "none")
plot.PCA(ACP.nat, axes = c(1, 2),
         choix = "ind",
         lim.cos2.var = 0.4,
         label = "none", invisible = "quali") # filtre cos2>=0.4
plot.PCA(ACP.nat, axes = c(1, 2),
         choix = "ind",
         lim.cos2.var = 0.4,
         label = "none",
         invisible = "quali",
         select = "contrib 200",
         unselect = 0.8) 
# on peut définir une couleur personnalisée
ciredium = rgb(67,135,135, max = 255) # petite coquetterie
plot.PCA(ACP.nat, axes = c(1, 2),
         choix = "ind",
         col.ind = rgb(67,135,135, max = 255),
         label = "none", invisible = "quali")
# on va utiliser l'enveloppe ggplot de facto.extra
# http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
# diagramme des valeurs propres
fviz_screeplot(ACP.nat, addlabels = TRUE, ylim = c(0, 25))

# on voit clairement un saut entre deux et trois. 
# Il est possible de changer les titres:
fviz_screeplot(ACP.nat, addlabels = TRUE, ylim = c(0, 25), 
               title = "Profil des valeurs propres",
               hjust = 0.5,
               xlab = "axes",
               ylab = "pourcentage d'inertie") # toutes les commanes ggplot 

# ajoutons des éléments de thème ggplot plus esthétiques:
theme_replace(plot.title = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5))

# calculons quelques indicateurs d'interet sur les valeurs propres.
#  ce sont des indicateurs persos
str(ACP.nat) # un objet très complexe !

(ACP.nat.VP <- ACP.nat$eig)

### quelques découvertes non triviales

str(ACP.nat.VP) # structure plus simple 11 lignes quatre colonnes yc en tetes
class(ACP.nat.VP) # objet de classe matrix array
mode(ACP.nat.VP) # de mode numérique

# on le tibblise:
ACP.nat.VP <- tibble(ACP.nat.VP)
class(ACP.nat.VP)
# notez les noms des variables !!!

# en fait, il vaut mieux faire:
(ACP.nat.VP <- as_tibble(ACP.nat$eig)) # il faut donc mieux le transformer directement en tibble

# moyenne logarithmique des valeurs propres:
# sur les données matricielles:
ACP.nat.VP$LM.VP <- (ACP.nat.VP$eigenvalue - lead(ACP.nat.VP$eigenvalue)) / 
                       log(ACP.nat.VP$eigenvalue/lead(ACP.nat.VP$eigenvalue))
# cela ne peut pas marcher car lead est une commande dplyr == sur df / ou tibble


ACP.nat.VP <- mutate(ACP.nat.VP, LM.VP = (eigenvalue - lead(eigenvalue, n = 1L)) /
                       (log(eigenvalue/lead(eigenvalue, n = 1L))))
(ACP.nat.VP)

# critère de Benzecri:
p <- nrow(ACP.nat.VP)
ACP.nat.VP <- mutate(ACP.nat.VP, BZ = (((p/(p-1)) * (eigenvalue - 1/p))^2))
(ACP.nat.VP)
# ACP.nat.VP$BZ <- ((p/(p-1)) * ((ACP.nat.VP$eigenvalue) - 1/p))^2

# ratio de vraissemblance des pourcentages d'inertie
ACP.nat.VP <- mutate(ACP.nat.VP,
                     log.OR = log((`percentage of variance`/100) /
                                  (1 - (`percentage of variance`/100))))
#ACP.nat.VP$log.OR.pc <- log(ACP.nat.VP[2]/(1-ACP.nat.VP[2]))
(ACP.nat.VP)
# nous obtenons un tibble utilisable pour faire quelques graphiques



### attention aux surprises dans ggplot !!!
# ggplot naif:
ggplot(data = ACP.nat.VP, 
       aes(x = order(as.numeric(rownames(ACP.nat.VP))),
           y = eigenvalue)) +
  geom_col(colour = ciredium, fill = ciredium) +
  scale_color_brewer(palette = "Set1")

### les noms de lignes ne sont pas des nombres...
ggplot(data = ACP.nat.VP, 
       aes(x = as.numeric(rownames(ACP.nat.VP)),
           y = eigenvalue)) +
       geom_col(colour = ciredium, fill = ciredium) +
       scale_color_brewer(palette = "Set1") +
       labs(title = "valeurs propres de l'ACP",
            x = "Axe", y = "valeur propre")
  
(row.names(ACP.nat.VP)) # c'est bien du texte !
class(row.names(ACP.nat.VP))

ggplot(data = ACP.nat.VP,
       aes(x = as.numeric(rownames(ACP.nat.VP)),
           y = eigenvalue),
           xlim(c(1,10))) +
  geom_col(colour = ciredium,
           fill = ciredium,) +
  labs(title = "valeurs propres de l'ACP",
       x = "Axe", y = "valeur propre")

# mais comment afficher tous les axes ?

### compromis final
ggplot(data = ACP.nat.VP, 
       aes(x = as.numeric(rownames(ACP.nat.VP)),
           y = eigenvalue,
           lims(x = c(1,10)))) +
       geom_col(colour = ciredium,
                  fill = ciredium) +
       geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
                     hjust = 0.5,
                      size = 3,
                     vjust = -0.5) +
       labs(title = "Spectre des aleurs propres de l'ACP avec poids nationaux",
                x = "Axe",
                y = "valeur propre")+
  scale_x_continuous(breaks = 1:nrow(ACP.nat.VP)) # afficher tous les axes en x

# visualiser les aides : valeurs propres et moyennes logarithmiques
ggplot(data = ACP.nat.VP, 
       aes(x = eigenvalue,
           y = LM.VP,
           lims(x = c(1,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis logarithmique de l'ACP avec poids nationaux",
       x = "valeurs propres",
       y = "moyennes logarithmiques")+
  scale_x_continuous(breaks = 1:nrow(ACP.nat.VP))

# visualiser le cumul et les moyennes logarithmiques
ggplot(data = ACP.nat.VP, 
       aes(x = `cumulative percentage of variance`,
           y = LM.VP)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis logarithmique de l'ACP avec poids nationaux",
       x = "pourcentage de variance cumulé",
       y = "moyennes logarithmiques") +
  xlim(c(0,100)) # échelle correctement affichée mais pas le titre d'axe

### moyennes logarithmiques et critère de Benzécri
ggplot(data = ACP.nat.VP, 
       aes(x = LM.VP,
           y = BZ,
           lims(x = c(0,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis de Benzécri de l'ACP avec poids nationaux",
       x = "moyenne log des valeurs propres",
       y = "inertie corrigée de Benzécri") +
  scale_x_continuous(breaks = 1:nrow(ACP.nat.VP))

### log vraissemblance et moyennes logarithmiques
ggplot(data = ACP.nat.VP, 
       aes(x = LM.VP,
           y = log.OR,
           lims(y = c(0,-5)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "moyenne log et log ratio des parts d'inertie de l'ACP avec poids nationaux",
       x = "moyenne log des valeurs propres",
       y = "log ratio de probabilités") +
  ylim(-3,-1)


### https://community.rstudio.com/t/displaying-too-many-decimal-points-in-ggplot/78050
# utiliser aussi l'utilitaire ggplot scales: https://scales.r-lib.org/
# https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/
# vjust: -0.5
# https://stackoverflow.com/questions/49718365/adding-data-labels-above-geom-col-chart-with-ggplot2


## descriptions des axes avec dimdesc dans factomineR
(desc.ACP.nat <- dimdesc(ACP.nat, axes = 1:11, proba = 0.01))
desc.ACP.nat
write.infile(desc.ACP.nat, "desc.ACP.nat.csv")

### vous pouvez vous laisser guider par facto investigate
# Investigate ne fonctionne pas: il produit une erreur le tri sur la description des composantes.
Investigate(ACP.nat) 
# malheureusement il plante en cours...

### visualisation des individus ------
# typologie nationale  2017 en quatre groupes
fviz_pca_ind(ACP.nat,
             label = "none", # hide individual labels
             habillage = data.aura$typo.2.4.2017, # typo 2.4 nationale
             addEllipses = TRUE) + # Concentration ellipses)
             scale_color_brewer(palette = "Set1")
  
# typologie nationale  2010 en quatre groupes
fviz_pca_ind(ACP.nat,
             label = "none", # hide individual labels
             habillage = data.aura$typo2010.acp.2017, # typo 2.4 nationale
             addEllipses = TRUE) + # Concentration ellipses)
  scale_color_brewer(palette = "Set1")

# variables socioéconomiques diverses:
# quintiles de niveau de vie
fviz_pca_ind(ACP.nat,
             label = "none", # hide individual labels
             habillage = data.aura$QNIVIE2, # quintiles de niveau de vie
             addEllipses = TRUE) + # Concentration ellipses)
  scale_color_brewer(palette = "Paired")
# type de ménage
fviz_pca_ind(ACP.nat,
             label = "none", # hide individual labels
             habillage = data.aura$TYPMEN5, # types de ménages
             addEllipses = TRUE) + # Concentration ellipses)
  scale_color_brewer(palette = "Paired")
# status d'occupation du logement
fviz_pca_ind(ACP.nat,
             label = "none", # hide individual labels
             habillage = data.aura$stalog4, # types de ménages
             addEllipses = TRUE) + # Concentration ellipses)
  scale_color_brewer(palette = "Paired") +
  scale_fill_discrete(labels = c('Propriétaire occupant',
                                 'Propriétaire accédant',
                                 'Locataire',
                                 'Autres cas')) + 
  labs(title = "Nuage des ménages dans le plan principal",
       subtitle = "coloré selon le statut d'occupation du logement") # corriger
glimpse(data.aura)
### classification sur l'ACP des poids nationaux.
# http://factominer.free.fr/more/HCPC_husson_josse.pdf

# première classification sur l'ACP complète (défaut)
cahi.ACP.nat <- HCPC(ACP.nat)
# il nous trouve trois classes
cahi.ACP.nat.4.cla <- HCPC(ACP.nat, nb.clust = 4)
write.infile(cahi.ACP.nat.4.cla, "classification.ACP.nat.4.classes.csv")
fviz_cluster(cahi.ACP.nat.4.cla,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes")
# trop d'étiquettes == par défaut geom = c("point, "label")
fviz_cluster(cahi.ACP.nat.4.cla,
             geom = "point",
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes") +
             labs(caption = "ACP avec poids nationaux")

# nous voulons juste quatre classes sur deux axes.
data.cahi.ACP.nat.2.axes <- ACP.nat$ind$coord[,1:2] #indexation directe: on a bien deux colonnes
cahi.ACP.nat.2.axes <- HCPC(data.cahi.ACP.nat.2.axes, nb.clust = 4, consol = TRUE)
write.infile(cahi.ACP.nat.2.axes, "classification.ACP.nat.sur.2.axes.csv")
fviz_cluster(cahi.ACP.nat.2.axes,
             geom = "point",
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes sur deux axes") +
             labs(caption = "ACP avec poids nationaux")

### utilisons crosstable pour voir ce qu'il se passe.
# faire quelques crosstables ici
str(cahi.ACP.nat.2.axes)
glimpse(cahi.ACP.nat.2.axes$data.clust) # en fait il ajoute la classification au jeu de données 
# on a donc deux possibilités: soit ajouter de nouvelles colonnes à data.aura
# soit faire les tableaux à partir des dataframes data.clust de chaque objet classification:
class(cahi.ACP.nat.2.axes)# objet HCPC
summary(cahi.ACP.nat.2.axes) 
#data.aura <- bind_cols(data.aura, cahi.ACP.nat.2.axes$data.clust$clust)
#data.aura <- rename(data.aura, "cah.4.2a.wnat" = "clust")

# nous prenons la première solution:
crosstable(cahi.ACP.nat.2.axes$data.clust,
           c(typo), by = clust , total="both") %>%
           as_flextable(keep_id = FALSE)
# maintenant croisons typologie nationale et typo Région Aura avec poids nationaux
crosstable(cahi.ACP.nat.2.axes$data.clust,
           c(typo.2.4.2017, typo2010.acp.2017),
           by = clust ,
           total="both") %>%
          as_flextable(keep_id = FALSE)
# on conserve bien la classification nationale, ce qui n'est pas très étonnant.

# autre possibilité:
# transformer les classes en indicatrices de classe x poids nationaux

# do it !!!


# ajouter les graphiques avec factoextra: 
# https://cran.r-project.org/web/packages/factoextra/readme/README.html

#### classifications sur les données repondérées sur la région Aura. -----
glimpse(data.aura)
#### Etape 1: refaire l'ACP avec les poids Aura.
# https://www.programmingr.com/r-error-messages/r-error-missing-value-where-true-false-needed/
# test sur imputation aura cs_24 Region Aura (84)
data.aura  <-  relocate(data.aura, pondmen.cs24_84, .before = pondmen)
glimpse(data.aura)
## problème de poids nuls générant des données NA
data.aura.pond.NA <- data.aura # je garde les données erronées
# modification des données par réaffectation du poids national aux ménages
# changer les valeurs selon des conditions:
# https://statisticsglobe.com/replace-values-in-data-frame-conditionally-in-r
data.aura$pondmen.cs24_84[data.aura$pondmen.cs24_84 == 0] <- data.aura$pondmen[data.aura$pondmen.cs24_84 == 0]
data.aura$pondmen.cs24.NPERS_84[data.aura$pondmen.cs24.NPERS_84 == 0] <- data.aura$pondmen[data.aura$pondmen.cs24.NPERS_84 == 0]
summary(data.aura[80:82]) # on a bien remplacé par les valeurs de pondmen...

# on teste en relocalisant la table
ACP.aura.cs <- PCA(data.aura%>%filter(pondmen.cs24_84>0),
                   quanti.sup = c(12:82),
                   quali.sup = c(83:137),
                   row.w = (data.aura%>%filter(pondmen.cs24_84>0))[,82])
summary(ACP.aura.cs)

 
# ACP avec imputation de poids nationaux
ACP.aura.cs <- PCA(data.aura,
                   quanti.sup = c(12:82),
                   quali.sup = c(83:137),
                   row.w = data.aura[,81]) 
summary(ACP.aura.cs)
write.infile(ACP.aura.cs, "ACP.aura.cs.csv")
### exploration des valeurs propres de l'ACP 
fviz_screeplot(ACP.aura.cs, addlabels = TRUE, ylim = c(0, 25), 
               title = "Profil des valeurs propres",
               hjust = 0.5,
               xlab = "axes",
               ylab = "pourcentage d'inertie") # toutes les commanes ggplot 

### analyse du spectre de l'ACP avec poids AURA cs_24
(ACP.aura.cs.VP <- as_tibble(ACP.aura.cs$eig)) # il faut donc mieux le transformer directement en tibble

(ACP.aura.cs.VP) # il faut recalculer les indicateurs

# moyenne logarithmique des valeurs propres:
# sur les données matricielles:
ACP.aura.cs.VP$LM.VP <- (ACP.aura.cs.VP$eigenvalue - lead(ACP.aura.cs.VP$eigenvalue)) / 
  log(ACP.aura.cs.VP$eigenvalue/lead(ACP.aura.cs.VP$eigenvalue))
(ACP.aura.cs.VP)

#ACP.nat.VP <- mutate(ACP.nat.VP, LM.VP = (eigenvalue - lead(eigenvalue, n = 1L)) / log(eigenvalue/lead(eigenvalue, n = 1L))))

# critère de Benzecri:
ACP.aura.cs.VP <- mutate(ACP.aura.cs.VP, BZ = (((p/(p-1)) * (eigenvalue - 1/p))^2))
# ACP.nat.VP$BZ <- ((p/(p-1)) * ((ACP.nat.VP$eigenvalue) - 1/p))^2

# ratio de vraissemblance des pourcentages d'inertie
ACP.aura.cs.VP <- mutate(ACP.aura.cs.VP,
                     log.OR = log((`percentage of variance`/100) /
                                    (1 - (`percentage of variance`/100))))
#ACP.nat.VP$log.OR.pc <- log(ACP.nat.VP[2]/(1-ACP.nat.VP[2]))

# ratio de pourcentage cumulé
ACP.aura.cs.VP <- mutate(ACP.aura.cs.VP,
                         log.COR = log((`cumulative percentage of variance`/100) /(1 - (`cumulative percentage of variance`/100))))
# log ratio des valeurs propres 
ACP.aura.cs.VP <- mutate(ACP.aura.cs.VP,
                         log.VP = log(eigenvalue/lead(eigenvalue)))

(ACP.aura.cs.VP)

### graphiques ggplot du spectre des valeurs propres.
### compromis final
ggplot(data = ACP.aura.cs.VP, 
       aes(x = as.numeric(rownames(ACP.aura.cs.VP)),
           y = eigenvalue,
           lims(x = c(1,10)))) +
  geom_col(colour = ciredium,
           fill = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -0.5) +
  labs(title = "Spectre des aleurs propres de l'ACP avec poids csp aura",
       x = "Axe",
       y = "valeur propre") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.VP)) 
# afficher tous les axes en x

# visualiser le cumul et les moyennes logarithmiques
ggplot(data = ACP.aura.cs.VP, 
       aes(x = LM.VP,
           y = eigenvalue)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis logarithmique de l'ACP avec poids csp aura",
       x = "moyennes logarithmiques",
       y = "pourcentage de variance cumulé") +
  xlim(c(0,2)) # échelle correctement affichée mais pas le titre d'axe
# VP-- LM VP
vp.lm.VP.cs <- ggplot(data = ACP.aura.cs.VP, 
       aes(x = LM.VP,
           y = `cumulative percentage of variance`)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis logarithmique de l'ACP avec poids csp aura",
       x = "moyennes logarithmiques",
       y = "pourcentage de variance cumulé") +
  xlim(c(0,2)) # échelle correctement affichée mais pas le titre d'axe 


### moyennes logarithmiques et critère de Benzécri
BZ.lm.VP.cs <- ggplot(data = ACP.aura.cs.VP, 
       aes(x = LM.VP,
           y = BZ,
           lims(x = c(0,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "Eboulis de Benzécri de l'ACP avec poids csp aura",
       x = "moyenne log des valeurs propres",
       y = "inertie corrigée de Benzécri") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.VP))

### log vraissemblance et moyennes logarithmiques
ggplot(data = ACP.aura.cs.VP, 
       aes(x = LM.VP,
           y = log.OR,
           lims(y = c(0,-5)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), #labels en % d'inertie
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "moyenne log et log ratio des parts d'inertie de l'ACP avec poids csp aura",
       x = "moyenne log des valeurs propres",
       y = "log ratio de probabilités") +
  ylim(-3,-1)

### experiments ----
ggplot(data = ACP.aura.cs.VP, 
       aes(x = log.VP,
           y = log.COR)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "log ratio des valeurs propres parts d'inertie cumulées de l'ACP avec poids csp aura",
       x = "log ratio des valeurs propres",
       y = "log ratio de probabilités cumulées")

### log OR vs COR
ggplot(data = ACP.aura.cs.VP, 
       aes(x = log.OR,
           y = log.COR)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "log ratio des parts d'inertie de l'ACP avec poids csp aura",
       x = "log ratio des parts d'inertie",
       y = "log ratio parts d'inertie cumulées")

### VP vs log VP
ggplot(data = ACP.aura.cs.VP, 
       aes(x = eigenvalue,
           y = log.VP)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(title = "valeur propre et leur log",
       x = "valeur propre",
       y = "log valeur propre")
glimpse(ACP.aura.cs.VP)
summary(ACP.aura.cs.VP)
(ACP.aura.cs.VP)

### graphique en facettes des différents diagnostics d'ACP avec ggpubr ----
### ggpubr: composer des objets ggplots arrangés avec ggarrange
### Liste des graphes arranger
# 1. Axes VP
axes.vp.cs <- ggplot(data = ACP.aura.cs.VP, 
                     aes(x = as.numeric(rownames(ACP.aura.cs.VP)),
                         y = eigenvalue,
                         lims(x = c(1,10)))) +
  geom_col(colour = ciredium,
           fill = ciredium) +
  labs(x = "Axe",
       y = "valeur propre") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.VP)) 
# afficher tous les axes en x

# 2. LM.VP et VP  
lmvp.vp.cs <- ggplot(data = ACP.aura.cs.VP, 
                    aes(x = LM.VP,
                        y = eigenvalue,
                        lims(x = c(1,10)))) +
  geom_point(colour = ciredium, fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "moyenne log valeur propre ",
       y = "valeur propre")
# 3. VP % inertie
VP.pc.cs <- ggplot(data = ACP.aura.cs.VP, 
                      aes(x = `percentage of variance`,
                          y = eigenvalue)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "pourcentage d'inertie",
       y = "valeur propre")
(VP.pc.cs)

# 4. Axes %cumulé
axes.pc.cum.cs <- ggplot(data = ACP.aura.cs.VP, 
                     aes(x = as.numeric(rownames(ACP.aura.cs.VP)),
                         y = `cumulative percentage of variance`,
                         lims(x = c(1,10)))) +
  geom_point(colour = ciredium,
           fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), 
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "Axe",
       y = "pourcentage d'inertie cumulée") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.VP))

# 5 LM.VP log OR(VP)
lmvp.pc.cum.cs <- ggplot(data = ACP.aura.cs.VP, 
                         aes(x = LM.VP,
                             y = `cumulative percentage of variance`,
                             lims(x = c(0,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)),
            hjust = 0.5, size = 3, vjust = -1) +
  labs(x = "moyenne log des valeurs propres",
       y = "pourcentage d'inertie")

(lmvp.pc.cum.cs)

# 6 log OR(VP) % inertie
pc.cum.BZ.cs <- ggplot(data = ACP.aura.cs.VP, 
                         aes(x = BZ,
                             y = `cumulative percentage of variance`,
                             lims(y = c(0,100)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)),
            hjust = -0.25, size = 3, vjust = -1) +
  labs(x = "inertie corrigée de Benzécri",
       y = "pourcentage d'inertie cumulée")
(pc.cum.BZ.cs)
  

### composition du graphique final avec ggarrange
aura.cs.grid <- ggarrange(axes.vp.cs, lmvp.vp.cs, VP.pc.cs,
          axes.pc.cum.cs, lmvp.pc.cum.cs, pc.cum.BZ.cs,
          ncol = 3, nrow = 2)
annotate_figure(aura.cs.grid,
                top = text_grob("Analyse du spectre des valeurs propres",
                                color = "red",
                                face = "bold",
                                size = 10),
                bottom = text_grob("ACP avec poids Aura CSP",
                                   color = "blue",
                                   hjust = 1,
                                   x = 1,
                                   face = "italic",
                                   size = 8),
                fig.lab = "Figure 1",
                fig.lab.face = "bold")
(aura.cs.grid) # voir pourquoi le ggpubr n'annote pas le graphique.
# https://twitter.com/datanovia/status/1257701866881851392


### classification sur ACP avec les poids région AURA cs -----
cahi.ACP.aura.cs.4.cla <- HCPC(ACP.aura.cs, nb.clust = 4)
write.infile(cahi.ACP.aura.cs.4.cla, "classification.ACP.aura.cs.4.classes.csv")
fviz_cluster(cahi.ACP.aura.cs.4.cla,
             geom = "point",
             palette = "Dark2",
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes") +
  labs(caption = "ACP avec poids aura CSP")

# nous voulons juste quatre classes sur deux axes
# il faut refaire l'ACP en choisissant ncp == 2
ACP.aura.cs.2a <-  PCA(data.aura,
                       ncp = 2,
                       quanti.sup = c(12:82),
                       quali.sup = c(83:137),
                       row.w = data.aura[,81])
summary(ACP.aura.cs.2a)
# classification 
cahi.ACP.aura.cs.2.axes <- HCPC(ACP.aura.cs.2a, nb.clust = 4)
write.infile(cahi.ACP.aura.cs.2.axes,
             "classification.ACP.aura.cs.sur.2.axes.csv")
fviz_cluster(cahi.ACP.aura.cs.2.axes,
             geom = "point",
             palette = "Dark1",
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes sur deux axes") +
  labs(caption = "ACP avec poids aura CSP")
# excellent: on obtient quatres classes bien séparées

### comparaison des typologies cs 24 AURA 2 et 5 axes avec typos nationales
### typo cinq axes 
# typo 2017.4
crosstable(cahi.ACP.aura.cs.4.cla$data.clust,
           c(typo.2.4.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# typo 2010 sur facteurs de 2017
crosstable(cahi.ACP.aura.cs.4.cla$data.clust,
           c(typo2010.acp.2017), by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# les deux typologies sur le même tableau:
crosstable(cahi.ACP.aura.cs.4.cla$data.clust,
           c(typo.2.4.2017, typo2010.acp.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)


### typo deux axes
# typo 2017.4
crosstable(cahi.ACP.aura.cs.2.axes$data.clust,
           c(typo.2.4.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# typo 2010 sur facteurs de 2017
crosstable(cahi.ACP.aura.cs.2.axes$data.clust,
           c(typo2010.acp.2017), by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# les deux typologies sur le même tableau:
crosstable(cahi.ACP.aura.cs.2.axes$data.clust,
           c(typo.2.4.2017, typo2010.acp.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)

# on voit bien que c'est le nombre d'axes qui compte == pas de différences entre
# partition nationale et partition AURA repondérée

#### ACP avec poids AURA cs x typmen --------------
glimpse(data.aura)
# ACP cinq axes
ACP.aura.cs.typmen <-  PCA(data.aura,
                           quanti.sup = c(12:82),
                           quali.sup = c(83:137),
                           row.w = data.aura[,80])
summary(ACP.aura.cs.typmen)
### analyse détaillée du spectre des VP
### ### analyse du spectre de l'ACP avec poids AURA cs_24
(ACP.aura.cs.typmen.VP <- as_tibble(ACP.aura.cs.typmen$eig)) 
# moyenne logarithmique des valeurs propres:
# sur les données matricielles:
ACP.aura.cs.typmen.VP$LM.VP <- (ACP.aura.cs.typmen.VP$eigenvalue -
                                lead(ACP.aura.cs.typmen.VP$eigenvalue)) / 
  log(ACP.aura.cs.typmen.VP$eigenvalue/lead(ACP.aura.cs.typmen.VP$eigenvalue))
(ACP.aura.cs.typmen.VP)

#ACP.nat.VP <- mutate(ACP.nat.VP, LM.VP = (eigenvalue - lead(eigenvalue, n = 1L)) / log(eigenvalue/lead(eigenvalue, n = 1L))))

# critère de Benzecri:
ACP.aura.cs.typmen.VP <- mutate(ACP.aura.cs.typmen.VP, BZ = (((p/(p-1)) * (eigenvalue - 1/p))^2))
# ACP.nat.VP$BZ <- ((p/(p-1)) * ((ACP.nat.VP$eigenvalue) - 1/p))^2

# ratio de vraissemblance des pourcentages d'inertie
ACP.aura.cs.typmen.VP <- mutate(ACP.aura.cs.typmen.VP,
                         log.OR = log((`percentage of variance`/100) /
                                        (1 - (`percentage of variance`/100))))
#ACP.nat.VP$log.OR.pc <- log(ACP.nat.VP[2]/(1-ACP.nat.VP[2]))

# ratio de pourcentage cumulé
ACP.aura.cs.typmen.VP <- mutate(ACP.aura.cs.typmen.VP,
                         log.COR = log((`cumulative percentage of variance`/100) /(1 - (`cumulative percentage of variance`/100))))
# log ratio des valeurs propres 
ACP.aura.cs.typmen.VP <- mutate(ACP.aura.cs.typmen.VP,
                         log.VP = log(eigenvalue/lead(eigenvalue)))

(ACP.aura.cs.typmen.VP)

### série de six graphiques sur ACP poids AURA cs24 x typmen
# 1. Axes VP
axes.vp.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                     aes(x = as.numeric(rownames(ACP.aura.cs.typmen.VP)),
                         y = eigenvalue,
                         lims(x = c(1,10)))) +
  geom_col(colour = ciredium,
           fill = ciredium) +
  labs(x = "Axe",
       y = "valeur propre") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.typmen.VP)) 
# afficher tous les axes en x
(axes.vp.cs.typ)

# 2. LM.VP et VP  
lmvp.vp.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                     aes(x = LM.VP,
                         y = eigenvalue,
                         lims(x = c(1,10)))) +
  geom_point(colour = ciredium, fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.typmen.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "moyenne log valeur propre ",
       y = "valeur propre")
(lmvp.vp.cs.typ)
# 3. VP % inertie
VP.pc.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                   aes(x = `percentage of variance`,
                       y = eigenvalue)) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%s", rownames(ACP.aura.cs.typmen.VP))),
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "pourcentage d'inertie",
       y = "valeur propre")
(VP.pc.cs.typ)

# 4. Axes %cumulé
axes.pc.cum.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                         aes(x = as.numeric(rownames(ACP.aura.cs.typmen.VP)),
                             y = `cumulative percentage of variance`,
                             lims(x = c(1,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)), 
            hjust = 0.5,
            size = 3,
            vjust = -1) +
  labs(x = "Axe",
       y = "pourcentage d'inertie cumulée") +
  scale_x_continuous(breaks = 1:nrow(ACP.aura.cs.typmen.VP))
(axes.pc.cum.cs.typ)
# 5 LM.VP log OR(VP)
lmvp.pc.cum.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                         aes(x = LM.VP,
                             y = `cumulative percentage of variance`,
                             lims(x = c(0,10)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)),
            hjust = 0.5, size = 3, vjust = -1) +
  labs(x = "moyenne log des valeurs propres",
       y = "pourcentage d'inertie cumulée")

(lmvp.pc.cum.cs.typ)

# 6 log OR(VP) % inertie
pc.cum.BZ.cs.typ <- ggplot(data = ACP.aura.cs.typmen.VP, 
                       aes(x = BZ,
                           y = `cumulative percentage of variance`,
                           lims(y = c(0,100)))) +
  geom_point(colour = ciredium,
             fill = ciredium) +
  geom_line(colour = ciredium) +
  geom_text(aes(label = sprintf("%1.1f%%", `percentage of variance`)),
            hjust = -0.25, size = 3, vjust = -1) +
  labs(x = "inertie corrigée de Benzécri",
       y = "pourcentage d'inertie cumulée")
(pc.cum.BZ.cs.typ)


### composition du graphique final avec ggarrange
aura.cs.typ.grid <- ggarrange(axes.vp.cs.typ, lmvp.vp.cs.typ, VP.pc.cs.typ,
                          axes.pc.cum.cs.typ, lmvp.pc.cum.cs.typ,
                          pc.cum.BZ.cs.typ,
                          ncol = 3, nrow = 2)
annotate_figure(aura.cs.typ.grid,
                top = text_grob("Analyse du spectre des valeurs propres",
                                color = "red",
                                face = "bold",
                                size = 10),
                bottom = text_grob("ACP avec poids Aura CSP",
                                   color = "blue",
                                   hjust = 1,
                                   x = 1,
                                   face = "italic",
                                   size = 8),
                fig.lab = "Figure 1",
                fig.lab.face = "bold")
(aura.cs.typ.grid)

### classification sur facteur d'ACP poids cs24xtypmen
### sur 5 axes
cahi.ACP.aura.cs.typmen.4.cla <- HCPC(ACP.aura.cs.typmen, nb.clust = 4)
write.infile(cahi.ACP.aura.cs.typmen.4.cla, "classification.ACP.aura.cs.4.classes.csv")
fviz_cluster(cahi.ACP.aura.cs.typmen.4.cla,
             geom = "point",
             palette = "Dark2",
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes sur cinq axes") +
  labs(caption = "ACP avec poids aura CSP x typmen")

# nous voulons juste quatre classes sur deux axes
# il faut refaire l'ACP en choisissant ncp == 2
ACP.aura.cs.typmen.2a <-  PCA(data.aura,
                              ncp = 2,
                              quanti.sup = c(12:82),
                              quali.sup = c(83:137),
                              row.w = data.aura[,80])
summary(ACP.aura.cs.typmen.2a)
# classification 
cahi.ACP.aura.cs.typmen.2.axes <- HCPC(ACP.aura.cs.typmen.2a, nb.clust = 4)
write.infile(cahi.ACP.aura.cs.typmen.2.axes,
             "classification.ACP.aura.cs.typmen.2.axes.csv")
fviz_cluster(cahi.ACP.aura.cs.typmen.2.axes,
             geom = "point",
             palette = "Dark2",
             ggtheme = theme_minimal(),
             main = "Partition hiérarchique en quatre classes sur deux axes") +
  labs(caption = "ACP avec poids aura CSP x typmen")
# excellent: on obtient quatres classes bien séparées

### comparaison des typologies cs 24 AURA 2 et 5 axes avec typos nationales
### typo cinq axes 
# typo 2017.4
crosstable(cahi.ACP.aura.cs.typmen.4.cla$data.clust,
           c(typo.2.4.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# typo 2010 sur facteurs de 2017
crosstable(cahi.ACP.aura.cs.typmen.4.cla$data.clust,
           c(typo2010.acp.2017), by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# les deux typologies sur le même tableau:
crosstable(cahi.ACP.aura.cs.typmen.4.cla$data.clust,
           c(typo.2.4.2017, typo2010.acp.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)


### typo deux axes
# typo 2017.4
crosstable(cahi.ACP.aura.cs.typmen.2.axes$data.clust,
           c(typo.2.4.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# typo 2010 sur facteurs de 2017
crosstable(cahi.ACP.aura.cs.typmen.2.axes$data.clust,
           c(typo2010.acp.2017), by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# les deux typologies sur le même tableau:
crosstable(cahi.ACP.aura.cs.typmen.2.axes$data.clust,
           c(typo.2.4.2017, typo2010.acp.2017),
           by = clust ,
           total="both") %>%
  as_flextable(keep_id = FALSE)
# pour améliorer la mise en forme:
# https://danchaltiel.github.io/crosstable/reference/index.html
# https://ardata-fr.github.io/flextable-book/
# voir le livre flextable
 

### classifications sur les variables: visualiser le rôle des variables -----
# testons kmeans avec quatre groupes:
set.seed(123)
km.res <- kmeans(scale(data.aura[1:11]), 4, nstart = 25)
# on peut utiliser factoextra pour visualiser 
fviz_cluster(km.res, 
             data = scale(data.aura[1:11]),
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partition kmeans en quatre classes sur données originales")
# ah il y a trop d'étiquetts ! 
# ce stacks explique comment faire:*
# https://stackoverflow.com/questions/57659922/delete-or-hide-the-data-points-labels

fviz_cluster(km.res, 
             data = scale(data.aura[1:11]),
             geom = "point",
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partition kmeans en quatre classes sur données originales")
# On observe une classe incluse dans 1 et 3 : c'est normal, la partition en 4 est contenue en 3...
 
# détecter le nombre de classes optimal
nbclust.cut <- scale(data.aura[1:11])
fviz_nbclust(nbclust.cut, kmeans, method = "gap_stat")
## sur les facteurs d'ACP:
nbclust.cut <- ACP.nat$ind$coord
fviz_nbclust(nbclust.cut, kmeans, method = "gap_stat") # il en trouve 6 !
km.res <- kmeans(nbclust.cut, 6, nstart = 25)
fviz_cluster(km.res,
             data = nbclust.cut,
             geom = "point",
             palette = "Dark2",
             ggtheme = theme_minimal(),
             main = "Partition kmeans en six classes sur 11 facteurs d'ACP")

#### utiliser la méthode hybride hkmeans: --------
## sur les données brutes:
res.data.hk <- hkmeans(scale(data.aura[1:11]), 4)

# Elements returned by hkmeans()
names(res.data.hk)

# Print the results
res.data.hk

# Visualize the tree
hkmeans_tree(res.data.hk, cex = 0.6)
# or use this
fviz_dend(res.data.hk, cex = 0.6)
# noter l'efficience du premier !!!

# Visualize the hkmeans final clusters
fviz_cluster(res.data.hk,
             geom = "point",
             palette = "Dark2",
             frame.type = "norm",
             frame.level = 0.68,
             ggtheme = theme_bw())
### Il y a clairement un problème avec le kmeans ! Noter l'hétérogénéité de la classe 4 !

### montrer qu'on peut passer par d'autres moyens hors factomineR ! 

# exemple == hclust 
### classification sur les variables en matrice de distance ----
dist.var <- as.dist(sqrt(1 - cor(data.aura[1:11])^2))
(dist.var) # matrice de distance de corrÃ©lation entre variables
clas.var <- hclust(dist.var, method = "ward.D")
options(repr.plot.width = 3, repr.plot.height = 3)
plot(clas.var$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(clas.var)
# avec kmeans:
clas.var.km <- kmeans(dist.var, 3, iter.max = 10, nstart = 1)
str(clas.var.km)
# il faudra construire graphiques plus adaptés.

### on peut utiliser d'autres corrélations:
# hclust est une mÃ©thode de stats
(Spr.spam <- cor(data.aura[1:11], method = "spearman"))
dist.var.Spr <- as.dist(sqrt(1 - cor(data.aura[1:11], method = "spearman")^2))
clas.var.Spr <- hclust(dist.var.Spr, method = "ward.D")
options(repr.plot.width = 3, repr.plot.height = 3)
plot(clas.var.Spr$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(clas.var.Spr)

### Kendall
(dist.var.Ken <- as.dist(sqrt(1 - cor(data.aura[1:11], method = "kendall")^2)))
clas.var.Ken <- hclust(dist.var.Ken, method = "ward.D")
options(repr.plot.width = 3, repr.plot.height = 3)
plot(clas.var.Ken$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(clas.var.Ken)

### calculer la similaritÃ© cosinus avec coop ----
library(coop)
cos.dist <- as.dist(1-cosine(data.aura[1:11]))
(cos.dist)
class.cos.dist <- hclust(cos.dist, method = "ward.D") 
options(repr.plot.width = 3, repr.plot.height = 3)
plot(class.cos.dist$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(class.cos.dist)

### Orloci chord distance
orl.dist <- as.dist(sqrt(2*(1-cosine(data.aura[1:11]))))
(orl.dist)
class.orl.dist <- hclust(orl.dist, method = "ward.D") # hclust admet une matrice de distance en entrÃ©e.
options(repr.plot.width = 3, repr.plot.height = 3)
plot(class.orl.dist$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(class.orl.dist)

### similaritÃ© des cosinus carrÃ©s
cos.sim <- as.dist(cosine(data.aura[1:11]))
(cos.sim)
# classification sur matrice de similaritÃ© cosinus
class.cos.sim <- hclust(cos.sim, method = "ward.D")
options(repr.plot.width = 3, repr.plot.height = 3)
plot(class.cos.sim$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(class.cos.sim)
# incorrect mais intÃ©ressant (surprenant !?)

### semi-mÃ©trique angulaire == arccos(cos(x))
ang.semim <- as.dist(acos(cosine(data.aura[1:11])))
(ang.semim)
class.ang.semim <- hclust(ang.semim, method = "ward.D")
options(repr.plot.width = 3, repr.plot.height = 3)
plot(class.ang.semim$height)
options(repr.plot.width = 8, repr.plot.height = 5)
plot(class.ang.semim)
## rÃ©sultat correct
str(ang.semim)
str(class.ang.semim)
### comparer les diffÃ©rentes partitions. PossibilitÃ©s:
### https://www.rdocumentation.org/packages/clusterSim/versions/0.49-2/topics/comparing.Partitions
### https://www.statmethods.net/advstats/cluster.html


# testons sur les variables avec hcut de factoextra:
res <- hcut(ACP.nat$ind$coord, k = 4, stand = TRUE)

# Visualize
fviz_dend(res,
          show_labels = FALSE,
          rect = TRUE,
          cex = 0.5,
          k_colors = "Dark2")

# C'est très long et on ne peut pas choisir l'indice de niveau où l'on coupe
# sur les variables:
res <- hcut(dist.var, k = 3, stand = TRUE)
# Visualize
fviz_dend(res,
          show_labels = TRUE,
          rect = TRUE,
          cex = 0.5,
          k_colors = "Dark2")
# l'aventage est ici de pouvoir utiliser la couleur pour dessiner les groupes 
# sur le dendrogramme.
 
# Spectral avec relaxation de Shi & Malik (2004)
# il faut une matrice de similarité == coorrélation ou distances inverses
 
# 
