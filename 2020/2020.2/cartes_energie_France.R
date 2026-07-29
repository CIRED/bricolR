################################################################################
#              script de cartographie sous R                                   #
#              composé via                                                     #
#              https://www.sylvaindurand.fr/cartographie-avec-R/               #
#              et:                                                             #
#        gpclibPermit()      http://wukan.ums-riate.fr/r2016/                  #
#              https://neocarto.hypotheses.org/1859                            #
#              https://www.r-bloggers.com/shapefiles-in-r/                     #
#      https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/           #
################################################################################

### données open street map fonds communaux:
### https://www.data.gouv.fr/fr/datasets/donnees-openstreetmap-integrales-de-france-metropolitaine/
### voir fichiers dans cartes !
### cartographie métropole
### https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/


### liens carto utiles
# http://eductice.ens-lyon.fr/EducTice/recherche/geomatique/veille/sig/Jeux-de-donnees/
### transformer openstreemap en shp:
#https://nmoyroud.teledetection.fr/index.php/geomatique/9-openstreetmap/20-extraire-et-transformer-des-donnees-openstreetmap-au-format-shapefile


### augmenter mémoire dans R:
# https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# Attention !  
library(rgdal)
library(sp)
library(sf)
library(cartography)
library(maptools)
# nb: cartography installe spData, rgdal, classint, sf en standard.
library(maptools)
library(plotrix)
library(classInt)
library(ggplot2)
library(ggthemes)
### optimisation du code
library(microbenchmark)
### importation du fichier communes avec rgdal mais cela ne marche pas
#communes <- readOGR(dsn="c:/cartes/communes", layer="GEOFLAlimitesdescommunesenFran")
#communes <- readOGR("c:/cartes/communes", "GEOFLAlimitesdescommunesenFran")

# voir avec le tuto rblogger
# https://www.r-bloggers.com/shapefiles-in-r/ 

###import SHP communal
communes <- readOGR(dsn = path.expand("C:/cartes/communes"), layer = 'communes-20181110', stringsAsFactors = FALSE)
ogrInfo("C:/cartes/communes") # il lit le fichier mais ne trouve pas de motifs...
class(communes)
plot(communes)
### execution ok ! cela marche !
proj4string(communes)
coordinates(communes)
data.frame(communes)
View(communes)
# Get a bounding box interactively
loc <- locator(n = 2)
#rm(loc)
### coordonnées renvoyées par locator au premier essai:
#$`x`
#[1] -6.82566 10.09251
#$y
#[1] 52.04254 41.76941
### ATTENTION ! l'ordre et la localisation des sélections importe !!!
# il faut sélectionner dans l'ordre:
# (1) le point inférieur gauche, (2) le point supérieur droit, sinon
# l'image renvoyée est inversée !!!
# exemple commenté ici: 
# https://rgeomatic.hypotheses.org/1288

# on peut ensuite afficher la carte dans la boite sélectionnée:
plot(communes, xlim = loc[[1]], ylim = loc[[2]])
# encore un peu petit mais on va recommencer:
loc.2 <- locator(n = 2)
# maintenant, on redessine dans le carré défini par loc.2
plot(communes, xlim = loc.2[[1]], ylim = loc.2[[2]])

### valeur utiles trouvées dans le texte de Coulmont
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51))

# première carte tracée. voir Coulmont p.10-11
summary(communes)
library(RColorBrewer)
jointure <- match(communes$insee, combus_RP_2015$insee)
TRP_elec <- combus_RP_2015$TRP_Elec
nclr <- 5
plotclr <- brewer.pal(nclr, "PuOr")
plotclr <- plotclr[nclr:1] # réordonne les couleurs
classe <- classIntervals(TRP_elec, nclr, style = "equal")
colcode <- findColours(classe, plotclr)
par(fg = NA)
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
### enfin ! voir plot commande, notamment 'colors specifications' pour les différents modes d'appel des couleurs.
### ATTENTION ! en aucun CAS la valeur NA est équivalente à "transparent"
#https://stackoverflow.com/questions/28821901/remove-country-borders-in-mapsmapworld-in-r
#  plot(world, col="gray", border="gray", bg="black") # voir aussi pour les polygones:
#https://gis.stackexchange.com/questions/36877/how-do-i-change-the-polygon-fill-color-and-border-color-for-spatialpolygons-obje
#locator(n = 1) # choix du point de la légende
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex = 0.6, bty = "n", text.col = "black")
#??? ça marche !!! voir les paramètres de legend:
# https://www.rdocumentation.org/packages/graphics/versions/3.5.1/topics/legend
# IL FAUDRA AJUSTER LE FORMAT des nombres et ajouter titres etc..
# https://stackoverflow.com/questions/36797375/adjust-the-font-of-legend-in-r



#### 1. total des RP -----
### TRES IMPORTANT: dans les cartographies construites avec classIntervals
### ce qui commande le nombre de décimales affichées ce n'est pas le paramètre
### sprintf de legend() MAIS le paramètre dataPrecision = k qui donne la précision
### demandée pour affichage des classes.
### intervalles fixées des cartes
fix_classe <- classIntervals(TRP_elec, nclr, style = "fixed", fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00))
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex = 0.6, bty = "n", text.col = "black")

### intervalles quantiles des cartes
quant_classe <- classIntervals(TRP_elec, nclr, style = "quantile", dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

### intervalles en style "pretty"
pretty_classe <- classIntervals(TRP_elec, nclr, style = "pretty", dataPrecision = 2)
colcode <- findColours(pretty_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

### intervalles en style "sd"
sd_classe <- classIntervals(TRP_elec, nclr, style = "sd", dataPrecision = 2)
colcode <- findColours(sd_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")
# une carte pas inintéressante mais pas enregistrée pour nos besoins.
 
### intervalles en style "jenks"
jenks_classe <- classIntervals(TRP_elec, nclr, style = "jenks", dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

### 2. RP au gaz de ville ==
### définition de la variable à représenter:
View(combus_RP_2015)
TRP_Gaz <- combus_RP_2015$TRP_Gaz
## Etendues égales
equal_classe <- classIntervals(TRP_Gaz, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode, 
     main = "Proportion de RP au gaz en 2015, étendues égales")
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TRP_Gaz,
                               nclr,
                               style = "fixed", 
                               fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                               dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TRP_Gaz,
                             nclr,
                             style = "quantile", 
                             dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TRP_Gaz,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

### 3. RP au produits pétroliers ==
### définition de la variable à représenter:
TRP_oil <- combus_RP_2015$TRP_Petrole
## Etendues égales
equal_classe <- classIntervals(TRP_oil, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TRP_oil,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TRP_oil,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TRP_oil,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 0.6, bty = "n", 
       text.col = "black")


### 5. RP au autres combustibles ==
### définition de la variable à représenter:
TRP_autre <- combus_RP_2015$TRP_Autre
summary(TRP_autre)
## Etendues égales
equal_classe <- classIntervals(TRP_autre, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TRP_autre,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.10, 0.20, 0.30, 0.40, 1.0),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TRP_autre,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TRP_autre,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")





### 2. Maisons individuelles ------
### intervalles égaux
TMI_elec <- combus_RP_2015$TMI_Elec
summary(TMI_elec)

### intervalles égaux
equal_classe <- classIntervals(TMI_elec, nclr, style = "equal")
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex = 1.0, bty = "n", text.col = "black")


### intervalles fixées des cartes
fix_classe <- classIntervals(TMI_elec, nclr, style = "fixed", fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00))
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex = 1.0, bty = "n", text.col = "black")

### intervalles quantiles des cartes
quant_classe <- classIntervals(TMI_elec, nclr, style = "quantile", dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### intervalles en style "pretty"
pretty_classe <- classIntervals(TMI_elec, nclr, style = "pretty", dataPrecision = 2)
colcode <- findColours(pretty_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### intervalles en style "sd"
sd_classe <- classIntervals(TMI_elec, nclr, style = "sd", dataPrecision = 2)
colcode <- findColours(sd_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")
# une carte pas inintéressante mais pas enregistrée pour nos besoins.

### intervalles en style "jenks"
jenks_classe <- classIntervals(TMI_elec, nclr, style = "jenks", dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 2. MI au gaz de ville ==
### définition de la variable à représenter:
View(combus_MI_2015)
TMI_Gaz <- combus_RP_2015$TMI_Gaz
summary(TMI_Gaz)
## Etendues égales
equal_classe <- classIntervals(TMI_Gaz, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TMI_Gaz,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TMI_Gaz,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TMI_Gaz,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 3. MI au produits pétroliers ==
### définition de la variable à représenter:
TMI_oil <- combus_RP_2015$TMI_Petrole
summary(TMI_oil)
## Etendues égales
equal_classe <- classIntervals(TMI_oil, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TMI_oil,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TMI_oil,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TMI_oil,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 4. MI au GPL ==
### définition de la variable à représenter:
TMI_gpl <- combus_RP_2015$TMI_GPL
## Etendues égales
equal_classe <- classIntervals(TMI_gpl, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TMI_gpl,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TMI_gpl,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TMI_gpl,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")



### 5. MI au autres combustibles ==
### définition de la variable à représenter:
TMI_autre <- combus_RP_2015$TMI_Autre
summary(TMI_autre)
## Etendues égales
equal_classe <- classIntervals(TMI_autre, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TMI_autre,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.10, 0.20, 0.30, 0.40, 1.0),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TMI_autre,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TMI_autre,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

###
### 3. Appartements ------
### intervalles égaux
library(microbenchmark)
microbenchmark(TAP_elec <- combus_RP_2015$TAP_Elec,
summary(TAP_elec))

### intervalles égaux
microbenchmark(times = 10L, equal_classe <- classIntervals(TAP_elec, nclr, style = "equal"),
colcode <- findColours(equal_classe, plotclr),
par(fg = NA),
plot.new(),
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode),
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),, 
       fill = attr(colcode, "palette"), cex = 1.0, bty = "n", text.col = "black"))


### intervalles fixées des cartes
fix_classe <- classIntervals(TAP_elec, nclr, style = "fixed", fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00))
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex = 1.0, bty = "n", text.col = "black")

### intervalles quantiles des cartes
quant_classe <- classIntervals(TAP_elec, nclr, style = "quantile", dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### intervalles en style "pretty"
pretty_classe <- classIntervals(TAP_elec, nclr, style = "pretty", dataPrecision = 2)
colcode <- findColours(pretty_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### intervalles en style "sd"
sd_classe <- classIntervals(TAP_elec, nclr, style = "sd", dataPrecision = 2)
colcode <- findColours(sd_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")
# une carte pas inintéressante mais pas enregistrée pour nos besoins.

### intervalles en style "jenks"
jenks_classe <- classIntervals(TAP_elec, nclr, style = "jenks", dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 2. AP au gaz de ville ==
### définition de la variable à représenter:
View(combus_AP_2015)
TAP_Gaz <- combus_RP_2015$TAP_Gaz
summary(TAP_Gaz)
## Etendues égales
equal_classe <- classIntervals(TAP_Gaz, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TAP_Gaz,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TAP_Gaz,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TAP_Gaz,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 3. AP au produits pétroliers ==
### définition de la variable à représenter:
TAP_oil <- combus_RP_2015$TAP_Petrole
summary(TAP_oil)
## Etendues égales
equal_classe <- classIntervals(TAP_oil, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TAP_oil,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TAP_oil,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TAP_oil,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

### 4. AP au GPL ==
### définition de la variable à représenter:
TAP_gpl <- combus_RP_2015$TAP_GPL
summary(TAP_gpl)
## Etendues égales
equal_classe <- classIntervals(TAP_gpl, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TAP_gpl,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1.00),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TAP_gpl,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TAP_gpl,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")



### 5. AP au autres combustibles ==
### définition de la variable à représenter:
TAP_autre <- combus_RP_2015$TAP_Autre
summary(TAP_autre)
## Etendues égales
equal_classe <- classIntervals(TAP_autre, nclr, style = "equal", dataPrecision = 2)
colcode <- findColours(equal_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues fixes
fix_classe <- classIntervals(TAP_autre,
                             nclr,
                             style = "fixed", 
                             fixedBreaks = c(0, 0.10, 0.20, 0.30, 0.40, 1.0),
                             dataPrecision = 2)
colcode <- findColours(fix_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues quantiles
quant_classe <- classIntervals(TAP_autre,
                               nclr,
                               style = "quantile", 
                               dataPrecision = 2)
colcode <- findColours(quant_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

## Etendues Jenks
jenks_classe <- classIntervals(TAP_autre,
                               nclr,
                               style = "jenks", 
                               dataPrecision = 2)
colcode <- findColours(jenks_classe, plotclr)
par(fg = NA)
plot.new()
plot(communes, xlim = c(1, 4), ylim = c(41.5, 51), col = colcode)
legend(-5.8894046, 45.9460684, legend = names(attr(colcode,"table")),
       fill = attr(colcode, "palette"), sprintf("%.2f"), cex = 1.0, bty = "n", 
       text.col = "black")

memory.size(max = FALSE)
memory.size(max = TRUE)
memory.limit(size = NA)


## etudier command sprintf (venue du C) : mais pas nécessaire dans ce contexte !
sprintf("%.0f%% said yes (out of a sample of size %.0f)", 66.666, 3)
sprintf("%.3f", pi)
##??? après conversion des coordonnées L 93 en WSG 84 via:
#¬ http://geofree.fr/gf/coordinateconv.asp#listSys
# cf commande plot.MAP : ol = couleur des bordures defaut = "black", c:





### données INSEE RGP logement:
# https://www.insee.fr/fr/statistiques/3561683?sommaire=3561690#consulter
# tables RPRINC9 et 10.

# autres manips intéressantes:
# https://abcdr.thinkr.fr/comment-lire-le-contenu-dun-shapefile-avec-r/

#A partir du tutoriel:
#https://neocarto.hypotheses.org/1859

# autre tuto employant sf et rgdal
## https://abcdr.thinkr.fr/comment-lire-le-contenu-dun-shapefile-avec-r/

# superbe carte suisse avec ggplot:
# Ihttps://rgeomatic.hypotheses.org/1086

### on va essaye de tracer la première carte:

# Échelle de couleurs
col <- findColours(classIntervals(
  communes$data$surf_ha, 100, style="quantile"),
  smoothColors("#0C3269",98,"white"))
# Légende
leg <- findColours(classIntervals(
  round(communes$data$surf_ha), 5, style="quantile"),
  smoothColors("#0C3269",3,"white"),
  under="moins de", over="plus de", between="-",
  cutlabels=FALSE)
summary(communes$data$surf_ha)


#bien regarder ce tuto assez complet
#https://mgimond.github.io/Spatial/data-manipulation-in-r.html#
