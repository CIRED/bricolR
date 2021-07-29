### représenter une matrice de corrélations dans ggplot en mosaiques
# https://stackoverflow.com/questions/39136211/title-in-r-corrplot-too-not-centred-and-too-high


# problèmes de titres avec corrplot:
# https://stackoverflow.com/questions/40509217/how-to-have-r-corrplot-title-position-correct
# https://github.com/taiyun/corrplot/issues/10
# changer noms de variables dans corrplot:
# https://github.com/taiyun/corrplot/issues/20

# très éclairant:
# https://stackoverflow.com/questions/41679136/r-corrplot-crops-bottom-axis-label

#### solution corrplot -----
"VADeaths" <-
  structure(c(11.7, 18.1, 26.9, 41, 66, 8.7, 11.7, 20.3, 30.9, 54.3, 15.4, 
              24.3, 37, 54.6, 71.1, 8.4, 13.6, 19.3, 35.1, 50), .Dim = c(5, 4),
            .Dimnames = list(c("50-54", "55-59", "60-64", "65-69", "70-74"),
                             c("Rural Male", "Rural Female", "Urban Male", "Urban Female")))

library(corrplot)
cors = cor(VADeaths)
corrplot(cors,tl.col="black",title="Example Plot",mar=c(0,0,5,0),tl.offset = 1)
### mexte et options mar attention aux options tl. elles s'appliquent aux différents éléments de texte
corrplot(cors,tl.col="black", mar=c(0,0,5,0), tl.offset = 1)
mtext("Example Plot", at=2.5, line=-0.5, cex=2)


#### reshape et mosaique ggplot ----

library(reshape2)
cors <- cor(VADeaths)
cor_data <- reshape2::melt(
  cors, 
  varnames = paste0("demographic", 1:2), 
  value.name = "correlation"
)

Then draw the plot.

library(ggplot2)
ggplot(cor_data, aes(demographic1, demographic2, fill = correlation)) + 
  geom_tile() + 
  ggtitle("Correlation across demographics for VA deaths")

# emploi de tl.offset:
# https://stackoverflow.com/questions/5359619/r-change-size-of-axis-labels-for-corrplot

# un autre stacks:
# https://stackoverflow.com/questions/39029526/how-to-change-the-margins-of-a-correlation-matrix-plot

library(corrplot) 
cor_matrix <- structure(c(1, 0.31596392056465, -0.120092224085334, -0.345097115278159, 
                          0.31596392056465, 1, 0.158912865564527, -0.606426850726639, -0.120092224085334, 
                          0.158912865564527, 1, -0.134795548155303, -0.345097115278159, 
                          -0.606426850726639, -0.134795548155303, 1), .Dim = c(4L, 4L), 
                        .Dimnames = list(NULL, c("var_1", "var_2", "var_3", "var_4")))

corrplot.mixed(cor_matrix, order = "AOE", upper = "ellipse", lower = "number", 
               tl.cex = 2, cl.cex = 2, number.cex = 2)

### réponse possible demandant autre package:
library(corrplot) 
library(scico)

col4 <- scico(100, palette = 'vik') # définition d'une palette par scico
filetag <- "corrplot_result.png"

png(filetag, height = 800, width = 800) # création d'un fichier png !
 
corrplot.mixed(cor_matrix, order = "AOE", upper = "ellipse", lower = "number", 
               upper.col = col4, lower.col = col4,
               tl.cex = 2, cl.cex = 2, number.cex = 2)
dev.off()
# https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet.html
# https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html
