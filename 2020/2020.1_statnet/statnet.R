### script de manipulation des réseaux suite bricole'R d'Améline
library(tidyverse)
library(statnet)
library(statnet.common)
library(GGally)

### documanta package:
citation("statnet")
citation("statnet.common")
citation("tidyverse")
citation("GGally")

###analyse de la base flo
data(florentine)
flobusiness
flomarriage
### donnees flo == c'est une matrice d'adjacence en fait.
data(flo)

gden(flo)
### c'est bien le bon fichier
# https://elementr.hypotheses.org/tag/statnethttps://elementr.hypotheses.org/tag/statnet
# https://github.com/statnet/Workshops/wiki
# https://igraph.org/r/
# tutoriel statnet:
# https://www.researchgate.net/publication/26538959_A_Statnet_Tutorial
# statnet github:
# https://github.com/statnet


nflo <- network(flo, directed = FALSE)
nflo

### sommet 9 == Medici
nflo[9,]
is.adjacent(nflo, 9, 1)
is.adjacent(nflo, 9, 7)
# densité du graphe
gden(nflo)

### statistiques indiquées par Améline:
network.size(nflo)
network.edgecount(nflo)
network.density(nflo)
has.loops(nflo)
is.bipartite(nflo)
is.directed(nflo)
is.hyper(nflo)
is.multiplex(nflo)

### transformer en sociomatrice:
as.sociomatrix(nflo)

### afficher la liste des arêtes:
as.matrix(nflo,matrix.type="edgelist")
plot(nflo, displaylabels=TRUE, boxed.labels=FALSE)
plot(nflo, displaylabels=TRUE, boxed.labels=FALSE, mode="circle")

#The sna package also supports a special kind of matrix called an \sna edgelist." These are three-column matrices, each row of which represents an edge (via its sender, recipient, and value, respectively). These sna edgelists" have special attributes that indicate their size, vertex names (if any), and bipartite status (if applicable).
eflo<-as.edgelist.sna(flo) # Coerce flo to an sna edgelist
eflo
attr(eflo,"n") # How many vertices are there?
attr(eflo,"vnames") # Are there vertex names?
as.sociomatrix.sna(eflo) # Can transform back w/ as.sociomatrix.sna 

#For more information. . .
?as.edgelist.sna
?as.sociomatrix.sna
?attr
?sna

### exploration du graphe

summary(nflo) # Get an overall summary
attr(nflo, "n", "vnames")
network.dyadcount(nflo) # How many dyads in nflo?
network.edgecount(nflo) # How many edges are present?
network.size(nflo) # How large is the network?
as.sociomatrix(nflo) # Show it as a sociomatrix
nflo

plot(nflo,displaylabels=T) # Plot with names
plot(nflo,displaylabels=T,mode="circle") # A less useful layout...

library(sna) # Load the sna library
gplot(nflo) # Requires sna
gplot(relations) # gplot Will work with a matrix object too

### manipulation du graphe
degree(nflo)
table(degree(nflo))
closeness(nflo)
betweenness(nflo)
grecip(nflo)
gtrans(nflo)
dyad.census(nflo)
triad.census(nflo)
geodist(nflo)
g1<-neighborhood(nflo,1)
plot.sociomatrix(g1)
g2<-neighborhood(nflo,2)
plot.sociomatrix(g2)

cutpoints(nflo)

kcores(nflo)

#Retrieving edge values
list.edge.attributes(nflo) # See whats available


### flomarriage est déjà un réserau !
### statistiques indiquées par Améline:
network.size(flomarriage)
network.edgecount(flomarriage)
network.density(flomarriage)
has.loops(flomarriage)
is.bipartite(flomarriage)
is.directed(flomarriage)
is.hyper(flomarriage)
is.multiplex(flomarriage)
degree(flomarriage)
table(degree(flomarriage))
closeness(flomarriage)
betweenness(flomarriage)
grecip(flomarriage)
gtrans(flomarriage)
dyad.census(flomarriage)
triad.census(flomarriage)
geodist(flomarriage)
g1<-neighborhood(flomarriage,1)
plot.sociomatrix(g1)
g2<-neighborhood(flomarriage,2)
plot.sociomatrix(g2)

cutpoints(flomarriage)

kcores(flomarriage)

#Retrieving edge values
list.edge.attributes(flomarriage) # See whats available
### transformer en sociomatrice:
as.sociomatrix(flomarriage)

### afficher la liste des arêtes:
as.matrix(flomarriage,matrix.type="edgelist")

plot(flomarriage, displaylabels=TRUE, boxed.labels=FALSE)
plot(flomarriage, displaylabels=TRUE, boxed.labels=FALSE, mode="circle")

### site briatte: ggnet2
### https://briatte.github.io/ggnet/
ggnet2(flomarriage)
ggnet2(flomarriage, label = TRUE)

### retrait du sommet Pucci
as.matrix(flomarriage,matrix.type="adjacency")
flo12 <- delete.vertices(flomarriage, 12)
ggnet2(flomarriage, label = TRUE)
plot(flo12, displaylabels=T,mode="circle") # A less useful layout..