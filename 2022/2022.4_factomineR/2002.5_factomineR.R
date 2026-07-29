### ACM et AFM sur données biovallée
glimpse(data.aura)
data(wine)
## faire ACM
data(tea)
data.aura.acm <- data.aura[83:137]
glimpse(data.aura.acm)
summary(data.aura.acm)
## déplacer les variables
data.aura.acm <- relocate(data.aura.acm, Nbphab, .after = Niveau)
data.aura.acm <- relocate(data.aura.acm, Surfhab, .after = Nbphab)
glimpse(data.aura.acm)
## conversion en facteur
## https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
data.aura.acm$Nbphab <- as.integer(data.aura.acm$Nbphab)
data.aura.acm$Surfhab <- as.integer(data.aura.acm$Surfhab)
quant.sup.acm <- data.aura[1:82]
### accolage des deux bases
data.aura.acm <- bind_cols(data.aura.acm, quant.sup.acm)
rm(quant.sup.acm)
glimpse(data.aura.acm)

### on fait l'ACM. 
### actives : 1--40, suppl = 41 -- 53, quanti.sup == 54 -- 137
aura.mca = MCA(data.aura.acm, 
               quali.sup=c(41:53),
               quanti.sup = c(54:137),
               row.w = data.aura.acm$pondmen.cs24.NPERS_84)
summary(aura.mca)
