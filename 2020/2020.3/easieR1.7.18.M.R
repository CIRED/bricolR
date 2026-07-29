################################
####                        ####
####          easieR        ####
####                        ####
################################

### starting options


### easieR

writeLines(c("easieR écrit par Nicolas Stefaniak", "Version 1.7.18, modifiée le 27/09/2017", "Tapez 'easieR()' dans la console pour utiliser ce package"))
writeLines("Certaines opérations sont longues à effectuer, il faut juste attendre. Soyez patient-e !")


easieR <- function(info=TRUE){
  # 1. l'argument info permettra a terme de choisir les informations qui s'affichent dans la console ou non 
  options (warn=1)
  options(scipen=999)
  require(tcltk)
  
  # 2. installer les packages nécessaires et MAJ des packages installés
  # 2a. packages à installer, par ordre alphabétique
  pack.to.inst <- c("afex", "akima",  "Amelia", "asbio","BayesFactor", "bibtex","car", "cobs", "corpcor", "DAAG","deldir", "DescTools","devtools", "doBy","dplyr", "epitools", "foreign", 
                    "ggplot2", "gmodels", "GPArotation", "gsl", "lars", "lsr", "MBESS", "mc2d", "mgcv", "mlogit", "nFactors", "nortest", 
                    "outliers", "pgirmess", "phia", "pkgmaker", "plyr", "ppcor", "psych", "pwr", "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", 
                    "reshape2", "rms", "robust", "robustbase", "rtf", "rrcov", "scatterplot3d","semPlot", "sos", "sp", "stringi", "stringr", "svDialogs", "TeachingDemos",
                    "trimcluster", "wle", "WRS2")
  
  # 2b. packages manquants
  pack.uninst <- pack.to.inst[!(pack.to.inst %in% rownames(installed.packages()))]
  
  # 2c. installer packages manquants si nécessaires et si utilisateur le souhaite
  if(length(pack.uninst)>0){
    inst <- menu(choices=c("oui","non"), graphics=TRUE, title="Voulez-vous installer les packages manquants ?")
    if(length(inst)==0 || inst==2){
      tk_messageBox(type="ok", caption="Attention", message="Vous avez choisi de ne pas installer les packages manquants, cela peut gêner l'exécution de certaines fonctions. Relancez easieR() si vous souhaitez installer les packages.")
    } else {
      writeLines("Installation des packages")
      print(pack.uninst)
      flush.console()
      ## install devtools if necessary
      install.packages('devtools')
      ## Load devtools package for install_github()
      library(devtools)
      ## get BayesFactorExtras from github
      try(install_github("richarddmorey/BayesFactorExtras", subdir="BayesFactorExtras"), silent=T)
      install.packages(pack.uninst, quiet=TRUE)
      #WRS is a special case because it is not on CRAN
      if (!("WRS" %in% rownames(installed.packages()))) {
        # third: install an additional package which provides some C functions
        library("devtools")
        try(install_github("nicebread/WRS", subdir="pkg"),silent=T)
      }
    }
  } 
  flush.console()
  
  
  # 3. choix du groupe de fonctions
  require(svDialogs)
  
  choix <- dlgList(c("Données - Importation, tri, sélection, prétraitements", "Analyses - Tests d'hypothèse", "Interface - objets en mémoire, nettoyer la mémoire, répertoire de travail"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
  if(length(choix)==0) writeLines("Vous avez quitté easieR") else {
    if(choix=="Données - Importation, tri, sélection, prétraitements") Resultats <- donnees()
    if(choix=="Analyses - Tests d'hypothèse") Resultats <-analyse()
    if(choix=="Interface - objets en mémoire, nettoyer la mémoire, répertoire de travail") Resultats <- interfaceR()
    return(Resultats)
  }
}


#############################################
####                                     ####
####           pédagogie                 ####
####                                     ####
#############################################



teaching<-function(){
  tcl<-function(){
    clt.examp(1)
    msgBox("êtes-vous prêt?")
    for(i in 1:50){
      clt.examp(i*2)
      Sys.sleep(1)
    }
  }
  
  c("psych", "svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  choix <- dlgList(c("Comprendre un intervalle de confiance", "Comprendre alpha et la puissance",
                     "Comprendre la corrélation",
                     "Comprendre le theorem central limit","Comprendre une corrélation 2",
                     "Comprendre la prévalence, la sensibilité et la spécificité",
                     "Comprendre la prévalence, la sensibilité et la spécificité 2",
                     "Comprendre le pouvoir prédictif positif et le pouvoir prédictif négatif",
                     "Comprendre une inférence bayesienne",
                     "Comprendre le maximum de vraisemblance",
                     "Comprendre les effets de variances hétérogènes"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
  if(length(choix)==0) return(easieR())
  
  switch(choix, 
         "Comprendre un intervalle de confiance"=ci.examp(), # peut être compléter par des arguments
         "Comprendre le theorem central limit"=tcl(),
         "Comprendre la prévalence, la sensibilité et la spécificité"= plotFagan2(),
         "Comprendre une inférence bayesienne"=plotFagan(),
         "Comprendre le maximum de vraisemblance"=mle.demo() , #des arguments peuvent être utilisés
         "Comprendre alpha et la puissance"=run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE), 
         "Comprendre la corrélation" = put.points.demo(),
         "Comprendre les effets de variances hétérogènes"={
           writeLines("Avec deux moyennes égales, ou pratiquement égales, le taux d'erreurs doit être de 5%.
                      Modifiez progressivement l'écart entre les écart-types et voyez comment le taux d'erreur alpha va être modifié")
           run.Pvalue.norm.sim()
         },
         "Comprendre la prévalence, la sensibilité et la spécificité 2"= roc.demo(),
         "Comprendre une corrélation 2"=run.cor2.examp(),
         "Comprendre le pouvoir prédictif positif et le pouvoir prédictif négatif"= {
           for(i in seq(1,11,2)) {
             SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i) # on peut modifier sensibilité et spécificité
             if( interactive() ) {
               readline("Press Enter to continue")  
             }
             
             
           }
         }
         
         
  )
  ref1(packages)->Resultats
  return(Resultats)
  
         }

################################
####                        ####
####    interface de R      ####
####                        ####
################################

### interface de R (rÃÂ©pertoire, donnÃÂ©es en mÃÂ©moire, connaÃÂ®tre les fonctions qui permettent de rÃÂ©aliser une analyse particuliÃÂ¨re)
# il manque la possibilitÃÂ© de choisir en fonction de la nature des objets qu'on veut voir dans ls()
vef.pack<-function(){
    pack.to.inst <- c("afex", "akima",  "Amelia", "asbio","BayesFactor", "bibtex","car", "cobs", "corpcor", "DAAG","deldir", "DescTools","devtools", "doBy","dplyr", "epitools", "foreign", 
                      "ggplot2", "gmodels", "GPArotation", "gsl", "lars", "lsr", "MBESS", "mc2d","mgcv", "mlogit", "nFactors", "nortest", 
                      "outliers", "pgirmess", "phia", "pkgmaker", "plyr", "ppcor", "psych", "pwr", "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", 
                      "reshape2", "rms", "robust", "robustbase", "rtf", "rrcov", "scatterplot3d","semPlot", "sos", "sp", "stringi", "stringr", "svDialogs", "TeachingDemos",
                      "trimcluster", "wle", "WRS","WRS2")

          list()->Resultats
    Resultats$packages.installés.correctement<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==TRUE) ]
    Resultats$Package.mal.installés<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==FALSE) ]
    return(Resultats)
}

interfaceR<-function(){
  options (warn=-1) 
  packages <- c("svDialogs","pkgmaker")
  lapply(packages, require,character.only=T)
  Resultats <- list()
  write.pkgbib(packages, file='references')
  
  
  choix <- dlgList(c("obtenir le répertoire de travail","spécifier le répertoire de travail", "Suppression d objet en mémoire", 
                     "liste des objets en mémoire", "rechercher une nouvelle fonction", "mise à jour des packages","Vérifier l installation des packages"), preselect=NULL, multiple = FALSE, title="Quel est votre choix ?")$res
  while(length(choix)==0) return(easieR())
  
  switch(choix, 
         "obtenir le répertoire de travail" = Resultats$"Répertoire de travail" <- getwd(),
         "liste des objets en mémoire"= Resultats$"Objets en mémoire" <- ls(envir=.GlobalEnv),
         "spécifier le répertoire de travail"={
           repertoire <- dlgDir(title="Veuillez choisir le répertoire de travail")$res
           if(length(repertoire)==0) repertoire <- getwd()
           setwd(repertoire)
           Resultats$"nouveau répertoire" <- paste("Le répertoire de travail est à présent", repertoire)
         },
         "Suppression d objet en mémoire"={
           ls(envir=.GlobalEnv)->tout
           Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
           tout[!is.element(tout,fonctions)]->tout
           X<-dlgList(tout, multiple = TRUE, title="Objets à supprimer")$res
           if(length(X)==0) return(easieR())
           rm(list=X, envir=.GlobalEnv)
           Resultats <- list()
           Resultats$"Liste des objects encore en mémoire de R" <- ls(envir=.GlobalEnv)
         },
         "rechercher une nouvelle fonction"={
           require(sos)
           writeLines("Pour trouver une nouvelle analyse, il est nécessaire de faire votre recherche en anglais. Vous pouvez utiliser plusieurs mots dans la recherche.
Une page html reprenant l'ensemble des packages faisant référence à l'analyse recherchée va s'ouvrir.")
           critere <- dlgInput("Quelle analyse recherchez vous ?", "Tapez votre recherche ici")$res
           if(length(critere)==0) return(easieR())
           critere <- strsplit(critere, ":")
           critere <- tail(critere[[1]],n=1)
           Resultats<- findFn(critere)
           return(Resultats)
         },
         "mise à jour des packages"= {update.packages(ask=FALSE)},
         "Vérifier l installation des packages"=vef.pack()->Resultats$"Vérification des packages")
  bibtex::read.bib('references.bib')->Resultats$"Références des packages utilisés"
  file.remove('references.bib')
  
  return(Resultats)
}
ez.install<-function(){
  require(tcltk)
  
  # # 2. installer les packages nécessaires et MAJ des packages installés
  # # 2a. packages à installer, par ordre alphabétique
  pack.to.inst <- c("afex", "akima",  "Amelia", "asbio","BayesFactor", "bibtex","car", "cobs", "corpcor", "DAAG","deldir", "DescTools","devtools", "doBy","dplyr", "epitools", "foreign", 
                    "ggplot2", "gmodels", "GPArotation", "gsl", "lars", "lsr", "MBESS", "mc2d", "mgcv","mlogit", "nFactors", "nortest", 
                    "outliers", "pgirmess", "phia", "pkgmaker", "plyr", "ppcor", "psych", "pwr", "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", 
                    "reshape2", "rms", "robust", "robustbase", "rtf", "rrcov", "scatterplot3d","semPlot", "sos", "sp", "stringi", "stringr", "svDialogs", "TeachingDemos",
                    "trimcluster", "wle", "WRS","WRS2")
  
  # 2b. packages manquants
  pack.uninst <- pack.to.inst[!(pack.to.inst %in% rownames(installed.packages()))]
  
  # 2c. installer packages manquants si nécessaires et si utilisateur le souhaite
  if(length(pack.uninst)>0){
    inst <- menu(choices=c("oui","non"), graphics=TRUE, title="Voulez-vous installer les packages manquants ?")
    if(length(inst)==0 || inst==2){
      tk_messageBox(type="ok", caption="Attention", message="Vous avez choisi de ne pas installer les packages manquants, cela peut gêner l'exécution de certaines fonctions. Relancez easieR() si vous souhaitez installer les packages.")
    } else {
      writeLines("Installation des packages")
      print(pack.uninst)
      flush.console()
      install.packages(pack.uninst, quiet=TRUE)
      #WRS is a special case because it is not on CRAN
      if (!("WRS" %in% rownames(installed.packages()))) {
        # third: install an additional package which provides some C functions
        library("devtools")
        install_github("nicebread/WRS", subdir="pkg")
      }
    }
  } 
  flush.console()
  vef.pack()->Resultats
  return(Resultats)
  
}

################################
####                        ####
####        Donnees         ####
####                        ####
################################

### Donnees

#### function qui permet centrer / centrer rÃÂ©duire 
Centrer.red<-function(x, data=NULL, info=TRUE){options (warn=-1) 
  packages<-c("svDialogs")
  #faire l analyse par groupe # regler le probleme des noms
  list()->Resultats
  X<-"autres données" 
  while(any(X=="autres données")){nom <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  if(info==TRUE) {print(("veuillez choisir la base de données"))}
  nom<-dlgList(c(nom,"autres données") , multiple = FALSE, title="Choix du dataframe")$res
  if(length(nom)==0) return(donnees())
  data<-get(nom)
  if(info==TRUE) {print(("veuillez choisir la ou les variables "))}
  X<-dlgList(names(data), multiple = TRUE, title="Variable(s)")$res
  if(length(X)==0) X<-donnees()
  if(any(sapply(data[,X], class) %in% c("integer", "numeric")==FALSE)) {print("au moins une variable n'est pas numérique")
    X<-"autres données"
    str(data)}
  }
  
  
  if(info==TRUE) {writeLines(
    "Centrer permet d'avoir une moyenne à zéro en maintenant l'écart-type. Centrer réduire correspond à la formule du z. 
    La moyenne est de 0 et l'écart-type vaut 1. La probabilité inférieure correspond à la probabilité d'avoir un z inférieur ou égal au z.
    La probabilité supérieure correspond à la probabilité d'avoir un z supérieur ou égal au z")}
  dlgList(c("centrer", "centrer réduire", "probabilité inférieure", "probabilité supérieure"), preselect="centrer réduire", multiple = TRUE, title="Que voulez-vous faire ?")$res->choix
  if(length(choix)==0) return(donnees())
  
  for(i in 1:length(choix)){
    if(choix[i]=="centrer") {S<-FALSE 
    nn<-"centrer"}else {S<-TRUE
    nn<-"centrer.réduite"}
    scale(data[,X], scale=S)->centree
    matrix(centree, ncol=length(X))->centree
     if(choix[i]=="probabilité supérieure"|choix[i]=="probabilité inférieure"){
      if(choix[i]=="probabilité supérieure"){
        nn<-"p.sup"
        lower<-FALSE
      }else {
        nn<-"p.inf"
        lower<-TRUE
      }
      round(pnorm(centree, lower.tail = lower),4)->centree
     }
    data.frame(data, centree)->data
    names(data)[(length(data)+1-length(X)):length(data)]<-paste(X, nn, sep=".")  
}

    assign(nom, data, envir=.GlobalEnv)
    View(data)
    Resultats<-paste("L'opération a été réalisée correctement")
    return(Resultats)
}


#### function qui permet de choisir l'opération qu'on veut réaliser
donnees<-function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("importer des données", "voir des données", "importer des résultats",
            "Sélectionner des observations","Sélectionner des variables",
            "Exporter des données", "Centrer / centrer réduire","trier",
            "Opérations mathématiques sur des variables"), preselect=NULL, multiple = FALSE, 
          title="Quelle analyse voulez-vous réaliser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="voir des données") voir()->Resultats
  if(choix=="importer des résultats") import.results()->Resultats
  if(choix=="importer des données") import()->Resultats
  if(choix=="Sélectionner des observations") selectionO()->Resultats
  if(choix=="Sélectionner des variables") SelectionV()->Resultats
  if(choix=="Exporter des données") exporterD()->Resultats
  if(choix=="trier") trier()->Resultats
  if(choix=="Centrer / centrer réduire") Centrer.red()->Resultats
  if(choix=="Opérations mathématiques sur des variables") maths()->Resultats
  return(Resultats)
}

#### Export des donnÃÂ©es en fichier csv
exporterD<-function(data=NULL, nom=NULL){options (warn=-1)   
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  data <- dlgList(Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv)), multiple = FALSE, 
                  title="Quelles données voulez-vous exporter ?")$res 
  if(length(data)==0) return(donnees())
  data<-get(data)
  nom <- dlgInput("Quel nom voulez-vous attribuer au fichier ?", "Nouveau.fichier")$res
  if(length(nom)==0) nom<-"Nouveau.fichier"
  strsplit(nom, ":")->nom
  tail(nom[[1]],n=1)->nom
  write.csv(data, file=paste(nom, ".csv"))
  paste("le fichier est sauvegardé dans", getwd())->Resultats
  return(Resultats)
}

##

# fonction d'importation de donnÃÂ©es
import<-function(info=TRUE){
  options (warn=-1)
  c("svDialogs",  "readxl","foreign")->packages
  lapply(packages, require,character.only=T)
  Resultats <- list()
  if(info==TRUE) print("Dans quel format est enregistré votre fichier ?")
  choix <- dlgList(c("Fichier CSV", "Fichier txt", "Fichier Excel", "fichier SPSS"), preselect="Fichier Excel", multiple = FALSE, title="Format du fichier?")$res
  if(length(choix)==0) return(donnees())
  
  fichier <- try(file.choose(), silent=TRUE)
  if(class(fichier)=="try-error") return(import())
  
  setwd(dirname(fichier))
  if(choix!="fichier SPSS"){
  if(info==TRUE) print("Est-ce que le nom des variables est sur la première ligne de votre base de données ? Choisir TRUE si c'est le cas")
   noms <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title="Nom de variables?")$res
  if(length(noms)==0) return(import())}
  
  if(info==TRUE) print("Si certaines données sont manquantes, comment sont-elles definies ? Vous pouvez laisser NA si les cellules sont vides")
  manquant <- dlgInput("Par quelle valeur sont definies les valeurs manquantes ?", "NA")$res
  if(length(manquant)==0) manquant <- "NA"
  manquant <- strsplit(manquant, ":")
  manquant <- tail(manquant[[1]],n=1)
  
  if(choix=="Fichier CSV"|choix=="Fichier txt"){
    if(info==TRUE) print("Lors de l'enregistrement de votre fichier, quel est l'indice de séparation des colonnes ?")
    sep <- dlgList(c("espace","tab","point virgule","virgule"), preselect="point virgule", multiple = FALSE, title="Separateur de colonnes")$res
    if(length(sep)==0) return(import())
    m1 <- matrix(c("espace","tab","point virgule","virgule"," ","\t",";",","),nrow=4)
    sep <- subset(m1, m1[,1] %in% sep)[,2]
    
    if(info==TRUE) print("Si certaines données contiennent des décimales, quel est le symbole indiquant la décimale ?")
    dec <- dlgList(c("point", "virgule"), preselect=NULL, multiple = FALSE, title="Separateur de decimales")$res
    if(length(dec)==0) return(import())
    m1 <- matrix(c("point", "virgule",".",","),nrow=2)
    dec <- subset(m1, m1[,1] %in% dec)[,2]  
  }
  if(choix=="fichier SPSS") {basename(fichier)->fichier
    data1<-read.spss(fichier, to.data.frame=TRUE)
    col.char <-sapply(data1, is.factor)
    if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
  if(choix=="Fichier CSV") data1 <- read.csv2(fichier, header=as.logical(noms), sep=sep, dec=dec, na.strings=manquant)
  if(choix=="Fichier txt") data1 <- read.table(fichier, header=as.logical(noms), sep=sep, dec=dec, na.strings=manquant)
  if(choix=="Fichier Excel"){
    basename(fichier)->fichier
    writeLines("Veuillez spécifier la feuille de calcul que vous souhaitez importer")
    dlgList( excel_sheets(fichier), preselect=FALSE, multiple = FALSE, title="Quelle feuille ?")$res->fichier2
    if(length(fichier2)==0) return(import())
    #avec readxl::read_excel
    #les arguments 'skip' et 'col_types' peuvent ÃÂªtre intÃÂ©ressants
    #j'ai aussi modifiÃÂ© un peu la partie factor() ; pour moi, c'est plus clair, et ÃÂ§a devrait marcher aussi bien
    eval(parse(text=paste0("data1 <- read_excel(path='", fichier, "', sheet='", fichier2, "', col_names=as.logical(", noms, "), na='", manquant, "')")))
#    data1 <- read_excel(path=fichier, sheet=fichier2, col_names=as.logical(noms), na=manquant)
    col.char <-sapply(data1, is.character)
    if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
  }
  
  fichier <- dlgInput("Quel nom voulez-vous donner aux données ?", "data1")$res
  if(length(fichier)==0) fichier <- "data1"
  fichier <- strsplit(fichier, ":")
  fichier <- tail(fichier[[1]],n=1)
  if(grepl("[^[:alnum:]]", fichier)) {
    writeLines("Des caractères non autorisés ont été utilisés pour le nom. Ces caractères ont été remplacés par des points")
    gsub("[^[:alnum:]]", ".", fichier)->fichier
  }
  data1<-data.frame(data1)
  
  if(any(nchar(names(data1))>30)) {
    dlgMessage("Certaines variables ont des noms particulièrement longs pouvant gêner la lecture. Voulez-vous les raccourcir?", "yesno")$res->rn
    if(rn=="yes"){
      which(nchar(names(data1))>30)->rn
      for(i in 1:length(rn)) {
        rn2<- dlgInput("Quel nom voulez-vous attribuer à", colnames(data1)[rn[i]])$res 
        if(length(rn2)!=0){
          strsplit(rn2, ":")->rn2
          tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
        }

      }
    }
  }
  
  if(any( grepl("[^[:alnum:][:space:]_.]", names(data1)))) {
    writeLines("Evitez les espaces ainsi que les signes de ponctuations, à l'exception . et _ ")
    dlgMessage("Certaines noms de variables contiennent des caractères spéciaux pouvant créer des bugs. Voulez-vous renommer ces variables ?", "yesno")$res->rn
    if(rn=="yes"){
      grep("[^[:alnum:][:space:]_.]", names(data1))->rn
      for(i in 1:length(rn)) {
        rn2<- dlgInput("Quel nom voulez-vous attribuer à", colnames(data1)[rn[i]])$res 
        strsplit(rn2, ":")->rn2
        tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
      }
    }
  }
  
  if(any(is.na(data1))){
    writeLines("Nombre de valeurs manquantes par variable")
    print(sapply(data1, function(x) sum(length(which(is.na(x))))) )
  }
  
  
  assign(x=fichier, value=data1, envir=.GlobalEnv)
  View(data1, "données que vous venez d importer")
  str(data1)
  Resultats <- "les données ont été importées correctement"
  return(Resultats)
  
}

#### fonction d'importation de rÃÂ©sultats/donnes issu du codage R (dput dget)
import.results<-function(){
  
  file.choose()->fichier
  dget(fichier)->data1
  fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "Resultats")$res
  if(length(fichier)==0) fichier<-"data1"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data1, envir=.GlobalEnv)
  Resultats<-paste("Les résultats ont été correctement importés dans", fichier)
  return(Resultats)
}




maths<-function(info=TRUE){
  options (warn=-1) 
  packages<-c("svDialogs")
  #faire l analyse par groupe # regler le probleme des noms
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  
  choix.data(nom=TRUE)->data1
  if(length(data1)==0) {return(donnees())}
  data1[[1]]->nom1
  data1[[2]]->data
  if(info=="TRUE") writeLines("Veuillez  choisir l'opération mathématique que vous désirez réaliser ")
  dlgList(c("additions","multiplication", "division", "soustraction","moyenne de colonnes", "exposant ou racine", 
            "logarithme", "exponentiel","valeur absolue","modèle complexe"), preselect="additions", multiple = FALSE, title="Quelle opération voulez-vous?")$res->choix
  if(length(choix)==0) return(donnees())
  
  variable<-function(multiple=TRUE){
    X<-dlgList(c(names(data), "annuler"), multiple = multiple, title="Variable(s)")$res
    if(any(sapply(data[,X], class)=="factor")) {writeLines("au moins une des variables n'est pas numérique")
      writeLines(str(data))
      return(maths())}
    return(X)}
  
  valeur<-function(info=TRUE, out=NULL){
    # info : logique pour déterminer les informations relatives aux paramètres doivent s'afficher dans la console
    # out : valeur renvoyée si valeur non numérique ou annulation
    if(info) writeLines("Veuillez spécifier la valeur pour réaliser votre opération mathématique.")
    msg<-"no"
    while(msg=="no" ){
      valeur1 <- dlgInput("Quelle valeur voulez-vous pour votre opération mathématique ?", out)$res 
      if(length(valeur1)!=0){
        strsplit(valeur1, ":")->valeur1
        if(class(valeur1)=="list") {  tail(valeur1[[1]],n=1)->valeur1}
        if(grepl("/",valeur1)) apply(sapply(strsplit(valeur1, split = "/"), as.numeric), 2, function(x) x[1] / x[2])->valeur1
        if(valeur1=="e") valeur1<-exp(1)
        as.numeric(valeur1)->valeur1
        msg<-"yes"} else return(out) 
      if(is.na(valeur1) ) { dlgMessage("la valeur que vous avez entrée n'est pas numérique.Voulez-vous annuler cette analyse ?", "yesno")$res->msg
        if(msg=="yes") return(out)}
      
    }
    return(valeur1)
  }
  nom<-function(data,info, nom1){
    if(info=="TRUE") writeLines("Quel nom voulez-vous attribuer à la nouvelle variable ? ")
    variable<-dlgInput("Nom de la nouvelle variable ?","nouvelle.variable")$res
    if(length(variable)==0) variable<-"nouvelle.variable"
    strsplit(variable, ":")->variable
    tail(variable[[1]],n=1)->variable
    names(data)<-c(names(data)[1:(length(data)-1)], variable)
    assign(nom1, data, envir=.GlobalEnv)
    Resultats<-paste("La variable", variable, "a été ajoutée à", nom1)
    return(Resultats)}
  
  if(choix=="additions") {
    if(info=="TRUE") writeLines("Si vous sélectionnez les deux options en même temps, la valeur spécifiée sera ajoutée à l'ensemble des colonnes choisies 
                                et ensuite les colonnes choisies seront additionnées. Pour additionner une valeur spécifique au total,
                                veuillez choisir l'option addition de colonnes uniquement.")
    dlgList(c("addition de colonnes","addition d'une valeur spécifique"), preselect="addition de colonnes", multiple = TRUE, title="Quelle opération voulez-vous?")$res->choix2
    if(length(choix2)==0) return(maths())
    if(any(choix2== "addition d'une valeur spécifique")){
      variable()->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      valeur(info=info)->valeur1
      if(is.null(valeur1)) return(maths())
      data.frame(data, data[,X]+valeur1)->data
      if(valeur1>0)      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "plus", valeur1, sep=".") else names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "moins", abs(valeur1), sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(valeur1, "a été ajoutée à la variable", X)->Resultats
    }
    
    if(any(choix2== "addition de colonnes")) {
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables à additionner.")
      variable()->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      X->X1
      X2<-X1[1]
      X1[-1]->X1
      while(length(X1)!=0){paste(X2,"+",X1[1])->X2
        X1[-1]->X1}
      rowSums(data[,X])->data$nouvelle_variable
      if(info=="TRUE") writeLines("Vous pouvez encore ajouter une valeur spécifique au total. Laissez 0 si vous ne souhaitez rien ajouter")    
      valeur(info=info, out=0)->valeur1
      if(valeur1!=0) {data$nouvelle_variable+valeur1->data$nouvelle_variable
        paste(X2, "+", valeur1)->X2}
      writeLines(paste("vous avez réalisé l'opération suivante :", X2))
      writeLines("voulez-vous encore ajouter une valeur au total ?")
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
  }
  
  if(choix=="multiplication"){
    if(info=="TRUE") writeLines("Si vous sélectionnez les deux options en même temps, la valeur spécifiée sera multipliée à l'ensemble des colonnes choisies 
                                et ensuite les colonnes choisies seront multipliées entre elles. Pour multiplier une valeur spécifique au total,
                                veuillez choisir l'option multipication de colonnes uniquement.")
    dlgList(c("multiplication de colonnes","multiplication d'une valeur spécifique"), preselect="multiplication de colonnes", multiple = TRUE, title="Quelle opération voulez-vous?")$res->choix2
    if(length(choix2)==0) return(maths())
    if(any(choix2== "multiplication d'une valeur spécifique")){
      if(info=="TRUE") writeLines("Veuillez sélectionner les variables à multiplier. ")
      variable()->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      valeur(info=info, out=NULL)->valeur1
      if(is.null(valeur1)) return(maths())
      data.frame(data, data[,X]*valeur1)->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "multiplié.par", valeur1, sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(valeur1, "a multiplié la -les- variable-s", X)->Resultats
    }
    
    if(any(choix2== "multiplication de colonnes")) {
      variable()->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      
      X->X1
      X2<-X1[1]
      X1[-1]->X1
      while(length(X1)!=0){paste(X2,"*",X1[1])->X2
        X1[-1]->X1}
      1*data[,X[1]]->nouvelle
      for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
      data.frame(data, nouvelle)->data
      
      if(info=="TRUE") writeLines("Vous pouvez encore multiplier le total par une valeur spécifique. Laissez 1 si vous ne souhaitez plus multiplier par une nouvelle valeur")    
      valeur(info=info, out=1)->valeur1
      if(valeur1!=1) {data$nouvelle*valeur1->data$nouvelle
        paste(X2, "*", valeur1)->X2}
      writeLines(paste("vous avez réalisé l'opération suivante :", X2))
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
  }
  if(choix=="division"){
    if(info=="TRUE") writeLines("Le numérateur est-il une variable ou une valeur ? ")
    numer<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Numérateur")$res
    if(length(numer)==0) return(maths())
    if(numer=="valeur") valeur(info=info, out=1)->X else{
      if(info=="TRUE") writeLines("Veuillez sélectionner la variable au numérateur ")
      variable(multiple=FALSE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      data[,X]->X
    }
    
    if(info=="TRUE") writeLines("Le dénominateur est-il une variable ou une valeur ? ")
    denom<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Dénominateur")$res
    if(length(denom)==0) return(maths())
    if(denom=="valeur") valeur(info=info, out=1)->Y else{
      if(info=="TRUE") writeLines("Veuillez sélectionner la variable au dénominateur ")
      variable(multiple=FALSE)->Y
      if(length(X)==0|| any(X=="annuler")) return(maths())
      data[,Y]->Y
      if(any(Y)==0) writeLines("Au moins une des valeurs au dénominateur est un 0. La valeur renvoyée dans ce cas est infinie - inf")
    }
    X/Y->data$nouvelle_variable
    nom(data=data, info=info,nom1=nom1)->Resultats
  }
  
  if(choix=="soustraction") {
    if(info=="TRUE") writeLines("Veuillez sélectionner les valeurs situées à gauche du symbole *moins*. Si plusieurs variables sont sélectionnées, 
                                les règles du calcul matriciel sont appliqués.")
    if(info=="TRUE") writeLines("Les valeurs positives sont-elles une/des variable(s) ou une valeur ? ")
    numer<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Valeurs positives")$res
    if(length(numer)==0) return(maths())
    if(numer=="valeur") valeur(info=info, out=0)->X else{
      if(info=="TRUE") writeLines("Veuillez sélectionner la -les- variable(s) à gauche du symbole *moins*")
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X=="annuler")) return(maths())
      data[,X]->X
      data.frame(X)->X
    }
    
    if(info=="TRUE") writeLines("Les valeurs à droite du symbole *moins* sont-elles une/des variable(s) ou une valeur  ? ")
    denom<-dlgList(c("valeur", "variable"), multiple = FALSE, title="Valeurs négatives")$res
    if(length(denom)==0) return(maths())
    if(denom=="valeur") valeur(info=info, out=0)->Y else{
      if(info=="TRUE") writeLines("Veuillez sélectionner la -les- variable(s) à droite du symbole *moins*.")
      Y<-NULL
      while(is.null(Y)){
        variable(multiple=TRUE)->Y
        if(length(Y)==0|| any(Y=="annuler")) return(maths())
        data[,Y]->Y
        data.frame(Y)->Y 
        if(length(X)!=1 & length(Y)!=1 & length(X)!=length(Y)) {
          writeLines("Il ne doity avoir qu'une colonne ou le nombre de colonnes à droite du symbole *moins* doit être égal 
                     au nombre de colonnes à gauche du symbole *moins*")
          Y<-NULL} else Y<-Y
      }
      }
    X-Y->new.var
    names(new.var)<-paste0(names(X), ".moins.", names(Y))
    data<-data.frame(data, new.var)
    assign(nom1, data, envir=.GlobalEnv)
    #nom(data=data, info=info,nom1=nom1)->Resultats
    Resultats<-"L'opération mathématique s'est déroulée correctement."
    }
  
  if(choix=="moyenne de colonnes")  {
    if(info=="TRUE") writeLines("Veuillez sélectionner les variables à moyenner ")
    X<-variable()
    if(length(X)==0|| any(X=="annuler")) return(maths())
    rowMeans(data[,X])->data$nouvelle_variable
    nom(data=data, info=info,nom1=nom1)->Resultats
  }
  if(choix== "exposant ou racine"){
    if(info=="TRUE") writeLines("Veuillez sélectionner les variables auxquelles s'applique l'exposant ")
    variable(multiple=TRUE)->X
    if(length(X)==0|| any(X=="annuler")) return(maths())
    if(info=="TRUE") writeLines("Veuillez préciser la valeur de l'exposant. 
                                NOTE : Pour les racines, l'exposant est l'inverse la valeur. Par exemple, La racine carrée vaut 1/2, la racine cubique 1/3... ")
    valeur(info=info)->Y
    if(class(Y)!="numeric") {writeLines("la valeur entrée n'est pas numérique")
      return(maths())}
    data.frame(data, data[,X]^Y)->data
    names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, "exposant", Y, sep=".")
    assign(nom1, data, envir=.GlobalEnv)
    paste("la variable", X, " a été élevée à la puissance", Y)->Resultats
    
  }
  if(choix== "logarithme"){
    if(info=="TRUE") writeLines("Veuillez sélectionner les variables dont il faut faire le logarithme ")
    variable(multiple=TRUE)->X
    if(length(X)==0|| any(X=="annuler")) return(maths())
    if(info=="TRUE") writeLines("Veuillez préciser la base du logarithme.Pour obtenir e, tapez e")
    valeur(info=info)->Y
    if(class(Y)!="numeric") {writeLines("la valeur entrée n'est pas numérique")
      return(maths())}
    if(Y<0) {writeLines("il n'est pas possible de calculer des logarithmes pour une base est négative. NA est renvoyé")
      return(maths()) }
    data.frame(data, log(data[,X], base=Y))->data
    names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("log.", X,  sep=".")
    assign(nom1, data, envir=.GlobalEnv)
    paste("le logarithme de base", Y, " a été appliqué à la variable", X)->Resultats
  }
  if(choix== "exponentiel"){
    if(info=="TRUE") writeLines("Veuillez sélectionner les variables servant à l'exponentiel ")
    variable(multiple=TRUE)->X
    if(length(X)==0|| any(X=="annuler")) return(maths())
    data.frame(data, exp(data[,X]))->data
    names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("exp.", X,  sep=".")
    assign(nom1, data, envir=.GlobalEnv)
    paste("l'exponentiel a été appliqué à la variable", X)->Resultats
  }
  if(choix== "valeur absolue"){
    if(info=="TRUE") writeLines("Veuillez sélectionner les variables dont il faut faire la valeur absolue ")
    variable(multiple=TRUE)->X
    if(length(X)==0|| any(X=="annuler")) return(maths())
    data.frame(data, abs(data[,X]))->data
    names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("valeur.absolue.", X,  sep=".")
    assign(nom1, data, envir=.GlobalEnv)
    paste("la valeur absolue a été appliqué à la variable", X)->Resultats
  }
  if(choix== "modèle complexe"){
    writeLines("L'expression doit être correcte. Vous pouvez utiliser directement le nom des variables
               les opérateurs sont +,-,*,/,^,(,). Une expression correcte serait :")
    print(paste(names(data)[1],"^2+5"), quote=FALSE)
    print(names(data))
    valeur1 <- dlgInput("Veuillez spécifier le modèle à réaliser")$res 
    if(length(valeur1)==0) return(maths())
    strsplit(valeur1, ":")->valeur1
    tail(valeur1[[1]],n=1)->valeur1
    try(eval(parse(text=valeur1), envir=data), silent=TRUE)->nouvelle
    if(class(nouvelle)=="try-error") {writeLines("Le modèle ne peut être évalué. Il doit contenir une erreur")
      return(maths())} else nouvelle->data$nouvelle
    
    nom(data=data,info=info, nom1=nom1)->Resultats
    
  }
  
  return(Resultats)
  }



#### sÃÂ©lectionner des observations
selectionO<-function(data=NULL, info=TRUE){options (warn=-1)
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  choix.data()->data
  if(length(data)==0) {return(donnees())}
  if(info==TRUE) writeLines("Il est possible d'appliquer plusieurs critères de sélection simultanément, impliquant ou non plusieurs variables. 
Veuillez préciser le nombre de variables sur lesquelles vous désirez appliquer un ou plusieurs critères de selection. 
Veuillez choisir les variables sur lesquelles vous déirez appliquer une sélection") 
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres donnees"), multiple = TRUE, 
             title="Variable")$res
  if(length(X)==0 ) return(donnees())
listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
subset(listes, listes[,1] %in% X)[,2]->X

for(i in 1:length(X)) {  
 if(class(data[,X[i]])=="factor"){
      if(info==TRUE) {writeLines("Veuillez sélectionner les modalités que vous désirez conserver.")
   writeLines(paste("Quelles modalités voulez-vous sélectionner pour la variable", names(data[,X])[i],"?" ))}
      Y<-dlgList(levels(data[,X[i]]), multiple = TRUE, 
                 title=paste("Quelles modalités voulez-vous sélectionner pour la variable", names(data[,X])[i],"?" ))$res
      if(length(Y)==0) return(selectionO())
      data[data[,X[i]]%in% Y,]->data
      factor(data[,X[i]])->data[,X [i]]}else{
        if(info==TRUE) {print("Veuillez spécifier les critères des observations que vous désirez conserver/garder.")
          writeLines(paste("Quel critère voulez-vous utiliser pour la variable", names(data[,X])[i], "?"))}
        dlgList(c("supérieur à","supérieur ou égal à", "inférieur à", "inférieur ou égal à", "égal à", "est différent de", "entre", 
                  "au-delà (avec une limite inférieure et supérieure"), 
                preselect=NULL, multiple = FALSE, title=paste("Quel critère voulez-vous utiliser pour la variable", names(data[,X])[i], "?"))$res->choix
        if(length(choix)==0) return(selectionO())
        if(choix=="supérieur à"|choix=="inférieur à"|choix=="égal à"|choix=="supérieur ou égal à"|
           choix=="inférieur ou égal à"|choix=="est différent de"){
          if(info==TRUE) writeLines("Veuillez préciser la valeur sur laquelle les observations doivent être sélectionnées.")
          seuil<- dlgInput("Precisez la valeur?", 0)$res
          if(length(seuil)==0) return(selectionO()) else {
            strsplit(seuil, ":")->seuil
            tail(seuil[[1]],n=1)->seuil
            as.numeric(seuil)->seuil}} else{seuil.inf<- dlgInput("Limite inférieure?", 0)$res
            while(length(seuil.inf)==0) {writeLines("vous devez préciser la limite inférieure")
              dlgMessage("Vous n'avez pas precisé la limite inférieure. Voulez-vous quitter la sélection ?", "yesno")$res->quitte
              if(quitte=="yes") return(selectionO())
              seuil.inf<- dlgInput("Limite inférieure?", 0)$res}
            strsplit(seuil.inf, ":")->seuil.inf
            tail(seuil.inf[[1]],n=1)->seuil.inf
            as.numeric(seuil.inf)->seuil.inf
            seuil.sup<- dlgInput("Limite supérieure?", 0)$res
            while(length(seuil.sup)==0) {writeLines("vous devez préciser la limite supérieure")
              dlgMessage("Vous n'avez pas precisé la limite supérieure. Voulez-vous quitter la sélection ?", "yesno")$res->quitte
              if(quitte=="yes") return(selectionO())
              seuil.sup<- dlgInput("Limite superieure?", 0)$res}
            strsplit(seuil.sup, ":")->seuil.sup
            tail(seuil.sup[[1]],n=1)->seuil.sup
            as.numeric(seuil.sup)->seuil.sup}
        if(choix=="supérieur à"){data[data[,X[i]]>seuil,]->data}
        if(choix=="inférieur à"){data[data[,X[i]]<seuil,]->data}
        if(choix=="égal à"){data[data[,X[i]]==seuil,]->data}
        if(choix=="est différent de"){data[data[,X[i]]!=seuil,]->data}
        if(choix=="supérieur ou égal à"){data[data[,X[i]]>=seuil,]->data}
        if(choix=="inférieur ou égal à"){data[data[,X[i]]<=seuil,]->data}
        if(choix=="entre"){data[data[,X[i]]>=seuil.inf & data[,X[i]]<=seuil.sup,]->data}
        if(choix=="au-delà (avec une limite inférieure et supérieure"){data[data[,X[i]]<seuil.inf & data[,X[i]]>seuil.sup,]->data}
      }
  }
  
  fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "selection")$res
  if(length(fichier)==0) return(selectionO())
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data, envir=.GlobalEnv)
  View(data, "données que vous venez de sélectionner")
  Resultats<-paste("les observations que vous avez sélectionnées sont dans", fichier)
  return(Resultats)
}

#### sÃÂ©lectionner des variables 
SelectionV<-function(data=NULL,info=TRUE){options (warn=-1)
  packages<-c("svDialogs")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  choix.data()->data
  if(length(data)==0) return(donnees())
  if(info==TRUE) print("Quelles sont les variables à sélectionner ?")
  X<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), "autres données"), multiple = TRUE, 
             title="Variable")$res
  if(length(X)==0) return(donnees())
   if( X== "autres données") return(SelectionV())
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), 1:length(data))
  subset(listes, listes[,1] %in% X)[,2]->X
  data[,X]->data
  fichier<- dlgInput("Quel nom voulez-vous donner au fichier?", "selection")$res
  if(length(fichier)==0) fichier<-"selection"
  strsplit(fichier, ":")->fichier
  tail(fichier[[1]],n=1)->fichier
  assign(x=fichier, value=data, envir=.GlobalEnv)
  View(data, "données que vous venez de sélectionner")
  Resultats<-paste("les variables sélectionnées sont dans", fichier)
  return(Resultats)
}

#### trier des observations
trier<-function(X, data=NULL, info=TRUE){options (warn=-1) 
  packages<-c("svDialogs")
  # faire en sorte que les donnees triees portent le nom initial des donnees
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  list()->Resultats
  choix.data(info=TRUE,nom=TRUE)->data
  if(length(data)==0) return(donnees())
  data[[1]]->nom1
  data[[2]]->data
  if(info==TRUE) writeLines("Veuillez sélectionner la (les) variable(s) à trier")
  X<-dlgList(c(names(data), "autres données"), multiple = TRUE, title="Variable(s)")$res
  if(any(X=="autres données")) return(trier())
  if(length(X)==0) return(donnees())
  X->diff
  Y2<-c()
  d<-c()
  for(i in 1:length(diff)) {
  writeLines(paste("Veuillez choisir le niveau", i, "de tri"))
    Y<-dlgList(diff, multiple = FALSE, title="Variable(s)")$res
    if(length(Y)==0) return(trier())
    setdiff(diff, Y)->diff
    c(Y2,Y)->Y2
  }
  data[do.call("order", data[Y2]), ]->data
  View(data)
  Resultats<-"les données ont été triées correctement "
  assign(x=nom1, value=data, envir=.GlobalEnv)
  return(Resultats)}


#### voir un dataframe 
# Il manque modifier un dataframe
voir<-function(){
  data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title="Choix du dataframe")$res
  if(length(data)==0) return(donnees())
  get(data)->data
  for(i in 1:ncol(data)) {
  	if(class(data[,i])!="factor"){
  	attributes(data[,i])<-NULL}}
  View(data)
}





################################
####                        ####
####        Analyse         ####
####                        ####
################################
### Analyses

#### function qui permet de choisir l'analyse qu'on veut rÃÂ©aliser
analyse<-function(){options (warn=-1)
  require(svDialogs)
  dlgList(c("Statistiques descriptives","chi deux","corrélations", 
            "t de Student", "analyse de variance et covariance",
            "régressions",
            "analyses de facteurs et de composantes",
            "analyse de fiabilité et d accord"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous réaliser?")$res->choix
  if(length(choix)==0) return(easieR())
  if(choix=="chi deux") chi()->Resultats
  if(choix=="t de Student") test.t()->Resultats
  if(choix=="analyse de variance et covariance") {
    Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
    if(length(nom1)==0) AN.C.OVA()->Resultats else {
      dlgList(c("Analyse principale", 
                "Résultats complémentaires (e.g. contrastes d'interaction et moyennes ajustées)"), 
              preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous réaliser?")$res->choix
      if(choix== "Analyse principale") AN.C.OVA()->Resultats else aov.plus()->Resultats
      
    }
    
   }
  if(choix=="corrélations") choix.corr()->Resultats
  if(choix=="régressions") choix.reg()->Resultats
  #if(choix=="régressions logistiques") regressions.log()->Resultats
  if(choix=="analyses de facteurs et de composantes") factor.an()->Resultats
  if(choix=="analyse de fiabilité et d accord") fiabilite()->Resultats
  if(choix=="Statistiques descriptives") stat.desc()->Resultats
  return(Resultats)
} 


#### Analyse de variance et de covariance 

AN.C.OVA<-function(option=T, longdata=NULL, inter=NULL, intra=NULL, VD=NULL, cov=NULL, desires=c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                   desires2=c("Modèle paramétrique","Modèle linéaire mixte"),ES="ges", sauvegarde=F, SumS=3, p.adjust=NULL, type.cont="aucun"){
  # option : logique, si TRUE, permet de spécifier à l'aide de boîtes de dialogue les options suivantes : desires, desires2, sauvegarde, SumS, ES par des boîtes de dialogue, 
  #          Dans le cas contraire, ce sont les valeurs spécifiées ou celles par défaut qui sont utilisées
  # longdata : données en format long (nécessaire le cas pour anova à groupes indépendants)
  # inter : variables intergroupes 
  # intra : variables intragroupes
  # VD : variable dépendante
  # cov : covariables
  # desires : vecteur avec une plusieurs des possibilités suivantes : c("Données complètes","Identification des outliers", "Données sans valeur influente"). Si desires est spécifié
  # desires2 : 
  # ES : taille d'effet qui doit être calculée. "ges" pour eta carré généralisé, et "pes" pour êta carré partiel
  # sauvegarde : logique, indique si les résultats doivent être sauvegardés
  # Sums : choix du type des sommes des carrés calculées. Peut être 2 ou 3
  # p.adjust : type de correction de la probabilité si type.cont vaut "Comparaison 2 à 2". La correction peut être "holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"
  
  packages<-c( "DescTools","outliers", "nortest", "psych", "reshape2", "car", "lawstat", "pgirmess","WRS","svDialogs", "WRS2", "nlme", "afex")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  large.long<-function(data, VIR){
    data[complete.cases(data[,VIR]),]->data
    paste("p", 1:length(data[,1]))->data$IDeasy
    factor(data$IDeasy)->data$IDeasy
    melt(data, setdiff(names(data),VIR))->longdata
    if(length(VIR)>3) N.facteurs <- dlgInput("Combien de facteurs en mesure répétée ?", 1)$res else N.facteurs<-"1"
    while(length(N.facteurs)=="0"){writeLines("vous devez spécifier le nombre de facteurs en mesure répétée")
      dlgMessage("Vous n avez pas précisé le nombre de facteurs en mesure répétée, voulez-vous quitté ?", "yesno")$res->quitte
      if(quitte=="yes") return(NULL) else  N.facteurs <- dlgInput("Combien de facteurs en mesure répétée ?", 1)$res }
    strsplit(N.facteurs, ":")->N.facteurs
    tail(N.facteurs[[1]],n=1)->N.facteurs
    as.numeric(N.facteurs)->N.facteurs
    if(is.na(N.facteurs)) { writeLines("La valeur entrée n'est pas numérique")
      return(NULL)}
    if(N.facteurs==1){list()->intra
      list()->modalites
      dlgInput("Nom du facteur ?", "Variable.1")$res->intra[[1]]
      if(length(intra[[1]])==0) return(large.long(data=data, VIR=VIR))
      strsplit(intra[[1]], ":")->intra[[1]]
      tail(intra[[1]][[1]],n=1)->intra[[1]]
      colnames(longdata)[length(longdata)-1]<-intra[[1]]
    } else {
      c()->N.modalites2
      while(prod(N.modalites2)!=length(VIR)){list()->intra
        list()->modalites
        c()->N.modalites2
        writeLines(paste("vous avez sélectionné", length(VIR), "colonnes"))
        writeLines("le produit des modalités de chacune des variables doit correspondre au nombre de colonnes sélectionnées.")
        for(i in 1:N.facteurs) {dlgInput(paste("Nom du facteur",i,  "?"), paste("Variable",i, sep="."))$res->intra[[i]]
          if(length(intra[[i]])==0) return(large.long(data=data, VIR=VIR))
          strsplit(intra[[i]], ":")->intra[[i]]
          tail(intra[[i]][[1]],n=1)->intra[[i]]
          N.modalites <- dlgInput(paste("Combien de modalités", intra[[i]]), 2)$res
          if(length(N.modalites)==0) return(large.long(data=data, VIR=VIR))
          strsplit(N.modalites, ":")->N.modalites
          tail(N.modalites[[1]],n=1)->N.modalites
          as.numeric(N.modalites)->N.modalites
          if(is.na(N.modalites)) writeLines("Vous n'avez pas entré une valeur numérique.")
          c(N.modalites2,N.modalites)->N.modalites2
          dlgForm(setNames(as.list(paste("modalité", 1:N.modalites2[i])), paste("modalité", 1:N.modalites2[i])),
                  paste("Noms des modalités pour", intra[[i]]) )$res->modalites[[i]]
        }
        
      }
      for(i in 1:length(intra)){
        if(i==length(intra)){a<-1} else {
          a<-prod(N.modalites2[(i+1):length(intra)])
        }
        gl(n=N.modalites2[[i]], k=length(data[,1])*a, length=length(data[,1])*prod(N.modalites2), labels=modalites[[i]])->longdata$variable1
        names(longdata)<-c(names(longdata[1:(length(longdata)-1)]),intra[[i]])
      }}
    View(longdata)
    cat ("Appuyez [entree] pour continuer")
    line <- readline()
    dlgMessage("Est-ce que la structure dans un format long de vos données est correcte ?", "yesno")$res->suppression
    if(suppression=="no") return(large.long(data=data, VIR=VIR)) else {assign("intra",intra,envir=.e)
      return(longdata)}
  }
  options.aov<-function(inter, intra){
    list()->Resultats
    writeLines("le modèle paramétrique renvoie l'anova classique,le non paramétrique calcule le test de Kruskal Wallis
               si c'est un modèle à groupes indépendants, ou une anova de Friedman pour un modèle en mesure répétée.
               Le modèle mixte est l'équivalent du modèle testé dans l'anova par un modèle linéaire mixte,
               les statistiques robustes sont des anovas sur des médianes avec ou sans bootstrap.")
    if(!is.null(cov)) {
      Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte"),
                                   preselect=c("Modèle paramétrique", "Modèle linéaire mixte"),
                                   multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
    } else {
      if((exists("inter") && (length(inter)==1 & is.null(intra))) | (!is.null(intra) && (length(intra)==1 & is.null(inter)))) {
        Resultats$desires2<- dlgList(c("Modèle paramétrique", "Modèle non paramétrique", "Modèle linéaire mixte",
                                       "Statistiques robustes - peut prendre du temps"),
                                     preselect=c("Modèle paramétrique", "Modèle linéaire mixte", "Modèle non paramétrique", "Statistiques robustes - peut prendre du temps"),
                                     multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
      } else {
        if((exists("inter") && (length(inter)==1 & !is.null(intra) && length(intra)==1)) || (exists("inter") && (length(inter)<4 & is.null(intra)))) {
          Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte", "Statistiques robustes - peut prendre du temps"),
                                       preselect=c("Modèle paramétrique", "Modèle linéaire mixte", "Statistiques robustes - peut prendre du temps"),
                                       multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
        } else {
          Resultats$desires2<- dlgList(c("Modèle paramétrique","Modèle linéaire mixte"), preselect=c("Modèle paramétrique", "Modèle linéaire mixte"),
                                       multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res
        }
      }
    }
    if(length(Resultats$desires2)==0) return(NULL)
    if(any(Resultats$desires2 %in% c("Modèle paramétrique","Modèle linéaire mixte"))){
      writeLines("Les données complètes représentent l'analyse réalisée sur l'ensemble des observations. L'analyse sans les valeurs influentes
                 est une analyse pour laquelle les valeurs influentes ont été supprimées. L'identification des valeurs influentes est realisée sur la bae du test de Grubbs")
      Resultats$desires<- dlgList(c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                                  preselect=c("Données complètes","Identification des outliers", "Données sans valeur influente"),
                                  multiple = TRUE, title="Quels Resultats voulez-vous obtenir ?")$res
      if(length(Resultats$desires)==0) return(options.aov(inter=inter, intra=intra))}else Resultats$desires<-"Données complètes"
    
    
    
    if(any(Resultats$desires2=="Modèle paramétrique")){
      writeLines("la taille d'effet la plus fréquente est le êta carré partiel - pes.
                 La taille d'effet la plus précise est le êta carré géneralisé - ges")
      Resultats$ES<- dlgList(c("ges", "pes"), preselect=c("ges"),multiple = FALSE, title="Quelle taille d effet voulez-vous  ?")$res
      if(length(Resultats$ES)==0) return(options.aov(inter=inter, intra=intra))
      writeLines("Il existe plusieurs manière de calculer la somme des carrés. Le choix par defaut des logiciels commerciaux est une somme des carrés
                 de type 3, mettant la priorité sur les interactions plutôt que sur les effets principaux.")
      SumS<- dlgList(c(2,3), preselect=3,multiple = FALSE, title="Quels sommes des carrés voulez-vous utiliser ?")$res
      as.numeric(SumS)->Resultats$SumS
      if(length(Resultats$SumS)==0) return(options.aov(inter=inter, intra=intra))
    }
    writeLines("Voulez-vous sauvegarder les résultats de l'analyse ?")
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->Resultats$sauvegarde
    if(length(Resultats$sauvegarde)==0) return(options.aov(inter=inter, intra=intra))
    return(Resultats)
  }
  contrastes.ez<-function(longdata, inter=NULL, intra=NULL){
    Resultats<-list()
    writeLines("Les contrastes a priori correspondent aux contrastes sans correction de la probabilité en suivant les règles de contrastes.
               Les contrastes 2 à 2 permettent de faire toutes les comparaisons 2 à 2 en appliquant ou non une correction à la probabilité")
    type.cont<- dlgList(c("a priori",  "Comparaison 2 à 2", "aucun"), preselect="a priori",multiple = FALSE, title="Quel types de contraste voulez-vous ?")$res
    if(length(type.cont)==0) return(NULL)
    Resultats$type.cont<-type.cont
    c(inter, unlist(intra))->interintra
    if(type.cont=="a priori") {
      contrastes<-list()
      writeLines("Vous pouvez choisir les contrastes que vous souhaitez. Néanmoins les règles concernant l'application des contrastes doivent être respectées.
                 Les contrastes peuvent etre specifiés manuellement. Dans ce cas, veuillez choisir spécifier les contrastes")
      cont.exemple<-list()
      contr.helmert(3)->cont.exemple$Orthogonaux
      apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inversés
      contr.poly(3)->cont.exemple$Polynomiaux
      contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
      print(cont.exemple)
      
      for (i in 1:length(interintra)){
        if(i>1) {
          type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", "spécifier les contrastes"),
                               preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res} else {
                                 type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base",
                                                        "spécifier les contrastes"),preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res
                               }
        if(length(type.cont2)==0) return(contrastes.ez())
        if(type.cont2=="orthogonaux") {contr.helmert(nlevels(longdata[,interintra[i]]))->contrastes[[i]]}
        if(type.cont2=="orthogonaux inversés") {apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrastes[[i]]}
        if(type.cont2=="polynomiaux")  contr.poly(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
        if(type.cont2=="comparaison à une ligne de base") {
          base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                         multiple = FALSE, title="Quelle est la ligne de base?")$res
          which(levels(longdata[, interintra[i]])==base)->base
          contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
        }
        if(type.cont2=="spécifier les contrastes"){
          ortho<-FALSE
          while(ortho!=TRUE){
            matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrastes3
            dimnames(contrastes3)[[1]]<-levels(longdata[,interintra[i]])
            dimnames(contrastes3)[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
            fix(contrastes3)->contrastes3
            if(any(colSums(contrastes3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrastes3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
              test.out<-rep(1, length(contrastes3[,1]))
              for(j in 1:length(contrastes3[1,])) {contrastes3[,j]*test.out->test.out}
              if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
            if(ortho==FALSE) {dlgMessage("Les contrastes doivent respecter l orthogonalité. Voulez-vous continuer ?", "yesno")$res->cont
              if(cont=="no") return(contrastes.ez(longdata=longdata, inter=inter, intra=intra ))  }
            contrastes[[i]]<-contrastes3
            
          }
          
        }
        
        dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
        dimnames(contrastes[[i]])[[1]]<-levels(longdata[,interintra[i]])
      }
      names(contrastes)<-interintra
      Resultats$contrastes<-contrastes
      
    }
    if(type.cont== "Comparaison 2 à 2"){
      list()->p.adjust
      writeLines("Quelle correction de la probabilité voulez-vous appliquer ? Pour ne pas appliquer de correction, choisir +none+")
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
      if(length(p.adjust)==0) return(contrastes.ez())
      Resultats$p.adjust<-p.adjust
    }
    return(Resultats)
  }
  
  
  .e <- environment()
  Resultats<-list()
  if(!is.null(c(inter,intra))) {type.v<-c()
  if(!is.null(inter)) c(type.v,("Groupes indépendants"))->type.v
  if(!is.null(intra)) c(type.v,("Mesure répétée"))->type.v
  } else { writeLines("Veuillez préciser le(s) type(s) de variable(s) que vous souhaitez inclure dans l'analyse.")
    type.v<-dlgList(c("Groupes indépendants", "Mesure répétée", "Covariables"), multiple = TRUE, title="Quel-s type-s de variables?")$res
    if(length(type.v)==0) return(analyse())}
  
  if(any(type.v== "Groupes indépendants") & any(type.v== "Mesure répétée")) plan<-"Plan mixte" else {
    if(all(type.v!="Mesure répétée") & any(type.v== "Groupes indépendants"))plan<-"Groupes independants" else {
      if(any(type.v=="Mesure répétée") & all(type.v!= "Groupes indépendants")) plan<-"Mesure repetee"
      else {
        writeLines("il est indispensable d'avoir au minimum des variables à groupes independants ou en mesure répétée")
        return(AN.C.OVA())
      }
    }
  }
  # revoir data pour passer de large en long par ligne de commamande
  if(is.null(longdata)) {
    choix.data(info=TRUE, nom=TRUE)->data
    if(length(data)==0) return(AN.C.OVA())
    data[[1]]->nom
    data[[2]]->data
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), names(data))
  }
  
  if(is.null(c(inter,intra))) {
    if(plan=="Groupes independants"){data->longdata
      data<-NULL
      intra<-NULL
      paste("p", 1:length(longdata[,1]))->longdata$IDeasy
      factor(longdata$IDeasy)->longdata$IDeasy}else{
      }
    if(any(type.v=="Mesure répétée")) {
      VIR<-"autres"
      while(any(VIR=="autres")){
        writeLines("veuillez sélectionner les variables OU les modalités de la (des) variables à mesure(s) répétée(s).")
        VIR<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, 
                     title="Mesures répétées")$res
        if(length(VIR)==0) return(AN.C.OVA())
        subset(listes, listes[,1] %in% VIR)[,2]->VIR
        as.character(VIR)->VIR
        if(all(sapply(data[,VIR], class)=="factor")){
          data->longdata
          VIR->intra
          writeLines("Quelle est la variable identifiant les participants ?")
          IDeasy<-dlgList(paste(names(longdata), "(format :", sapply(data, class), ")", sep=" "), multiple = TRUE, title="Identifiant participant")$res
          if(length(IDeasy)==0) return(AN.C.OVA())
          subset(listes, listes[,1] %in% IDeasy)[,2]->IDeasy
          names(longdata)[which(names(longdata)== IDeasy)]<-"IDeasy"
          factor(longdata$IDeasy)->longdata$IDeasy
          format<-"long"
          if(length(intra)==1) nlevels(longdata[,unlist(intra)])->N.modalites2 else {sapply(longdata[,unlist(intra)],nlevels)->N.modalites2 }
          if(nlevels(longdata$IDeasy)*prod(N.modalites2)!=length(longdata[,1])) {
            okCancelBox("Chaque participant doit apparaître une et une seule fois pour chaque combinaison des modalités")
            return(AN.C.OVA())}
        }else{
          format<-"court"
          if(length(VIR)==1) {
            writeLines("Pour un facteur en mesure répétée, il faut au moins deux colonnes")
            VIR<-"autres"}
          if(length(setdiff(sapply(data[,VIR], class), c("numeric","integer")))!=0 ){ 
            writeLines("Si vos données sont en format large, les mesures doivent toutes être numériques ou des integers.")
            VIR<-"autres" 
            
          }
          if( all(sapply(data[,VIR], class)%in% c("numeric", "integer"))) {
            data[complete.cases(data[,VIR]),]->data
            large.long(data=data, VIR=VIR)->longdata
            assign(x=paste0(nom,".format.long"), value=longdata, envir=.GlobalEnv)}
        }
      }
      if(plan=="Mesure repetee") inter<-NULL
    }
  }
  
  if(plan=="Groupes independants"|plan=="Plan mixte"){ 
    if(plan=="Groupes independants") intra<-NULL
    setdiff(names(longdata), c("IDeasy", "variable", "value", intra))->diffs
    inter<-"autres donnees"
    while(inter=="autres donnees"){ 
      writeLines("Veuillez choisir les variable-s à groupes indépendants")
      if(length(diffs)==1) {inter<-dlgList(paste(diffs, "(format :",class(longdata[,diffs]),")"), multiple = TRUE, 
                                           title="Variables à groupes indépendants")$res} else {
                                             inter<-dlgList(paste(diffs, "(format :", sapply(longdata[,diffs], class), ")", sep=" "), multiple = TRUE, 
                                                            title="Variables à groupes indépendants")$res}
      
      if(length(inter)==0) {
        if(okCancelBox("Vous n avez pas choisi de variable à groupes indépendants. Voulez-vous continuer  (ok) ou abandonner (annuler) cette analyse ?"))  inter<-"autres donnees" else return(AN.C.OVA())
      }
      if(inter!="autres donnees") {subset(listes, listes[,1] %in% inter)[,2]->inter
        as.character(inter)->inter}
    }
    if(length(inter)==1){
      if(class(longdata[,inter])!="factor") factor(longdata[,inter])->longdata[,inter]
    }else {
      if(any(sapply(longdata[,inter],class)!="factor")) lapply(longdata[,inter],factor)->longdata[,inter] 
    }
  }
  
  if(plan=="Groupes independants" || format=="long") {
    writeLines("Veuillez choisir la variable dépendante.") 
    setdiff(names(longdata), c("IDeasy", "variable", unlist(intra), inter))->diffs 
    vd.num<-FALSE
    while( vd.num!=TRUE){
      if(length(diffs)==1)  VD<-dlgList(paste(diffs, "(format :", class(longdata[, diffs]), ")", sep=" "), multiple = FALSE, title="Variable dépendante")$res else {
        VD<-dlgList(c(paste(diffs, "(format :", sapply(longdata[, diffs], class), ")", sep=" ")), multiple = FALSE, title="Variable dépendante")$res}
      if(length(VD) == 0L) return(AN.C.OVA())
      subset(listes, listes[,1] %in% VD)[,2]->VD
      as.character(VD)->VD
      if(!is.element(class(longdata[,VD]), c("integer", "numeric"))) {
        if (okCancelBox("Vous n'avez pas choisi une variable dépendante numérique. La variable dépendante doit être numérique. Continuer ?")) vd.num<-FALSE  else return(AN.C.OVA())}else vd.num<-TRUE
    }
    
    if(!is.null(intra)) {
      if( min(table(longdata$IDeasy))!=  max(table(longdata$IDeasy)))  msgBox("Certains participants ont des valeurs manquantes sur les facteurs en mesures répétées. Ils vont être supprimés des analyses")
      
      while(min(table(longdata$IDeasy))!=  max(table(longdata$IDeasy))){
        names(table(longdata$IDeasy))[which.min(table(longdata$IDeasy))]->mid
        longdata[-which(longdata$IDeasy==mid) , ]->longdata
        factor(longdata$IDeasy)->longdata$IDeasy
      }       
    }
  }
  
  
  if(any(type.v=="Covariables")) {
    if(exists("diffs")) setdiff(names(longdata), c("IDeasy", "variable", "value", unlist(intra),VD, inter))->diffs else setdiff(names(longdata), c("IDeasy", "variable", "value",inter, VD, unlist(intra)))->diffs
    writeLines("Veuillez choisir la ou les covariables")
    cov<-dlgList(c(paste(diffs, "(format :",sapply(longdata[, diffs], class),")")), multiple = TRUE, title="Covariable-s?")$res
    if(length(cov) == 0L | cov=="aucune") return(AN.C.OVA())
    subset(listes, listes[,1] %in% cov)[,2]->cov
    as.character(cov)->cov
    longdata[complete.cases(longdata[,c(cov)]),]->longdata
  }else cov<-NULL
  
  if(length(intra)==1 & is.null(inter)) nlevels(longdata[,unlist(intra)])->N.modalites2 else {
    if(length(inter)==1 & is.null(intra)) nlevels(longdata[,unlist(inter)])->N.modalites2 else sapply(longdata[,c(inter, unlist(intra))],nlevels)->N.modalites2 }
  if(prod(N.modalites2)>3*length(longdata[,1])) return("Il n'y a pas assez d'observations pour réaliser l'analyse. Veuillez vérifier vos données et vous assurez qu'il y a au moins trois observations par modalité de chaque facteur")
  
  if(option) {options.aov(inter=inter, intra=intra)->options.out
    if(is.null(options.out)) return(AN.C.OVA())
    options.out$desires->desires
    options.out$desires2->desires2
    options.out$sauvegarde->sauvegarde
    options.out$ES->ES
    options.out$SumS->SumS}
  # REVOIR Pour FORMAT LONG      
  if(is.null(VD)) VD<-"value"
  
  longdata[complete.cases(longdata[,c(inter,unlist(intra), VD)]),]->longdata
  ftable(longdata[,c(inter,unlist(intra))])->aov.check
  if(any(is.na(aov.check)) || min(aov.check)<3) {msgBox("Certains groupes ont moins de 3 observations. Vérifiez vos données.")
    return(aov.check)
  }
  if(length(unique(longdata[,VD]))<3) return("La variable dépendante a moins de trois valeurs différentes. Vérifiez vos données ou l'analyse que vous tentez de réaliser n'est pas pertinente.")
  
  if(any(desires2%in%c("Modèle paramétrique","Modèle linéaire mixte"))){
    contrastes.ez(longdata, inter=inter, intra=intra)->cont
    if(is.null(cont)) return(AN.C.OVA()) 
    cont$type.cont->type.cont
    cont$p.adjust->p.adjust
    cont$contrastes->contrastes    
  } else{
    type.cont<-"aucun"
    p.adjust<-NULL
    contrastes<-NULL
  }
  
  anova2<-function(VD=NULL, inter=NULL, intra=NULL, longdata,  type.cont,p.adjust, SumS, desires2, cov=NULL, contrastes=NULL)   {
    
    list()->Resultats
    cov1<-NULL
    if(!is.null(cov)) { 
      for(i in 1:length(cov)) {paste0(cov1,cov[i],"+")->cov1}}
    
    if(!is.null(inter))  {pred.ind<-inter[1]  
    if(length(inter)>1) {
      for(i in 1:(length(inter)-1)){ paste(pred.ind, "*",inter[1+i])->pred.ind}}
    paste0("~1|IDeasy")->random}
    
    if(!is.null(intra))  {
      ez.principal<-intra[[1]]
      erreur<-paste0("+Error(", intra[[1]])
      random<-paste0("~1|IDeasy/", intra[[1]])
      if(length(intra)>1) {for(i in 1:(length(intra)-1)){
        paste(ez.principal, "*",intra[[i+1]])->ez.principal
        paste(erreur, "*", intra[[i+1]])->erreur
        paste0(random, "/", intra[[i+1]])->random
      }
      }
      paste(ez.principal, erreur,"|IDeasy)")->pred.rep
    }
    
    if(!is.null(inter) & !is.null(intra)) paste(pred.ind, "*",pred.rep)->predicteurs else {
      if(!is.null(inter) & is.null(intra)) paste0(pred.ind,"+Error(1|IDeasy)")->predicteurs else pred.rep->predicteurs
    }
    as.formula(paste0(VD, "~",cov1, predicteurs))->modele  
    modele->Resultats$"Modèle testé"
    
    psych::describeBy(longdata[,VD], longdata[ ,c(inter, unlist(intra))] ,mat=TRUE,type=3)->Resultats$"statistiques descriptives"$indices
    list()->aov.plus.in
    for(i in 1:length(c(inter, unlist(intra)))){
      combn(c(inter, unlist(intra)), i)->facteurs
      for(j in 1:ncol(facteurs)){
        psych::describeBy(longdata[,VD], longdata[ ,facteurs[,j]] ,mat=TRUE,type=3)->sd.aov
        
        if(nrow( facteurs) ==1) paste("Statistiques descriptives de la variable", facteurs[,j])->nsd else {
          paste("Statistiques descriptives de l'interaction entre", facteurs[1,j])->nsd
          for(k in 2: nrow( facteurs)){paste(nsd, ":",  facteurs[k,j])->nsd
          }
          
        }
        sd.aov->aov.plus.in[[nsd]]
      }
    }
    Resultats$"statistiques descriptives"$Information<-"Pour obtenir les statistiques descriptives par facteur, veuillez utiliser aov.plus() "
    
    if(any(Resultats$"statistiques descriptives"$n<2)) {
      "il y a moins de 3 observations pour un des groupes"-> Resultats$"information"
      return(Resultats)
    }  
    
    if(any(desires2=="Modèle paramétrique") | any(desires2=="Modèle linéaire mixte")){
      if(any(Resultats$"statistiques descriptives"$indices$sd==0)) Resultats$Avertissement<-"La variance d'au moins un groupe vaut 0. Les résultats risquent d'être considerablement biaisés"  
      
      if(exists("pred.ind") & exists("ez.principal")) paste(pred.ind, "*",ez.principal)->predicteurs else {
        if(exists("pred.ind") & !exists("ez.principal")) pred.ind->predicteurs else ez.principal->predicteurs}
      lm(as.formula(paste0(VD,"~",predicteurs)),na.action=na.exclude, data=longdata)->lm.r1
      resid(lm.r1)->longdata$residu
      assign(x="longdata", value=longdata, envir=.e)
      if(length(longdata$residu)<5000){
        shapiro.test(longdata$residu)->Shapiro_Wilk # realise le Shapiro-Wilk
        lillie.test(longdata$residu)->Lilliefors  # realise le Lilliefors
        round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
        names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
        dimnames(normalite)[1]<-" "
        format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->Resultats$"Tests de normalité"}
      h<-hist(longdata$residu, breaks=10, density=10, col="black", xlab="residus", main="Distribution des résidus") 
      xfit<-seq(min(longdata$residu),max(longdata$residu),length=40) 
      yfit<-dnorm(xfit,mean=mean(longdata$residu),sd=sd(longdata$residu)) 
      yfit <- yfit*diff(h$mids[1:2])*length(longdata$residu) 
      lines(xfit, yfit, col="darkblue", lwd=2) 
      if(!is.null(cov) & !is.null(inter)){
        options(contrasts = c("contr.helmert", "contr.poly"))
        for(i in 1:length(cov)){
          aov(as.formula(paste0(cov[i], "~",pred.ind)), data=longdata)->aov.cov
          Anova(aov.cov, type="III")->aov.cov
          names(aov.cov)<-c("SC", "ddl", "F", "valeur.p")
          aov.cov->Resultats$"Conditions d'application de l'ancova"[[paste0("Test de l'absence de différence entre les groupes sur ", cov[i])]]
          if(i==1) {paste(cov[1],"*")->cov2} else {paste0(cov2, cov[i],"*")->cov2}
        }
        aov(as.formula(paste0(VD, "~", cov2,pred.ind)), data=longdata)->aov.cov
        Anova(aov.cov, type="III")->aov.cov
        names(aov.cov)<-c("SC", "ddl", "F", "valeur.p")
        aov.cov-> aov.cov->Resultats$"Conditions d'application de l'ancova"$"Test de l'homogénéité des pentes entre les groupes sur la variable dépendante"
        
      }
      if(any(desires2=="Modèle paramétrique")){
        if(!is.null(inter)){
          paste0(VD, "~",pred.ind)->modele2
          leveneTest(as.formula(modele2),data=longdata)->Levene # test de Levene pour homogeneite des variances
          round(unlist(Levene)[c(1,2,3,5)],3)->Levene
          names(Levene)<-c("ddl1","ddl2","F","valeur.p")
          Levene->Resultats$"Test de Levene vérifiant l'homogénéité des variances"
        }
        options(contrasts=c("contr.sum","contr.poly"))
        if(!is.null(cov)) factorize<-FALSE else factorize<-TRUE
        aov_4(as.formula(modele),data=longdata, es_aov=ES, type=SumS,factorize=factorize)->aov.out
        
        if(length(c(inter, unlist(intra)))>1) {
          c(unlist(intra), inter)->intrainter
          graph.modele<-paste0(intrainter[1],"~",intrainter[2])
          if(length(intrainter)>2){paste0(graph.modele, "|",intrainter[3] )->graph.modele
            if(length(intrainter)>3){ for(i in 4:length(intrainter)){paste0(graph.modele, "*",intrainter[i] )->graph.modele} 
              
            }} 
          x11()
          lsmip(aov.out,as.formula(graph.modele))
        }
        
        
        summary(aov.out)->aov.out2 
        nice(aov.out, correction="none", intercept=T, es=ES,type=SumS)->aov.out
        names(aov.out)<-c("Effet","ddl.num, ddl.denom", "CME", "F", names(aov.out)[5], "valeur.p" )
        format(aov.out, width = max(sapply(names(aov.out), nchar)), justify = "centre")->aov.out
        format(names(aov.out), justify = "centre")->names(aov.out)
        if(!is.null(intra) && any( sapply(longdata[,c(unlist(intra))],nlevels)>2)) {
          round(aov.out2$sphericity.test,5)->Resultats$"test de Mauchly testant la sphéricité de la matrice de covariance"
        }
        
        aov.out->Resultats$"Analyse principale"
        if(!is.null(intra) && any( sapply(longdata[,c(unlist(intra))],nlevels)>2)) {data.frame(round(aov.out2$pval.adjustments,5))->GG.HF
          names(GG.HF)<-c("GG.eps", "GG.valeur.p","HF.eps", "HF.valeur.p")
          GG.HF->Resultats$"Correction de Greenhouse-Geisser et de  Hyunh-Feldt"}
        if(length(inter)==1 & is.null(intra) & is.null(cov)) {oneway.test(as.formula(paste(VD,"~", inter)),data=longdata)->Welch
          round(data.frame("F"=Welch$statistic,"ddl.num"=Welch$parameter[1],"ddl.denom"=Welch$parameter[2],"valeur.p"=Welch$p.value),4)->Welch
          Welch->Resultats$"Anova avec correction de Welch pour variances hétérogènes"
        }  
      }
      
      if(type.cont=="a priori" | any(desires2== "Modèle linéaire mixte")){
        if(type.cont=="a priori" ){
          for(i in 1:length(contrastes)){
            contrastes[[i]]->contrasts(longdata[,names(contrastes)[i]])
          }
          
        }
        modele.lme<-paste0(VD, "~",cov1, predicteurs) 
        #   paste0("lme(", modele.lme, ", random=", random, ",data=", ifelse(!is.null(intra) , paste0(nom,".format.long"), nom),
        #         ", method='REML')->modele.lme")->Resultats$"Modèle linéaire mixte"
        
        try(  eval(parse(text=paste0("lme(", modele.lme, ", random=", random, ",data= longdata, method='REML')"))),silent=T)->modele.lme1        
        if(class(modele.lme1)=="try-error"){
          while(class(modele.lme1)=="try-error")
            dlgMessage("Le modèle n a pas pu converger. Vous pouvez modifier les paramètres de convergence ou abandonner. Voulez-vous modifier les paramètres de convergence ?", "yesno")$res->modele.lme1
          if(modele.lme1=="yes"){
            Form <- list("maxIter:NUM"=50, "msMaxIter:NUM"=50, "niterEM:NUM"=25)
            dlgForm(Form, "Paramètres du modèle LME")$res->Form  
            if(any(is.na(unlist(Form))))  Form <- list("maxIter:NUM"=50, "msMaxIter:NUM"=50, "niterEM:NUM"=25)
            lmeControl(maxIter=Form$maxIter,msMaxIter=Form$msMaxIter,niterEM=Form$niterEM )->controle
            #try( lme(as.formula(modele.lme), random=as.formula(random), data=longdata, method="REML", control=controle),silent=T)->modele.lme
            try(eval(parse(text=paste0("lme(", modele.lme, ", random=", random, ",data= longdata, method='REML')"))),silent=T)->modele.lme1
            
          }
        }
        if(class(modele.lme1)=="lme"){ 
          modele.lme1->aov.plus.in$modele.lme1
          if(any(desires2== "Modèle linéaire mixte")) anova(modele.lme1)->Resultats$"modèle linéaire mixte avec comme estimateur le maximum de vraisemblance - REML"
          if(type.cont=="a priori"){
            contrastes->Resultats$"Contrastes a priori"$"Matrice de coefficients variables"  
            round(summary(modele.lme1)$tTable,4)->tableT
            data.frame(tableT)->tableT
            names(tableT)<-c("estimateur", "erreur.st", "ddl","valeur.t", "valeur.p")
            round(tableT$valeur.t^2/(tableT$valeur.t^2+tableT$ddl),4)->tableT$R.deux
            if(!is.null(inter)) {
              grepl(paste(inter,collapse = "|"), unlist(dimnames(tableT)[1]))->tableT$D.Cohen
              round( ifelse(tableT$D.Cohen==T, (2*tableT$valeur.t)/(nlevels(longdata$IDeasy)^0.5), tableT$valeur.t/(nlevels(longdata$IDeasy)^0.5)),4)->tableT$D.Cohen
            }else round(tableT$valeur.t/((nlevels(longdata$IDeasy))^0.5),4)->tableT$D.Cohen
            
            tableT[1,"D.Cohen"]<-""
            tableT[1,"R.deux"]<-""
            tableT->Resultats$"Table des contrastes sur le modele linéaire mixte"
          }
        }
        
        
        if(!is.null(intra) & is.null(inter) & is.null(cov) & type.cont=="a priori"){
          longdata[do.call("order", longdata[unlist(intra)]), ]->longdata
          list()->combinaison
          for(i in 1:length(contrastes)){ combn(1:length(contrastes), i)->combinaison[[i]]        }
          Table.contrastes<-c()
          for(i in 1:length(combinaison) ){
            
            for(j in 1:ncol(combinaison[[i]])){
              M1<-matrix(rep(1, length(longdata[,VD])), ncol=1)
              for(k in 1:nrow(combinaison[[i]])){
                M2<-c()
                for(l in 1:ncol(contrastes[[combinaison[[i]][k,j]]])){
                  rep(contrastes[[combinaison[[i]][k,j]]][,l], each=length(longdata[,VD])/prod(N.modalites2[1:combinaison[[i]][k,j]]), len =length(longdata[,VD]))->coef1
                  cbind(M2,coef1)->M2
                  
                }
                M4<-c()
                for(m in 1:ncol(M1))  {
                  for(n in 1 : ncol(M2)){
                    M1[,m]*M2[,n]->M3
                    cbind(M4, M3)->M4
                  }
                  
                }
                M4->M1
              }
              for(o in 1:ncol(M1)){
                longdata[,VD]*M1[,o]->coef1
                t.test(rowSums( matrix(coef1, ncol=prod(N.modalites2))), mu = 0, paired = FALSE, conf.level = 0.95)->C1
                rbind(Table.contrastes,c(C1$estimate, C1$parameter, C1$statistic, C1$p.value))->Table.contrastes
                
              }
            }
            
          }
          
          round(Table.contrastes,4)->Table.contrastes
          data.frame(Table.contrastes)->Table.contrastes  
          names(Table.contrastes)<-c("estimateur", "ddl","valeur.t", "valeur.p")
          dimnames(Table.contrastes)[[1]]<-dimnames(tableT)[[1]][-1]
          Table.contrastes$valeur.t^2/(Table.contrastes$valeur.t^2+Table.contrastes$ddl)->Table.contrastes$R.deux
          round(Table.contrastes$valeur.t/(nlevels(longdata$IDeasy))^0.5,4)->Table.contrastes$D.Cohen
          Table.contrastes->Resultats$"Table des contrastes imitant les logiciels commerciaux"
          
        } 
        
      }
      
      if(type.cont== "Comparaison 2 à 2"){
        c(inter, unlist(intra))->interintra
        list()[1:length(interintra)]->comparaisons
        names(comparaisons)<-interintra
        for(i in 1:length(interintra)){
          if(interintra[i] %in% intra) {pairwise.t.test(longdata[,VD],longdata[,interintra[[i]]], paired=T,p.adj=p.adjust)$p.value->comparaisons[[i]]$"table des probabilités"}else{
            pairwise.t.test(longdata[,VD],longdata[,interintra[[i]]], paired=F,p.adj=p.adjust)$p.value->comparaisons[[i]]$"table des probabilités"
          }
        }
        Resultats$"Comparaisons 2 à 2"<-comparaisons
      }
    }
    
    assign("aov.plus.in",aov.plus.in,envir=.e)
    if(any(desires2=="Modèle non paramétrique" )){
      if(!is.null(inter)){
        kruskal.test(as.formula( paste0(VD, "~",inter[1])), data = longdata)->KW
        round(data.frame(KW$statistic,KW$parameter,KW$p.value),4)->KW
        names(KW)<-c("H","ddl","valeur.p")
        round((KW$H-nlevels(longdata[,inter])+1)/(length(longdata[,1])-nlevels(longdata[,inter])),4)->eta
        if(eta<0.0001) "<0.001"->KW$eta.carré.de.H else KW$eta.carré.de.H
        round(KW$H/((length(longdata[,1])^2-1)/(length(longdata[,1])+1)),4)->KW$espilon.carré
        KW->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis"
        
        if(!is.null(contrastes) && any(rowSums((contrastes[[1]]!=0))==0)) {kruskalmc( as.formula(paste0(VD, "~",inter[1])), 
                                                                                      data=longdata, cont='two-tailed')->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis - Comparaison à une ligne de base"} else{
                                                                                        kruskalmc( as.formula( paste0(VD, "~",inter[1])), data=longdata)->Resultats$"Analyse non paramétrique"$"Test de Kruskal-Wallis - Comparaison deux à deux"   
                                                                                        
                                                                                      }
      }else{
        friedman.test(as.formula(paste(VD,"~", intra[[1]], "|IDeasy" )),data=longdata)->friedman
        round(data.frame(friedman$statistic,friedman$parameter,friedman$p.value),4)->friedman
        names(friedman)<-c("chi.deux","ddl","valeur.p")
        round(friedman$chi.deux/(length(longdata[,1])*(nlevels(longdata[,unlist(intra)])-1)),4)->friedman$W.de.Kendall
        friedman->Resultats$"Analyse non paramétrique"$"Anova de Friedman"
        friedmanmc(longdata[,VD], longdata[,intra[[1]]], longdata$IDeasy)->Resultats$"Comparaison 2 à 2 pour ANOVA de Friedman"
      }
    }
    
    if(any(desires2=="Statistiques robustes - peut prendre du temps")){
      if(length(inter)==1 & is.null(intra)){
        if(is.null(contrastes)) Contrasts(levels(longdata[,inter]))->contrastes else contrastes[[1]]->contrastes
        split(longdata[,VD], longdata[,inter])->robuste
        try(unlist(WRS::med1way(robuste,iter = 1000)), silent=T)->mediane
        if(class(mediane)!="try-error"){
          names(mediane)<-c("Test", "Valeur.critique","valeur.p")
          round(mediane,4)->Resultats$"Anova basée sur les médianes"$"Analyse principale"
          WRS::medpb(robuste,alpha=.05,nboot=1000,con=contrastes,bhop=FALSE)->cont
          dimnames(cont$output)[[2]]<-c("Numéro.contraste","Valeur.contraste",
                                        "valeur.p","p.critique.corrigée","lim.inf.IC","lim.sup.IC")
          cont$output->Resultats$"Anova basée sur les médianes"$"Contrastes"
        }else {
          "Désolé, nous n'avons pas pu calculé l'anova sur les médianes, possiblement en raison d'un nombre import d'ex aequo."->Resultats$"Anova basée sur les médianes"
        }
        try( WRS2::t1way(as.formula(paste0(VD, "~",inter)), tr=.2,data=longdata),silent=T)->AR1
        if(class(AR1)!="try-error"){
          WRS2::t1way(as.formula(paste0(VD, "~",inter)), tr=.2,data=longdata)->AR1
          WRS2::t1waybt(as.formula(paste0(VD, "~",inter)), tr=.2, nboot=2000,data=longdata)->AR2
          data.frame(AR1[[2]],AR1[[3]],AR1[[1]],AR2[[2]],AR2[[3]],AR2[[4]], AR2[[5]])->AR1
          names(AR1)<-c("ddl.num","ddl.denom","Stat","valeur.p","Var.expliquée","Taille.effet","Nombre.bootstrap" )
          AR1->Resultats$"Anova basée sur les moyennes tronquées"$"Analyse principale"
          "Les probabilités et les IC sont estimés sur la base d'un bootsrap. L'IC est corrigé pour comparaison multiple, contrairement à la probabilité reportée"->Resultats$"Anova basée sur les moyennes tronquées"$"Information"
          try(WRS::lincon(robuste, tr=.2, con=contrastes),silent=T)->cont
          try(WRS::mcppb20(robuste, tr=.2, nboot=2000, con=contrastes),silent=T)->cont2
          if(class(cont)!= "try-error") {data.frame(cont$psihat[,2],cont$test[,4],cont$test[,5],cont$test[,2],cont$test[,3],cont2$psihat[,4],cont2$psihat[,5],cont2$psihat[,6])->cont
            names(cont)<-c("Valeur.contraste","erreur.standard","ddl","test","seuil.critique","lim.inf.IC","lim.sup.IC","valeur.p")
            cont->Resultats$"Anova basée sur les moyennes tronquées"$"Contrastes"}
          if(class(cont2)!="try-error") cont2[3]->Resultats$"Anova basée sur les moyennes tronquées"$"Coefficients des contrastes"
          
        }else{
          "Désolé, nous n'avons pas pu calculé l'anova sur les moyennes tronquées."->Resultats$"Anova basée sur les moyennes tronquées"
        }
        
      }
      
      
      if(length(inter)==2 & is.null(intra)) { 
        
        
        try( WRS2::t2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, tr = 0.2), silent=T)->T2
        if(class(T2)!="try-error"){
          round(matrix(unlist(T2[1:6]), ncol=2, byrow=T),4)->T2
          dimnames(T2)[[2]]<-c("valeur", "valeur.p")
          c(names(longdata[,inter]), paste(names(longdata[,inter])[1],":",names(longdata[,inter])[2]))->dimnames(T2)[[1]]
          T2->Resultats$"ANOVA sur moyennes tronquées a 0.2"$"Analyse principale"
        }
        try(WRS2::pbad2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "mom", nboot = 599)->Resultats$"ANOVA sur M estimator"$"Analyse principale",silent=T)
        try(WRS2::pbad2way(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median", nboot = 599)->Resultats$"ANOVA sur les médianes"$"Analyse principale",silent=T)
        try(model.matrix(mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median"))->Resultats$"Comparaisons post hoc"$"Matrice de contrastes", silent=T)
        try(WRS2::mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "mom", nboot = 599), silent=T)->mediane
        if(class(mediane)!="try-error") {
          paste0("WRS2::mcp2a(formula = ", paste0(VD, "~", inter[1], "*", inter[2]), ", data = longdata, est = 'mom', nboot = 599)")->mediane$call
          mediane->Resultats$"Comparaisons post hoc"$"ANOVA sur le M estimator"
        }
        
        try(WRS2::mcp2a(as.formula(paste0(VD, "~",inter[1],"*",inter[2])), data=longdata, est = "median", nboot = 599), silent=T)->mediane
        if(class(mediane)!="try-error") {
          paste0("WRS2::mcp2a(formula = ", paste0(VD, "~", inter[1], "*", inter[2]), ", data = longdata, est = 'median', nboot = 599)")->mediane$call
          mediane->Resultats$"Comparaisons post hoc"$"ANOVA sur la médiane"
        }
      }
      
      if(length(inter)==3 & is.null(intra)){
        try( WRS2::t3way(as.formula(paste0(VD, "~",inter[1],"*",inter[2],"*",inter[3])), data=longdata, tr = 0.2), silent=T)->tronquees
        if(class(tronquees)!="try-error") {paste0("WRS2::t3way(", VD, "~",inter[1],"*",inter[2],"*",inter[3], ", data=longdata, tr = 0.2)")->tronquees$call
          tronquees->Resultats$'Anova sur les moyennes tronquées'  
        }
      }
      if(length(intra)==1 & is.null(inter)){
        try( rmanova(longdata$value,longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->ANOVA.tr
        if(class(ANOVA.tr)!="try-error"){
          round(data.frame("Valeur.test"= ANOVA.tr$test,"ddl1"=ANOVA.tr$df1, "ddl2"=ANOVA.tr$df2,"valeur.p"=ANOVA.tr$p.value),4)->ANOVA.tr
          ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronqueés à 20%"
          if((nlevels(longdata[,intra[[1]]]))>2) {rmmcp(longdata[,VD],longdata[, intra[[1]]],longdata$IDeasy)->comp
            comp$call<-paste0("rmmcp(longdata$", VD, ", longdata$", intra[[1]], ",longdata$IDeasy")
            comp->Resultats$"Statistiques robustes"$"Comparaisons 2 à 2 sur moyennes tronquées à 20%"}else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        }
        try( rmanovab(longdata[,VD],longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->ANOVA.tr
        if(class(ANOVA.tr)!="try-error"){
          data.frame("Valeur.test"=ANOVA.tr[[1]],"Valeur critique"=ANOVA.tr[[2]], "significativité"=if(ANOVA.tr[[1]]<ANOVA.tr[[2]]){"non significatif"}else"significatif")->ANOVA.tr
          ANOVA.tr->Resultats$"Statistiques robustes"$"Anova sur moyennes tronquées à 20% avec bootstrap"   
        }else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        
        if((nlevels(longdata[,intra[[1]]]))>2) {
          try(pairdepb(longdata[,VD],longdata[,intra[[1]]] ,longdata$IDeasy), silent=T)->comp
          if(class(comp)!="try-error") {paste0("pairdepb(y = longdata$", VD, ", groups = longdata$", intra[[1]],", blocks = longdata$IDeasy)" )->comp$call
            comp->Resultats$"Statistiques robustes"$"Comparaisons 2 à 2 sur les moyennes tronquées à 20% avec bootsrap"}else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        }
      } 
      
      if(length(inter)==1 & length(intra)==1){
        as.formula(paste0(VD, "~", predicteurs))->modeleR
        try(WRS2::tsplit( modeleR, IDeasy, data=longdata, tr = 0.2), silent=T)->tronquees
        if(class(tronquees)!="try-error"){
          tronquees$call<- paste0("WRS2::tsplit(", VD,"~", intra[[1]],"*", inter, ", IDeasy, data=longdata, tr = 0.2)")
          tronquees->Resultats$'Anova sur les moyennes tronquées' # anova mixte sur moyennes tronquÃÂÃÂÃÂÃÂ©es 
          WRS2::sppba(modeleR, IDeasy, data=longdata, est = "mom", avg = TRUE, nboot = 500, MDIS = FALSE)->MoMa # anova sur moyenne oÃÂÃÂÃÂÃÂ¹ on enlÃÂÃÂÃÂÃÂ¨ve les valeurs aberrantes avec bootstrap pour l'effet de A
          WRS2::sppbb(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMb# anova avec bootstrap pour l'effet de B
          WRS2::sppbi(modeleR, IDeasy, data=longdata, est = "mom", nboot = 500)->MoMi # # anova avec bootstrap pour l'effet d'interaction
          data.frame("effet"= c(inter,intra[[1]],"interaction"), "valeur.p"=c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )->MoM
          MoM->Resultats$"Anova sur l'estimateur modifié de localisation de Huber"
        }else Resultats$"Statistiques robustes"<-"Désolé, nous n'avons pas pu calculé l'anova robuste"
        
      }
      
    }
    return(Resultats)
  }   
  
  list()->aov.plus.list
  anova2(VD=VD, inter=inter, intra=intra, longdata=longdata,  type.cont=type.cont, SumS=SumS,
         desires2=desires2, cov=cov, p.adjust=p.adjust, contrastes=contrastes)->complet
  if(any(desires=="Données complètes")){
    complet->Resultats$"Données complètes"
    aov.plus.in->aov.plus.list$"Données complètes"}
  
  if(any(desires=="Identification des outliers")|any(desires=="Données sans valeur influente")) { 
    if(is.null(longdata$residu)) {"L'analyse n'a pas pu aboutir"->Resultats$"Arrêt prématuré de l'analyse"
      return(Resultats)}
    valeurs.influentes(X="residu", critere="Grubbs",z=3.26, data=longdata)->influentes
    
    if(any(desires=="Identification des outliers")) influentes->Resultats$"Valeurs influentes"
    if(any(desires=="Données sans valeur influente")){
      if(!is.null(influentes$"observations influentes"$IDeasy)){
        setdiff(longdata$IDeasy,influentes$"observations influentes"$IDeasy)->diffs
        longdata[which(longdata$IDeasy%in%diffs), ]->nettoyees
        factor(nettoyees$IDeasy)->nettoyees$IDeasy
        anova2(VD=VD, inter=inter, intra=intra, longdata=nettoyees, type.cont=type.cont, SumS=SumS,
               desires2=desires2, cov=cov,p.adjust=p.adjust, contrastes=contrastes)->Resultats$"Données sans valeur influente"
        aov.plus.in->aov.plus.list$"Données sans valeur influente"
      }
      if(all(desires!="Données complètes"))  complet->Resultats$"Données sans valeur influente"
      
    }
    
  }
  class(aov.plus.list)<-"aovplus"
  assign("aov.plus.in", aov.plus.list,envir=.GlobalEnv) 
  ref1(packages)->Resultats$"Références des packages utilisés pour cette analyse"
  if(sauvegarde==T) save(Resultats=Resultats ,choix =paste("anova sur", nom), env=.e)
  return(Resultats)
  }


aov.plus<-function(aov.plus.list=NULL, info=T){
  options (warn=-1)
  packages<-c("psych","svDialogs","phia")
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)}
  
  contrastes.ez2<-function(longdata, var=NULL){
    Resultats<-list()
    contrastes<-list()
    for(i in 1:length(var)){
      matrix(rep(0,times=nlevels(longdata[,var[i]])), nrow=nlevels(longdata[,var[i]]))->contrastes3
      dimnames(contrastes3)[[1]]<-levels(longdata[,var[i]])
      fix(contrastes3)->contrastes3
      contrastes3[which(is.na(contrastes3))]<-0
      b<-rle(c(contrastes3))
      if(b$values[which.max(b$lengths)]==0 & max(b$lengths)>2*(nlevels(longdata[,var[i]])-2)) {
        if(okCancelBox("il ne peut pas y avoir uniquement des 0 dans une colonne. Appuyez sur ok pour continuer, et annuler pour annuler")) {return(contrastes.ez2(longdata, var))}else return(NULL)
      }
      
      paste0(contrastes3, "*",dimnames(contrastes3)[[1]])->noms
      for(j in 1:(length(noms)/length(dimnames(contrastes3)[[1]]))){
        inf<-1+(length(dimnames(contrastes3)[[1]])*(j-1))
        sup<-j*length(dimnames(contrastes3)[[1]])
        paste(noms[inf:sup],collapse="")->nom
        nom->dimnames(contrastes3)[[2]][j]
      }
      contrastes[[i]]<-contrastes3
    }
    names(contrastes)<-var
    return(contrastes)
  }
  
  if(is.null(aov.plus.list)){
    Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
    if(length(nom1)==0) {
      writeLines("il n'y a pas d'objet compatible avec aov.plus dans la mémoire de R. Vous devez réaliser une analyse de variance au préalable")
      return(AN.C.OVA())}
    if(length(nom1)==1)  aov.plus.list<-get(nom1) else{
      if(info=="TRUE") writeLines("Veuillez choisir le modèle que vous désirez analyser avec aov.plus")
      nom1 <- dlgList(nom1, multiple = FALSE, title="Modèle ?")$res
      if(length(nom1)==0) {nom1<-NULL
      aov.plus.list<-NULL}
      if(!is.null(nom1))  aov.plus.list<-get(nom1)
    } 
  }
  
  .e <- environment()
  if(is.null(aov.plus.list)) {
    return(writeLines("La fonction aov.plus nécessite qu'une anova ait été réalisée. 
                      Pour pouvoir utiliser l'ensemble des options, il est nécessaire d'avoir choisi modèle linéaire mixte"))
  }
  if(length(aov.plus.list)==2){
    writeLines("Voulez-vous réaliser les analyses sur les données complètes ou sur les données sans les valeurs influentes ?")
    type<-dlgList(names(aov.plus.list), multiple = FALSE, title="Quelles données voulez-vous analyser?")$res
    print(type)
    if(length(type)==0) return("vous avez quitté aov.plus")
    if(type=="Données complètes") aov.plus.list[[1]]->aov.plus.list else aov.plus.list[[2]]->aov.plus.list
  }else aov.plus.list[[1]]->aov.plus.list
  
  if(length(grep("modele.lme1", names(aov.plus.list))!=0)) {
    aov.plus.list[[grep("modele.lme1", names(aov.plus.list))]]->modele.lme
    aov.plus.list[[grep("modele.lme1", names(aov.plus.list))]]<-NULL
    writeLines("Cette fonction permet de fournir les statistiques descriptives détaillées par variable avec le choix statistiques
               descriptives complètes. Vous pouvez afficher les moyennes et erreurs-types ajustées ainsi que le graphique correspondant.
               Avec le choix post hoc sur les interactions, vous pouvez tester les effets d'interaction 2 à 2 et les effet simpes.")
    choix<-dlgList(c("statistiques descriptives détaillées","moyennes et erreurs-types ajustées","contrastes sur les interactions"), 
                   multiple = TRUE, title="Quelles données voulez-vous analyser?")$res 
    if(length(choix)==0) return(analyse())
  }else{writeLines("Cette fonction permet de fournir les statistiques descriptives détaillées par variable avec le choix statistiques
descriptives complètes.")
    choix<-"statistiques descriptives détaillées"
  }
  
  Resultats<-list()
  if(any(choix=="statistiques descriptives détaillées")){
    writeLines("Veuillez choisir les variables ou combinaison de variables pour lesquelles vous désirez afficher les statistiques descriptives")
    vars<-dlgList(names(aov.plus.list), multiple = TRUE, title="Que voulez-vous afficher ?")$res
    if(length(vars)==0) return(aov.plus())
    for(i in 1:length(vars)){View(aov.plus.list[[vars[i]]],title= vars[i])}    
  }
  
  if(any(choix=="moyennes et erreurs-types ajustées")){
    writeLines("Pour quels (combinaison de) facteurs désirez-vous afficher les moyennes ajustées ?")
    facteurs<-dlgList(names(modele.lme$contrasts), multiple = TRUE, title="Que voulez-vous afficher ?")$res
    if(length(facteurs)==0) return(aov.plus()) 
    # rajouter la pente
    means.lme <- interactionMeans(modele.lme, facteurs)
    plot(means.lme, abbrev.levels=TRUE)
    recordPlot()->graphe
    Resultats$"Moyennes ajustée"<- means.lme
  }
  
  if(any(choix=="contrastes sur les interactions")){
    writeLines("Vous pouvez sélectionner plusieurs options. Quelles options, voulez-vous spécifier ? Les comparaisons 2 à 2 vous permettent d'avoir les comparaisons 2 à 2;
               Spécifier les contrastes vous permettent de tester virtuelle n'importe quel contraste. Si plusieurs variables sont introduites pour 
               l'ensemble des deux options, seuls les contrastes d'interaction seront calculés.
               La décomposition des effets va vous peremttre d'obtenir les comparaisons spécifiées dans les deux options précédentes pour chaque modalité des variables spécifiées à ce niveau.
               Toutes les variables ne doivent pas nécessairement être introduites dans l'analyse. Dans ce cas, les contrastes choisis seront calculés sur
               l'ensemble des modalités confondues")
    choix<-dlgList(c("Comparaison 2 à 2", "Spécifier contrastes", "Décomposer les effets par modalité"), multiple = TRUE, title="Que voulez-vous spécifier ?")$res
    if(length(choix)==0) return(aov.plus())
    facteurs<-names(modele.lme$contrasts)
    if(any(choix=="Comparaison 2 à 2")) {   
      paires<-dlgList(facteurs, multiple = TRUE, title="Comparaison 2 à 2 ?")$res
      if(length(paires)==0) return(aov.plus())
    }else paires<-NULL
    setdiff(facteurs, paires)->diff
    if(length(diff)!=0 & any(choix=="Spécifier contrastes")) {   
      cont.spe1<-dlgList(diff, multiple = TRUE, title="Variables à spécifier ?")$res
      if(length(cont.spe1)==0) return(aov.plus())
      contrastes.ez2(longdata=modele.lme$data, var=cont.spe1)->cont.spe
    }else {cont.spe<-NULL
    cont.spe1<-NULL}
    setdiff(diff, cont.spe1)->diff
    
    if(length(diff)!=0 & any(choix=="Décomposer les effets par modalité")){
      fixed1<-dlgList(diff, multiple = TRUE, title="Variable à décomposer ?")$res
      if(length(fixed1)==0) return(aov.plus())}else {
        fixed1<-NULL 
      }
    dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
    if(length(p.adjust)==0) p.adjust<-"none"
    testInteractions(modele.lme,pairwise=paires, fixed=fixed1, adjustment=p.adjust, custom=cont.spe)->Resultats$"Contrastes d'interaction"
  }
  
  
  ref1(packages)->Resultats$"Références des packages utilisés pour cette analyse"
  writeLines("Voulez-vous sauvegarder les résultats de l'analyse ?")
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-F
  if(sauvegarde==T) save(Resultats=Resultats ,choix ="Resultats.aov.plus", env=.e)
  
  return(Resultats)
  
  }

### chi deux #### 
# fonction OK 

chi<-function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, sauvegarde=F,choix=NULL, data=NULL, info=TRUE, n.boot=NULL, priorConcentration =1,  
              SampleType=NULL,fixedMargin=NULL, choix2=c("test non paramétrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens") ,rscale=2^0.5/2){
  # X = character or vector.  First set of variables
  # Y = character or vector. Second set of variables
  # Effectifs = character. Name of weighting variable. Must be positive integer
  # save = logical. Should the results be saved ?
  # p = vector of probabilities. Must be equal to 1. The lenght must be equel to number of levels of X
  # choix = character. One among "Ajustement", "Indépendance", or "Test de McNemar"
  # data = name of the dataframe 
  # B = number of bootstrap fro computing p.values by Monte-Carlo simulation
  # priorConcentration : prior concentration paramter, set to 1 by default (see ?contingencyTableBF)
  # SampleType : the sampling plan (see details)
  # fixedMargin : for the independent multinomial sampling plan, which margin is fixed ("rows" or "cols")
  # rscale : prior scale. A number of preset values can be given as strings
  chi.in<-function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL){
    if(!is.null(choix)) dial<-F else dial<-T
    if(is.null(choix) || (choix %in%c("Ajustement", "Indépendance", "Test de McNemar")==FALSE)){
      if(info) writeLines("Veuillez préciser le type de chi carré que vous souhaitez réaliser.")
      choix<- dlgList(c("Ajustement", "Indépendance", "Test de McNemar"), preselect="Indépendance", multiple = FALSE, title="Type de khi deux")$res
      if(length(choix)==0) return(NULL)
    }
    
    choix.data(data=data, info=info, nom=T)->data
    if(length(data)==0) return(NULL)
    data[[1]]->nom
    data[[2]]->data
    msg3<-"Veuillez choisir le premier set de facteur(s) catégoriel(s)"
    if(choix=="Indépendance") multiple<-T else multiple<-F
    X<-.var.type(X=X, info=info, data=data, type="factor", check.prod=F, message=msg3,  multiple=multiple, title="Variable-s", out=NULL)
    if(is.null(X)) {
      chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
      return(Resultats)}
    X$data->data
    X$X->X
    
    if(choix!="Ajustement"){
      msg4<-"Veuillez choisir le second set de facteur(s) catégoriel(s)"
      Y<-.var.type(X=Y, info=info, data=data, type="factor", check.prod=F, message=msg4,  multiple=multiple, title="Variable-s", out=NULL)
      if(is.null(Y)) {
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
        return(Resultats)}
      Y$data->data
      Y$X->Y
      if(choix=="Test de McNemar" & any(sapply(data[,c(X,Y)],nlevels)!=2)) {
        msgBox("Le test de McNemar implique un tableau 2x2. Les dimensions de votre tableau sont différentes.")
        print(table(data[,X], data[,Y], dnn=c(X,Y)))
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
        return(Resultats)
      }
    }
    
    if(dial){       
      if(info==T) writeLines("Faut-il pondérer l'analyse par une variable effectif ?")
      Effectifs<-dlgList(c("oui", "non"), multiple = F, preselect="non", title="Spécifier effectifs ?")$res
      if(length(Effectifs)==0) {
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
        return(Resultats)}
      if(Effectifs=="non") Effectifs<-NULL}
    
    if(!is.null(Effectifs)){
      msg5<-"Veuillez choisir la ou les variables définissant les effectifs"
      .var.type(X=Effectifs, info=T, data=data, type="integer", message=msg5,multiple=F, title="Spécifier la vriable effectifs ?", out=c(X, Y))->Effectifs
      if(is.null(Effectifs)) {
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
        return(Resultats)}
      Effectifs$X->Effectifs
    }
    
    # check variable
    if(!is.null(Effectifs)) sum(data[,Effectifs])->tot else length(data[,1])->tot
    if(choix!="Ajustement") {
      expand.grid(X, Y)->comb
      comb[which(as.vector(comb[,1])!=as.vector(comb[,2])),]->comb
      if(any(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)){
        which(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)->trop
        for(i in length(trop):1){
          msg6<-paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". Cette analyse ne sera pas réalisée.")
          msgBox(msg6)
          comb[ -which(dimnames(comb)[[1]]==names(trop)[i]),]->comb
        }
        if(length(comb[,1])==0) {
          msgBox("Les variables que vous avez choisies pour réaliser votre analyse ne permettent de faire aucune analyse. Veuillez redéfinir votre analyse")
          return(NULL)
        } 
      }
    }
    
    if(choix=="Ajustement") {
      if(dial==F & is.null(p)) rep(1/nlevels(data[,X]),times=nlevels(data[,X]))->p
      if(sum(p)!=1 | any(p)>1 | any(p)<0) p<-NULL
      
      while(is.null(p)){
        if(info==T) writeLines("Veuillez entrer les probabilités correspondant à chaque modalité de la variable.")
        dlgForm(setNames(as.list(rep(1/nlevels(data[,X]),times=nlevels(data[,X]))), levels(data[,X])), "Vecteur des probabilites. Attention : ne pas entrer des fractions")$res->niveaux
        stack(niveaux)[,1]->p
        if(sum(p)!=1 ||length(p)!=nlevels(data[,X]) | any(p)>1 | any(p)<0){
          if( dlgMessage("La somme des probabilités est différente de 1 ou le nombre de probabilités ne correspond pas au nombre de modalités de la variable.
                         Veuillez entrer un vecteur de probabilités valide","okcancel")$res=="cancel") {
            chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
            return(Resultats)} else return(NULL)
      } 
      }
      }
    if(choix=="Test de McNemar") robust<-F else robust<-T
    if(choix=="Ajustement") Bayes<-F else Bayes<-T 
    msg.options<-"Dans ce cas, le test non paramétrique est le test de chi carré classique"
    .ez.options(options="choix", n.boot=n.boot,param=F, non.param=T, robust=robust, Bayes=Bayes, msg.options1=NULL, msg.options2=msg.options, info=T, dial=dial, choix=choix2,
                sauvegarde=sauvegarde)->Options
    if(is.null(Options)){  chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
      return(Resultats)}
    if(dial==T || any(SampleType %in% c("poisson", "jointMulti","hypergeom", "indepMulti"))==F || SampleType=="indepMulti" & any(fixedMargin %in% c("rows","cols"))==F){
      
      if(any(Options$choix=="Facteurs bayesiens") && choix== "Indépendance" ){
        if(info==T) {
          writeLines("Quel type d'échantillonnage  avez-vous réalisé pour votre analyse ?") 
          cat("Si l'effectif total est non fixé, on fait l'hypothèse que les observations surviennent en respectant une loi de poisson.
              La répartition sur les niveaux d'un facteur surviennent avec une probabilité fixe. La distribution est une distribution poisson")
          print(matrix(c(100,50,200,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))
          
          writeLines("L'option *Effectif total fixé* doit être choisi si on fait l'hypohèse nulle que la répartition dans chacune des cellules du tableau est fixée.
                     La distribution est une distribution multinomiale jointe")
          print(matrix(c(100,100,100,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))
          
          writeLines("L'option Effectif total fixé pour les lignes* doit être choisi si les effectifs pour chaque ligne est identique, 
                     comme lorsqu'on veut s'assurer d'un appariement entre groupes. La distribution est une distribution multinomiale indépendante")
          print(matrix(c(15,40,55, 85,60,145, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
          writeLines("L'option Effectif total fixé pour les colonnes* est identique à la précédente pour les colonnes")
          writeLines("L'option *Effectif total fixé pour les lignes et les colonnes* lorsque les totaux pour les lignes et les colonnes sont fixes.La distribution est hypergéométrique")
          print(matrix(c(15,85,100, 85,15,100, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
        } 
        SampleType<-c()
        FM<-c()
        for(i in 1:length(comb[,1])){
          
          if(nlevels(data[,as.character(comb[i,1])])==2 && nlevels(data[,as.character(comb[i,2])])==2) possible<- c("poisson - Effectif total non fixé", "jointMulti - Effectif total fixé", 
                                                                                                                    paste("indepMulti - Effectif total fixé pour les lignes - variable", comb[i,1]), 
                                                                                                                    paste("indepMulti - Effectif fixé pour les colonnes - variable", comb[i,2]), 
                                                                                                                    "hypergeom -  Effectif total fixé pour les lignes et les colonnes") else {
                                                                                                                      possible<- c("poisson - Effectif total non fixé", "jointMulti - Effectif total fixé", 
                                                                                                                                   paste("indepMulti - Effectif total fixé pour les lignes - variable", comb[i,1]), 
                                                                                                                                   paste("indepMulti - Effectif fixé pour les colonnes - variable", comb[i,2]))
                                                                                                                    }
          SampleType1<-dlgList(possible, preselect="Effectif total non fixé", multiple = FALSE, title=paste("Pan expérimental entre", comb[i,1], "et",comb[i,2], "?"))$res
          if(length(SampleType1)==0) {chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL, sauvegarde=NULL)->Resultats
            return(Resultats)}
          ifelse(SampleType1 == paste("indepMulti - Effectif total fixé pour les lignes - variable", comb[i,1]), fixedMargin<-"rows",
                 ifelse(SampleType1 == paste("indepMulti - Effectif fixé pour les colonnes - variable", comb[i,2]), fixedMargin<-"cols", fixedMargin<-0))
          FM<-c(FM,fixedMargin )  
          ST<- switch(SampleType1, "poisson - Effectif total non fixé"= "poisson",
                      "jointMulti - Effectif total fixé"="jointMulti",
                      "hypergeom -  Effectif total fixé pour les lignes et les colonnes"= "hypergeom")
          if(SampleType1==paste("indepMulti - Effectif total fixé pour les lignes - variable", comb[i,1])) ST<-"indepMulti"
          if(SampleType1==paste("indepMulti - Effectif fixé pour les colonnes - variable", comb[i,2])) ST<-"indepMulti"
          SampleType<-c(SampleType, ST)
        }  
        
      }
    }
    
    list()->Resultats
    Resultats$analyse<-choix
    Resultats$data<-data
    Resultats$nom.data<-nom
    if(choix=="Ajustement") Resultats$Variables<-X else Resultats$Variables<-comb
    Resultats$Effectifs<-Effectifs
    Resultats$p<-p
    Resultats$choix<-Options$choix
    Resultats$sauvegarde<-Options$sauvegarde
    Resultats$n.boot<-Options$n.boot
    Resultats$SampleType<-SampleType
    Resultats$fixedMargin<-FM
    return(Resultats)
    } 
  Cramer<-function(chi.r){
    x<-chi.r$statistic
    n<-sum(chi.r$expected)
    dims<-dim(chi.r$expected)
    V<-round((x/((min(dims)-1)*n))^0.5,3)
    V.sq<-round(V^2,3)
    resultats<-data.frame("V"=V, "V.carré"=V.sq)
    return(resultats)}
  chi.out<-function(data=NULL, X=NULL, Y=NULL, p=NULL, choix=NULL, Effectifs=NULL, n.boot=NULL, SampleType=NULL,
                    fixedMargin=NULL, choix2=NULL, rscale=2^0.5/2,priorConcentration=1){
    Resultats<-list()
    if(choix=="Ajustement"){
      if(!is.null(Effectifs)){
        tapply(data[,Effectifs], data[,X],sum,na.rm=TRUE)->tab
        rbind(tab,p, p*sum(data[,Effectifs]))->Distribution} else {
          table(data[,X])->tab
          rbind(tab, p, sum(tab)*p)->Distribution}
      dimnames(Distribution)[[1]]<-c("Observés", "probabilités","Attendus")
      Resultats$"Tableau de synthèse"<-Distribution 
      chi<-chisq.test(tab, p=p, B=n.boot)
      Resultats$"chi.deux d'ajustement"<-data.frame(chi.deux=round(chi$statistic,3), ddl=chi$parameter) 
      if(any(choix2== "Test non paramétrique")) Resultats$"chi.deux d'ajustement"$valeur.p<-round(chi$p.value,4)
      if(!is.null(n.boot) && n.boot>1){
        Resultats$"chi.deux d'ajustement"$"Valeur estimée de p par simulation de Monte Carlo"<-round(chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value,4)}
      
    }
    if((choix!="Ajustement")){
      if (is.null(Effectifs)) tab<-table(data[,X],data[ ,Y], dnn=c(X, Y))else {
        tab<-tapply(data[,Effectifs],list(data[,X],data[,Y]),sum,na.rm=TRUE) 
        tab[is.na(tab)] <- 0
        as.table(tab)->tab
        names(attributes(tab)$dimnames)<-c(X,Y)
      }
      # graphique   
      spineplot(tab, col=topo.colors(nlevels(data[,Y])))
      table.margins(tab)->Resultats$"Effectifs Observés"
      if(choix=="Indépendance"){
        mon.chi<-chisq.test(tab, B=n.boot, correct=F)
        mon.chi$expected->Resultats$"Effectifs attendus"
        if(any(choix2 %in% c("Test non paramétrique","Test robustes - impliquant des bootstraps")))    {
          SY<-data.frame( "chi.deux"=round(mon.chi$statistic,4), 
                          "ddl"=mon.chi$parameter, Cramer(mon.chi))
          if(any(choix2=="Test non paramétrique")) SY$valeur.p<-round(mon.chi$p.value,4) 
          try(fisher.test(tab),silent=T)->fisher
          if(class(fisher)!="try-error") SY$"Fisher.Exact.Test"=round(fisher$p.value,4)
          if(all(dim(tab)==2)){
            mon.chi<-chisq.test(tab, B=n.boot, correct=T)
            AY<-data.frame("chi.deux"=round(mon.chi$statistic,4),"ddl"=mon.chi$parameter,   Cramer(mon.chi),"Fisher.Exact.Test"="" )
            if(any(choix2=="Test non paramétrique")) AY$valeur.p<-round(mon.chi$p.value,4)
            SY<-rbind(SY, AY)
            dimnames(SY)[[1]][1]<-c("Sans correction de Yates", "Avec correction de Yates") 
          } else dimnames(SY)[[1]][1]<-c("Sans correction de Yates")
          if(!is.null(n.boot) && n.boot>1){
            SY$"Valeur p par simulation de Monte Carlo"<-chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value
          } 
          Resultats$"Analyse principale"<-SY
          # Rapport de vraisemblance 
          RV<-2* sum(mon.chi$observed[which(mon.chi$observed!=0)] * 
                       log(mon.chi$observed[which(mon.chi$observed!=0)]/mon.chi$expected[which(mon.chi$observed!=0)],base=exp(1)))
          PRV<-pchisq(RV, mon.chi$parameter, ncp = 0, lower.tail = F, log.p = FALSE)
          p<-mon.chi$observed/sum(mon.chi$observed)
          q<-mon.chi$expected/sum(mon.chi$expected)
          RVES<-(-1/(log(min(q[which(p!=0)]), base=exp(1)))) *sum(p *log(p[which(p!=0)]/q[which(p!=0)], base=exp(1))) # ES from JOHNSTON et al. 2006
          RV<-data.frame("chi.carré"=RV, "ddl"=mon.chi$parameter, "valeur.p"=round(PRV,4), "Taille.effet"=round(RVES,4))
          Resultats$"Rapport de vraisemblance (G test)"<-RV
        }
        # facteur bayesien
        if(any(choix2=="Facteurs bayesiens")) {
          if(!is.null(fixedMargin) && fixedMargin==0) fixedMargin<-NULL
          bf<-contingencyTableBF(tab, sampleType = SampleType, fixedMargin = fixedMargin, priorConcentration=priorConcentration)
          bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",round(extractBF(bf, onlybf=T),4)))
          bf<-data.frame("Facteur bayesien"=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)),SampleType))
          dimnames(bf)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle", "Type")
          Resultats$"Facteur bayesien"<-bf 
        }
        
        # Odd ratio 
        as.matrix(tab)->tab
        if(all(dim(tab)>2) |any(mon.chi$observed==0)) {
          "On ne peut pas calculer les OR pour des tableaux plus grands que 2x3 ou des tableaux contenant des 0"->Resultats$"Odd ratio"
        }else{
          if(length(tab[1,])>2) tab<-apply(tab,1, rev)
          Resultats$"Odd ratio"<- oddsratio.wald(x=tab,conf.level = 0.95,rev = c("neither"),correction = FALSE,verbose = FALSE)$measure
        }
        if(any(choix2 %in% c("Test non paramétrique","Test robustes - impliquant des bootstraps")))      {
          if(is.null(SY$"Valeur p par simulation de Monte Carlo")) p<-SY$valeur.p else p<-SY$"Valeur p par simulation de Monte Carlo"
          if(p<0.05)  {
            round(mon.chi$residuals,3)->Resultats$"Résidus"
            round((mon.chi$observed-mon.chi$expected)/(mon.chi$expected^0.5),3)->Resultats$"Résidus standardisés"
            round(mon.chi$stdres,3)->Resultats$"Résidus standardisés ajustés"
            p.adjust(2*(1-pnorm(abs(Resultats$"Résidus standardisés ajustés"))), method="holm")->valeur.p
            matrix(valeur.p, nrow=nrow(tab))->valeur.p
            dimnames(tab)->dimnames(valeur.p)
            round(valeur.p,4)->Resultats$"Significativité des résidus - probabilité corrigée en appliquant la méthode de Holm"
          }
        }
        round(table.margins(prop.table(mon.chi$observed))*100,1)->Resultats$"Pourcentage total"
        round(sweep(addmargins(mon.chi$observed, 1, list(list(All = sum, N = function(x) sum(x)^2/100))), 2,apply(mon.chi$observed, 2, sum)/100, "/"), 1)->Resultats$"Pourcentage par colonne"
        round(sweep(addmargins(mon.chi$observed, 2, list(list(All = sum, N = function(x) sum(x)^2/100))), 1,apply(mon.chi$observed, 1, sum)/100, "/"), 1)->Resultats$"Pourcentage par ligne"
        
      }
      if(choix=="Test de McNemar"){
        if(any(choix2== "Test non paramétrique"))    {
          MCN<-mcnemar.test(tab, correct=F)
          MCN<-data.frame("chi.deux"=round(MCN$statistic,3), "ddl"=MCN$parameter, "valeur.p"= round(MCN$p.value,4))
          MCN2<-mcnemar.test(tab, correct=T)
          MCN2<-data.frame("chi.deux"=round(MCN2$statistic,3), "ddl"=MCN2$parameter, "valeur.p"= round(MCN2$p.value,4))
          MCN<-rbind(MCN, MCN2)
          dimnames(MCN)[[1]]<-c("Test de McNemar sans correction de continuité", "Test de McNemar avec correction de continuité" )
          MCN->Resultats$"Test de McNemar avec correction de Yates" # test de McNemar    
        }
        if(any(choix2=="Facteurs bayesiens")) {
          bf<-proportionBF(y=tab[1,2], tab[1,2]+tab[2,1], p=0.5,rscale=rscale)
          erreur<-bf@numerator[[1]]@analysis$properror
          erreur<-ifelse(erreur<.0001, "<0.0001", erreur)
          bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",extractBF(bf, onlybf=T)))
          samples =proportionBF(y = tab[1,2], N = tab[1,2]+tab[2,1], p = .5, posterior = TRUE, iterations = 10000)
          plot(samples[,"p"])
          bf<-data.frame("Facteur bayesien"=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)), erreur, rscale))
          dimnames(bf)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle", "erreur", "rscale")
          Resultats$"Facteurs bayesiens"<-bf
        }
        
        if( all(dimnames(tab)[[1]]==dimnames(tab)[[2]])) Resultats$Avertissement<-"Les cellules utilisées pour le calcul du McNemar  sont celles de la 1e ligne 2e colonne et de la 2e ligne 1e colonne" else
          Resultats$Avertissement<-"Test de McNemar : les modalités ne sont pas les mêmes pour le test de McNemar. Est-ce bien un facteur en mesure répetée ?"}
      
    }
    return(Resultats) 
  }
  
  c("svDialogs", "epitools", "BayesFactor", "ggplot2")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  Resultats<-list()
  
  
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data 
  
  chi.in(X=X, Y=Y, Effectifs=Effectifs,p=p, choix=choix, data=data, info=info, n.boot=n.boot, SampleType=SampleType, FM=fixedMargin, choix2=choix2, sauvegarde=sauvegarde)->chi.options
  if(is.null(chi.options)) return(analyse())
  if(chi.options!="Ajustement"){
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
  }
  
  
  if(class(chi.options$Variables)=="data.frame") {
    X<- chi.options$Variables[,1]
    Y<- chi.options$Variables[,2]
  } else {X<-chi.options$Variables
  Y<-NULL}
  
  if(length(X)>1) Resultats$"Avertissement alpha"<-paste("vous multipliez l'erreur de 1e espèce. Le risque de commettre une erreur de 1e espèce est de", 100*(1-0.95^length(X)), "%", sep=" ")  
  for(i in 1:length(X)) {
    as.character(X[i])->Xi
    as.character(Y[i])->Yi
    chi.results<-chi.out(data=chi.options$data, X=Xi, Y=Yi,p=chi.options$p, choix=chi.options$analyse, 
                         Effectifs =chi.options$Effectifs, n.boot=chi.options$n.boot, choix2=chi.options$choix,
                         SampleType=chi.options$SampleType[i],  fixedMargin=chi.options$fixedMargin[i], rscale=rscale, priorConcentration =priorConcentration)
    Resultats[[i]]<-chi.results
    if(chi.options$analyse=="Ajustement") nom<-paste("chi deux d'ajustement sur la variable", X, sep =" ")
    if(chi.options$analyse=="Indépendance") nom<-paste("Résultats du chi.deux entre la variable", Xi,
                                                       "et la variable", Yi,sep=" ")
    if(chi.options$analyse=="Test de McNemar") nom<-paste("Résultats du test de McNemar entre la variable", Xi,
                                                          "et la variable", Yi,sep=" ")
    names(Resultats)[i]<-nom
  } 

  paste(unique(X), collapse="','", sep="")->X
  if(!is.null(Y)) paste(unique(Y), collapse="','", sep="")->Y
  paste(chi.options$choix, collapse="','", sep="")->choix2
  paste(chi.options$p, collapse=",", sep="")->p
  if(!is.null(chi.options$SampleType)) paste(chi.options$SampleType, collapse="','", sep="")->SampleType
  paste(chi.options$fixedMargin, collapse="','", sep="")->FM
  paste0("chi(X=c('", X,ifelse(!is.null(Y), paste0("'),Y=c('", Y, "')"), "'), Y=NULL"), 
         ifelse(is.null(chi.options$Effectifs),",Effectifs=NULL", paste0(",Effectifs='", chi.options$Effectifs, "'")),
         ifelse(!is.null(Y), ", p=NULL", paste0(", p=c(", p,")")), ",sauvegarde=", chi.options$sauvegarde,
         ", choix='", chi.options$analyse, "',data=", chi.options$nom.data, ",info=", info, ",n.boot=", ifelse(is.null(chi.options$n.boot), "NULL",chi.options$n.boot) , 
         ",priorConcentration =" ,priorConcentration, ",SampleType=", ifelse(is.null(chi.options$SampleType), 'NULL', paste0("c('",SampleType,"')")), 
         ",fixedMargin=", ifelse(is.null(chi.options$fixedMargin), 'NULL', paste0("c('",FM,"')")), ",choix2=c('",choix2,
         "'),rscale=", round(rscale,3), ")")->Resultats$Call
  .add.history(data=chi.options$data, command=Resultats$Call, nom=chi.options$nom)
  .add.result(Resultats=Resultats, name =paste(chi.options$analyse, Sys.time() ))
  
  if(chi.options$sauvegarde){save(Resultats=Resultats ,choix ="chi.deux", env=.e)}
  
  ref1(packages)->Resultats$Références
  ### Obtenir les Resultats
  return(Resultats) 
  }


#### CFA
ez.cfa<-function(modele=NULL, X=NULL, data=NULL,ord=NULL, outlier="Données complètes",imp="rm", output="default", info=T, sauvegarde=F, mimic=NULL, fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                 estimator="ML",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                 link="probit",int.ov.free=FALSE, int.lv.free=FALSE, std.lv=FALSE, n.boot=1000, group.w.free=F,
                 group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", "lv.variances" , "lv.covariances")){
  # modele : lavaan modele if X is null
  # data : dataframe
  # X : character. names of the variables if modele is null
  # LV : Vector. names of LV=atent Variables
  # ord: Character. Vector of ordered variables among X
  # outlier : should outliers be detected and removed on Mahalanobis distance ? ("Données sans valeur influente") or not ("Données complètes")
  # imp : How must missing data be dealt :"rm"= remove, "mean" = impute mean, "median"=impute median, "amelia"=use amelia algorithm for imputation. 
  # output : character vector. List of output that has to be shown. 
  # info : logical. Should information be printed in the console ? 
  # sauvegarde : logical. Must the output be saved in external file ? 
  # mimic : forced argument to determine whether to use or not dialog boxes in specifying options
  # for other options, see lavOptions
  
  options (warn=-1)
  Lav.modele<-function(X=NULL, modele=NULL, LV=NULL, info=T){
    # X : character. Names pf tje manifest variables
    # LV : character. Vector of latent variable names. 
    # modele : lavaan modele
    if(!is.null(modele)){
      semPlot.modele<-try(semPlotModel_lavaanModel(modele))
      if(class(semPlot.modele)=="try-error"){
        msgBox("Le modèle semble incorrect et n'a pas pu être créé.")
        return(NULL)
      }
      semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Est-ce que votre modèle est correct ?", "yesno")$res->suppression
      if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
      return(modele)
    }
    
    if(is.null(LV) && length(X)>3) {
      if(info)   writeLines("Veuillez préciser le nombre de variables latentes")
      nF<-NA
      while(!is.numeric(nF)) {
        if(info) writeLines("Veuillez préciser le nombre de variables latentes") 
        nF <- dlgInput("Nombre de facteurs ?", 2)$res
        if(length(nF)==0) return(NULL)
        strsplit(nF, ":")->nF
        tail(nF[[1]],n=1)->nF
        as.numeric(nF)->nF
        if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>length(X) ){
          msgBox("Le nombre de facteur doit être un entier positif inférieur au nombre de variables")
          nF<-NA }
      }} else if(!is.null(LV)) nF<-length(LV) else nF<-1
      
      O2<-c()
      X->reste
      list()->modele2
      for(i in 1:nF){
        if(is.null(LV[i]))  {dlgInput(paste("Nom de la variable latente",i,  "?"), paste("Facteur",i, sep="."))$res->noms
          if(length(noms)==0) return(Lav.modele(X=X, LV=NULL))
          strsplit(noms, ":")->noms
          tail(noms[[1]],n=1)->noms} else noms<-LV[i]
          title<-paste("Variables manifestes de", noms)
          if(i==nF) O1<-reste else O1<- dlgList(reste, preselect=NULL, multiple = TRUE, title=title)$res
          O2<-c(O2,O1)	
          setdiff(reste,O2)->reste
          paste(noms, "=~", O1[1])->modele
          for(j in 2 :(length(O1))){paste(modele, "+", O1[j])->modele}
          modele2[[i]]<-modele
          modele2[[1]]->modele
          if(i>1) {
            for(j in 2 : i){paste(modele,"\n", modele2[[j]])->modele   }
          }
          semPlot.modele<-semPlotModel_lavaanModel(modele)
          semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
      }
      
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Est-ce que votre modèle est correct ?", "yesno")$res->suppression
      if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
      return(modele)
  }
  .ez.lavaan.options<-function(modele=NULL, data=NULL, X=NULL, info=TRUE, opt.list=NULL, dial=T, imp=NULL, outlier=NULL,output=NULL){
    if(dial || is.null(opt.list$mimic) || !opt.list$mimic%in% c("default", "Mplus", "EQS")){dial<-T
    if(info) writeLines("Voulez-vous spécifier tous les paramètres [default] ou imiter un logiciel particulier ?")
    opt.list$mimic<-dlgList(c("default", "Mplus", "EQS"), preselect="default", multiple = FALSE, title="Imiter ?")$res
    if(length(opt.list$mimic)==0) return(NULL)
    }
    
    if(dial){ 
      if(opt.list$mimic=="default"){ 
        options2<-c("Variables exogènes fixées [fixed.x=default]", "information [information=default]", "correction de continuité [zero.keep.margins=default]",
                    "Vraisemblance (seulement pour estimator=ML) [likelihood=default]") 
      } else options2<-c()
      options<-c("estimateur [estimator=ml])", "groupes [group=NULL]", "test [test=standard]", "erreur standard [se=standard]", "standardisation des variables observées [std.ov=T]", 
                 "Orthogonalité des facteurs [orthogonal=FALSE]", "Lien (seulement pour estimator=MML) [link=probit]",
                 "Intercept des variables observées [int.ov.free=FALSE]", "Intercept des variables latentes [int.lv.free=FALSE]", "Variables exogènes fixées [fixed.x=default]",
                 "Estimation des indicateurs des variables latentes [std.lv=FALSE]", options2)
      
      if(info) writeLines("Quelles options voulez-vous spécifier ?")
      options<-dlgList(c("Garder les valeurs par défaut", options), preselect=c("estimateur [estimator=ml])","test [test=standard]", "erreur standard [se=standard]"), multiple = TRUE, title="Quelles options ?")$res
      if(length(options)==0) return(NULL)
      if(options=="Garder les valeurs par défaut") return(list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                                                               estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                                                               link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                                                               group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                                                             "lv.variances" , "lv.covariances")))
    } else options<-NULL
    
    
    
    if(any(options=="estimateur [estimator=ml])")|is.null(opt.list$estimator) || length(opt.list$estimator)!=1|| 
       try(opt.list$estimator %in%c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ),silent=T)!=T){
      if(info){  writeLines("[WLS] correspond à [ADF]. Les estimateurs avec les extensions [M],[MV],[MVSF],[R] 
                            sont des versions robustes des estimateurs classiques [MV],[WLS], [DWLS], [ULS]")
        abb<-data.frame(abb=c("ML","GLS", "WLS", "ULS", "DWLS"), nom=c("maximum de vraisemblance","moindre carré généralisés","moindre carré pondéré","moindre carré non pondéré","moindre carré  pondéré diagonalement"))
        print(abb)    }
      opt.list$estimator<-dlgList(c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ), multiple = FALSE, title="Quelles estimateur ?")$res
      if(length(opt.list$estimator)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    if(any(options=="test [test=standard]") || length(opt.list$test)!=1 || !opt.list$test%in% c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted",
                                                                                                "scaled.shifted", "bootstrap","Bollen.Stine")){
      if(info) writeLines("Quel test voulez-vous utiliser ?")
      opt.list$test<-dlgList(c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted","scaled.shifted", "bootstrap","Bollen.Stine"), multiple = FALSE, title="Quelles estimateur ?")$res
      if(length(opt.list$test)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine") &&!is.null(opt.list$n.boot) && ((class(opt.list$n.boot)!="numeric" & class(opt.list$n.boot)!="integer") ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1)){
      msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
      opt.list$n.boot<-NULL
    }
    if(dial & opt.list$test%in%c("boot","bootstrap","Bollen.Stine") || is.null(opt.list$n.boot) & opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) {
      while(is.null(opt.list$n.boot)){
        writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
        n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
        if(length(n.boot)==0) {Resultats<-Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->opt.list$n.boot
        if(is.na(opt.list$n.boot) ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1){
          msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
          opt.list$n.boot<-NULL
        }
      }
    } 
    
    if( any(is.na(data[,X])) & opt.list$estimator=="ml" & opt.list$mimic=="default") opt.list$missing<-"fiml" else opt.list$missing<-"default"
    
    if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) se1<-c("standard","first.order", "robust", "bootstrap","none" ) else se1<-c("standard","first.order", "robust", "none" )
    if(any(options=="erreur standard [se]") || is.null(opt.list$se) || !opt.list$se%in%se1)  {
      if(info) writeLines("Comment l'erreur standard doit-elle être estimée ?")
      opt.list$se<-dlgList(se1, multiple = FALSE, title="Erreur standard ?")$res
      if(length(opt.list$se)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="groupes [group=NULL]") || !is.null(opt.list$group)){
      msg2<-"Veuillez choisir la définissant les groupes"
      .var.type(X=opt.list$group, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable [groupes] ?", out=X)->group
      if(is.null(group)){
        Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      } 
      group$data->data
      group$X->opt.list$group
      if(dial|| any(opt.list$group.equal %in% c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))==FALSE){
        if(info) writeLines("Quels sont les paramètres que vous désirez maintenir constants ?")
        opt.list$group.equal<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), multiple = T, 
                                      preselect=c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), title="Paramètres constants ?")$res
        if(length(opt.list$group.equal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
        }}
      # écrase group equal puisque àa libère les group sur cette contraintes ==> utilité ? 
      #group.partial<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))
      if(info) writeLines("est-ce que les fréquences des différents group est un paramètre libre ? ") 
      opt.list$group.w.free<-dlgList(c(TRUE, FALSE), multiple=F, preselect=FALSE, title="Constance de la fréquence ?")$res
      if(length(opt.list$group.w.free)==0) {Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      }
    }
    ### zero.keep.margins
    if(any(options=="correction de continuité [zero.keep.margins]") || is.null(opt.list$zero.keep.margins)||(!is.logical(opt.list$zero.keep.margins) & opt.list$zero.keep.margins!="default")){
      if(info) writeLines("Faut-il ajouter une valeur aux cellules vides pour les corrélations polychorique ? Pour spécifier les valeurs,choisissez TRUE, sinon choisissez [default]")
      opt.list$zero.keep.margins<-dlgList(c(TRUE, FALSE,"default"), preselect="default", multiple = FALSE, title="Cellules vides ?")$res
      if(length(opt.list$zero.keep.margins)==0) {
        Resultats<-Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      }
    } 
    
    if( opt.list$zero.keep.margins==TRUE){
      if(!is.null(opt.list$zero.add) && ((class(opt.list$zero.add)!="numeric" ) || any( opt.list$zero.add<0) || any(opt.list$zero.add>1))){
        msgBox("La correction pour le calcul de corrélations polycoriques doit être comprise entre 0 et 1.") 
        opt.list$zero.add<-NULL
      }
      while(is.null(opt.list$zero.add)){
        writeLines("Veuillez préciser la valeur pour les tableaux 2x2")
        zero.add1<-dlgInput("tableau 2x2 ?", 0.5)$res
        if(length(zero.add1)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
        strsplit(zero.add1, ":")->zero.add1
        tail(zero.add1[[1]],n=1)->zero.add1
        as.numeric(zero.add1)->zero.add1
        if(is.na(zero.add1) ||  zero.add1<0 || zero.add1>1){
          msgBox("La valeur doit être comprise entre 0 et 1") 
          opt.list$zero.add<-NA} else{
            writeLines("Veuillez préciser la valeur pour les tableaux plus grand que 2x2")
            zero.add2<-dlgInput("tableau > 2x2 ?", 0)$res
            if(length(zero.add2)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
            return(Resultats)}
            strsplit(zero.add2, ":")->zero.add2
            tail(zero.add2[[1]],n=1)->zero.add2
            as.numeric(zero.add2)->zero.add2
            if(is.na(zero.add2) ||  zero.add2<0 || zero.add2>1){
              msgBox("La valeur doit être comprise entre 0 et 1") 
              opt.list$zero.add<-NA}
          }
        opt.list$zero.add<-c(zero.add1,zero.add2)
        
      }
    } 
    
    
    ### fin zero.keep.margins
    if(any(options=="Vraisemblance (seulement pour estimator=ML) [likelihood=default]") & opt.list$mimic=="default" & opt.list$estimator=="ML" ||is.null(opt.list$likelihood) || length(opt.list$likelihood)!=1 || try(opt.list$likelihood%in%c("wishart","normal", "default" ),silent=T)!=T) {
      if(info) writeLines("Veuillez préciser la vraisemblance.")
      opt.list$likelihood<-dlgList(c("wishart","normal", "default" ), multiple=F, preselect="default", title="Vraisemblance ?")$res # dépend de mimic
      if(length(opt.list$likelihood)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="Lien (seulement pour estimator=MML) [link=probit]") & opt.list$estimator=="MML" ||length(opt.list$link)!=1 || try(opt.list$link%in%c("logit","probit" ),silent=T)!=T ){
      if(info) writeLines("Veuillez préciser la famille (i.e. forme de la distribution).")
      opt.list$link<-dlgList(c("logit","probit" ), multiple=F, preselect=FALSE, title="Distribution ?")$res
      if(length(opt.list$link)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    
    if(any(options=="information [information=default]") ||is.null(opt.list$information) || try(opt.list$information%in%c("expected","observed", "default" ),silent=T)!=T ){
      if(info) writeLines("Sur quelle matrice d'information doit se réaliser l'estimation des erreurs standards ?")
      opt.list$information<-dlgList(c("expected","observed", "default" ), multiple=F, preselect=FALSE, title="Matrice d'information ?")$res
      if(length(opt.list$information)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    if(any(options=="Variables exogènes fixées [fixed.x=default]") ||length(opt.list$fixed.x)!=1 || (!is.logical(opt.list$fixed.x) & opt.list$fixed.x!="default") ){
      if(info) writeLines("Si vrai, on considère les covariés exogènes comme fixés, sinon on les considère comme aléatoires et leurs paramètres sont libres")
      opt.list$fixed.x<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Covariables fixées ?")$res
      if(length(opt.list$fixed.x)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    if(any(options=="Orthogonalité des facteurs [orthogonal=FALSE]") ||length(opt.list$orthogonal)!=1 || !is.logical(opt.list$orthogonal) ){
      if(info) writeLines("Est-ce que les facteurs sont corrélés (FALSE) ou sont-ils orthogonaux (TRUE)?")
      opt.list$orthogonal<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Orthogonalité des facteurs ?")$res
      if(length(opt.list$orthogonal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="standardisation des variables observées [std.ov=T]") ||length(opt.list$std.ov)!=1 || !is.logical(opt.list$std.ov) ){
      if(info) writeLines("Faut-il standardisé (i.e. centrer réduire) les variables observées au prélable (TRUE) ou non (FALSE) ?")
      opt.list$std.ov<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Standardisation ?")$res
      if(length(opt.list$std.ov)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    #####
    if(any(options=="Intercept des variables observées [int.ov.free=FALSE]") ||length(opt.list$int.ov.free)!=1 || !is.logical(opt.list$int.ov.free) ){
      if(info) writeLines("Faut-il fixer l'intercept des variables observées à 0 ?")
      opt.list$int.ov.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VO=0 ?")$res
      if(length(opt.list$int.ov.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }
    
    
    if(any(options=="Intercept des variables latentes [int.lv.free=FALSE]") ||length(opt.list$int.lv.free)!=1 || !is.logical(opt.list$int.lv.free) ){
      if(info) writeLines("Est-ce que l'intercept des variables latentes doit être fixé à 0 ?")
      opt.list$int.lv.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VL=0 ?")$res
      if(length(opt.list$int.lv.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }
    
    
    
    if(any(options=="Estimation des indicateurs des variables latentes [std.lv=FALSE]") ||length(opt.list$std.lv)!=1 || !is.logical(opt.list$std.lv) ){
      if(info) writeLines("Si vrai, les résidus des variables latentes sont fixés à 1, sinon les paramètres de la variable latente sont estimés en fixant le premier indicateur à 1")
      opt.list$std.lv<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Standardisation VL?")$res
      if(length(opt.list$std.lv)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    return(opt.list)
  }
  
  
  
  cfa.in<-function(modele=NULL,X=NULL,LV=NULL, data=NULL, ord=NULL, outlier=NULL,imp=NULL,output=NULL, info=T, opt.list=list(), sauvegarde=F){
    
    Resultats<-list()
    if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL) 
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    
    if(is.null(modele)){ 
      msg3<-"Veuillez choisir les variables manifestes que vous désirez analyser. Vous devez choisir au moins 3 variables" 
      
      X<-.var.type(X=X, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
      data<-X$data
      X<-X$X
      if(is.null(X) || length(X)<3) return(NULL)
      
      
      if(dial || length(outlier)>1 || outlier %in% c("Données complètes", "Données sans valeur influente") ==FALSE){
        if(info) writeLines("Désirez-vous l'analyse sur les données complètes ou sur les données pour lesquelles les valeurs influentes ont été enlevées ?")
        if(info) writeLines("les valeurs influentes sont identifiées sur la base de la distance de Mahalanobis avec un seuil du chi à 0.001")
        outlier<- dlgList(c("Données complètes", "Données sans valeur influente"), preselect="Données complètes",multiple = FALSE, title="Quels résultats voulez-vous obtenir ?")$res
        if(length(outlier)==0) { Resultats<-cfa.in()
        return(Resultats)}
      }
      
      if(outlier=="Données sans valeur influente"){
        inf<-VI.multiples(data[,X])
        Resultats$"Valeurs considérées comme influentes"<-inf$"Valeurs considérées comme influentes"
        data<-inf$data
      }
      
      if(dial){
        if(info) writeLines("Veuillez préciser le type de variables. Des corrélations tétra/polychoriques seront réalisées sur les variables dichotomiques/ordinales et Bravais-Pearson sur les variables continues")
        if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c("dichotomiques/ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else {
          type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res 
        }
        
        if(length(type)==0) {Resultats<-cfa.in()
        return(Resultats)}
      } else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"
      }
      
      if(type!="continues"){ 
        if(type=="mixte") {
          if(info) writeLines("Veuillez préciser les variables ordinales ?") 
          ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
          if(length(ord)==0) {Resultats<-cfa.in()
          return(Resultats)}
        }else ord<-X
      }
      
      modele<-Lav.modele(X=X, LV=LV)
      if(is.null(modele)) {
        Resultats<-cfa.in()
        return(Resultats) 
      }}else{
        modele<-Lav.modele(modele=modele)
        if(is.null(modele)) {
          Resultats<-cfa.in()
          return(Resultats)
        }
      }
    if(any(is.na(data[,X]))) {
      if(is.null(imp))  {msgBox("Des valeurs manquantes ont été détectées. Comment voulez-vous les traiter ?")
        imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes","Remplacer par la médiane","Multiple imputation - Amelia"), 
                      preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes ?")$res}
      if(length(imp)==0){
        Resultats<-cfa.in()
        return(Resultats)
      }
      data1<-ez.imp(data[, X], imp=imp, ord= ord)
      diff<-setdiff(names(data), X)
      data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),diff])
    }  
    
    
    
    Resultats$opt.list<-.ez.lavaan.options(data=data, X=X, info=TRUE, opt.list=opt.list, dial=dial) 
    if(is.null( Resultats$opt.list)) {
      Resultats<-cfa.in()
      return(Resultats)
    }
    
    
    if(dial || class(output)!="character"|| any(!output%in% c("default", "Sorties par défaut", "parEst", "Paramètres estimés", "parSt", "Paramètres standardisés","Matrice de covariance ajustée", "fitted.cov",
                                                              "Résidus standardisés", "res.St","res.Unst","Résidus non standardisés","vcov","Matrice de covariance estimée",
                                                              "AIC", "BIC", "Mesures d'adéquation","fitM", "Inspecter les valeurs de départ", "start", "Inspecter les matrices du modèle",
                                                              "modmat", "Inspecter la représentation du modèle", "modrep"))==TRUE){
      if(info) writeLines("Quels résultats souhaitez-vous ? Attention : les sorties par défaut ne peuvent être sauvegrdées. Si vous voulez une sauvarde, choisissez le détail")
      output<-c( "Sorties par défaut", "Paramètres estimés", "Paramètres standardisés","Matrice de covariance ajustée", 
                 "Résidus standardisés", "Résidus non standardisés","Matrice de covariance estimée","AIC", "BIC", "Mesures d'adéquation", 
                 "Inspecter les valeurs de départ",  "Inspecter les matrices du modèle", "Inspecter la représentation du modèle")
      if(info) writeLines("Quelles sorties de résultats souhaitez-vous ?")
      output<- dlgList(output, preselect="Sorties par défaut", multiple = TRUE, title="Sorties de résultats ?")$res
      if(is.null( Resultats$opt.list)) {
        Resultats<-cfa.in()
        return(Resultats)
      }
    }
    
    
    if(dial || length(sauvegarde)!=1 || !is.logical(sauvegarde)){
      sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Voulez-vous sauver les résultats ?")$res
      if(length(sauvegarde)==0) {
        Resultats<-cfa.in()
        return(Resultats)}
    }   
    
    Resultats$ord<-ord
    Resultats$data<-data
    Resultats$nom<-nom
    Resultats$modele<-modele 
    Resultats$output<-output 
    Resultats$sauvegarde<-sauvegarde
    
    return(Resultats)  
  }
  cfa.out<-function(cfa.options){
    .e <- environment()
    list()->Resultats
    
    data<-cfa.options$data   
    modele<-cfa.options$modele
    nom.v<-strsplit(modele, split="[\\|,+,='\n'~' ']+")
    var.mod<-which(names(data)%in% nom.v[[1]])
    ord<-cfa.options$ord
    output<-cfa.options$output
    sauvegarde<-cfa.options$sauvegarde
    cfa.options$opt.list->opt.list   
    
    opt.list$mimic->mimic
    opt.list$fixed.x->fixed.x
    opt.list$missing->missing
    opt.list$information->information
    opt.list$zero.keep.margins->zero.keep.margins
    opt.list$zero.add->zero.add
    if(is.null(zero.add)) zero.add<-"default"
    opt.list$estimator->estimator
    if(estimator=="ML" & (!is.null(ord)|any( unlist(sapply(data[,var.mod], class))=="factor") )) estimator<-"WLSMV"
    opt.list$group->group
    opt.list$test->test
    opt.list$se->se
    opt.list$std.ov->std.ov
    opt.list$orthogonal->orthogonal
    opt.list$likelihood->likelihood
    if(estimator!="ML")likelihood<-"default"
    opt.list$link->link
    opt.list$int.ov.free->int.ov.free
    opt.list$int.lv.free->int.lv.free
    opt.list$fixed.x->fixed.x
    opt.list$std.lv->std.lv
    opt.list$n.boot->n.boot
    opt.list$group.w.free->group.w.free
    if(is.null(group.w.free)) group.w.free<-F
    opt.list$group.equal->group.equal
    
    
    fit<-try( lavaan::cfa(modele, data = data, ordered=ord,estimator=estimator, test=test,
                          bootstrap=n.boot,meanstructure="default", check="start",zero.cell.warn=F, 
                          missing=missing, group=group, ifelse(!is.null(group), group.equal=group.equal,group.equal="mean"),
                          group.w.free= group.w.free,fixed.x=fixed.x,information=information,se=se,std.ov=as.logical(std.ov),
                          orthogonal=as.logical(orthogonal),likelihood=likelihood, link=link, int.ov.free=as.logical(int.ov.free),
                          int.lv.free=as.logical(int.lv.free),std.lv=as.logical(std.lv),zero.add=zero.add, zero.keep.margins=zero.keep.margins), silent=T)
    if(class(fit)=="try-error") {msgBox("Nous n'avons pas pu terminer correctement l'analyse. Veuillez tenter de respécifier les paramètres")
      return(ez.cfa())}
    
    if(any(output== "default") | any(output== "Sorties par défaut"))  {summary(fit, fit.measures = TRUE, standardized=T)->Resultats$"Résultats de l'analyse factorielle confirmatoire"
      if(length(output)==1) fit->>modele.cfa}
    if(any(output== "parEst") | any(output=="Paramètres estimés")) parameterEstimates(fit)->Resultats$"Paramètres estimés non standardisés"
    if(any(output== "parSt") | any(output=="Paramètres standardisés")) standardizedSolution(fit)->Resultats$"Paramètres estimés standardisés"
    if(any(output== "Matrice de covariance ajustée") | any(output=="fitted.cov")) fitted(fit)->Resultats$"Matrice de covariance ajustée"
    if(any(output== "Résidus standardisés") | any(output=="res.St")) resid(fit, type="standardized")->Resultats$"Résidus standardisés"
    if(any(output== "Résidus non standardisés") | any(output=="res.Unst")) resid(fit)->Resultats$"Résidus non standardisés"
    if(any(output== "vcov") | any(output=="Matrice de covariance estimée")) vcov(fit)->Resultat$"Matrice de covariance estimée"
    if(any(output== "AIC") ) AIC(fit)->Resultats$AIC
    if(any(output== "BIC") ) BIC(fit)->Resultats$BIC
    if(any(output== "Mesures d'ajustement") | any(output=="fitM")) fitMeasures(fit)->Resultat$"Mesure d'ajustement"
    if(any(output== "Inspecter les valeurs de départ") | any(output=="start"))inspect(fit, what=start)->Resultats$"Valeurs de départ"
    if(any(output== "Inspecter les matrices du modèle") | any(output=="modmat")) inspect(fit)->Resultats$"Matrices du modèles"
    if(any(output== "Inspecter la représentation du modèle") | any(output=="modrep"))inspect(fit, what=list)->Resultats$"Représentation du modèle"
    semPaths(fit, what="path", whatLabels="std", edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T)
    
    
    return(Resultats)
    
    
    
  } 
  
  
  packages<-c("svDialogs", "psych","lavaan","semPlot")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()    
  
  Resultats<-list()
  opt.list<-list(mimic=mimic,fixed.x=fixed.x,missing=missing,information=information,zero.keep.margins=zero.keep.margins,zero.add=zero.add,
                 estimator=estimator,group=group,test=test,se=se,std.ov=std.ov,orthogonal=orthogonal,likelihood=likelihood,
                 link=link,int.ov.free=int.ov.free,int.lv.free=int.lv.free,fixed.x=fixed.x,std.lv=std.lv,n.boot=n.boot,group.w.free=group.w.free,group.equal=group.equal)
  cfa.options<-cfa.in(modele=modele,X=X, data=data, ord=ord, outlier=outlier,imp=imp,output=output, info=T, opt.list=opt.list, sauvegarde=sauvegarde)
  if(is.null(cfa.options)) return(analyse())
  AFC<-cfa.out(cfa.options)
  if(!is.null(AFC)) Resultats$AFC<-AFC
  
  
  
  def.values<-list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                   estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                   link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                   group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                 "lv.variances" , "lv.covariances"))
  
  if(!is.null(cfa.options$ord)) paste(cfa.options$ord, collapse="','", sep="")->ord
  paste(cfa.options$output, collapse="','", sep="")->output
  call<-paste0("ez.cfa(modele='", cfa.options$modele, "',data=", cfa.options$nom, ",ord=", ifelse(is.null(cfa.options$ord), "NULL",paste0("c('",ord,"')")),",outlier='", outlier, 
               "', imp='",imp,"',output=c('", output,"'), sauvegarde=", cfa.options$sauvegarde, ", mimic='", cfa.options$opt.list$mimic, "'")
  
  for(i in 1:length(def.values)){
    if(names(def.values)[i]!="group" & names(def.values)[i]!="mimic") n<-which(names(cfa.options$opt.list) == names(def.values)[i]) else n<-NULL
    if(is.null(def.values[[i]])) call<-ifelse(is.null(cfa.options$opt.list$group),paste0(call, ", group=NULL"), paste0(call,", group =",cfa.options$opt.list$group)) 
    if(length(n)==1){
      if( def.values[[i]] !=cfa.options$opt.list[[n]]){
        if(is.logical(def.values[[i]]) ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"=",cfa.options$opt.list[[n]])
        if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))==1 ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"='",cfa.options$opt.list[[n]],"'")
        if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))>1 ){
          paste(cfa.options$opt.list[[n]], collapse="','", sep="")->param
          call<-paste0(call, ",", names(cfa.options$opt.list)[i],"=c('",param,"')") 
        } 
      }} 
  }
  call<-paste0(call,")")
  Resultats$Call<-call
  
  .add.history(data=cfa.options$data, command=Resultats$Call, nom=cfa.options$nom)
  .add.result(Resultats=Resultats, name =paste("AFC", Sys.time() ))  
  
  if(sauvegarde) save(Resultats=Resultats, choix="AFC", env=.e)
  Resultats$ref<-ref1(packages)
  return(Resultats)
  }






#### fonction qui permet de choisir lle type de correlation ÃÂ  rÃÂ©aliser
choix.corr<-function(){options (warn=-1) 
  c( "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  writeLines("l'analyse détaillée permet d'avoir les statistiques descriptives, les tests de normalité, le nuage de points,
\n des statistiques robustes, l'ensemble des coefficients de corrélations. 
\n la matrice de corrélation permet de contrôler l'erreur de 1e espèce et est adaptée pour un grand nombre de corrélations
\n la comparaison de corrélations permet de comparer 2 corrélations dépendantes ou indépendantes
\n Le choix + autre correlations + permet d'avoir les correlation tétrachoriques et polychoriques")
  dlgList(c("Analyse détaillee (Bravais Pearson/Spearman/tau) pour une ou peu de corrélations", 
            "Matrice de corrélations", 
            "Comparaison de deux corrélations",
            "Autres corrélations"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous?")$res->choix
  if(length(choix)==0) return(analyse())
  switch(choix,
         "Analyse détaillee (Bravais Pearson/Spearman/tau) pour une ou peu de corrélations"=corr.complet()->Resultats,
         "Matrice de corrélations"= corr.matrice()->Resultats,
         "Comparaison de deux corrélations"= comp.corr()->Resultats,
         "Autres corrélations"= tetrapoly()->Resultats
  )
  return(Resultats)
}

#### Comapraison de corrÃÂ©lations 
comp.corr<-function(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE){options (warn=-1) 
  #xy : value of the correlation between x and y
  #xz : value of the correlation between x and z
  #yz : value of the correlation between y and z. Should be null for independant comparisons et having a value for paired.
  # n : sample size for the correlation xy.
  # n2 : sample size for the correlation xz. 
  # twotailed : logical. Should the estimation of p be one(FALSE) or twotailed (TRUE). 
  
  c("psych", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  
  if((all(c(xy, yz, xz)<=1) & all(c(xy, yz, xz)>=-1)) & 
     all(c(n,n2)>0) & all(c(n,n2)%%1==0)) {
    paired.r(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->r
  } else {
    msgBox("Les valeurs des corrélations doivent être comprises entre -1 et 1/n
           et les effectifs doivent être des entiers positifs")
  }
  
  if(exists("r") && length(r$p)!=0 && !is.na(r$p)) {
    Resultats$"comparaison des deux corrélations"<-r
    Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
    data1<-data.frame()
    .add.history(data=data1, command=Resultats$call, nom=paste("comparaison des corrélations XY=", xy, "et YZ =", yz ))
    .add.result(Resultats=Resultats, name =paste("comparaison de corrélations", Sys.time() ))
    Resultats$"Références"<-ref1(packages)
    return(Resultats)
  } else{
    type<- dlgList(c("Corrélations appariées", "Corrélations indépendantes"), preselect=FALSE, multiple = TRUE, title="Comparaison de deux corrélations")$res
    if(length(type)==0) return(choix.corr())
    
    if(type=="Corrélations indépendantes") {
      Form <- list(
        "Corrélation entre XY:NUM" = 0,
        "N de la corrélation XY:NUM" = 100,
        "Corrélation entre XZ:NUM" = 0,
        "N de la corrélation XZ:NUM" = 100)
    }else{
      Form <- list(
        "Corrélation entre XY:NUM" = 0,
        "Corrélation entre XZ:NUM" = 0,
        "Corrélation entre YZ:NUM" = 0,
        "Taille de l'échantillon:NUM" = 100)
    }
    
    value<-dlgForm(Form, "Veuillez entrer les différentes valeurs")$res
    if(any(is.na(value))) {
      msgBox("Toutes les valeurs entrées ne sont pas numérique. Veuillez entrer des valeurs numériques uniquement")
      comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
      return(Resultats)
    }
    xy<-value$"Corrélation entre XY"
    xz<-value$"Corrélation entre XZ"
    yz<-value$"Corrélation entre YZ"
    if(type==  "Corrélations appariées"){n<-value$"Taille de l'échantillon"} else {
      n<-value$"N de la corrélation XY"
      n2<-value$"N de la corrélation XZ"
    }
    comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
    return(Resultats)
  }
  }

#### Analyse dÃÂ©taillÃÂ©e de corrÃÂ©lation simple/partielle/semi-partielle + Graphique
# fonction ok 

corr.complet<-function(X=NULL, Y=NULL, Z=NULL,data=NULL,  group=NULL, param=c("test paramétrique", "test non paramétrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens"), 
                       sauvegarde=F, outlier=c("Données complètes", "Identification des outliers","Analyse sans les valeurs influentes"),  z=NULL, info=T, n.boot=NULL, rscale=0.353){options (warn=-1) 
  
  
  corr.complet.in<-function(X=NULL, Y=NULL,Z=NULL, data=NULL, group=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T,n.boot=NULL, rscale=0.707){
    
    Resultats<-list()
    if(!is.null(X) & !is.null(data) & !is.null(Y)) {dial<-F 
    if(is.null(Z)) choix<-"Corrélations" else choix<-"Corrélations partielle et semi partielle"
    }  else {dial<-T
    choix<-NULL}
    
    if(is.null(choix) ){
      if(info) writeLines("Veuillez préciser le type de corrélation que vous souhaitez réaliser.")
      choix<-dlgList(c("Corrélations", "Corrélations partielle et semi partielle"), preselect="Corrélations", multiple = FALSE, title="Corrélations simples ou partielles?")$res
      if(length(choix)==0) return(NULL)
    }
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(NULL)
    nom<-data[[1]]
    data<-data[[2]]
    
    
    msg3<-"Veuillez choisir la variable en abcisse"
    msg4<-"Veuillez choisir la variable en ordonnée"
    
    X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variable-s en abcisse", out=NULL)
    if(is.null(X)) {
      corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                      n.boot=NULL, rscale=0.707)->Resultats
      return(Resultats)}
    data<-X$data
    X1<-X$X
    
    Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title="Variable-s en ordonnée", out=X1)
    if(is.null(Y)) {
      corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                      n.boot=NULL, rscale=0.707)->Resultats
      return(Resultats)}
    data<-Y$data
    Y<-Y$X 
    if(choix=="Corrélations partielle et semi partielle"){
      msg6<-"Veuillez préciser la ou les variables à contrôler" 
      Z<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title="Variable-s à contrôler", out=c(X1,Y))
      if(is.null(Z)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)}
      data<-Z$data
      Z<-Z$X 
    }
    
    
    if(dial){
      
      if(info==TRUE) writeLines("Si vous souhaitez réaliser l'analyse pour différents sous-échantillons en fonction d'un critère catégoriel (i.e; réaliser une analyse par groupe)
                                \n choisissez oui. Dans ce cas, l'analyse est realisée sur l'échantillon complet et sur les sous-échantillons.
                                \n Si vous désirez l'analyse pour l'échantillon complet uniquement, chosissez non.
                                \n l'analyse par groupe ne s'appliquent pas aux statistiques robustes.")
      dlgList(c("oui", "non"), preselect="non", multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
      if(length(par.groupe)==0) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL,param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      } 
      msg5<-"Veuillez choisir le facteur de classement catégoriel."
      if(par.groupe=="oui"){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title="Variable-s", out=c(X1,Y,Z)) 
      if(length(group)==0) {  corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                              n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)}
      data<-group$data
      group<-group$X 
      if(any(ftable(data[,group])<3)){
        msgBox("Certaines combinaisons des modalités ont moins de 3 observations. Vous devez avoir au moins 3 observations pour chaque combinaison")
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      }
      }
    }
    
    msg.options1<-"Le test paramétrique est la corrélation de Bravais-Pearson"
    msg.options2<- "Le test non paramétrique correspond au rho de Spearman et au tau de Kendall"
    
    options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                         choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
    if(is.null(options)){
      corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                      n.boot=NULL, rscale=0.707)->Resultats
      return(Resultats)
    }
    Resultats$choix<-choix
    Resultats$nom<- nom
    Resultats$data<-data
    Resultats$X<-X1
    Resultats$Y<-Y
    if(exists("Z")) Resultats$Z<-Z
    if(exists("group")) Resultats$group<-group
    Resultats$options<-options
    return(Resultats)
  }
  corr.complet.out<-function(X=NULL, Y=NULL, Z=NULL, data=NULL, choix=NULL, group=NULL, param=NULL,n.boot=NULL, rscale=0.353) {
    boot_BP<-function(data,i)cor(data[ , X1][i], data[ , Y1][i], use="complete.obs", method="pearson")
    boot_Spearman<-function(data,i)cor(data[ ,X1][i], data[ , Y1][i], use="complete.obs", method="spearman")
    boot_BPSP<-function(data,i)cor(data[ , X][i], data[ , Y1][i], use="complete.obs", method="pearson")
    boot_SpearmanSP<-function(data,i)cor(data[ ,X][i], data[ , Y1][i], use="complete.obs", method="spearman")
    list()->Resultats
    Resultats$"statistiques descriptives"<-.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=T)
    if(!is.null(group)) {Resultats$"statistiques descriptives par groupe"<-.stat.desc.out(X=c(X,Y,Z), groupes=group, data=data, tr=.1, type=3, plot=T) }
    
    
    if(choix== "Corrélations") {
      title<-"Corrélation de Bravais-Pearson"
      title2<-"Rho de Spearman"
      X1<-X
      Y1<-Y} else {
        title<-"Corrélation partielle de Bravais-Pearson"
        title2<-"Rho partiel de Spearman"
        modele1<-as.formula(paste0(X,"~",Z[1]))
        modele2<-as.formula(paste0(Y,"~", Z[1]))
        if(length(Z)>1) for(i in 2:length(Z)){
          modele1<-update(modele1, as.formula(paste0(".~.+",Z[i])))
          modele2<-update(modele2, as.formula(paste0(".~.+",Z[i])))
        }
        lm.r1<-lm(modele1, data)
        lm.r2<-lm(modele2, data)
        data$residus1<-lm.r1$residuals
        data$residus2<-lm.r2$residuals
        X1<-"residus1"
        Y1<-"residus2"
      }
    modele<-as.formula(paste0(X1,"~",Y1))
    lm.r<-lm(modele,na.action=na.exclude,data=data)
    resid(lm.r)->data$residus # recuperation du residu sur le modele lineaire
    
    if(any(param=="Bayes") | any(param=="Facteurs bayesiens") | any(param=="param") | any(param=="Test paramétrique"))  {
      Resultats$"Tests de normalité"<-.normalite(data=data, X="residus", Y=NULL)
      p<-ggplot(data, aes(x=data[, X1[1]], y=data[,Y1[1]]))+ geom_point(shape=1) 
      p+ labs(x = X, y=Y)->p
      p+ geom_smooth(method=lm)->p
      p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title)
      p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
      print(p) 
      if(!is.null(group)){
        p<-ggplot(data, aes(x=data[, X1[1]], y=data[,Y1[1]], color=data[,group[1]]))+ geom_point()
        p+ labs(x =X, y=Y, colour=group[1])->p
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title)
        if(length(group)>1) {p<-p+aes(shape=data[,group[2]])+ labs(shape=group[2]) } 
        if(length(group)>2){
          for(i in 3:length(group)){
            if(i==3) paste0(".~", group[3])->panneau
            if(i==4) paste0(group[4],"~", group[3])->panneau
            if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
            if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
          }
          p<-+ facet_grid(as.formula(panneau))
        }
        if(length(group)==1) p+ geom_smooth(method=lm)->p else p<-p+geom_smooth(method=lm, se=F)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        print(p)
      }
    }
    
    if(any(param=="param") | any(param=="Test paramétrique")){
      
      if(choix!="Corrélations") {
        cor.part<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")[1:3],
                         spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "pearson")[1:3])
        cor.part$estimate^2->cor.part$r.carre
        round(cor.part, 4)->cor.part
        cor.part$ddl<-(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")$n-2-length(Z))
        dimnames(cor.part)<-list(c("Corrélation partielle de Bravais Pearson","Corrélation semi-partielle de Bravais Pearson"), c("Corrélation", "valeur.p", "test.t", "r.carré","ddl"))
        Resultats$"Corrélation partielle/semi-partielle de Bravais Pearson"<-cor.part
        
      } else {
        BP<-cor.test(data[, X1], data[ ,Y1], method = "pearson")
        Resultats$"Corrélation de Bravais Pearson"<-round(data.frame("r"=BP$estimate,"r.deux"=BP$estimate^2, "IC lim inf"=BP$conf.int[1],"IC lim sup"=BP$conf.int[2], "t"=BP$statistic, 
                                                                     "ddl"=BP$parameter, "valeur.p"=BP$p.value),4)
      } 
      
      
      if(!is.null(group)){  
        if(choix=="Corrélations") {corr.g<-function(X2){ return(data.frame(BP.r= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$estimate,
                                                                           BP.ddl= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$parameter,
                                                                           BP.t= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$statistic,
                                                                           BP.p= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$p.value))}} else {
                                                                             corr.g<-function(X2){ return(data.frame(BP.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$estimate,
                                                                                                                     BP.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z])$n-2-length(Z),
                                                                                                                     BP.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$statistic,
                                                                                                                     BP.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$p.value))}   
                                                                           }
        
        BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
        BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T), 4) 
        if(length(group)==1) {gr.l<-expand.grid(levels(data[,group])) 
        names(gr.l)<-group}else gr.l<-expand.grid(sapply(data[,group],levels))
        
        dimnames(BPgroup)[[2]]<- c("BP.r", "BP.ddl", "BP.t", "BP.p")
        BPgroup<-data.frame(gr.l,BPgroup )
        if(choix!="Corrélations") Resultats$"Corrélation partielle de Bravais-Pearson par groupe"<-BPgroup else Resultats$"Corrélation de Bravais-Pearson par groupe"<-BPgroup
        
      }
    }
    if(any(param=="non param")| any(param=="Test non paramétrique")){
      
      p<-ggplot(data, aes(x=rank(data[, X1[1]]), y=rank(data[,Y1[1]])))+ geom_point(shape=1) 
      p<-p+ labs(x = X, y=Y)
      p<-p+ geom_smooth(method=lm)
      p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title2)
      p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
      print(p)
      
      
      
      if(choix!="Corrélations") {
        spear<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "spearman")[1:3],spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "spearman")[1:3])
        tau<-rbind(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3],spcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3])       
        spear<-round(spear,4)
        tau<-round(tau,4)
        spear$estimate^2->spear$r.carre
        round(spear, 4)->cor.part
        dimnames(spear)<-list(c("Rho partiel de Spearman","Rho semi-partiel de Spearman"), c("rho", "valeur.p", "t", "r.carré"))
        Resultats$"Rho partiel/semi partiel de Spearman"<-spear
        tau<-round(tau,4)
        dimnames(tau)<-list(c("Tau partiel de Kendall","Tau semi-partiel de Kendall"), c("tau", "z", "valeur.p"))
        Resultats$"Tau partiel/semi-partiel de Kendall"<-tau
      } else { Spear<-cor.test(data[,X1], data[ ,Y1], method = "spearman", exact=T, continuity=T)
      cor.test(data[,X1], data[ ,Y1], method = "kendall")->Kendall 
      Resultats$"Rho de Spearman"<-round(data.frame("rho"=Spear$estimate,"rho.deux"=Spear$estimate^2,"S"=Spear$statistic,"valeur.p"=Spear$p.value),4)
      round(data.frame("tau"=Kendall$estimate,"z"=Kendall$statistic,"valeur.p"=Kendall$p.value),4)->Resultats$"Tau de Kendall"}
      
      
      if(!is.null(group)){
        if(choix=="Corrélations") {corr.g<-function(X2){ return(data.frame(BP.r= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$estimate,
                                                                           BP.ddl= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$p.value,
                                                                           BP.t= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$estimate,
                                                                           BP.p= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$p.value))}
        } else {
          corr.g<-function(X2){ return(data.frame(BP.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                  BP.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z],method="spearman")$n-2-length(Z),
                                                  BP.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                  BP.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$p.value))
          }}   
        
        BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
        BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T),4)
        if(length(group)==1) {gr.l<-expand.grid(levels(data[,group]))
        names(gr.l)<-group}else gr.l<-expand.grid(sapply(data[,group],levels))
        BPgroup<-data.frame(gr.l,BPgroup )
        if(choix!="Corrélations"){
          dimnames(BPgroup)[[2]]<- c(group, "Spearman.rho", "Spearman.ddl", "Spearman.t", "Spearman.p")
          Resultats$"Corrélation partielle de Spearman par groupe"<-BPgroup 
        } else {dimnames(BPgroup)[[2]]<- c(group, "Spearman.r", "Spearman.p", "Tau.Kendall.r", "Tau.Kendall.p")
        Resultats$"Corrélation de Spearman/Kendall par groupe"<-BPgroup}
        p<-ggplot(data, aes(x=rank(data[, X1[1]]), y=rank(data[,Y1[1]]), color=data[,group[1]]))+ geom_point()
        p+ labs(x =X, y=Y, colour=group[1])->p
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title2)
        if(length(group)>1) {p<-p+aes(shape=data[,group[2]])+ labs(shape=group[2]) } 
        if(length(group)>2){
          for(i in 3:length(group)){
            if(i==3) paste0(".~", group[3])->panneau
            if(i==4) paste0(group[4],"~", group[3])->panneau
            if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
            if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
          }
          p<-+ facet_grid(as.formula(panneau))
        }
        if(length(group)==1) p+ geom_smooth(method=lm)->p else p<-p+geom_smooth(method=lm, se=F)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        print(p)
        
        
      }
    }
    
    if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
      boot_BP_results<-boot(data, boot_BP, n.boot)
      if(!is.null(Resultats$"Corrélation de Bravais Pearson")) {
        try(Resultats$"Corrélation de Bravais Pearson"$"Bca lim inf"<-round( boot.ci(boot_BP_results)$bca[,4],4), silent=T)
        try(Resultats$"Corrélation de Bravais Pearson"$"Bca lim sup"<-round( boot.ci(boot_BP_results)$bca[,5],4),silent=T)
      } else if(!is.null(Resultats$"Corrélation partielle/semi-partielle de Bravais Pearson")) {
        boot_BPSP_results<-boot(data, boot_BPSP, n.boot)  
        try(Resultats$"Corrélation partielle/semi-partielle de Bravais Pearson"$"Bca lim inf"<-round( c(boot.ci(boot_BP_results)$bca[,4], boot.ci(boot_BPSP_results)$bca[,4]),4),silent=T)
        try(Resultats$"Corrélation partielle/semi-partielle de Bravais Pearson"$"Bca lim sup"<-round( c(boot.ci(boot_BP_results)$bca[,5], boot.ci(boot_BPSP_results)$bca[,5]) ,4), silent=T)
      } else try(Resultats$"Bootstrap sur la corrélation de Bravais Pearson"<-round(data.frame("Bca.lim.inf"= boot.ci(boot_BP_results)$bca[,4], " Bca.lim.sup"=boot.ci(boot_BP_results)$bca[,5] ), 4),silent=T)
      
      if(any(param=="non param")| any(param=="Test non paramétrique")) {
        boot_Spearman_results<-boot(data, boot_Spearman, n.boot)
        if(!is.null(Resultats$"Rho de Spearman")) {
          try(Resultats$"Rho de Spearman"$"Bca lim inf"<-round( boot.ci(boot_Spearman_results)$bca[,4],4), silent=T)
          try(Resultats$"Rho de Spearman"$"Bca lim sup"<-round( boot.ci(boot_Spearman_results)$bca[,5],4), silent=T)
        } else{
          boot_SpearmanSP_results<-boot(data, boot_SpearmanSP, n.boot)
          
          try(Resultats$"Rho partiel/semi partiel de Spearman"$"Bca lim inf"<-round(c( boot.ci(boot_Spearman_results)$bca[,4], boot.ci(boot_SpearmanSP_results)$bca[,4]),4), silent=T)
          try(Resultats$"Rho partiel/semi partiel de Spearman"$"Bca lim sup"<-round(c( boot.ci(boot_Spearman_results)$bca[,5], boot.ci(boot_SpearmanSP_results)$bca[,5]),4), silent=T)
        } 
        
      }
    }
    
    
    if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
      
      BF<-regressionBF(modele, data=data, rscaleCont=rscale )
      sample<-posterior(BF, iterations = ifelse(is.null(n.boot), 1000, n.boot))
      BF<-extractBF(BF, onlybf=F)
      BF<-data.frame("Facteur bayesien"=c(ifelse(BF$bf>10000,">10000", round(BF$bf,5)), 
                                          ifelse(1/BF$bf>10000, ">10000", round((1/BF$bf),5))), "Erreur"=round(c( BF$error, BF$error),5))
      
      dimnames(BF)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle")
      # what is the t-value for the data?
      r2Val <-cor.test(data[,X1],data[,Y1])$estimate
      BF$r<-r2Val
      r2Val<-r2Val^2
      BF$r.carre<-r2Val
      Resultats$"Facteurs Bayesiens pour la corrélation de Bravais-Pearson"<-BF
      
      if(any(param=="non param")| any(param=="Test non paramétrique")) {
        data2<-sapply(data[,c(X,Y,Z)], rank, ties.method="average", na.last="keep")
        data2<-data.frame(data2)
        if(choix!="Corrélations"){
          lm.r1<-lm(modele1, data2)
          lm.r2<-lm(modele2, data2)
          data2$residus1<-lm.r1$residuals
          data2$residus2<-lm.r2$residuals
        }
        
        BFS<-regressionBF(modele, data=data2, rscaleCont=rscale )
        BFS<-extractBF(BFS, onlybf=F)
        BFS<-data.frame("Facteur bayesien"=c(ifelse(BFS$bf>10000,">10000", round(BFS$bf,5)), 
                                             ifelse(1/BFS$bf>10000, ">10000", round((1/BFS$bf),5))), "Erreur"=round(c( BFS$error, BF$error),5))
        dimnames(BFS)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle")
        Resultats$"Facteurs Bayesiens pour la corrélation de Spearman"<-BFS
        
      }
      
      if(!is.null(group)){
        
        corr.g<-function(X2){  BF<-regressionBF(modele, X2, rscaleCont=rscale ,progress=F)
        BF<-extractBF(BF, onlybf=F)
        return(data.frame("Facteur bayesien"=round(BF$bf,5), "Erreur"=round(BF$error,5)))}
        
        BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
        BPgroup<-round(matrix(unlist(BPgroup), ncol=2, byrow=T), 4) 
        dimnames(BPgroup)[[2]]<- c("FB", "erreur")
        if(length(group)==1) {gr.l<-expand.grid(levels(data[,group])) 
        names(gr.l)<-group}else gr.l<-expand.grid(sapply(data[,group],levels))
        BPgroup<-data.frame(gr.l,BPgroup )
        
        
        
        if( any(param=="non param")| any(param=="Test non paramétrique")){
          BFgroupS<-by(data=data2, INDICES=data[,group], FUN=corr.g)
          BFgroupS<-matrix(unlist(BFgroupS), ncol=2, byrow=T)
          BPgroup<-cbind(BPgroup, BFgroupS)
          names(BPgroup)<-c(group, "FB.BP","Erreur.BP", "FB.Spearman", "Erreur.Spearman")
        }  
        BPgroup->Resultats$"Facteur bayesien par groupe"
      }
      
      plot(sample)
      bfs<-c()
      for (i in 5:length(data[,X1])) {
        bfm <- regressionBF(modele, data=data[1:i,],progress=F, rscaleCont=0.353)
        bfl <- regressionBF(modele, data=data[1:i,], progress=F, rscaleCont=0.5)
        bful <- regressionBF(modele,data=data[1:i,], progress=F, rscaleCont=0.707)
        bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
      }
      
      SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs, 
                      "rscale"=rep(c("moyen - 0.353", "large - 0.5", "ultra large - 0.707"), length.out= 3*(length(data[,X])-4) ))
      names(SBF)<-c("n", "BF", "rscale")
      reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
      .plotSBF(SBF)
      
      ##### Début du graphique  Bayes Factor Robustness Check     
      
      
      # linearReg.R2stat(N=length(data[,1]), p=1, R2=r2val, rscale = x, simple = T)  
      
      # how many points in the prior should be explored?
      nPoints <- 1000
      # what Cauchy rates should be explored?
      cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
      # what effect sizes should be plotted?
      effSize <- seq(from = -2, to = 2, length.out = 1000)
      
      # get the Bayes factor for each prior value
      bayesFactors <- sapply(cauchyRates, function(x) exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = x, simple = T)))
      
      exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.353, simple = T))->r1
      exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.5, simple = T))->r2
      exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.707, simple = T))->r3
      plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
      # do the Bayes factor plot
      if(max(bayesFactors)>10^40) bayesFactors[which(bayesFactors>10^40)]<-10^40
      if(r1>10^40) r1<-10^40
      if(r2>10^40) r2<-10^40
      if(r3>10^40) r3<-10^40
      seq(min(bayesFactors),  max(bayesFactors), length.out = 5)->axe2
      format(axe2, scientific=T)->axe2b
      par(mar = c(4, 10, 0.5, 0.5), mgp = c(8, 1, 0))
      plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", ylim= c(min(bayesFactors), max(bayesFactors)),
           yaxt = "n"    , xaxt = "n",  xlab = "Cauchy Prior Width (r)" , ylab = "Bayes Factor (10)")
      axis(2, labels=axe2b, at=axe2, las=2)
      abline(h = 0, lwd = 1)
      abline(h = 6, col = "black", lty = 2, lwd = 2)
      axis(1, at = seq(0, 1.5, 0.25))
      
      
      
      # add the BF at the default Cauchy point
      points(2^0.5/4, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
      points(0.5, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
      points(2^0.5/2, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
      # add legend
      legend(x="topright", legend = c("r = 0.353 - medium", "r = 0.5 - wide ", "r = 0.707 - ultrawide"),
             pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
             col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
      
      
    }
    
    return(Resultats)
  }
  
  
  # package supprimé "plyr",
  packages<-c("BayesFactor", "boot", "ggplot2","nortest", "ppcor","outliers","psych",  "svDialogs")
  
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  Resultats<-list()
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data  
  
  corr.options<-corr.complet.in(X=X, Y=Y,Z=Z, data=data, group=group, param=param, outlier=outlier, sauvegarde=sauvegarde, info=T, n.boot=n.boot, rscale=rscale)
  if(is.null(corr.options)) return(analyse())
  choix<-corr.options$choix
  X<-corr.options$X
  Y<-corr.options$Y
  Z<-corr.options$Z
  group<-corr.options$group
  data<-corr.options$data
  param<-corr.options$options$choix
  if(corr.options$options$rscalei==T) rscale<-corr.options$options$rscale/2 else rscale<-corr.options$options$rscale
  n.boot<-corr.options$options$n.boot
  sauvegarde<-corr.options$options$sauvegarde
  outlier<-corr.options$options$desires
  
  expand.grid(X,Y)->XY
  for(i in 1:length(XY[,1]))
  {
    X1<-as.character(XY[i,1])
    Y1<-as.character(XY[i,2])
    data1<-data[complete.cases(data[,c(Y1,X1,Z)]),]
    R1<-list()
    if(any(outlier==  "Données complètes")){
      R1$"Données complètes"<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=data1, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
    } 
    if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Données sans valeur influente")){
      modele<-as.formula(paste0(X1,"~",Y1))
      if(!is.null(Z)){for(i in 1:length(Z))      modele<-update(modele, as.formula(paste0(".~.+",Z[i])))}
      data1$residu<-resid(lm(modele, data=data1))
      critere<-ifelse(is.null(z), "Grubbs", "z")
      valeurs.influentes(X="residu", critere=critere,z=z, data=data1)->influentes
    }
    if(any(outlier== "Identification des valeurs influentes")){influentes->R1$"Valeurs influentes"}
    if(any(outlier== "Données sans valeur influente")) {
      if(length(influentes$"observations influentes")!=0 | all(outlier!="Données complètes")){
        get("nettoyees", envir=.GlobalEnv)->nettoyees
        R1$"Données sans valeur influente"<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=nettoyees, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
      }
    }
    Resultats[[i]]<-R1
    names(Resultats)[i]<-paste("Corrélation entre la variable", X1, "et la variable", Y1)
  }
  
  paste(X, collapse="','", sep="")->X
  paste(Y, collapse="','", sep="")->Y
  if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
  if(!is.null(group)) paste(group, collapse="','", sep="")->group
  
  
  paste(outlier,  collapse="','", sep="")->outlier
  paste(param,  collapse="','", sep="")->param
  Resultats$Call<-paste0("corr.complet(X=c('", X,
                         "'), Y=c('", Y, 
                         "'), Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom, 
                         ", group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"), 
                         ", param=c('", param, "'), sauvegarde=", sauvegarde, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),", info=T, rscale=", rscale, ", n.boot=", n.boot, ")")
  
  .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
  .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
  
  if(sauvegarde){save(Resultats=Resultats ,choix =choix, env=.e)}
  
  ref1(packages)->Resultats$Références
  ### Obtenir les Resultats
  return(Resultats) 
  }


#### Matrice de corrÃÂ©lations BP, SPEARMAN, KENDALL. Possiblités d'avoir corrélations partielles
corr.matrice<-function(X=NULL, Y=NULL, Z=NULL,data=NULL, group=NULL,method="pearson",param=c("H0","FB"), sauvegarde=F, outlier=c("Données complètes"),n.boot=1,  rscale=0.354, info=T,
                       p.adjust="holm",out.m=2, na.rm=NULL) { 
  # X : character or vector. First set of variables
  # Y : character or vector. Second set of variables Must be NULL if Z is not
  # Z : character or vector. Names of the variables to control in partial correlation. Must be NULL if Y is not
  # data : dataset
  # group : character or vector. Names of the classifying variables 
  # method : one among c("pearson", "spearman", "kendall") 
  # param :  one or both among "H0" (null hypoethesis testing) et "FB"(bayesian factors)
  # sauvegarde : logical. Must the analyses be saved ? 
  # outlier : One among   c("Données complètes", "Données sans valeur influente")
  # rscale : numeric. If not null, bayesian factors are computed. Can also be "moyen", "large", "ultralarge"
  # info : logical. Must information be displayed in dialog box interface. 
  # correction : character. Probability adjustement. See p.adjust for list of possibilities
  # out.m : 1 for deleting one observation at the time in outlier detection. 2 for all at the same time. 
  # na.rm : character. How to deal with missing values ? 
  
  corr.matrice.in<-function(X=NULL, Y=NULL, Z=NULL, group=NULL, data=NULL, p.adjust="holm", rscale=0.354,sauvegarde=F,outlier="Données complètes", info=T, method="pearson", param=c("H0","FB"), n.boot=NULL){
    Resultats<-list()
    if(!is.null(X) & !is.null(data) & (is.null(Y) | is.null(Z))) {dial<-F 
    if(is.null(Z)) choix<-"Corrélations" else choix<-"Corrélations partielle et semi partielle"
    if(!is.null(Y)) carre<-"rectangulaire" else carre<-"carrée"
    }  else {dial<-T
    choix<-NULL}
    
    if(is.null(choix) ){
      if(info) writeLines("Veuillez préciser le type de corrélation que vous souhaitez réaliser.")
      choix<-dlgList(c("Corrélations", "Corrélations partielles"), preselect="Corrélations", multiple = FALSE, title="Corrélations ou corrélations partielles?")$res
      if(length(choix)==0) return(NULL)
    }
    
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(NULL)
    nom<-data[[1]]
    data<-data[[2]]
    
    if(choix=="Corrélations" & dial==T){
      writeLines("Une matrice carrée est une matrice avec toutes les Corrélations 2 à 2. 
                 Une matrice rectangulaire est une matrice dans laquelle un premier ensemble de variables est mis en corrélations avec un second jeu de variables")
      carre<-dlgList(c("carrée", "rectangulaire"), multiple = FALSE, title="type de matrice")$res
      if(length(carre)==0){Resultats<-corr.matrice.in()
      return(Resultats)}
    } else carre<-"carrée"
    
    msg3<-"Veuillez choisir le premier jeu de variables"
    
    
    X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
    if(is.null(X)) {
      corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                      n.boot=NULL, rscale=0.353)->Resultats
      return(Resultats)}
    data<-X$data
    X1<-X$X
    if(carre=="rectangulaire"){
      msg4<-"Veuillez choisir le second jeu de variables"
      Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title="Second jeu de variables", out=X1)
      if(is.null(Y)) {
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-Y$data
      Y<-Y$X 
      
    }
    if(choix=="Corrélations partielles"){
      msg6<-"Veuillez préciser la ou les variables à contrôler" 
      Z<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title="Variable-s à contrôler", out=c(X1,Y))
      if(is.null(Z)) {
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-Z$data
      Z<-Z$X 
    }
    
    
    if(dial){
      
      if(info==TRUE) writeLines("Si vous souhaitez réaliser l'analyse pour différents sous-échantillons en fonction d'un critère catégoriel (i.e., réaliser une analyse par groupe)
                                \n choisissez oui. Dans ce cas, l'analyse est realisée sur l'échantillon complet et sur les sous-échantillons.
                                \n Si vous désirez l'analyse pour l'échantillon complet uniquement, chosissez non.")
      dlgList(c("oui", "non"), preselect="non", multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
      if(length(par.groupe)==0) {
        corr.matrice.in(X=NULL, Y=NULL, data=NULL,method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)
      } } else par.groupe<-"non"
    msg5<-"Veuillez choisir le facteur de classement catégoriel."
    if(par.groupe=="oui" || !is.null(group)){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title="Variable-s", out=c(X1,Y,Z)) 
    if(length(group)==0) {   corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                             n.boot=NULL, rscale=0.353)->Resultats
      return(Resultats)}
    data<-group$data
    group<-group$X 
    if(any(ftable(data[,group])<3)){
      msgBox("Certaines combinaisons des modalités ont moins de 3 observations. Vous devez avoir au moins 3 observations pour chaque combinaison")
      corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                      n.boot=NULL, rscale=0.353)->Resultats
      return(Resultats)
    }
    }
    
    if(dial || length(outlier)>1 || outlier %in% c("Données complètes", "Données sans valeur influente") ==FALSE){
      if(info) writeLines("Désirez-vous l'analyse sur les données complètes ou sur les données pour lesquelles les valeurs influentes ont été enlevées ?")
      outlier<- dlgList(c("Données complètes", "Données sans valeur influente"), preselect=c("Données complètes"),
                        multiple = FALSE, title="Quels résultats voulez-vous obtenir ?")$res
      if(length(outlier)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                          n.boot=NULL, rscale=0.353)
      return(Resultats)}
    }
    if(dial || length(method)>1 || method %in% c("pearson", "spearman","kendall") ==FALSE){
      if(info) writeLines("Veuillez choisir le type de corrélations que vous désirez réaliser")
      method<-dlgList(c("pearson", "spearman","kendall"), preselect="pearson", multiple = FALSE, title="Type de corrélations ?")$res
      if(length(method)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                         n.boot=NULL, rscale=0.353)
      return(Resultats)}
    }
    
    
    if(is.null(Y) & is.null(Z)){
      
      if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
        msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
        n.boot<-NULL
      }
      while(is.null(n.boot)){
        writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
        
        n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
        if(length(n.boot)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                          n.boot=NULL, rscale=0.353)
        return(Resultats)}
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->n.boot
        if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
          msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
          n.boot<-NULL
        }
      }
    } 
    
    
    if((dial)|| !is.null(rscale) & ((is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("moyen", "large", "ultralarge")==F))) {
      if(info) writeLines("Voulez-vous les tests d'hypothèes nuls ou/et les facteurs bayesiens ?")   
      param<-dlgList(c("Facteurs bayesiens","Tests de H0"), preselect=c("Facteurs bayesiens","Tests de H0"), multiple = T, title="Approche statistique ?")$res
      if(length(param)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                        n.boot=NULL, rscale=0.353)
      return(Resultats)}
      
      if(any(param=="Facteurs bayesiens") | any(param=="FB")){
        if(info) writeLines("Veuillez préciser la distribution a priori de Cauchy")
        
        rscale<-dlgList(c("moyen", "large", "ultralarge"), preselect="moyen", multiple = F, title="Quelle distribution voulez-vous  ?")$res 
        if(length(rscale)==0) {
          Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                     n.boot=NULL, rscale=0.353)
          return(Resultats)
        }
        ifelse(rscale=="moyen", rscale<-2^0.5/4, ifelse(rscale=="large", rscale<-0.5, ifelse(rscale=="ultralarge", rscale<-2^0.5/2, rscale<-rscale)))} else rscale<-NULL
    } 
    
    if(any(param=="Tests de H0") |any(param=="H0")){
      if(dial | length(p.adjust)!=1 || p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")==FALSE){
        writeLines("Veuillez préciser le type de correction de la probabilité que vous désirez réaliser")
        dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect=NULL, multiple = FALSE, title="Type de correction ?")$res->p.adjust
        if(length(p.adjust)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      } 
    } else p.adjust<-"none"
    if(dial | length(sauvegarde)!=1 || !is.logical(sauvegarde )){
      writeLines("voulez-vous sauvegarder les résultats")
      sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title="Enregistrer les résultats ?")$res
      if(length(sauvegarde)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)->Resultats
      return(Resultats)}
      
    } 
    
    if(any(is.na(data[,c(X1,Y,Z)]))){ 
      msgBox("Des valeurs manquantes ont été détectées. Comment voulez-vous les traiter ? Garder l'ensemble des observations peut biaiser les résultats.")
      imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes", "Remplacer par la moyenne",
                      "Remplacer par la médiane","Multiple imputation - Amelia"), preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes")$res
      if(length(imp)==0){
        Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,
                                   n.boot=NULL, rscale=0.353)
        return(Resultats)
      }
      data1<-ez.imp(data[, c(X1,Y,Z)], imp=imp)
      data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),group])
  }
    
    Resultats$nom<- nom
    Resultats$data<-data
    Resultats$X<-X1
    if(exists("Y")) Resultats$Y<-Y
    if(exists("Z")) Resultats$Z<-Z
    if(exists("group")) Resultats$group<-group
    Resultats$method<- method
    Resultats$outlier<-outlier
    Resultats$param<-param
    Resultats$rscale<-rscale
    Resultats$n.boot<-n.boot
    Resultats$sauvegarde<-sauvegarde
    Resultats$p.adjust<-p.adjust
    return(Resultats)
}

  
  
  corr.matrice.out<-function(data, X, Y, Z, p.adjust, method,sauvegarde, rscale, n.boot, param){
    Resultats<-list()
    Resultats$"Statistiques descriptives"<-.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=F)
    Resultats$"Normalité multivariée"<-.normalite(data, c(X,Y,Z)) 
    
    if(is.null(Z)){
      if(is.null(Y)) { Y<-NULL
      pairs.panels(data[,X], density=T, lm=T, digits=3, ellipses=F, method=method, cor=T, jiggle=F, smoother=F, stars=T, pch=".")}else {
      Y1<-as.data.frame(data[,Y])
      names(Y1)<-Y
      }
      X1<-as.data.frame(data[,X])
      names(X1)<-X
      corr.test(x=X1, y=Y1, use = "pairwise",method=method,adjust=p.adjust, alpha=.05,ci=TRUE)->matrice  
      r1<-round(matrice$r,3)
      if(is.null(Y)) r1[which(lower.tri(r1, diag = T))]<-"-"
      Resultats$"Matrice de corrélation"<-as.data.frame(r1)
      
    } else{
      data[,c(X,Z)]->d2
      partial.r(d2, 1:length(X), (length(X)+1):length(d2))->matrice
      matrice<-corr.p(matrice, adjust=p.adjust, n=length(data[,1])-length(Z))
      
      r1<-round(matrice$r, 3)
      class(r1)<-"matrix"
      r1[which(lower.tri(r1, diag = T))]<-"-"
      Resultats$"Matrice de Corrélations partielles" <-as.data.frame(r1)
    }    
    
    class(r1)<-"matrix"
    dimnames(r1)[[1]]<-paste(dimnames(r1)[[1]], "r")
    matrice$n->Resultats$"taille de l'échantillon"
    
    if(any(param=="H0")|any(param=="Tests de H0")) {paste("la correction appliquée est la correction de",p.adjust)->Resultats$Correction[1]
      if(is.null(Y)) Resultats$Correction[2]<-"Seules les valeurs au-dessus de la diagonales sont ajustées pour comparaisons multiples"
      round(matrice$p,3)->r2
      class(r2)<-"matrix"
      Resultats$"matrice des probabilités"<-r2
      dimnames(r2)[[1]]<-paste0(dimnames(r2)[[1]], ".p")
      if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-NA
      r1<-rbind(r1,r2)
    }
    if(method=="kendall") {
      r2<-round(sin(0.5*pi*matrice$r)^2,3) # from David A. Walker 2003 JMASM9: Converting Kendall's Tau For Correlational Or Meta-Analytic Analyses 
      Resultats$"Information"<-"La taille d'effet est calculée à partir de la formule proposée par Walker, 2003"   
    } else r2<-round(matrice$r^2,3)
    
    
    
    if(!is.null(rscale)){
      r2[which(r2==1)]<-0
      if(is.null(Z))  N<-length(data[,1]) else    N<-length(data[,1])-length(Z)
      matriceBF<-function(X){return(linearReg.R2stat(N=N, 1, X, rscale = rscale, simple = TRUE))}
      r3<-round(apply(X=r2,c(1,2), FUN=matriceBF),3)
      r3<-format(r3, scientific=T)
      if(is.null(Y)) r3[which(lower.tri(r3, diag = T))]<-"-"
      dimnames(r3)[[1]]<-paste0(dimnames(r3)[[1]], ".FB")
      Resultats$"Facteurs bayesiens"<-as.data.frame(r3)
      r1<-rbind(r1, r3)
    }
    class(r2)<-"matrix"
    if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-"-"
    Resultats$"matrice des r.deux" <-as.data.frame(r2)
    dimnames(r2)[[1]]<-paste(dimnames(r2)[[1]], "r^2")
    r1<-rbind(r1, r2)
    r1<-r1[order(rownames(r1)), order(colnames(r1))]
    r1[which(is.na(r1))]<-"-" 
    #View(r1)
    if(sauvegarde) {
      tps<-as.character(Sys.time())
      tps<-gsub(":",".",tps)
      nom<-paste0("corr.mat.",tps, ".doc")
      rtf<-RTF(nom,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
      addTable(rtf,r1,font.size=12,row.names=TRUE,NA.string="-" )
      done(rtf)
    }
    
    if(is.null(Y) & is.null(Z) & n.boot>100) round(cor.ci(data[,X], n.iter=n.boot, plot=FALSE)$ci,4)->Resultats$"Intervalle de confiance estimé par bootstrap" else  round(matrice$ci,4)->Resultats$"Intervalle de confiance" 
    
    
    return(Resultats)  
    
  }
  
  options (warn=-1) 
  packages<-c("BayesFactor","nortest", "psych",  "rtf", "svDialogs")
  
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  Resultats<-list()
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data  
  
  corr.options<-corr.matrice.in(X=X, Y=Y, Z=Z, data=data, group=group, param=param, outlier=outlier, sauvegarde=sauvegarde, info=T,  rscale=rscale, n.boot=n.boot)
  if(is.null(corr.options)) return(analyse())

  choix<-corr.options$choix
  X<-corr.options$X
  Y<-corr.options$Y
  Z<-corr.options$Z
  group<-corr.options$group
  data<-corr.options$data
  param<-corr.options$param
  rscale<-corr.options$rscale
  sauvegarde<-corr.options$sauvegarde
  outlier<-corr.options$outlier
  method<-corr.options$method
  p.adjust<-corr.options$p.adjust
  n.boot<-corr.options$n.boot
  
  if(outlier=="Données sans valeur influente"){
    inf<-VI.multiples(data[,c(X,Y,Z)])
    Resultats$"Valeurs considérées comme influentes"<-inf$"Valeurs considérées comme influentes"
    data<-inf$data
  }
  
  Resultats$"Matrice des corrélations"<-corr.matrice.out(data=data, X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,sauvegarde=sauvegarde, rscale=rscale, n.boot=n.boot, param=param)
  
  
  
  if(!is.null(group))   {
    G<-data[,group]
    if(length(group)>1) G<-as.list(G)
    G<-split(data[,c(X,Y,Z)], G)
    for(i in 1:length(G)){
      resg<-corr.matrice.out(data=G[[i]], X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,sauvegarde=sauvegarde, rscale=rscale, n.boot=n.boot, param=param)  
      Resultats[[length(Resultats)+1]]<-resg
      names(Resultats)[length(Resultats)]<-names(G)[i]
    }
  } 
  
  
  paste(X, collapse="','", sep="")->X
  if(!is.null(Y)) paste(Y, collapse="','", sep="")->Y
  if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
  if(!is.null(group)) paste(group, collapse="','", sep="")->group
  
  
  paste(outlier,  collapse="','", sep="")->outlier
  paste(param,  collapse="','", sep="")->param
  Resultats$Call<-paste0("corr.matrice(X=c('", X,
                         "'), Y=", ifelse(!is.null(Y),paste0("c('",Y,"')"), "NULL"), 
                         ", Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom, ", p.adjust='", p.adjust,
                         "', group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"), 
                         ", param=c('", param, "'), sauvegarde=", sauvegarde, ",outlier=c('", outlier, "'), info=T, rscale=", ifelse(!is.null(rscale),rscale, "NULL"), ", n.boot=", n.boot, ")")
  
  .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
  .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
  
  
  
  if(sauvegarde) save(Resultats=Resultats, choix=paste("corrélation de", method), env=.e)
  ref1(packages)->Resultats$Références
  return(Resultats)
  }




#### Analyse factorielle exploratoire, analyse en composante principale, analye factorielle confirmatoire ####
# il reste l'analyse factorielle confirmatoire, corriger les corrÃÂ©lations tetrachoriques et polychoriques qui bug sur mac
# vÃÂ©rifier le fichier de sortie 

factor.an<-function(data=NULL, X=NULL, nF=NULL, rotation="none", methode="ml", sat=0.3, outlier=c("Données complètes"),
                    imp=NULL, ord=NULL, sauvegarde=FALSE, scor.fac=FALSE,n.boot=1, hier=F, nfact2=1, choix="afe",info=T){
  
  # data : dataframe
  # X : character. Vector of variable names
  # nF : number of factors
  # rotation : character. One among c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
  # "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
  # methode : character. One among c("ml", "minres" "minchi", "wls","gls","pa")
  # sat : numeric. Level of loading below which loading is not printed. 
  # outlier : one among "Données complètes" or "Données sans valeur influente"
  # imp : character. How should missing values be treated ? One among "mean" (use mean), "median" (use median), "amelia", "rm" (remove)
  # ord : character vector. Which variables among X are ordinal ? (or dichotomous)
  # sauvegarde : logical. Should result be saved in rtf ? 
  # n.boot : integer. Number of iterations for bootstrap. 
  # hier : Logical. Should hierarchical factor analysis be done. Possible only if nF>1, methode is not "pa" and rotation is oblique. 
  # nfact2 : number of factors for hierarchical level. Must be inferior to nF/2 
  # choix : character. One among "afe" and "acp". If afc is choosen, open dialog box for confirmatory factor analysis
  # info : Logical. Should information be printed in the console when using dialog boxes. 
  
  
  fa.in<-function(data=NULL, choix=NULL, X=NULL, imp=NULL, ord=NULL, nF=NULL, rotation="none", methode="minres", sat=NULL, 
                  scor.fac=FALSE,n.boot=NULL, info=T, outlier=NULL,hier=NULL, nfact2=1, sauvegarde=F){
    
    Resultats<-list()
    if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
    if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Analyse factorielle exploratoire","afe",
                                                                 "afc","acp","Analyse factorielle confirmatoire","Analyse en composante principale")==FALSE){
      dial<-T  
      if(info) writeLines("Veuillez choisir l'analyse que vous désirez réaliser.")
      dlgList(c("Analyse factorielle exploratoire", 
                "Analyse factorielle confirmatoire",
                "Analyse en composante principale"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez vous realiser?")$res->choix
      if(length(choix)==0) return(NULL)
      if(choix=="Analyse factorielle confirmatoire") return(ez.cfa())
      try( windows(record=T), silent=T)->win
      if(class(win)=="try-error") quartz()
      
    }
    
    
    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL) 
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    if(choix=="fa" | choix=="Analyse factorielle exploratoire") msg3<-"Veuillez choisir les variables que vous désirez analyser. Vous devez choisir au moins 5 variables" else{
      msg3<-"Veuillez choisir les variables que vous désirez analyser. Vous devez choisir au moins 3 variables"
    }
    
    X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
    data<-X$data
    X<-X$X
    if(is.null(X) || length(X)<3) {
      Resultats<-fa.in()
      return(Resultats)}
    
    
    
    if(dial || length(outlier)>1 || outlier %in% c("Données complètes", "Données sans valeur influente") ==FALSE){
      if(info) writeLines("Désirez-vous l'analyse sur les données complètes ou sur les données pour lesquelles les valeurs influentes ont été enlevées ?")
      if(info) writeLines("les valeurs influentes sont identifiées sur la base de la distance de Mahalanobis avec un seuil du chi à 0.001")
      outlier<- dlgList(c("Données complètes", "Données sans valeur influente"), preselect="Données complètes",multiple = FALSE, title="Quels résultats voulez-vous obtenir ?")$res
      if(length(outlier)==0) { Resultats<-fa.in()
      return(Resultats)}
    }
    
    if(outlier=="Données sans valeur influente"){
      inf<-VI.multiples(data[,X])
      Resultats$"Valeurs considérées comme influentes"<-inf$"Valeurs considérées comme influentes"
      data<-inf$data
    }
    
    
    
    if(dial){
      if(info) writeLines("Veuillez préciser le type de variables. Des corrélations tétra/polychoriques seront réalisées sur les variables dichotomiques/ordinales et Bravais-Pearson sur les variables continues")
      if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c("dichotomiques/ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else {
        type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res 
      }
      
      if(length(type)==0) {Resultats<-fa.in()
      return(Resultats)}
    } else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"
    }
    
    
    if(type=="continues"){ methode<-c("ml")
    cor<-"cor"
    Matrice<-corr.test(data[,X], method="pearson")$r }else {
      cor<-"poly"
      methode<-c("minres")
      if(type=="mixte") {cor<-"mixed"
      if(info) writeLines("Veuillez préciser les variables ordinales ?") 
      ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
      if(length(ord)==0) {Resultats<-fa.in()
      return(Resultats)}
      }else ord<-X
      Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor',imp=imp)[[1]],silent=T)
      if(all(class(Matrice)!="matrix")) {
        sortie<-dlgMessage("La matrice de corrélation n'a pu être réalisée. Voulez-vous réessayer ?", type="yesno")$res
        if(sortie=="yes") return(NULL) else Matrice<-try(tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]],silent=T)
        if(class(Matrix)=="try-error")  {Matrice<-corr.test(data[,X], method="Spearman")$r
        msgBox("Les corrélations polychoriques ont échoué. Les corrélations utilisées sont des rho de Spearman")}
      }
    }    
    
    Matrice1 <- mat.sort(Matrice)
    if(length(X)>30) numbers<-F else numbers<-T
    try(cor.plot(Matrice1, show.legend=FALSE, main="Matrice de corrélation utilisée pour AFE", labels=NULL, n.legend=0, MAR=TRUE, numbers=numbers,cex=1), silent=T)
    round(Matrice,3)->Resultats$"Matrice de corrélations"
    round(unlist(cortest.bartlett(data[,X])),4)->bartlett
    names(bartlett)<-c("chi.carre","valeur.p","ddl")
    ### doit etre significatif (attention depend de la taille de l echantillon)
    bartlett->Resultats$"Mesure d'adéquation de la matrice"$"Test de Barlett"
    KMO1<-KMO(Matrice)
    if(is.na(KMO1)) {msgBox("Le KMO sur la matrice n'a pu être obtenu. Nous tentons de réaliser un lissage de la matrice de corrélation")
      Matrice<-cor.smooth(Matrice)
      KMO1<-KMO(Matrice)}
    if(is.na(KMO1)) {
      msgBox("Le KMO sur la matrice n'a pu être obtenu.")
      Resultats$"Mesure d'adéquation de la matrice"$"Indice de Kaiser-Meyer-Olkin global"<-"Le KMO n'a pas pu être calculé. Vérifiez votre matrice de corrélation."
    } else {
      round(KMO1$MSA,3)->Resultats$"Mesure d'adéquation de la matrice"$"Indice de Kaiser-Meyer-Olkin global" ### doit etre superieur a 0.5 sinon la matrice ne convient pas pour analyse factorielle. Dans lÃÂÃÂideal, avoir au moins 0.8. Si des X presentent un KMO<0.5, on peut envisager de les supprimer. 
      round(KMO1$MSAi,3)->Resultats$"Mesure d'adéquation de la matrice"$'Indice de Kaiser-Meyer-Olkin par item'
      round(det(Matrice),5)->Resultats$"Mesure d'adéquation de la matrice"$"Déterminant de la matrice de corrélation"
      Resultats$"Mesure d'adéquation de la matrice"$"Déterminant de la matrice de corrélations : information"<-"risque de multicolinearité si le déterminant de la matrice est inférieur à 0.00001"
    }
    
    
    if(dial){
      print(Resultats$"Mesure d'adéquation de la matrice")
      print("le KMO doit absolument être supérieur à 0.5")
      cat ("Appuyez sur [entrée] pour continuer")
      line <- readline()  
      dlgMessage(c("La matrice est-elle satisfaisante pour une AFE ?", "Continuer ?"), "okcancel")$res->res.kmo
      if(res.kmo=="cancel") {print("vous avez quitté l'AFE")
        return(analyse())}
    }
    
    
    if(dial || length(methode)>1 || is.null(methode) || methode%in%c("minres","wls","gls","pa", "ml","minchi")==FALSE){
      if(info) writeLines("Pour les variables ordinales et dichomiques, préférez la méthode du minimum des résidus - minres -
                          ou des moindres carrés pondérés - wls. Pour les variables continues, le maximum de vraisemblance si la normalité est respectée - ml")
      methode<-dlgList(c("minres","wls","gls","pa", "ml","minchi"), preselect= methode, multiple = FALSE, title="Quel algorithme désirez-vous?")$res
      if(length(methode)==0) {Resultats<-fa.in()
      return(Resultats)}
      
    }
    
    eigen(Matrice)$values->eigen
    parallel(length(data[,1]), length(X), 100)->P1
    nScree(x =eigen, aparallel=P1$eigen$mevpea)->result
    result->Resultats$"analyses parallèles"
    plotnScree(result)
    if(dial | is.null(nF) | !is.numeric(nF)) {
      msgBox(paste("le nombre de facteurs à retenir selon l'analyse en parallèle est de",result$Components$nparallel, "facteurs." ))
      cat ("Appuyez sur [entrée] pour continuer")
      line <- readline() 
      nF<-NA
      while(!is.numeric(nF)) {
        writeLines("Veuillez préciser le nombre de facteurs.") 
        nF <- dlgInput("Nombre de facteurs ?", 2)$res
        if(length(nF)==0) {Resultats<-fa.in()
        return(Resultats)
        }
        strsplit(nF, ":")->nF
        tail(nF[[1]],n=1)->nF
        as.numeric(nF)->nF
        if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>(length(X)/2) ){
          msgBox("Le nombre de facteur doit être un entier positif inférieur au nombre de variables")
          nF<-NA
        }
      }
    }
    
    
    
    if(dial & nF>1 || (length(rotation)>1 | rotation %in% c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor",
                                                            "promax",  "oblimin",  "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")==FALSE)){
      if(choix=="acp" | choix=="Analyse en composante principale") rotation<- c("none", "varimax", "quartimax", "promax",  "oblimin",  "simplimax","cluster") else{
        rotation<-c("none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT","bifactor", "promax",  "oblimin",  
                    "simplimax","bentlerQ", "geominQ","biquartimin", "cluster")
      }
      writeLines("Veuillez choisir le type de rotation. Oblimin est adapté en sciences humaines")
      rotation<-dlgList(rotation, preselect= "oblimin", multiple = FALSE, title="Quelle rotation")$res
      if(length(rotation)==0) {Resultats<-fa.in()
      return(Resultats)}
    }
    if(dial | !is.logical(scor.fac)){
      writeLines("Voulez-vous que les scores factoriels soient integrés à vos données ?")
      dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Scores factoriels?")$res->scor.fac
      if(length(scor.fac)==0) {Resultats<-fa.in()
      return(Resultats)}
    }
    
    if(!is.numeric(sat) || sat>1 || sat<0 || is.null(sat)){
      sat<-NULL 
    }
    while(is.null(sat)){
      if(info)  writeLines("Le critère de saturation permet de n'afficher dans le tableau de résultats 
                           que les saturation supérieure au seuil fixé")
      sat <- dlgInput("Quel est le critère de saturation que vous voulez utiliser ?", 0.3)$res
      
      if(length(sat)==0) {Resultats<-fa.in()
      return(Resultats)  }
      strsplit(sat, ":")->sat
      tail(sat[[1]],n=1)->sat
      as.numeric(sat)->sat
      if(is.na(sat)) {sat<-NULL
      msgBox("Le critère de saturation doit être compris entre 0 et 1.") }
    }
    
    
    
    if(choix=="Analyse factorielle exploratoire") {  
      if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
        msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
        n.boot<-NULL
      }
      while(is.null(n.boot)){
        writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
        n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
        if(length(n.boot)==0) {Resultats<-fa.in()
        return(Resultats)}
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->n.boot
        if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
          msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
          n.boot<-NULL
        }
      }
      if(dial & nF>1 & methode!="pa" & rotation%in%c("oblimin","simplimax", "promax") || hier==T && nFact2>=nF/2){
        if(info) writeLines(" Désirez-vous tester une structure hiérarchique ? L'omega teste une structure hiérarchique et une AFE hiérarchique seront réalisées.")
        dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Faut-il réaliser une analyse hiérarchique ?")$res->hier
        if(length(hier)==0) {Resultats<-fa.in()
        return(Resultats)  
        }
        if(!is.null(hier) && hier==TRUE){
          nfact2<-NA
          while(!is.numeric(nfact2)) {
            nfact2<-NA
            writeLines("Veuillez préciser le nombre de facteurs de la structure hiérarchique.") 
            nfact2 <- dlgInput("Nombre de facteurs du niveau supérieur ?", 1)$res
            if(length(nfact2)==0) {Resultats<-fa.in()
            return(Resultats)
            }
            strsplit(nfact2, ":")->nfact2
            tail(nfact2[[1]],n=1)->nfact2
            as.numeric(nfact2)->nfact2
            if(any(nfact2%%1==0 %in% c(FALSE, NA))|| nfact2<0 || nfact2>=nF/2 ){
              msgBox("Le nombre de facteur doit être un entier positif inférieur au nombre de facteurs")
              nfact2<-NA
            }
          }
          
        }
      }
    }
    
    
    
    if(dial | !is.logical(sauvegarde)){
      if(info) writeLines("Désirez-vous sauvegarder les résultats dans un fichier externe ?")
      dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="Voulez-vous sauvegarder?")$res->sauvegarde
      if(length(sauvegarde)==0) {Resultats<-fa.in()
      return(Resultats)    
      }
    }
    
    Resultats$choix<-choix
    Resultats$data<-data
    Resultats$nom<-nom
    Resultats$X<-X
    Resultats$Matrice<-Matrice
    Resultats$n.boot<-n.boot
    Resultats$rotation<-rotation
    Resultats$methode<-methode
    Resultats$sat<-sat
    Resultats$nF<-nF
    Resultats$type<-type
    Resultats$sauvegarde<-sauvegarde
    if(is.null(hier)) hier<-FALSE else Resultats$hier<-hier
    Resultats$cor<-cor
    Resultats$scor.fac<-scor.fac
    Resultats$ord<-ord
    Resultats$nfact2<-nfact2
    return(Resultats) 
  }
  
  fa.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, n.boot, nom, hier=FALSE, cor="cor", nfact2){
    
    if( cor=="cor") { Resultats$"Normalité multivariée"<-.normalite(data, X)} else cor<-"mixed"
    if(n.boot==1) {
      FA.results<-fa(Matrice,nfactors= nF, n.obs=length(data[,1]),fm=methode, rotate=rotation, n.iter=1) # realise l AFE
    } else {
      FA.results<-try(fa(data[,X], nfactors= nF, fm=method, rotate=rotation, n.iter=n.boot, cor=cor), silent=T)
      if(class(FA.results)=="try-error") { 
        msgBox("Le modèle n'a pas pu converger. Les paramètres ont été adaptés pour permettre au modèle de converger")
        FA.results<-try(fa(data[,X], nfactors= nF, fm=methode, rotate=rotation, n.iter=1, cor="cor", SMC=F), silent=T)
        if(class(FA.results)=="try-error"){
          msgBox("Nous n'avons pas réussi à faire converger le modèle. Veuillez vérifier votre matrice de corrélations et réessayer avec d'autres paramètres")
          return(analyse())}
      }
    }
    
    
    Resultats<-list()
    Resultats$analyse<-paste("analyse factorielle en utilisant la fonction fa du package psych avec la méthode", FA.results$fm)
    if(rotation=="none") Resultats$rotation<-"il n'y a pas de rotation" else Resultats$rotation<-paste("la rotation est un rotation", rotation)
    FA.results<-fa.sort(FA.results,polar=FALSE)
    loadfa<-round(as(FA.results$loadings, "matrix"),3)
    loadfa[which(abs(loadfa)<sat)]<-" "
    data.frame(communauté=round(FA.results$communality,3),
               spécifité=round(FA.results$uniquenesses,3),
               complexité=round(FA.results$complexity,2))->communaute
    Resultats$"saturations standardisées basées sur la matrice de corrélations"<-data.frame(loadfa, communaute)
    
    var.ex <- round(FA.results$Vaccounted,3)
    if(nF>1){dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée",
                                      "proportion de variance expliquée cumulée", "Proportion de l'explication", 
                                      "Proportion cumulée de l'explication")} else {
                                        dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée")
                                      }
    Resultats$"Variance expliquée"<-var.ex
    
    paste("ML",1:nF)->noms1
    if(nF>1 & rotation=="oblimin"){
      round(FA.results$Phi, 3)->cor.f
      Resultats$"corrélations entre facteurs"<-cor.f}
    paste("la complexité moyenne est de", round(mean(FA.results$complexity),3), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexité moyenne"
    if(length(X)>5){
      round(matrix(c(FA.results$null.chisq, FA.results$null.dof,FA.results$null.model,
                     FA.results$dof, FA.results$objective, FA.results$RMSEA,
                     FA.results$TLI,FA.results$BIC, FA.results$SABIC,FA.results$rms, FA.results$crms, FA.results$fit.off, 
                     FA.results$chi, FA.results$EPVAL, FA.results$STATISTIC, FA.results$PVAL, FA.results$n.obs), ncol=1),4)->stats
      c("chi carre du modèle null", "Degrés de liberté du modèle null", "fonction objective du modèle null",
        "degrés de liberté du modèle", "fonction objective du modèle", "RMSEA", "limite inférieure du RMSEA", "limite supérieure du RMSEA",
        "Seuil de confiance (1- alpha)", "facteur de fiabilité de Tucker Lewis - TLI", "BIC", "EBIC", 
        "RMSR", "RMSR corrigé", "Adéquation basee sur les valeurs en dehors de la diagonale", "chi carré empirique", "valeur de la proabilite du chi carre empirique",
        "chi carré du maximum de vraisemblance", "valeur de la probabilité du chi carré du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
      
      "valeurs"->dimnames(stats)[[2]]
      stats->Resultats$"Indices d'adéquation et d'ajustement"
      if(all(FA.results$R2<1)){
        round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
        dimnames(stats)[[1]]<-c("Corrélations des scores avec les facteurs", "R carré multiple des scores avec les facteurs",
                                "Corrélation minimale possible des scores avec les facteurs")
        dimnames(stats)[[2]]<-noms1
        stats->Resultats$"Corrélations des scores avec les facteurs" 
      }
      
      if(n.boot>1) {
        IC<-c()
        for(i in 1:nF){
          cbind(round(FA.results$cis$ci[,i],3), 
                round(as(FA.results$loadings, "matrix"),3)[,i],
                round(FA.results$cis$ci[,i+nF],3))->IC2
          dimnames(IC2)[[2]]<-c("lim.inf", dimnames(FA.results$loadings)[[2]][i],"lim.sup")
          cbind(IC, IC2)->IC
        }
        IC->Resultats$"Intervalle de confiance des saturations sur la base du bootstrap - peut être biaisé en présence de Heyhood case"
      }
    }
    print(fa.diagram(FA.results))#representation graphique des saturations}
    if(scor.fac){Scores.fac<-c()
    sapply(data[,X], scale)->centrees
    FA.results$weights->matrice2
    t(matrice2)->matrice2
    for(i in 1 : nF){
      apply(centrees%*%matrice2[i,],1,sum)->centrees2
      cbind(Scores.fac,centrees2)->Scores.fac
    }
    
    data<-data.frame(data,Scores.fac)
    names(data)[(length(data)+1-nF):length(data)]<-paste0("facteur.", 1:nF)
    assign(nom, data,envir=.GlobalEnv)
    
    }
    
    if(hier) {
      if(cor!="cor") poly<-TRUE else poly<-FALSE
      Resultats$"Analyse factorielle hiérarchique"$Omega<-psych::omega(data[,X], nfactors=nF, n.iter=n.boot,fm=methode, poly=poly, flip=T, digits=3, sl=T, plot=T, n.obs=length(data[,1]), rotate=rotation)
      multi<-fa.multi(Matrice, nfactors=nF, nfact2=nfact2, n.iter=1,fm=methode, n.obs=length(data[,1]), rotate=rotation)
      multi$f2->FA.results
      
      FA.results<-fa.sort(FA.results,polar=FALSE)
      loadfa<-round(as(FA.results$loadings, "matrix"),3)
      loadfa[which(abs(loadfa)<sat)]<-" "
      data.frame(communauté=round(FA.results$communality,3),
                 spécifité=round(FA.results$uniquenesses,3),
                 complexité=round(FA.results$complexity,2))->communaute
      Resultats$"Analyse factorielle hiérarchique"$"saturations standardisées basées sur la matrice de corrélations"<-data.frame(loadfa, communaute)
      
      var.ex <- round(FA.results$Vaccounted,3)
      if(nfact2>1){dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée",
                                            "proportion de variance expliquée cumulée", "Proportion de l'explication", 
                                            "Proportion cumulée de l'explication")} else {
                                              dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée")
                                            }
      Resultats$"Analyse factorielle hiérarchique"$"Variance expliquée"<-var.ex
      
      paste("ML",1:nfact2)->noms1
      
      paste("la complexité moyenne est de", round(mean(FA.results$complexity),3), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexité moyenne"
      
      round(matrix(c( FA.results$null.dof,FA.results$null.model,
                      FA.results$dof, FA.results$objective, 
                      FA.results$rms, FA.results$fit.off), ncol=1),4)->stats
      c( "Degrés de liberté du modèle null", "fonction objective du modèle null",
         "degrés de liberté du modèle", "fonction objective du modèle",    "RMSR", 
         "Adéquation basee sur les valeurs en dehors de la diagonale")->dimnames(stats)[[1]]
      
      "valeurs"->dimnames(stats)[[2]]
      stats->Resultats$"Analyse factorielle hiérarchique"$"Indices d'adéquation et d'ajustement"
      if(all(FA.results$R2<1)){
        round(rbind((FA.results$R2)^0.5,FA.results$R2,2*FA.results$R2-1),2)->stats
        dimnames(stats)[[1]]<-c("Corrélations des scores avec les facteurs", "R carré multiple des scores avec les facteurs",
                                "Corrélation minimale possible des scores avec les facteurs")
        dimnames(stats)[[2]]<-noms1
        stats->Resultats$"Analyse factorielle hiérarchique"$"Corrélations des scores avec les facteurs"
        fa.multi.diagram(multi)
      }
    }
    return(Resultats)
    
  } 
  acp.out<-function(Matrice, data, X, nF, methode, rotation, sat, scor.fac, nom){
    principal(Matrice, nfactors= nF, n.obs=length(data[,1]), rotate=rotation)->PCA
    list()->Resultats
    Resultats$analyse<-paste("analyse en composante principale en utilisant la fonction [principal] du package psych, l'algorithme est", PCA$fm)
    if(!is.null(rotation)) Resultats$rotation<-paste("la rotation est un rotation", rotation) 
    
    PCA<-fa.sort(PCA,polar=FALSE)
    loadfa<-round(as(PCA$loadings, "matrix"),3)
    loadfa[which(abs(loadfa)<sat)]<-" " 
    data.frame(communauté=round(PCA$communality,3),
               spécifité=round(PCA$uniquenesses,3),
               complexité=round(PCA$complexity,2))->communaute
    Resultats$"saturations standardisées basées sur la matrice de corrélations"<-data.frame(loadfa, communaute)
    var.ex<-round(PCA$Vaccounted,3)
    
    if(nF>1){dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée",
                                      "proportion de variance expliquée cumulée", "Proportion de l'explication", 
                                      "Proportion cumulée de l'explication")} else {
                                        dimnames(var.ex)[[1]]<-c("Sommes des carrés des saturations", "proportion de variance expliquée")
                                      }
    Resultats$"Variance expliquée"<-var.ex
    
    paste("TC",1:nF)->noms1
    if(nF>1 & rotation=="oblimin"){  round(PCA$r.scores,3)->cor.f
      Resultats$"corrélations entre facteurs"<-cor.f}
    paste("la complexité moyenne est de", mean(PCA$complexity), "Cela teste si", nF, "facteurs suffise(nt)" )-> Resultats$"Complexité moyenne"
    round(matrix(c(PCA$null.dof,PCA$null.model,
                   PCA$dof, PCA$objective, 
                   PCA$rms, PCA$fit.off, 
                   PCA$chi, PCA$EPVAL, PCA$STATISTIC, PCA$PVAL, PCA$n.obs), ncol=1),4)->stats
    
    
    c("Degrés de liberté du modèle null", "fonction objective du modèle null","degrés de liberté du modèle", "fonction objective du modèle",
      "RMSR",  "Adéquation basée sur les valeurs en dehors de la diagonale", "chi carré empirique", "valeur de la probabilité du chi carré empirique",
      "chi carré du maximum de vraisemblance", "valeur de la probabilité du chi carré du maximum de vraisemblance", "nombre total d'observations")->dimnames(stats)[[1]]
    
    "valeurs"->dimnames(stats)[[2]]
    stats->Resultats$"Indices d'adéquation et d'ajustement"
    if(scor.fac){
      Scores.fac<-c()
      sapply(data[,X], scale)->centrees
      PCA$weights->matrice2
      t(matrice2)->matrice2
      for(i in 1 : nF){
        apply(centrees%*%matrice2[i,],1,sum)->centrees2
        cbind(Scores.fac,centrees2)->Scores.fac
      }
      data<-data.frame(data,Scores.fac)
      names(data)[(length(data)+1-nF):length(data)]<-paste0("facteur.", 1:nF)
      assign(nom, data,envir=.GlobalEnv)
      
    }
    return(Resultats)
  }   
  
  options (warn=-1)
  
  packages<-c("svDialogs", "GPArotation","psych","lavaan", "nFactors")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  list()->Resultats
  cor<-ifelse(is.null(ord), "cor", "mixed")    
  fa.options<-fa.in(data=data, choix=choix, X=X, imp=imp, ord=ord, nF=nF, rotation=rotation, methode=methode, sat=sat, scor.fac=scor.fac, n.boot=n.boot, hier=hier,nfact2=nfact2, outlier=outlier,
                    sauvegarde=sauvegarde, info=info)
  if(is.null(fa.options)) return(analyse())
  if(is.null(fa.options$choix)) return(fa.options)
  fa.options->>fa.options
  Matrice<-fa.options$Matrice
  data<-fa.options$data
  X<-fa.options$X
  nF<-fa.options$nF
  methode<-fa.options$methode
  rotation<-fa.options$rotation
  sat<-fa.options$sat
  scor.fac<-fa.options$scor.fac
  n.boot<-fa.options$n.boot
  nom<-fa.options$nom
  cor<-fa.options$cor
  hier<-fa.options$hier
  nfact2<-fa.options$nfact2
  Resultats$"Matrice de corrélations"<-fa.options$"Matrice de corrélations"
  Resultats$"Mesure d'adéquation de la matrice"<-fa.options$"Mesure d'adéquation de la matrice"
  Resultats$"analyses parallèles"<-fa.options$"analyses parallèles"
  
  
  
  if(fa.options$choix==  "Analyse factorielle exploratoire" |choix=="afe"){
    Resultats$"Analyse factorielle"<-fa.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, 
                                            scor.fac=scor.fac, n.boot=n.boot, nom=nom, hier=hier, cor=cor, nfact2=nfact2)  }
  
  if(fa.options$choix==  "Analyse en composante principale" |choix=="acp"){
    Resultats$"Analyse en composante principale"<-acp.out(Matrice=Matrice, data=data, X=X, nF=nF, methode=methode, rotation=rotation, sat=sat, scor.fac=scor.fac, nom=nom)
  }
  
  
  paste(X, collapse="','", sep="")->X
  if(!is.null(fa.options$ord)) paste(fa.options$ord, collapse="','", sep="")->ord
  Resultats$Call<-paste0("factor.an(data=", nom, ",X=c('",X, "'),nF=", nF,", rotation='", rotation, "',methode='",methode, "',sat=", sat,
                         ",outlier='", outlier, "',imp=", ifelse(is.null(imp), "NULL", paste0("'",imp,"'")),",ord=", ifelse(!is.null(ord), paste0("c('", ord,"')"), "NULL"),
                         ",sauvegarde=", sauvegarde, ",scor.fac=", scor.fac, ",n.boot=", n.boot,",hier=", hier, ",nfact2=", nfact2, ",choix='", fa.options$choix, "',info=T)"
  )
  
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste(fa.options$choix, Sys.time() ))
  
  
  if(fa.options$sauvegarde) save(Resultats=Resultats, choix=fa.options$choix, env=.e)
  ref1(packages)->Resultats$"Références des packages utilisés pour cette analyse"
  return(Resultats)
  }


#### alpha de Cronbach, coefficient d'accord de Kendall, corrÃÂ©lation intra-classe ####

fiabilite<-function(X=NULL,Y=NULL, data=NULL, choix=NULL, ord=NULL,outlier="Données complètes", keys=NULL, n.boot=NULL, sauvegarde=F, info=T, imp=NULL){options (warn=-1)
  packages<-c("svDialogs", "psych", "lavaan")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  .e<- environment()
  Resultats<-list()
  if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
  if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Alpha de Cronbach","alpha","ICC","CCK","Correlation intra-classe","Coefficient de concordance de Kendall")==FALSE){
    dial<-T  
    if(info) writeLines("Veuillez choisir l'analyse que vous désirez réaliser.")
    dlgList(c("Alpha de Cronbach", "Correlation intra-classe","Coefficient de concordance de Kendall"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous réaliser?")$res->choix
    if(length(choix)==0) return(analyse())
  }
  
  
  if(dial || class(data)!="data.frame"){
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(analyse())
    nom<-data[[1]]
    data<-data[[2]]  
  }else{
    deparse(substitute(data))->nom  
  }
  
  if(choix=="CCK" | choix=="Coefficient de concordance de Kendall"){
    msg3<-"Veuillez choisir le premier juge"
    type<-"factor"
    title<-"Juge 1"
    multiple<-T
  } else{
    multiple<-T
    msg3<-"Veuillez choisir les variables que vous désirez analyser."
    type<-"numeric"
    title<-"variables"
  }
  
  X<-.var.type(X=X, info=info, data=data, type=type, check.prod=F, message=msg3,  multiple=multiple, title=title, out=NULL)
  if(is.null(X)) {
    Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
    return(Resultats)}
  data<-X$data
  X<-X$X
  
  if(choix %in% c("Alpha de Cronbach","Correlation intra-classe","ICC","alpha") ){
    if(dial || length(outlier)>1 || outlier %in% c("Données complètes", "Données sans valeur influente") ==FALSE){
      if(info) writeLines("Désirez-vous l'analyse sur les données complètes ou sur les données pour lesquelles les valeurs influentes ont été enlevées ?")
      if(info) writeLines("les valeurs influentes sont identifiées sur la base de la distance de Mahalanobis avec un seuil du chi à 0.001")
      outlier<- dlgList(c("Données complètes", "Données sans valeur influente"), preselect="Données complètes",multiple = FALSE, title="Quels résultats voulez-vous obtenir ?")$res
      if(length(outlier)==0) { Resultats<-fiabilite()
      return(Resultats)}
    }
    
    if(outlier=="Données sans valeur influente"){
      inf<-VI.multiples(data[,X])
      Resultats$"Valeurs considérées comme influentes"<-inf$"Valeurs considérées comme influentes"
      data<-inf$data
    }
    
    
    if(choix %in% c("Alpha de Cronbach","alpha"))  {
      if(dial){
        if(info) writeLines("Veuillez préciser le type de variables. Des corrélations tétra/polychoriques seront réalisées sur les variables ordinales et Bravais-Pearson sur les variables continues")
        type<-dlgList(c("dichotomiques/ordinales", "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res
        if(length(type)==0) {Resultats<-fiabilite()
        return(Resultats)
        }} else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"}
      
      if(dial){
        if(info) writeLines("Y a-t-il des items inversés ?") 
        rev<-dlgList(c(TRUE,FALSE), multiple = TRUE, title="items inversés?")$res
        if(length(rev)==0) {Resultats<-fiabilite()
        return(Resultats)
        }  } else rev<-FALSE
        
        if(rev=="TRUE" || !is.null(keys) && any(keys %in% X==FALSE)){
          if(info) writeLines("Veuillez préciser les items inversés")
          keys<-dlgList(X, multiple = TRUE, title="items inversés?")$res
          if(length(keys)==0) {Resultats<-fiabilite()
          return(Resultats)
          }else keys<-NULL
        }
        
        
        
        if(type=="continues"){
          if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
            msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
            n.boot<-NULL
          }
          while(is.null(n.boot)){
            writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
            n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
            if(length(n.boot)==0) {Resultats<-fiabilite()
            return(Resultats)}
            strsplit(n.boot, ":")->n.boot
            tail(n.boot[[1]],n=1)->n.boot
            as.numeric(n.boot)->n.boot
            if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
              msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
              n.boot<-NULL
            }
          }
          psych::alpha(data[,X], keys=keys, n.iter=n.boot)->cron
        }else{
          n.boot<-0
          if(type=="mixte") {
            if(info) writeLines("Veuillez préciser les variables ordinales ?") 
            ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
            if(length(ord)==0){
              Resultats<-fiabilite()
            return(Resultats)
            }
          }else ord<-X
          Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp=imp)[[1]]
          if(all(class(Matrice)!="matrix")) {
            sortie<-dlgMessage("Vous essayez de faire un alpha sur autre chose qu'un matrice. Voulez-vous sortir de cette analyse?", type="yesno")$res
            if(sortie=="yes") return(analyse()) else Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]]
            }
          
          psych::alpha(Matrice, keys=keys,n.obs=length(data[,1]))->cron
        }
        
        round(cron$total,3)->Resultats$"Alpha de Cronbach sur la totalité de l'éhelle"
        if(n.boot>1) cron$boot.ci->Resultats$"Intervalle de confiance basé sur le bootstrap"
        cron$total[,1]->a1
        cron$total[,6]->ase
        data.frame(Lim.inf.IC.95=a1-1.96*ase, alpha=a1, Lim.sup.IC.95=a1+1.96*ase)->Resultats$"Intervalle de confiance basé sur l'erreur standard de l'alpha"
        round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Resultats$"fiabilité par item supprimé"
        
    }
    
    if(choix=="Correlation intra-classe"| choix=="ICC"){ICC(data[,X], missing=FALSE)->ICC.out
      ICC.out[[1]]->Resultats$"correlation intra-classe"
      Resultats$"informations"<-paste("le nombre de juge =", length(X), "et le nombre d'observations =", ICC.out$n.obs) } 
  }
  
  
  if(choix=="Coefficient de concordance de Kendall"){  
    msg4<-"Veuilez choisir le second juge"
    Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=F, title="Juge 2", out=X)
    if(is.null(Y)) {
      Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
      return(Resultats)}
    data<-Y$data
    Y<-Y$X
    cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
    dimnames(CK.out$confid)<-list(c("Coefficient kappa non pondéré","Coefficient kappa pondéré"),c("lim.inf","estimation","lim.sup"))
    round(CK.out$confid,3)->Resultats$"Coefficient de concordance de Kendall"
    CK.out$agree->Resultats$"Accord"
    Resultats$information<-paste("le nombre d'observations =", CK.out$n.obs)
  }
  
  if(dial) dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez-vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) {
    Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
    return(Resultats)
  }
  
  paste(X, collapse="','", sep="")->X
  if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
  if(!is.null(keys)) paste(ord, collapse="','", sep="")->keys
  
  Resultats$Call<-paste0("fiabilite(X=c('", X,"'),Y=", ifelse(is.null(Y), "NULL", paste0("'",Y,"'")), ",data=", nom, ",choix='", choix,"',ord=", 
                         ifelse(!is.null(ord),paste0("c('", ord, "')"), "NULL" ), ",outlier='", outlier, "', keys=", ifelse(!is.null(keys), paste0("c('",keys,"')"), "NULL"),
                         ",n.boot=", ifelse(!is.null(n.boot), n.boot, "NULL"), ", sauvegarde=", sauvegarde, ", info=T)")
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))  
  
  
  if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
  ref1(packages)->Resultats$Références
  return(Resultats)
}


#### ModÃÂ¨les linÃÂ©aires avec ou sans interaction - rÃÂ©gressions + mediation et donc modÃÂ©ration ####

regressions<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=0.15 , scale=T, dial=T, info=T,
                      sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
  
  
  
  regressions.in<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=F, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=NULL , scale=T, dial=T, info=T,
                           sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
    options (warn=-1) 
    Resultats<-list()
    if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
    
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(NULL) 
    nom<-data[[1]]
    data<-data[[2]]  
    
    
    if(dial && is.null(modele)){
      if(info) writeLines("Veuillez choisir le(s) type(s) de relations entre les variables. Les effets additifs prennent la forme de
                          y=X1+X2 tandis que les effets d'interaction prennent la forme de Y=X1+X2+X1:X2")
      dlgList(c("Effets additifs", "Effets d'interaction", "Spécifier le modèle"), preselect="Régressions", multiple = TRUE, title="Quel type de régression ?")$res->link
      if(length(link)==0) return(NULL) } else link<-"none"
    
    if(length(Y)>1){
      msgBox("Il ne peut y avoir qu'une seule variable dépendante.")
      Y<-NULL }
    if(any(link %in% c("Effets additifs", "Effets d'interaction"))){
      msg3<-"Veuillez choisir la variable dépendante."
      Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=FALSE, title="Variable dépendante", out=NULL)
      if(is.null(Y)) {
        regressions.in()->Resultats
        return(Resultats)}
      data<-Y$data
      Y<-Y$X
      
      if(any(link=="Effets additifs") || !is.null(X_a)| any(X_a %in% names(data)==F)) {
        msg3<-"Veuillez choisir la variable dépendante."
        X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modèle additif", out=Y)
        if(is.null(X_a)) {
          regressions.in()->Resultats
          return(Resultats)}
        data<-X_a$data
        X_a<-X_a$X
        
      }else X_a<-NULL 
      
      if(any(link=="Effets d'interaction") || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
        msg3<-"Veuillez choisir les prédicteurs à entrer dans le modèle d'interaction. Il est nécessaire d'avoir au moins deux variables"
        X_i<-c()
        while(length(X_i)<2){
          X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modèle interactif", out=c(X_a,Y))
          if(is.null(X_i)) {
            regressions.in()->Resultats
            return(Resultats)}
          data<-X_i$data
          X_i<-X_i$X
        }
      }else X_i<-NULL
      
      
      
      paste0(Y," ~ ")->modele
      if(!is.null(X_a ))  {
        X_a.mod<-X_a[1]
        if(length(X_a)>1) for(i in 2 : length(X_a)) paste0(X_a.mod, "+", X_a[i])-> X_a.mod
      } else X_a.mod<-NULL
      
      if(!is.null(X_i)){
        X_i.mod<-X_i[1]
        if(length(X_i)>1) for(i in 2 : length(X_i)) paste0(X_i.mod, "*", X_i[i])-> X_i.mod
      } else X_i.mod<-NULL
      
      if(!is.null(X_a.mod) & !is.null(X_i.mod)) {
        paste0(modele, X_a.mod, "+", X_i.mod)->modele
      } else paste0(modele, X_a.mod, X_i.mod)->modele
      
    }
    
    if(any(link=="Spécifier le modèle")) {
      if(is.null(modele)) modele<-" "
      modele<-fix(modele)}
    modele<-as.formula(modele)
    variables<-terms(modele)
    variables<-as.character( attributes(variables)$variables)[-1]
    
    
    model.test<-try(model.matrix(modele, data), silent=T)
    if(class(model.test)=="try-error") {
      msgBox("Le modèle spécifié est incorrect. Vérifiez vos variables et votre modèle")
      return(regressions.in())
    }
    
    
    data[complete.cases(data[,variables]),]->data
    msg.options1<-"Le test paramétrique est la régression classique et les tests robustes sont une estimation sur un M estimeur ainsi qu'un bootstrap."
    
    options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=F, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                         choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
    if(is.null(options)) return(regressions.in())
    
    reg.options<- .regressions.options(data=data, modele=modele, CV=CV, inf=inf, select.m=select.m, method=method, criteria=criteria, step=step, group=group, scale=scale, dial=dial,info=info)
    if(is.null(reg.options)) return(regressions.in())
    
    
    Resultats$data<-data
    Resultats$nom<-nom
    Resultats$modele<-modele
    Resultats$options<-options
    Resultats$reg.options<-reg.options
    return(Resultats)   
    
  }
  
  regressions.out<-function(data=NULL, modele=NULL,  VC=F, select.m="none", method=NULL, step=NULL, group=NULL, criteria=NULL , scale=T,
                            sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
    
    Resultats<-list()
    variables<-terms(as.formula(modele))
    variables<-as.character( attributes(variables)$variables)[-1]
    pred<-attributes(terms(as.formula(modele)))$term.labels
    Resultats$"Statistiques descriptives"<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)
    
    if(scale==T || scale=="Centré") {Resultats$info<-"En accord avec les recommandations de Schielzeth 2010, les données ont été préalablement centrées"
    
    which(sapply(data[,pred[which(pred %in% variables)]],class)!="factor")->centre
    if(length(centre)==1) data[,names(centre)]-mean(data[,names(centre)],na.rm=T)->data[,names(centre)] else{
      sapply(X=data[,names(centre)], fun<-function(X){X-mean(X, na.rm=T)})->data[,names(centre)]
    }
    }
    
    
    mod<-list()
    modele1<-as.formula(paste0(variables[1], "~", pred[1]))
    lm( modele1,na.action=na.exclude, data=data)->lm.r1
    lm.r1->mod[[1]]
    if(length(pred)>1) {
      for(i in 2:length(pred)){update(lm.r1, as.formula(paste0(".~.+",pred[i])))->lm.r1
        lm.r1->mod[[i]]}
    }
    resid(lm.r1)->data$residu
    Resultats$"Tests de normalité"<-.normalite(data=data, X="residu", Y=NULL)
    if(length(variables)>1)  {
      cont<-variables[which(sapply(data[,variables],class)!="factor")]
      Resultats$"Normalité multivariée"<-.normalite(data=data, X=cont, Y=NULL)
      vif(lm.r1)->FIV # calcul du facteur d inflation de la variance 
      Resultats$"Tests de multicolinéarité"$Tests<-data.frame(Tolérance=round(1/FIV,4) , FIV= round(FIV,4))
      Resultats$"Tests de multicolinéarité"$Information<-"FIV : facteur d'inflation de la variance"
      dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
      Resultats$"Test de Durbin-Watson - autocorrélations"<-round(data.frame(Autocorrélation=DWT.results[1],"statistique de D-W"=DWT.results[2],"valeur p"=DWT.results[3]),4)->DWT.results
      ncvTest(lm.r1)->var.err
      Resultats$"Vérification de la non-constance de la variance d'erreur (test de Breusch-Pagan)"<-data.frame(chi=var.err$ChiSquare,
                                                                                                               ddl=var.err$Df,valeur.p=var.err$p)
      
      
      try(ceresPlots(lm.r1, main="Graphique de Ceres testant la linéarité"), silent=T)
    }
    if(select.m!="none"){
      if(method %in% c("F", "valeur du F", "p", "valeur de la probabilité")){
        select.m<-switch(select.m,"Forward - pas-à-pas ascendant"="Forward", "Backward- pas-à-pas descendant"="Backward", "Bidirectionnel"="Stepwise",
                         "forward"="Forward", "bidirectional"="Stepwise","backward"="Backward" )
        select.m.out<-mle.stepwise(modele, data, type=select.m, model=T,f.in=criteria, x=T, y=T) 
        select.m.out<-select.m.out$step
        if(any(select.m.out!=0)){
          if(!is.null(dim( select.m.out))) {data.frame(select.m.out)->select.m.out}else t(as.matrix(select.m.out1$step))->select.m.out
          
          names(select.m.out)[length(select.m.out)]<-"F d'entrée"
          dimnames(select.m.out)[[1]]<-paste("étape", 1:length(select.m.out[,1]))
          Resultats$"Méthode de sélection"<-select.m.out 
        } else  Resultats$"Méthode de sélection"<-"Aucune variable n'a été retenue par la méthode de sélection. L'analyse est réalisée sur l'ensemble des prédicteurs."
      }
      
      if(method %in% c("AIC - Akaike Information criterion","AIC")){ 
        select.m<-switch(select.m,"Forward - pas-à-pas ascendant"="forward", "Backward- pas-à-pas descendant"="backward", "Bidirectionnel"="both",
                         "forward"="forward", "bidirectional"="both","backward"="backward" )
        lm.r1<-lm(modele, data=data)
        steps<-stepAIC(lm.r1, direction=select.m) 
        Resultats$"Méthode de sélection - critères d'information d'Akaike"<-steps$anova
        modele<-as.formula(attributes(steps$anova)$heading[5])
        pred<-attributes(terms(modele))$term.labels
        
      }
      
      if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
        BF.out<-try(regressionBF(modele, data=data,progress=F, rscaleCont=rscale), silent=T)
        if(class(BF.out)!="try-error") {
          plot(BF.out) 
          Resultats$"Méthodes de sélection : facteurs bayesiens"<-head(BF.out)
        } else Resultats$"Méthodes de sélection : facteurs bayesiens"<-"Les méthodes de sélection pour les facteurs bayesiens ne s'appliquent pas pour des modèles complexes."
      } 
    }
    
    if(!is.null(step)){
      
      as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
      list()->modele.H1
      list()->formule.H1
      for(i in 1:length(step)){
        
        for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
        formule.H1[[i]]<-modele.H
        lm(modele.H, data=data, na.action=na.exclude )->lm.H
        lm.H->modele.H1[[i]]}
      
      if(any(param=="param")|any(param=="Test paramétrique")) {
        hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
        if(length(modele.H1)>2){
          for(i in 3: length(modele.H1)){
            hier<-paste0(hier, ",modele.H1[[", i, "]]")
          }
        }
        hier<-paste0(hier,")")
        hier<-eval(parse(text=hier))
        attributes(hier)$heading[1]<-"Table de l'analyse de variance des modèles hiérarchiques"
        names(hier)<-c("ddl.résid", "SC.résid","ddl.effet", "SC", "F", "p")
        Resultats$"Analyse hiérarchique des modèles "<-hier
        
        
        
        c(summary(modele.H1[[1]])$sigma, summary(modele.H1[[1]])$r.squared, summary(modele.H1[[1]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
        pf(summary(modele.H1[[1]])$fstatistic[1], summary(modele.H1[[1]])$fstatistic[2],summary(modele.H1[[1]])$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
        c(significativite_modele , p.value)->modele_avec_outliers 
        
        for(i in 2:(length(modele.H1))){
          c(summary(modele.H1[[i]])$sigma, summary(modele.H1[[i]])$r.squared, summary(modele.H1[[i]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
          pf(summary(modele.H1[[i]])$fstatistic[1], summary(modele.H1[[i]])$fstatistic[2],summary(modele.H1[[i]])$fstatistic[3], lower.tail=F)->valeur.p #permet de savoir si le F est significatif
          rbind(modele_avec_outliers, c(significativite_modele , p.value))->modele_avec_outliers  
        }
        round(modele_avec_outliers,3)->modele_avec_outliers 
        c("Erreur residuelle", "R.deux", "F", "Ddl(1)", "Ddl(2)","valeur.p")->dimnames(modele_avec_outliers)[[2]]
        paste("étape", 1:length(modele_avec_outliers[,1]))->dimnames(modele_avec_outliers)[[1]]
        Resultats$"Modèles hiérarchique - significativité du modèle complet à chaque étape"<-modele_avec_outliers
        
      }
      
      if(any(param=="Bayes")|any(param=="Facteurs bayesiens")) {
        BF<-lmBF(formula= as.formula(formule.H1[[1]]), data=data, rscaleFixed=rscale)
        BF.modele<-extractBF(BF, onlybf=T)
        BF.hier<-c(NA)
        for(i in 2:length(formule.H1)){
          numBF<-lmBF(formula= as.formula(formule.H1[[i]]), data=data, rscaleFixed=rscale)
          BF.modele<-c(BF.modele, extractBF(numBF, onlybf=T))
          denomBF<-lmBF(formula= as.formula(formule.H1[[i-1]]), data=data, rscaleFixed=rscale)
          OddBF<-numBF/denomBF
          BF.hier<-c(BF.hier, extractBF(OddBF, onlybf=T))}
        
        # BF.out[formule.H1[[i]]]/BF.out[formule.H1[[i-1]]]->BF.comp
        
        BF.hier<-data.frame("Rapport des FB entre les modèles"=BF.hier, "FB du modèle"= BF.modele)
        dimnames(BF.hier)[[1]]<- unlist(as.character(formule.H1))
        Resultats$"Approche bayesienne des modèles hiérarchique"<-BF.hier
      }
      
    }
    # "test paramétrique", "test non paramétrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens"   
    if(any(param=="param")|any(param=="Test paramétrique")) {
      c(summary(lm.r1)$sigma, summary(lm.r1)$r.squared, summary(lm.r1)$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
      pf(summary(lm.r1)$fstatistic[1], summary(lm.r1)$fstatistic[2],summary(lm.r1)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
      c(significativite_modele , p.value)->modele.F # on combine les precedents 
      round(modele.F,3)->modele.F # on arrondit les nombres a la 3e decimale
      c("Erreur résiduelle", "R.deux", "F", "Ddl (num)", "Ddl (dnom)","valeur.p")->names(modele.F)# attribue le nom aux colonnes
      modele.F->Resultats$"Estimation  du modèle global"
      
      
      data.frame(summary(lm.r1)$coefficients)->table # fournit le b, le t et la valeur de la probabilite. On le stocke dans table
      round(table[,1:4],3)->table # on arrondit les valeurs a 3 decimales 
      
      beta<-coef(lm.r1)*sapply(data.frame(model.matrix(lm.r1)),sd) /sd(data[variables[1]])
      c("",round(beta[-1],5))->table$beta # fournit les betas qu on inclut a la table 
      names(table)<-c("b","erreur.standard","t","valeur.p","beta")
      
      r_carre<- matrix(c(0,0,0),1)
      
      for(i in 1:length(mod)){
        rep(summary(mod[[i]])$r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre2
        summary(mod[[i]])$r.squared-r_carre[length(r_carre[,2]),2]->diff
        rep(diff, (length(coef(mod[[i]]))-length(r_carre[,1])))->diff
        rep(summary(mod[[i]])$adj.r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre_adj
        
        round(cbind(r_carre2, diff, r_carre_adj), 4)->r_carre2
        rbind(r_carre,r_carre2 )->r_carre
        
      }
      
      dimnames(r_carre)<-list(ligne=NULL, c("R.deux", "Delta R.deux", "R.deux.aj"))
      data.frame(table,r_carre)->table
      table[is.na(table)]<-""
      table->Resultats$"table des bêtas"
      
    }
    
    if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
      
      lmBF(modele1, data=data)->BF.out
      BF.table<-extractBF(BF.out)[1:2]
      if(length(pred)>1) { for(i in 2:length(pred)){
        modele1<-update(modele1, as.formula(paste0(".~.+",pred[i])))
        lmBF(modele1, data=data)->BF.out
        BF.table<-rbind(BF.table, extractBF(BF.out)[1:2])
      }
      } 
      Resultats$"Facteurs bayesiens"<-BF.table
      
    }
    
    if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
      
      rlm(formula=modele, data=data)->modele_robuste
      summary(modele_robuste)->res_modele_robuste
      (1-pt(abs(res_modele_robuste$coefficients[,3]), (length(data[,1])-1-length(pred)), lower.tail=TRUE))*2->proba
      round(cbind(res_modele_robuste$coefficients, proba),3)->M_estimator
      data.frame(M_estimator)->M_estimator
      noms<-c("b (M estimator)", "SE", "t.value", "p.valeur")
      
      
      if(n.boot>100){ 
        bootReg<-function(formula, data, i)
        {  d <- data[i,]
        fit <- lm(formula, data = d)
        return(coef(fit))}
        bootResults<-boot(statistic=bootReg, formula= modele , data=data, R=n.boot) # cree le bootstrap
        intervalle<-c()
        try(for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "bca", index = i)$bca[,4:5]->IC1
          rbind(intervalle, IC1)->intervalle}, silent=T)
        if(is.null(intervalle)){
          for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "perc", index = i)$percent[,4:5]->resultats
            rbind(intervalle, resultats)->intervalle}
          noms<-c(noms, "Percentile.lim.inf", "Percentile.lim.sup")
        } else{
          noms<-c(noms, "Bca.lim.inf", "Bca.lim.sup")
        }
        data.frame(M_estimator, round(intervalle,4))->M_estimator
      }
      names(M_estimator)<-noms
      Resultats$"Statistiques robustes"<-M_estimator
    }  
    
    
    if(CV) CVlm(data=data, form.lm=modele, m=2, plotit=FALSE)
    
    return(Resultats) 
    
  }
  options (warn=-1) 
  .e <- environment()
  c("BayesFactor","boot","car","DAAG","ggplot2","gsl","MASS", "MBESS","nortest","psych","QuantPsyc","svDialogs", "wle")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  Resultats<-list() 
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()
  if(class(data)=="data.frame") deparse(substitute(data))->data 
  reg.in.output<-regressions.in(data=data, modele=modele, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                                CV=CV, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale, info=info,
                                sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
  if(is.null(reg.in.output)) return(choix.reg()) 
  data<-reg.in.output$data
  nom<-reg.in.output$nom
  modele<-reg.in.output$modele
  param<-reg.in.output$options$choix
  n.boot<-reg.in.output$options$n.boot
  if(reg.in.output$options$rscalei) rscale<-reg.in.output$options$rscale/2 else rscale<-reg.in.output$options$rscale
  outlier<-reg.in.output$options$desires
  sauvegarde<-reg.in.output$options$sauvegarde
  scale<-reg.in.output$reg.options$scale
  inf<-reg.in.output$reg.options$inf
  CV<-reg.in.output$reg.options$CV
  step<-reg.in.output$reg.options$step
  select.m<-reg.in.output$reg.options$select.m
  method<-reg.in.output$reg.options$method
  criteria<-reg.in.output$reg.options$criteria
  group<-reg.in.output$reg.options$group
  
  
  
  
  
  
  
  
  if(any(outlier==  "Données complètes")){
    Resultats$"Données complètes"<-regressions.out(data=data, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                   sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
    if(!is.null(group))   {  
      R1<-list()
      G<-data[,group]
      if(length(group)>1) G<-as.list(G)
      G<-split(data, G)
      for(i in 1:length(G)){
        resg<-regressions.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                              sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
        
        R1[[length(R1)+1]]<-resg
        names(R1)[length(R1)]<-names(G)[i]
      }
      Resultats$"Données complètes"$"Analyse par groupe"<-R1
    } 
    
  } 
  if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Données sans valeur influente")|inf==T){
    lm.r1<-lm(modele, data)
    as.character(attributes(terms(modele))$variables)->variables
    variables[2:length(variables)]->variables
    plot(lm.r1, which = 5)
    if(inf) {
      influence.measures(lm.r1)->mesure_influence
      data<-data.frame(data, round(mesure_influence$infmat,3))
      rstandard(lm.r1)->data$res.stand
      rstudent(lm.r1)->data$res.student # idem avec le residu studentise
      data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residual, lower.tail=F)
      data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
      data$est.inf<-" "
      data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
      
      data[order(data$res.student.p.Bonf), ]->data
      writeLines("Les observations marquées d'un astérisque sont considérées comme influentes au moins sur un critère")
      View(data)
      suppression<-"yes"
      outliers<-data.frame()
      nettoyees<-data
      while(suppression=="yes"){
        
        cat ("Appuyez [entrée] pour continuer")
        line <- readline()
        sup<-NA
        while(is.na(sup)){
          sup <- dlgInput("Quelle observation souhaitez-vous retirer des analyses ? 0=aucune", 0)$res
          if(length(sup)==0) return(regressions())
          strsplit(sup, ":")->sup
          tail(sup[[1]],n=1)->sup
          as.numeric(sup)->sup
          if(is.na(sup)) msgBox("Vous devez entrer le numéro permettant de savoir quelle observation doit être supprimée.")  
        }
        if(sup==0) suppression<-"no" else {
          rbind(outliers, nettoyees[which(dimnames(nettoyees)[[1]]==sup),])->outliers
          nettoyees[-which(dimnames(nettoyees)[[1]]==sup),]->nettoyees
        }
        
      }
      if(length(outliers)!=0) outliers<-outliers[,variables]
      assign(nom, data, envir=.GlobalEnv)
    } else {
      4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes 
      cooks.distance(lm.r1)->data$cook.d  
      data[which(data$cook.d<= seuil_cook), ]->nettoyees 
      data[which(data$cook.d>= seuil_cook), ]->outliers
      cbind(outliers[,variables],outliers$cook.d)->outliers
      Resultats$"information"$"les valeurs influentes sont identifiées sur la base de 4/n"
    }
    nettoyees->>nettoyees   
    
    if(any(outlier== "Identification des valeurs influentes")){
      length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
      data.frame("N.retiré"=N_retire, "Pourcent.obs.retirées"=Pourcentage_retire)->Resultats$"Synthèse du nombre d'observations considerées comme influentes"
      if(length(outliers)!=0) Resultats$"Identification des valeurs influentes"$"Observations considerées comme influentes"<-outliers
      
    }
    if(any(outlier== "Données sans valeur influente")) {
      if(N_retire!=0 | all(outlier!="Données complètes")){
        Resultats$"Données sans valeur influente"<-regressions.out(data=nettoyees, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                                   sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
        
        if(!is.null(group))   {  
          R1<-list()
          G<-nettoyees[,group]
          if(length(group)>1) G<-as.list(G)
          G<-split(nettoyees, G)
          for(i in 1:length(G)){
            resg<-regressions.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                  sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
            
            R1[[length(R1)+1]]<-resg
            names(R1)[length(R1)]<-names(G)[i]
          }
          Resultats$"Données sans valeur influente"$"Analyse par groupe"<-R1
        } 
        
        
      }
    }
  }
  
  
  paste(outlier, collapse="','", sep="")->outlier
  paste(param, collapse="','", sep="")->param
  as.character(modele)->m1
  modele<-paste0(m1[2],"~", m1[3])
  if(!is.null(group)) paste(group, collapse="','", sep="")->group
  if(!is.null(step)) {
    paste0("list(")->step.call
    for(i in 1:length(step)){
      if(i>1) n.step<-paste0(", step",i) else n.step<-paste0("step",i)
      paste(step[[i]], collapse="','", sep="")->var.step
      step.call<-paste0(step.call,n.step,"=c('", var.step, "')")
    }
    step.call<-paste0(step.call, ")")
  }
  Resultats$Call<-paste0("regressions(data=", nom, ",modele=",  modele, ",outlier=c('", outlier, "'),inf=", inf, ",CV=", CV,",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                         ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),
                         ",criteria=", criteria, ",scale=", scale, ",dial=T, info=T,sauvegarde=", sauvegarde, ",n.boot=", n.boot, ",param=c('", param, "'),rscale=", round(rscale,3), ")")
  
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste("regressions.multiples", Sys.time() ))  
  if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix="Régressions.multiples", env=.e)
  Resultats$Références<-ref1(packages)
  return(Resultats)
}

.regressions.options<-function(data=NULL, modele=NULL, CV=F, inf=F, select.m="none", method="p", criteria=NULL, step=NULL, group=NULL, scale=T, dial=T,info=T){
  # data : dataframe 
  # modele : formula as it is used in lm
  # CV : logical. Should a cross validation to be performed ? 
  # inf : Logical. Should influential observations be checked ? 
  # select.m : character specifying method of selection. One among "none", "forward", "backward" and "bidirectional"
  # method : if select is different of "none", one among "AIC", "F", or "p"
  # criteria : if method is "F", specify F value to use. If method is "p", specify p value to use as cutoff criteria. 
  # step : list. Each element of the list is a vector with the effect to test at the specific step (see details)
  # group : character. Name of the factor variable definying the groups
  # scale : Logical. Should the predictor be scaled before the analysis (recommended) ? 
  
  Resultats<-list()
  step1<-terms(as.formula(modele))
  
  step2<-as.character( attributes(step1)$variables)[-1]
  step1<-attributes(step1)$term.labels
  if(dial || !is.logical(scale)){
    if(info)   writeLines("Voulez-vous centrer les variables numériques ? Centrer est généralement conseillé (e.g., Schielzeth, 2010).")
    scale<-dlgList(c("Centré", "Non centré"), multiple = FALSE, title="Centrer?")$res
    if(length(scale)==0) return(NULL)
    scale<-ifelse(scale=="Centré",T,F) 
  }
  Resultats$scale<-scale
  if(dial || !is.logical(inf) || !is.logical(CV)) {
    writeLines("Voulez-vous préciser d'autres options ? Vous pouvez en sélectionner plusieurs.
               Les méthodes de sélection permettent de sélectionner le meilleur modèle sur la base de critères statistiques.
               Les modèles hiérarchiques permettent de comparer plusieurs modèles. 
               Les validations croisées permettent de vérifier si un modèle n'est pas dépendant des données. Cette option est à utiliser notamment 
               avec les méthodes de sélection. L'analyse par groupe permet de réaliser la même régression pour des sous-groupes.
               Les mesures d'influences sont les autres mesures habituellement utilisées pour identifier les valeurs influentes.")
    autres.options<-c("Méthodes de sélection", "Modèles hiérarchiques", "Validation croisée","Mesure d influence",  "aucune")
    if(length(step2)<length(data))  autres.options<-c("analyse par groupes",autres.options)
    
    autres.options<- dlgList( autres.options, preselect=c("aucune"), multiple = TRUE, title="Autres options?")$res 
    if(length(autres.options)==0) return(.regressions.options(data=data, modele=modele))
    # if(any(autres.options=="aucune")) return(Resultats)   
    if(any(autres.options=="Mesure d influence") ) Resultats$inf<-T else  Resultats$inf<-F
    if(any(autres.options=="Validation croisée") ) Resultats$CV<-T else Resultats$CV<-F
  }else{Resultats$inf<-inf
  Resultats$CV<-CV 
  autres.options<-"aucune"
  }
  
  
  if(any(autres.options=="analyse par groupes") || !is.null(group)) {
    
    msg5<-"Veuillez choisissez le facteur de classement catégoriel."
    group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=T, message=msg5,  multiple=FALSE, title="Variable-s groupes", out=step2)
    if(length(group)==0) { return(.regressions.options(data=data, modele=modele))}
    data<-group$data
    group<-group$X 
    ftable(data[,group])->groupe.check
    if(any(is.na(groupe.check)) || min(groupe.check)<(length(dimnames(model.matrix(as.formula(modele), data))[[2]])+10)) {
      msgBox("Il faut au moins 10 observations plus le nombre de variables pour réaliser l'analyse. Vérifiez vos données.")
      return(groupe.check)
    }
  }
  
  if(any(autres.options=="Méthodes de sélection") || select.m!="none" & length(select.m)!=1 | !select.m%in%c("none","forward", "backward", "bidirectional","Forward - pas-à-pas ascendant",
                                                                                                             "Backward- pas-à-pas descendant", "Bidirectionnel")){
    if(info) writeLines("Veuillez choisir la méthode de sélection que vous souhaitez utiliser")
    select.m<- dlgList(c("Forward - pas-à-pas ascendant","Backward- pas-à-pas descendant", "Bidirectionnel"), 
                       preselect=NULL, multiple = FALSE, title="Choix de la méthode")$res
    if(length(select.m)==0) return(.regressions.options(data=data, modele=modele))
  } 
  if(!is.null(method)){
    if(any(autres.options=="Méthodes de sélection")   || (select.m!="none" && !method%in%c("AIC", "p", "F", "valeur du F","valeur de la probabilité", "AIC - Akaike Information criterion")) ){
      if(info) writeLines("Quel méthode faut-il appliquer pour la méthode de sélection ?")
      method<- dlgList(c("valeur du F","valeur de la probabilité", "AIC - Akaike Information criterion"), 
                       preselect=c("valeur du F"), multiple = FALSE, title="Choix de la méthode")$res
      if(length(method)==0) return(.regressions.options(data=data, modele=modele)) 
    }
    
    if(select.m!="none" & (method=="valeur du F" | method=="F")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<1)) {msgBox("Vous devez spécifier la valeur du F. Cette valeur doit être supérieure à 1")
        criteria<-NULL}
      
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur du F voulez-vous utiliser ?", 4)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria<1) {criteria<-NULL
          msgBox("Vous devez spécifier la valeur du F. Cette valeur doit être supérieure à 1")
          }
        }
      }
    }
    
    if(select.m!="none" & (method=="valeur de la probabilité" | method=="p")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<0 || criteria>1)) {msgBox("Vous devez spécifier la valeur de la probabilité. Cette valeur doit être entre 0 et 1")
        criteria<-NULL}
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur de la probabilité voulez-vous utiliser ?", 0.15)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria>1 || criteria<0 ) {criteria<-NULL
          msgBox("Vous devez spécifier la valeur de la probabilité. Cette valeur doit être entre 0 et 1")}
        }
      }
      qf(criteria, 1, (length(data[,1])-1-length(step1)), lower.tail = F, log.p = FALSE)->criteria
    }
  }
  if(any(autres.options=="Modèles hiérarchiques")| !is.null(step)) {
    
    if(!is.null(step) ){
      st1<-unlist(step)
      if(any(table(st1>1))) st1<-"erreur"
      if(any(!st1%in%step1 ))st1<-"erreur"
      if(st1=="erreur"){
        msgBox("Un problème a été identifié dans les étapes de votre régression hiérarchique")
        step<-NULL
      }
    }         
    if(is.null(step)){
      if(info) writeLines("Veuillez choisir les variables à utiliser pour chaque étape")      
      step<-list()
      step[[1]]<- dlgList(step1, preselect=NULL, multiple = TRUE, title="Variable(s) de cette étape")$res
      if(length(step[[1]])==0) return(.regressions.options(data=data, modele=modele))
      setdiff(step1,step[[1]])->step1
      
      while(length(step1!=0)){
        step[[length(step)+1]]<-dlgList(step1, multiple = TRUE,title="Variable(s) de cette étape")$res
        if(length(step[[length(step)]])==0) return(.regressions.options(data=data, modele=modele))
        setdiff(step1,step[[length(step) ]])->step1
      } 
    }
  } 
  
  Resultats$step<-step
  Resultats$select.m<-select.m
  Resultats$method<-method
  Resultats$criteria<-criteria
  Resultats$group<-group 
  return(Resultats) 
}

choix.reg<-function(){
  try(library(svDialogs), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  dlgList(c("Régressions", 
            "Effets de mediation", 
            "Régressions logistiques"), preselect="Régressions", multiple = FALSE, title="Quel type de régression ?")$res->choix
  if(length(choix)==0) return(analyse())
  if(choix=="Régressions") regressions()->Resultats
  if(choix=="Effets de mediation") ez.mediation()->Resultats
  if(choix=="Régressions logistiques") regressions.log()->Resultats
  return(Resultats)
  
}

ez.mediation<-function(info=T){
  options (warn=-1) 
  .e <- environment()
  c("boot", "MBESS","QuantPsyc", "svDialogs")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  Resultats<-list()
  dlgList(c("Effets de médiation simple", 
            "Effet de médiation distante"), preselect=NULL, multiple = FALSE, title="Quel type de médiation ?")$res->choix
  if(length(choix)==0) return(analyse())
  choix.data(nom=T)->data
  if(is.null(data)) return(ez.mediation())
  data[[1]]->nom
  data[[2]]->data
  listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), names(data))
  if(info) writeLines("veuillez préciser le prédicteur")
  X<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = F, 
             title="Prédicteur")$res
  if(length(X)==0) return(ez.mediation())
  subset(listes, listes[,1] %in% X)[,2]->X
  as.character(X)->X
  if(info) writeLines("veuillez choisir le médiateur")
  Mediator<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                    title="Médiateur")$res
  if(length(Mediator)==0) return(ez.mediation())
  subset(listes, listes[,1] %in% Mediator)[,2]->Mediator
  as.character(Mediator)->Mediator
  if(choix=="Effet de médiation distante"){
    writeLines("veuillez préciser le second médiateur.")
    Mediator2<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, title="Mediateur 2")$res
    if(length(Mediator2)==0) return(ez.mediation())
    subset(listes, listes[,1] %in% Mediator2)[,2]->Mediator2
    as.character(Mediator2)->Mediator2
  }
  
  if(info) writeLines("veuillez choisir la variable dépendante")
  VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
              title="Variable dépendante")$res
  subset(listes, listes[,1] %in% VD)[,2]->VD
  as.character(VD)->VD
  writeLines("veuillez préciser le nombre de bootstrap. Un minimum de 500 est idéalement requis. Peut prendre du temps pour N>1000")
  n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
  if(length(n.boot)==0) n.boot<-"0"
  strsplit(n.boot, ":")->n.boot
  tail(n.boot[[1]],n=1)->n.boot
  as.numeric(n.boot)->n.boot
  if(!is.na(n.boot) && any(n.boot>50)) bootstrap<-TRUE else bootstrap<-FALSE
  
  if(choix=="Effets de médiation simple"){
    mediation(data[,X], data[,Mediator], data[,VD], conf.level = 0.95, bootstrap = bootstrap, B = n.boot, which.boot="both", save.bs.replicates=TRUE, complete.set=TRUE)->mediation.out
    for(i in 1:length(mediation.out)){
      if(class(mediation.out[[i]])== "list") for(j in 1 : length(mediation.out[[i]])){
        round(mediation.out[[i]][[j]], 4)->mediation.out[[i]][[j]]} else {
          round(mediation.out[[i]], 4)->mediation.out[[i]]}
    }  
    Resultats$Analyse.mediation<-mediation.out
    Resultats$Information<-"Pour une description détaillée des résultats, ?mediation"
    mediation.effect.bar.plot2(data[,X], data[,Mediator], data[,VD],main = "Mediation Effect Bar Plot", width = 1, left.text.adj = 0,right.text.adj = 0, rounding = 3, file = "", save.pdf = FALSE,save.eps = FALSE, save.jpg = FALSE)
  }else { data2<-data[,c(X, Mediator, Mediator2, VD)]
  names(data2)<-c("x", "m1","m2","y")
  distal.med(data2)->results
  data.frame(results)->results
  round(as.numeric(as.character(results$Effect)),4)->results$Effect
  round(as.numeric(as.character(results$SE)),4)->results$SE
  round(as.numeric(as.character(results[,3])),3)->results$t.ratio
  round(as.numeric(as.character(results$Med.Ratio)),4)->results$Med.Ratio 
  names(results)<-c("Effet", "Erreur.st","test.t", "Ratio.med")
  results->Resultats$"Médiation à distance"
  Resultats$Information<-"Pour une description détaillée des résultats, ?distal.med"
  distmed.boot <- boot(data2, distInd.ef, R=n.boot)
  boot.ci(distmed.boot, conf=.95, type=c("basic","perc", "norm"))->IC.boot
  round(matrix(c(IC.boot$normal[,2:3],IC.boot$basic[,4:5],IC.boot$percent[,4:5]), ncol=2 ),4)->IC.boot
  dimnames(IC.boot)[[1]]<-c("normal","basic","percentile")
  dimnames(IC.boot)[[2]]<-c("limite.inf","limite.sup")
  IC.boot->Resultats$"Intervalle de confiance éstimé par bootstrap"}
  
  dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez-vous sauvegarder?")$res->sauvegarde
  if(length(sauvegarde)==0) sauvegarde<-FALSE  
  if(sauvegarde) save(Resultats=Resultats, choix=choix, env=.e)
  ref1(packages)->Resultats$Références    
  return(Resultats)  
  
}
#### RÃÂ©gressions logistiques ####
# ajouter les modÃÂ¨les multinomiques
# laisser la possibilitÃÂ© de faire d'autres distributions 
regressions.log<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
                          sauvegarde=F,proba=F){
  
  logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance
    nullDev <- LogModel$null.deviance
    modelN <-  length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
  } 
  reg.log.in<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
                       sauvegarde=F,proba=F){
    
    options (warn=-1) 
    Resultats<-list()
    if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(NULL) 
    nom<-data[[1]]
    data<-data[[2]]  
    
    
    if(dial && is.null(modele)){
      if(info) writeLines("Veuillez choisir le(s) type(s) de relations entre les variables. Les effets additifs prennent la forme de
                          y=X1+X2 tandis que les effets d'interaction prennent la forme de Y=X1+X2+X1:X2")
      dlgList(c("Effets additifs", "Effets d'interaction", "Spécifier le modèle"), preselect="Régressions", multiple = TRUE, title="Quel type de régression ?")$res->link
      if(length(link)==0) return(NULL)} else link<-"none"
    
    if(length(Y)>1){
      msgBox("Il ne peut y avoir qu'une seule variable dépendante.")
      Y<-NULL }
    if(any(link %in% c("Effets additifs", "Effets d'interaction"))){
      msg3<-"Veuillez choisir la variable dépendante."
      Y<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=FALSE, title="Variable dépendante", out=NULL)
      if(is.null(Y)) {
        reg.log.in()->Resultats
        return(Resultats)}
      data<-Y$data
      Y<-Y$X
      
      if(length(unique(data[,Y]))!=2) {
        msg1<-paste("Votre vériable dépendante a", length(unique(data[,Y])), "modalités. Elle est incompatible avec une régression logistique. Elle doit être dichotomique" )
        msgBox(msg1)
        if(class(data[,Y]) %in%c("numeric","integer")){
          dlgMessage("voulez-vous convertir la variable dépendante en une variable dichotomique,  ?","yesno")$res->conv
          
          if(conv=="no") return(reg.log.in())  else{
            if(info) writeLines("Veuillez spécifier le critère sur lequel vous souhaitez dichotomiser votre variable.Vous pouvez utiliser la médiane ou choisir un seuil spécifique.")
            dlgList(c("Médiane", "Seuil"), preselect="Médiane", multiple = FALSE, title="Quel critère de codage voulez-vous ?")$res->codage
            if(length(codage)==0) return(reg.log.in())
            if(codage=="Médiane") data[,Y]<-ifelse(data[,Y]>median(data[,Y]),1, 0)
            View(data)
            readline()
            if(codage=="Seuil") {
              seuil<-NA
              while(is.na(seuil)){
                seuil<-dlgInput("Veuillez préciser la valeur de séparation", median(data[,Y]))$res 
                if(length(seuil)==0) return(reg.log.in())
                strsplit(seuil, ":")->seuil
                tail(seuil[[1]],n=1)->seuil
                as.numeric(seuil)->seuil
                if(is.na(seuil) || seuil>max(data[,Y]) || seuil<min(data[,Y])) {msgBox("La valeur doit être numérique et comprise entre le minimum et le maximum de la variable dépendante.")
                  Y<-NA}
              }
              data[,Y]<-ifelse(data[,Y]>seuil,1, 0)
              
            } # seuil
          }
        }
        if(class(data[,Y]) %in%c("factor","character")){
          dlgMessage("Voulez-vous faire des regroupements entre les modalités ?","yesno")$res->reg
          if(reg=="no") return(reg.log.in()) else {
            if(info) writeLines("Veuillez spécifier la/les modalité(s) qui serviront pour la ligne de base (e.g. 0). Les autres modalités seront regroupés dans la catégorie 1.")
            reg<- dlgList(levels(data[,Y]), preselect=NULL, multiple = TRUE, title="Modalités à regrouper")$res
            setdiff(levels(data[,Y]),reg)->reste
            data[,Y]<-ifelse(data[,Y]%in%reg, 0,1) 
            data[,Y]<-factor(data[,Y])
          }
        }      
      }
      
      
      if(any(link=="Effets additifs") || !null(X_a)| any(X_a %in% names(data)==F)) {
        msg3<-"Veuillez choisir la variable dépendante."
        X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modèle additif", out=Y)
        if(is.null(X_a)) {
          reg.log.in()->Resultats
          return(Resultats)}
        data<-X_a$data
        X_a<-X_a$X
        
      }else X_a<-NULL 
      
      if(any(link=="Effets d'interaction") || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
        msg3<-"Veuillez choisir les prédicteurs à entrer dans le modèle d'interaction. Il est nécessaire d'avoir au moins deux variables"
        X_i<-c()
        while(length(X_i)<2){
          X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modèle interactif", out=c(X_a,Y))
          if(is.null(X_i)) {
            reg.log.in()->Resultats
            return(Resultats)}
          data<-X_i$data
          X_i<-X_i$X
        }
      }else X_i<-NULL
      
      
      
      paste0(Y," ~ ")->modele
      if(!is.null(X_a ))  {
        X_a.mod<-X_a[1]
        if(length(X_a)>1) for(i in 2 : length(X_a)) paste0(X_a.mod, "+", X_a[i])-> X_a.mod
      } else X_a.mod<-NULL
      
      if(!is.null(X_i)){
        X_i.mod<-X_i[1]
        if(length(X_i)>1) for(i in 2 : length(X_i)) paste0(X_i.mod, "*", X_i[i])-> X_i.mod
      } else X_i.mod<-NULL
      
      if(!is.null(X_a.mod) & !is.null(X_i.mod)) {
        paste0(modele, X_a.mod, "+", X_i.mod)->modele
      } else paste0(modele, X_a.mod, X_i.mod)->modele
      
    }
    
    
    
    if(any(link=="Spécifier le modèle")) modele<-fix(modele)
    variables<-terms(as.formula(modele))
    variables<-as.character( attributes(variables)$variables)[-1]
    pred<-attributes(terms(as.formula(modele)))$term.labels
    if(any(ftable(data[,which(lapply(sapply(data,unique),length)<5)])<3)) {
      msgBox("Les observations sont en nombre insuffisant (<3) pour certaines combinaisons de niveaux des variables du modèle")
      return(ftable(data[,which(lapply(sapply(data,unique),length)<5)]))
    }
    if(dial){
      if(length(pred>1)){
        pred.ord<-c()
        while(length(pred)!=0){
          if(info)  writeLines("L'ordre d'entrée des variables est important pour le calcul du maximum de vraisemblance. Veuillez
                               préciser l'ordre d'entrée des variables") 
          V1<-dlgList(pred, multiple = FALSE,title="Quelle variable à cette étape")$res
          c(pred.ord,V1)->pred.ord
          setdiff(pred,V1)->pred}
      }else pred.ord<-pred
      
      paste0(Y," ~ ", pred.ord[1])->modele
      if(length(pred.ord)>1) for(i in 2 : length(pred.ord)) paste0(modele, "+", pred.ord[i])-> modele
      modele<-as.formula(modele)}
    
    model.test<-try(model.matrix(modele, data), silent=T)
    if(class(model.test)=="try-error") {
      msgBox("Le modèle spécifié est incorrect. Vérifiez vos variables et votre modèle")
      return(reg.log.in())
    }
    
    
    data[complete.cases(data[,variables]),]->data
    options<-.ez.options(options=c("outlier"), n.boot=NULL,param=F, non.param=F, robust=F, Bayes=F, msg.options1=NULL, msg.options2=NULL, info=info, dial=dial, 
                         choix=NULL,sauvegarde=sauvegarde, outlier=outlier, rscale=NULL)
    if(is.null(options)) return(reg.log.in())
    
    reg.options<- .regressions.options(data=data, modele=modele, CV=FALSE, inf=inf, select.m=select.m, method=NULL, criteria=NULL, step=step, group=group, scale=scale, dial=dial,info=info)
    if(is.null(reg.options)) return(reg.log.in())
    
    if(dial){
      if(info) writeLines('voulez-vous intégrer les probabilités à votre base de données ?')
      dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Probabilités ?")$res->proba
      
    }
    
    Resultats$proba<-proba
    Resultats$data<-data
    Resultats$nom<-nom
    Resultats$modele<-modele
    Resultats$options<-options
    Resultats$reg.options<-reg.options
    return(Resultats)   
    
  }
  
  reg.log.out<-function(data=NULL, modele=NULL,  select.m="none", step=NULL, scale=T, nom=NULL,proba=F){
    
    Resultats<-list()
    variables<-terms(as.formula(modele))
    variables<-as.character( attributes(variables)$variables)[-1]
    pred<-attributes(terms(as.formula(modele)))$term.labels
    Resultats$"Statistiques descriptives"<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)
    
    if(scale==T || scale=="Centré") {Resultats$info<-"En accord avec les recommandations de Schielzeth 2010, les données ont été préalablement centrées"
    fun<-function(X){X-mean(X)}
    variables[-1]->pred2
    sapply(X=data[, names(which(sapply(data[,pred2],class)!="factor"))], fun)->data[,names(which(sapply(data[,pred2],class)!="factor"))]
    }
    if(class(data[,variables[1]])=="character") factor(data[,variables[1]])->data[,variables[1]]
    
    if(!is.null(step)){
      
      as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
      list()->modele.H1
      list()->formule.H1
      for(i in 1:length(step)){
        
        for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
        formule.H1[[i]]<-modele.H
        glm(modele.H, data=data, na.action=na.exclude , family="binomial")->lm.H
        lm.H->modele.H1[[i]]}
      
      hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
      if(length(modele.H1)>2){
        for(i in 3: length(modele.H1)){
          hier<-paste0(hier, ",modele.H1[[", i, "]]")
        }
      }
      hier<-paste0(hier,")")
      hier<-eval(parse(text=hier))
      
      attributes(hier)$heading[1]<-"Table de l'analyse de la déviance des modèles hiérarchiques"
      round(1-pchisq(hier$Deviance,hier$Df,lower.tail=F),4)->hier$valeur.p
      names(hier)<-c("ddl.résid", "Déviance.résid","ddl.effet", "Déviance", "valeur.p")
      Resultats$"Analyse hiérarchique des modèles "<-hier
    }
    
    
    
    
    
    mod<-list()
    modele1<-as.formula(paste0(variables[1], "~", pred[1]))
    glm(modele1, data=data, family="binomial")->glm.r1
    mod <- list() 
    glm.r1->mod[[1]] 
    if(length(pred)>1) {
      for(i in 2:length(pred)){update(glm.r1, as.formula(paste0(".~.+",pred[i])))->glm.r1
        glm.r1->mod[[i]]}
    }
    
    anova(mod[[length(mod)]])->Amelioration_du_MV
    
    summary(mod[[length(mod)]])->resultats
    as(resultats$call,"character")->texte
    paste("le modele testé est" , texte[2])->Resultats$"Modèle testé"
    
    cbind(rms::vif(mod[[length(mod)]]), 1/rms::vif(mod[[length(mod)]]))->MC
    dimnames(MC)[[2]]<-c("Facteur d'inflation de la variance", "Tolérance")
    round(MC,4)->Resultats$"Test de multicolinéarité"
    
    sum(Amelioration_du_MV$Df[2:length(Amelioration_du_MV$Df)])->ddl
    Amelioration_du_MV$`Resid. Dev`[1]-Amelioration_du_MV$`Resid. Dev`[length(Amelioration_du_MV$`Resid. Dev`)]->chi.carre.modele
    round(1-pchisq(chi.carre.modele,ddl),4)->valeur.p
    logisticPseudoR2s(mod[[length(mod)]])->Pseudo.R.carre
    data.frame(chi.carre.modele, ddl, valeur.p,Pseudo.R.carre[1],Pseudo.R.carre[2],Pseudo.R.carre[3])->mod.glob
    names(mod.glob)<-c("chi.2.modèle", "ddl", "valeur.p","Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
    mod.glob->Resultats$"Significativité du modèle global"
    
    
    Amelioration_du_MV$chi.deux.prob<-1-pchisq(Amelioration_du_MV$Deviance, Amelioration_du_MV$Df)
    round(Amelioration_du_MV,4)->Amelioration_du_MV
    names(Amelioration_du_MV)<-c("ddl predicteur", "MV","ddl.residuels","MV residuel","valeur.p")
    Resultats$"Amélioration de la vraisemblance pour chaque variable"<-data.frame(Amelioration_du_MV)
    
    data.frame(resultats$coefficients)->table
    (table$z.value)^2->table$Wald.statistic
    exp(table$Estimate)->table$Odd.Ratio
    round(table,4)->table
    names(table)<-c("b","Erreur.standard","valeur.Z","p.Wald", "Wald","Odd.ratio")
    cbind(table, round(exp(confint(mod[[length(mod)]])),4))->table
    table$interpretation<-ifelse(table$Odd.ratio>=1,paste(table$Odd.ratio, "fois plus"), paste(round(1/table$Odd.ratio,4), "fois moins"))
    table->Resultats$"Table des coefficients"
    
    R_sq<-NULL
    for(i in 1:length(mod)){logisticPseudoR2s(mod[[i]])->R_squared
      rbind(R_sq, R_squared)->R_sq}
    diff(R_sq,lag=1)->R_sq[2.]
    dimnames(R_sq)[[1]]<-pred
    dimnames(R_sq)[[2]]<-c("Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
    R_sq->Resultats$"Delta du pseudo R carre"
    
    if(proba=="TRUE")	{ 
      round(fitted(mod[[length(mod)]]),4)->data$"Probabilités prédites"
      head(data)
      print(nom)
      assign(x=nom, value=data, envir=.GlobalEnv)}
    
    if(select.m!="none"){
      select.m<-switch(select.m,"Forward - pas-à-pas ascendant"="forward", "Backward- pas-à-pas descendant"="backward", "Bidirectionnel"="both",
                       "forward"="forward", "bidirectional"="both","backward"="backward" )
      glm(modele, data=data, family="binomial")->glm.r1
      
      steps<-stepAIC(glm.r1, direction=select.m) 
      Resultats$"Méthode de sélection - critères d'information d'Akaike"<-steps$anova
      modele<-as.formula(attributes(steps$anova)$heading[5])
    }
    
    return(Resultats)
    
    
    
  }
  
  
  c("boot","car","psych", "mlogit","svDialogs","rms","MASS")->packages
  if(class(data)=="data.frame") deparse(substitute(data))->data 
  options (warn=-1) 
  .e <- environment()
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  Resultats<-list() 
  reg.in.output<-reg.log.in(data=data, modele=modele, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                            select.m=select.m,  step=step, group=group,  scale=scale, info=info, sauvegarde=sauvegarde, proba=proba)
  if(is.null(reg.in.output)) return(choix.reg())
  data<-reg.in.output$data
  nom<-reg.in.output$nom
  modele<-reg.in.output$modele
  outlier<-reg.in.output$options$desires
  sauvegarde<-reg.in.output$options$sauvegarde
  scale<-reg.in.output$reg.options$scale
  inf<-reg.in.output$reg.options$inf
  step<-reg.in.output$reg.options$step
  select.m<-reg.in.output$reg.options$select.m
  group<-reg.in.output$reg.options$group
  proba<-reg.in.output$proba
  
  if(!is.null(reg.in.output$reg.options$CV) && reg.in.output$reg.options$CV==TRUE) print("La validation croisée n'est pas encore disponible.")
  
  if(any(outlier==  "Données complètes")){
    Resultats$"Données complètes"<-  reg.log.out(data=data, modele=modele,  select.m=select.m, step=step, scale=scale, proba=proba, nom=nom)
    if(!is.null(group))   {  
      R1<-list()
      G<-data[,group]
      if(length(group)>1) G<-as.list(G)
      G<-split(data, G)
      for(i in 1:length(G)){
        resg<-  try(reg.log.out(data=G[[i]], modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba), silent=T)
        if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"Le nombre d'observations est insuffisant pour mener à bien les analyses pour ce groupe" else R1[[length(R1)+1]]<-resg
        names(R1)[length(R1)]<-names(G)[i]
      }
      Resultats$"Données complètes"$"Analyse par groupe"<-R1
    } 
    
  } 
  if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Données sans valeur influente")|inf==T){
    
    lm.r1<-glm(modele, data, na.action=na.exclude ,family="binomial")
    as.character(attributes(terms(modele))$variables)->variables
    variables[2:length(variables)]->variables
    plot(lm.r1, which = 5)
    if(inf) {
      influence.measures(lm.r1)->mesure_influence
      data<-data.frame(data, round(mesure_influence$infmat,3))
      rstandard(lm.r1)->data$res.stand
      rstudent(lm.r1)->data$res.student # idem avec le residu studentise
      data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residual, lower.tail=F)
      data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
      data$est.inf<-" "
      data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
      
      data[order(data$res.student.p.Bonf), ]->data
      writeLines("Les observations marquées d'un astérisque sont considérées comme influentes au moins sur un critère")
      View(data)
      suppression<-"yes"
      outliers<-data.frame()
      nettoyees<-data
      while(suppression=="yes"){
        
        cat ("Appuyez [entrée] pour continuer")
        line <- readline()
        sup<-NA
        while(is.na(sup)){
          sup <- dlgInput("Quelle observation souhaitez-vous retirer des analyses ? 0=aucune", 0)$res
          if(length(sup)==0) return(regressions())
          strsplit(sup, ":")->sup
          tail(sup[[1]],n=1)->sup
          as.numeric(sup)->sup
          if(is.na(sup)) msgBox("Vous devez entrer le numéro de l'observation")  
        }
        if(sup==0) suppression<-"no" else {
          rbind(outliers, nettoyees[sup,])->outliers
          nettoyees[-sup,]->nettoyees
        }
        
      }
      if(length(outliers)!=0) outliers<-outliers[,variables]
      assign(nom, data, envir=.GlobalEnv)
    } else {
      4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes 
      cooks.distance(lm.r1)->data$cook.d  
      data[which(data$cook.d<= seuil_cook), ]->nettoyees 
      data[which(data$cook.d>= seuil_cook), ]->outliers
      cbind(outliers[,variables],outliers$cook.d)->outliers
      Resultats$"information"$"les valeurs influentes sont identifiées sur la base de 4/n"
    }
    nettoyees->>nettoyees   
    
    if(any(outlier== "Identification des valeurs influentes")){
      length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
      data.frame("N.retirées"=N_retire, "Pourcentage.obs.retirées"=Pourcentage_retire)->Resultats$"Synthèse du nombre d'observations considerées comme influentes"
      if(length(outliers)!=0) Resultats$"Identification des valeurs influentes"$"Observations considerées comme influentes"<-outliers
      
    }
    if(any(outlier== "Données sans valeur influente")) {
      if(N_retire!=0 | all(outlier!="Données complètes")){
        so<- try(reg.log.out(data=nettoyees,modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba, nom=paste0(nom,".nettoyees")),silent=T)
        if(class(so)=="try-error") Resultats$"Données sans valeur influente"<-"La suppression des valeurs influentes entraîne un effectif trop faible sur certaines modalités pour mener à bien l'analyse" else{
          Resultats$"Données sans valeur influente"<-so 
          
          if(!is.null(group))   {  
            R1<-list()
            G<-nettoyees[,group]
            if(length(group)>1) G<-as.list(G)
            G<-split(nettoyees, G)
            for(i in 1:length(G)){
              resg<- try( reg.log.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group,  scale=scale,proba=proba), silent=T)
              
              if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"Le nombre d'observations est insuffisant pour mener à bien les analyses pour ce groupe" else R1[[length(R1)+1]]<-resg
              names(R1)[length(R1)]<-names(G)[i]
            }
            Resultats$"Données sans valeur influente"$"Analyse par groupe"<-R1
          } 
        } 
        
      }
    }
  }
  
  
  paste(outlier, collapse="','", sep="")->outlier
  as.character(modele)->m1
  modele<-paste0(m1[2],"~", m1[3])
  if(!is.null(group)) paste(group, collapse="','", sep="")->group
  if(!is.null(step)) {
    paste0("list(")->step.call
    for(i in 1:length(step)){
      if(i>1) n.step<-paste0(", step",i) else n.step<-paste0("step",i)
      paste(step[[i]], collapse="','", sep="")->var.step
      step.call<-paste0(step.call,n.step,"=c('", var.step, "')")
    }
    step.call<-paste0(step.call, ")")
  }
  Resultats$Call<-paste0("regressions.log(data=", nom, ",modele=",  modele, ",outlier=c('", outlier, "'),inf=", inf, ",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                         ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),",dial=T, info=T,sauvegarde=", sauvegarde,",proba=",proba ,")")
  
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste("Régressions.logistique", Sys.time() ))  
  if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix="Régressions.logistique", env=.e)
  Resultats$Références<-ref1(packages)
  return(Resultats)
  
}



#### statistiques descriptives ####

stat.desc<-function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3, plot=T, ref=T, save=F){
  # X = vecteur de variables dont il faut faire les statistiques descriptives (character)
  # groupes = vecteur de variables dont il faut décomposer les statistiques descriptives par sous-groupe (character)
  # data = nom de la base de données
  # tr = troncature
  # type = type d'asymétrie et d'aplatissement à calculer (voir ?psych::describe)
  # plot = logical. Show the plot ? 
  # ref = logical. Show packages used in this function ?
  # save = logical. Should the output be saved in rtf and R file ? 
  stat.desc.in<-function(x=NULL, groupes=NULL, data=NULL, tr=.1, type=3, save=NULL){
    list()->Resultats
    choix.data(data=data, info=TRUE, nom=T)->data 
    if(length(data)==0) { return(NULL)} else {
      data[[1]]->nom1
      data[[2]]->data}
    # choix X
    if(!is.null(x)) dial<-F else dial<-T
    
    msg1<-"veuillez choisir les variables pour lesquelles vous désirez obtenir les statistiques descriptives"
    .var.type(X=X, info=T, data=data, type=NULL, message=msg1,multiple=T, title="Variable à analyser ?")->X1
    if(is.null(X1)) return(NULL)
    X1$X->x
    setdiff(names(data), x)->diff
    if(length(diff)==0 & !is.null(groupes)) {
      msgBox("Vous ne pouvez pas avoir de variable *groupes* étant donné que toutes les variables doivent être décrites")
      groupes<-NULL
    } 
    
    if(length(diff)>0){
      if(dial){
        writeLines("Vous pouvez décomposer les statistiques descriptives par sous-groupe en choisissant une ou plusieurs variables catégorielles. Voulez-vous spécifier les sous-groupes ?")
        groupes<-dlgList(c("oui", "non"), multiple = F, preselect="non", title="Spécifier groupes ?")$res
        if(length(groupes)==0) {stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
          return(Resultats)}
        if(groupes=="non") groupes<-NULL
      }
      
      if(!is.null(groupes)){
        msg2<-"Veuillez choisir la ou les variables définissant les groupes"
        .var.type(X=groupes, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable(s)  groupes ?", out=x)->groupes
        if(is.null(groupes)){
          stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
          return(Resultats)
        } 
        groupes$data->data
        groupes$X->groupes
      }
    } 
    
    if(dial==T | tr>1 | tr<0 | (type %in% 1:3==F) ) {
      writeLines("Vous pouvez spécifier la troncature et les paramètres pour l'aplatissement et l'asymétrie en choisissant autres options")
      options<-dlgList(c("oui", "non"), multiple = F, preselect="non", title="Spécifier les autres options?")$res
      if(length(options)==0) {
        stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
        return(Resultats)
      }
      if(options=="oui") {opts2<-NA
      while(any(is.na(opts2))){
        dlgForm(list("Troncature:NUM"=0.1, "Type de skew et kurtosis, doit se situer entre 1 et 3:NUM"=3),  "Veuillez fixer le seuil de la troncature")$res->opts2
        if(opts2[[1]]>0.5 | opts2[[1]]<0 ) NA->opts2[[1]] else tr<-opts2[[1]]
        if(opts2[[2]]%in% 1:3)  type<-opts2[[2]]  else opts2[[2]]<-NA  
        
      }
      }
      #      ez.options(save=T)$sauvegarde->save
    }
    Resultats$data<-data
    Resultats$nom1<-nom1
    Resultats$X<-x
    Resultats$groupes<-groupes
    Resultats$tr<-tr
    Resultats$type<-type
    Resultats$sauvegarde<-FALSE
    return(Resultats)
  }


  
  options (warn=-1)
  c( "ggplot2", "psych", "svDialogs")->packages
  lapply(packages, require, character.only=T)
  list()->Resultats
  .e <- environment()
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data
  
  stat.desc.in(x=X, groupes=groupes, data=data, tr=tr, type=type,save=save)->data.in
  if(is.null(data.in)) return(analyse())
  .stat.desc.out(X=data.in$X, groupes=data.in$groupes,data=data.in$data ,tr=data.in$tr, type=data.in$type)->Resultats 
  paste(data.in$X, collapse="','", sep="")->X
  if(is.null(data.in$groupes)) paste0("'), groupes = NULL, data=")->groupes else { paste(data.in$groupes, collapse="','", sep="")->groupes
    paste0("'), groupes =c('",groupes ,"'), data=")->groupes}
  paste0("stat.desc(X=c('", X, groupes, data.in$nom1, ",tr=" , tr, ",type=", type, ", plot=", plot, ", ref=", ref, ")")->Resultats$Call
  .add.history(data=data.in$data, command=Resultats$Call, nom=data.in$nom1)
  if(data.in$sauvegarde==TRUE) save(Resultats=Resultats ,choix =paste("Statistiques descriptives sur",data.in$nom1 ), env=.e)
  if(ref) ref1(packages)->Resultats$"Références des packages utilisés pour cette analyse"
  return(Resultats)
} 

#### toutes les formes de t de Student ####
test.t<-function(X=NULL, Y=NULL, group=NULL, choix=NULL,
                 sauvegarde=F, outlier=c("Données complètes",  "Identification des valeurs influentes","Données sans valeur influente"),  z=NULL, data=NULL,
                 alternative="two.sided", mu=NULL, formula=NULL, n.boot=NULL, 
                 param=c("test paramétrique", "test non paramétrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens"), info=TRUE, rscale=0.707){
  # X : Character specifying the dependant variable in dataframe. 
  # Y : character specifying either a two levels factor in dataframe or a numeric variable if paired is TRUE
  # group : Factor vector allowing to decompose analysis by group in one sample t test
  # choix : Character. One among c("Comparaison à une norme", "Deux échantillons appariés","Deux échantillons indépendants")
  # sauvegarde : logical. Should the results be saved ? 
  # outlier : character. One or several possibilities among c("Données complètes",   "Identification des valeurs influentes", "Données sans valeur influente")
  # z : if NULL and the identification/exclusion of outlier is desired, outlier are identified on Grubbs' test. If z is numeric, outliers are identified on abs(z)
  # data : data on which analysis has to be performed. 
  # alternative : one among c("greater", "lower", "two.sided"). Two sided is default. 
  # formula : a formula of the form dependant.variable~independant.variable
  # n.boot : number of bootstrap. Must be a positive value
  # param : character vector with one or several choices among c("test paramétrique", "test non paramétrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens")
  # info : logical. If dialog box are used, Should information be printed in the console
  # rscale : if "Facteurs bayesiens is choosen in "param", rscale is the prior scale. See t.testBF for more information
  
  #### 5 fonctions qui seront appelées pour réaliser l'analyse
  test.t.in<-function(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL, mu=NULL){
    
    Resultats<-list()
    if(!is.null(choix)) dial<-F else dial<-T
    if(is.null(choix) || (choix %in%c("Comparaison à une norme", "Deux échantillons appariés","Deux échantillons indépendants")==FALSE)){
      if(info) writeLines("Veuillez préciser le type de test t que vous souhaitez réaliser.")
      choix<-dlgList(c("Comparaison à une norme", "Deux échantillons appariés",
                       "Deux échantillons indépendants"), preselect=NULL, multiple = FALSE, title="Choix du test t")$res
      if(length(choix)==0) return(NULL)
    }
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(NULL)
    nom<-data[[1]]
    data<-data[[2]]
    if(is.null(Y) || class(data[,Y]) == "factor") format<-"long" else format<-"large"
    
    if(is.null(formula)){
      if(choix=="Deux échantillons appariés"){
        if(dial){
          if(info==TRUE){
            temps1<-1:3
            temps2<-4:6
            data.frame("temps1"=temps1,"temps2"=temps2)->large
            data.frame(c(rep("temps1",3),rep("temps2", 3)), 1:6)->long
            names(long)<-c("moment","mesure")
            writeLines("ceci est le format large")
            print(large)
            writeLines("ceci est le format long")
            print(long)}
          format<-dlgList(c("large", "long"), preselect="large", multiple = FALSE, title="Quel est le format de vos données?")$res
          if(length(format)==0) {
            Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                 formula=NULL,n.boot=NULL, rscale=NULL)
            return(Resultats)
          }
        }}  
      if(format=="large") {
        msg3<-"Veuillez choisir le temps 1."
        msg4<-"Veuillez choisir le temps 2."
      } else{
        msg3<-"Veuillez choisir la variable dépendante."
        msg4<-"Veuillez choisir la variable indépendante."
      }
      
      if(choix=="Deux échantillons appariés") {multiple<-F 
      if(length(X)>1){
        msgBox("Il ne peut y avoir qu'une seule variable dépendante pour les t de student pour échantillons appariés")
        X<-NULL }}else multiple<-T
        X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=multiple, title="Variable-s dépendante-s", out=NULL)
        if(is.null(X)) {
          test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                    formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
          return(Resultats)}
        data<-X$data
        X1<-X$X
        
        if(choix!="Comparaison à une norme"){
          if(choix=="Deux échantillons appariés" && format=="large") type<-"numeric" else type<-"factor"
          Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=FALSE, title="Variable indépendante", out=X1)
          if(is.null(Y)) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          data<-Y$data
          Y<-Y$X 
          if(class(data[,Y])=="factor" && nlevels(data[,Y]!=2)) {
            msgBox("Vous devez utiliser une variable indépendante catégorielle à 2 modalités")
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)
          }
        } 
    } else {
      X1<-as.character(formula[2])
      Y<-as.character(formula[3])
    }
    
    
    
    
    
    if(choix=="Deux échantillons appariés"){
      if(format=="large"){
        if(dial){
          if(info==TRUE)writeLines("Veuillez donner un nom à la variable indépendante. Donner un nom explicite à la variable indépendante rendra la lecture des résultats plus lisible")
          nomVI <- dlgInput("Quel est le nom de la variable indépendante?", "Moment")$res
          if(length(nomVI)==0) {
            Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                 formula=NULL,n.boot=NULL, rscale=NULL)
            return(Resultats)
          }
          strsplit(nomVI, ":")->nomVI
          tail(nomVI[[1]],n=1)->nomVI
          if(info==TRUE) writeLines("Veuillez donner un nom à la variable dépendante. Donner un nom explicite à la variable dépendante rendra la lecture des résultats plus lisible")
          nomVD <- dlgInput("Quel est le nom de la variable dépendante?", "Résultat")$res
          if(length(nomVD)==0) {
            Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                 formula=NULL,n.boot=NULL, rscale=NULL)
            return(Resultats)
          }
        } else {
          nomVD<-"Résultat"
          nomVI<-"Moment"
        }
        strsplit(nomVD, ":")->nomVD
        tail(nomVD[[1]],n=1)->nomVD
        data[complete.cases(data[,c(X1, Y)]),]->data
        data$IDeasy<-paste0("p", 1:length(data[,X1]))
        melt(data=data, measure.vars=c(X1,Y) , variable.name=nomVI, value.name=nomVD)->data
        assign(x=paste0(nom,".format.long"), value=data, envir=.GlobalEnv)
        X1<-nomVD
        Y<-nomVI
      }
      if(format=="long") {
        if( length(unique(table(data[,Y])))!=1) {
          msgBox("Le nombre d'occurrence pour chaque modalité de votre variable indépendante n'est pas identique. Veuillez choisir un identifiant participant") 
          msg4<-"Veuillez choisir la variable identifiant les participants"
          ID<-.var.type(X=NULL, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=multiple, title="Variable *Identifiant*", out=c(X1,Y))
          if(is.null(ID)) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          ID<-ID$X 
          ID.fail<-names(which(table(data[,ID])!=2))
          data<-data[which(data[,ID]!=ID.fail),]
          data<-data[order(data[,c(Y,ID)]), ]
        } else {
          data[order(data[,Y]),]->data
          data$IDeasy<-rep(paste0("p", 1:(length(data[,X1])/2)), 2) 
        }
      }
      
    }
    
    if(choix=="Comparaison à une norme"){
      writeLines("Veuillez spécifier la valeur de la norme")
      if(class(mu) !="numeric") mu<-NA
      while(is.na(mu)){
        mu <- dlgInput("Quelle est la valeur de la norme ?", 0)$res
        if(length(mu)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
          return(Resultats)}
        strsplit(mu, ":")->mu
        tail(mu[[1]],n=1)->mu
        as.numeric(mu)->mu
        if(is.na(mu)) msgBox("La norme doit être une valeur numérique.")  
      }
      if(dial){
        
        
        if(info==TRUE) writeLines("Une analyse bilatérale teste l'existence d'une différence. Le choix supérieur teste si la moyenne est strictement supérieure
                                  \n Le choix inférieur teste l'existence d'une différence strictement inférieure")
        dlgList(c("Bilatéral", "Supérieur", "Inférieur"), preselect=NULL, multiple = FALSE, title="Comparaison de moyennes")$res->alternative
        if(length(alternative)==0) {
          test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                    formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
          return(Resultats)
        } else car::recode(alternative, "'Bilatéral'= 'two.sided';'Supérieur'='greater'; 'Inférieur'='less'")->alternative
        
        if(info==TRUE) writeLines("Si vous souhaitez réaliser l'analyse pour différents sous-échantillons en fonction d'un critère catégoriel (i.e; réaliser une analyse par groupe)
                                  \n choisissez oui. Dans ce cas, l'analyse est realisée sur l'échantillon complet et sur les sous-échantillons.
                                  \n Si vous désirez l'analyse pour l'échantillon complet uniquement, chosissez non.
                                  \n l'analyse par groupe ne s'appliquent pas aux statistiques robustes.")
        dlgList(c("oui", "non"), preselect="non", multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
        if(length(par.groupe)==0) {
          test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                    formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
          return(Resultats)
        } 
        msg5<-"Veuillez choisissez le facteur de classement catégoriel."
        if(par.groupe=="oui"){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=FALSE, title="Variable-s", out=X1)
        if(length(group)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                         formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
          return(Resultats)}
        data<-group$data
        group<-group$X 
        }
      }
    }
    msg.options1<-"Le test paramétrique est le test t classique"
    msg.options2<- "Le test non paramétrique est le test de Wilcoxon (ou Mann-Whitney)"
    
    options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                         choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
    if(is.null(options)){
      test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
      return(Resultats)
    }
    Resultats$choix<-choix
    Resultats$nom<-ifelse(format=="large", paste0(nom,".format.long"), nom)
    Resultats$data<-data
    Resultats$X<-X1
    if(exists("Y")) Resultats$Y<-Y
    if(exists("mu")) Resultats$mu<-mu
    if(exists("alternative")) Resultats$alternative<-alternative
    if(exists("group")) Resultats$group<-group
    Resultats$options<-options
    return(Resultats)
    }
  
  norme<-function(X, mu, data, param=c("param", "non param", "robustes"), group=NULL, alternative="two.sided", n.boot=NULL, rscale=0.707){
    Resultats<-list()
    .e <- environment()
    Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=NULL, data=data, tr=.1, type=3, plot=F)
    
    cutoff <- data.frame(x = c(-Inf, Inf), y = mu, cutoff = factor(mu) )
    X1<-data[,X]
    p2<- ggplot(data, aes(x=factor(0), y=X1)) + geom_violin()
    p2<-p2+geom_line(aes( x, y, linetype = cutoff ), cutoff)
    p2<-p2+ labs( y=X)
    p2<-p2 + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
    p2<-p2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)
    p2<-p2 + theme(legend.position="none")
    p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("Moyenne et écart-type")
    print(p2)
    
    
    
    if(!is.null(group)) {Resultats$"statistiques descriptives par groupe"<-.stat.desc.out(X=X, groupes=group, data=data, tr=.1, type=3, plot=T) }
    if(any(param=="param") | any(param=="Test paramétrique")){
      Resultats$"Tests de normalité"<-.normalite(data=data, X=X, Y=NULL)
      t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)->ttest
      ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
      cohensD(data[,X], mu=mu)->dc
      data.frame("t test"=round(ttest$statistic,3), "ddl"=ttest$parameter, "valeur.p"=round(ttest$p.value,4), "Lim.inf.IC"=ttest$conf.int[[1]], "Lim.sup.IC"=ttest$conf.int[[2]], 
                 "R.carré"=round(R_carre,4), "D Cohen"=round(dc,3))->ttest
      dimnames(ttest)[1]<-" "
      ttest->Resultats$"Test de Student - comparaison à une norme"
      if(!is.null(group)){
        data<-data[complete.cases(data[,group]),]
        func <- function(data, moy=mu){ 
          t.test(data, mu = moy)->ttest
          ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
          cohensD(data[,1], mu=moy)->dc
          return(data.frame(test.t=round(ttest$statistic,3), 
                            ddl=ttest$parameter, 
                            valeur.p=round(ttest$p.value,4), 
                            IC.inf=ttest$conf.int[[1]], 
                            IC.sup=ttest$conf.int[[2]], 
                            R.carré=round(R_carre,4), 
                            D.Cohen=round(dc,3)))}
        data.frame(data[,X])->Y
        
        ddply(.data=Y, .(data[,group]), func)->t.groupes
        t.groupes->Resultats$"t de Student par groupe"}}
  
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
      if(all(param!="param") & all(param!="Test paramétrique")) Resultats$"Tests de normalité"<-.normalite(data=data, X=X, Y=NULL)
      
      BF<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale)
      BF<-extractBF(BF, onlybf=F)
      BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
      dimnames(BF)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle")
      Resultats$"Facteurs Bayesiens"<-BF
      if(!is.null(group)){
        func <- function(data, moy=mu, scale=rscale){ 
          ttestBF(data, mu = moy, rscale=scale)->BF
          BF<-extractBF(BF, onlybf=F)
          return(data.frame("Facteur bayesien"=round(BF$bf,5), "Erreur"=round(BF$error,5)))
        }
        BFgroup<-tapply(X=data[,X], data[,group], func,scale=rscale, moy=mu)
        BFgroup<-matrix(unlist(BFgroup), ncol=2, byrow=T)
        dimnames(BFgroup)<-list(levels(data[,group]), c("FB", "erreur"))
        BFgroup->Resultats$"Facteur bayesien par groupe"
      }
      samples<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
      plot(samples[,"mu"])
      
      
      bfs<-c()
      for (i in 5:length(data[,X])) {
        bfm <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=0.707)
        bfl <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1)
        bful <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1.41)
        bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
      }
      
      SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs, 
                      "rscale"=rep(c("moyen", "large", "ultra large"), length.out= 3*(length(data[,X])-4) ))
      names(SBF)<-c("n", "BF", "rscale")
      reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
      .plotSBF(SBF)
      
      ##### Début du graphique  Bayes Factor Robustness Check     
      
      # what is the t-value for the data?
      tVal <-  t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
      # how many points in the prior should be explored?
      nPoints <- 1000
      # what Cauchy rates should be explored?
      cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
      # what effect sizes should be plotted?
      effSize <- seq(from = -2, to = 2, length.out = 1000)
      
      # get the Bayes factor for each prior value
      bayesFactors <- sapply(cauchyRates, function(x) 
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
      
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
      plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
      # do the Bayes factor plot
      plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
           ylim = c(0, max(bayesFactors)), xaxt = "n", 
           xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
      abline(h = 0, lwd = 1)
      abline(h = 6, col = "black", lty = 2, lwd = 2)
      axis(1, at = seq(0, 1.5, 0.25))
      # add the BF at the default Cauchy point
      points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
      points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
      points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
      # add legend
      legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
             pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
             col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
      
    }
    
    if(any(param=="non param")| any(param=="Test non paramétrique")){
      
      wilcox.test(x= data[,X], y = NULL, alternative = alternative, mu = mu, paired = FALSE, exact = T,  
                  conf.int = TRUE, conf.level = 0.95)
      WT<-wilcox.test(data[,X],y=NULL, mu=mu, alternative, conf.int=T, conf.level=0.95)
      if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
      r<-z/(length(data[,X]))^0.5
      Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                      "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
      
      if(!is.null(group)){
        func <- function(data,Y=X, moy=mu, alt=alternative){
          WT<-wilcox.test(data[,Y],mu=moy, alternative=alt)
          if(alt!="two.sided") abs( qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
          r<-z/(length(data[,X]))^0.5
          return(data.frame(Wilcoxon.W=WT$statistic, valeur.p=round(WT$p.value,4), z=round(z,4), r=round(r,4)))
        }
        
        ddply(.data=data, .(data[, group]), func)->Wilcox.groupes
        Wilcox.groupes->Resultats$"Wilcoxon par groupe"
      }
    }
    
    if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
      try( round(unlist(WRS::trimci(data[,X],tr=.2,alpha=.05, null.value=mu)),4), silent=T)->m.tr
      if(m.tr!="try-error"){
        names(m.tr)<-c("lim.inf.IC","lim.sup.IC", "M.tronquée","test.t", "se","valeur.p","n")
        m.tr->Resultats$'Test sur la moyenne tronquée à 0.2' 
        data[,X]->x
        try(WRS::trimcibt(x, tr=.2,alpha=.05,nboot=n.boot,plotit=T,op=3)$ci, silent=T)->trimci
        try(WRS::mestci(x,alpha=.05,nboot=n.boot,bend=1.28,os=F),silent=T)->M.estimator
        try(WRS:: momci(x,alpha=.05,nboot=n.boot),silent=T)->MoM
        IC.robustes<-data.frame()
        if(class(trimci)!="try-error") {IC.robustes<-rbind(IC.robustes,trimci)
        dimnames(IC.robustes)[[1]][1]<-"bootstrap-t method"}
        if(class(M.estimator)!="try-error") {IC.robustes<-rbind(IC.robustes,M.estimator$ci)
        dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator"}
        if(class(MoM)!="try-error") {IC.robustes<-rbind(IC.robustes,MoM$ci)
        dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator modifié"}
        if(all(dim(IC.robustes)!=0)) names(IC.robustes )<-c("lim.inf.IC", "lim.sup.IC")
        Resultats$Robustes<-IC.robustes
        c("Le bootstrap-t method est un bootstrap adapté au calcul de la moyenne tronquée", 
          " Cet indice est adapté dans la plupart des situations. Le M-estimator modifié doit être préferé pour N<20",
          "La troncature sur le M-estimator s'adapte en fonction des caractéristiques de l'échantillon.")->Resultats$infos
      } else Resultats$Robustes<-"Les statistiques robustes n'ont pu être réalisées. Vérifiez que le packages WRS est correctement installé"
    }
    
    return(Resultats)
  }
  apparies<-function(X, Y, data=NULL, param=c("param", "non param", "robustes"),alternative="two.sided", n.boot=NULL, rscale=0.707){
    Resultats<-list()
    .e <- environment()
    Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
    large<-data.frame("t1"=data[which(data[,Y]==levels(data[,Y])[1]), X], "t2"=data[which(data[,Y]==levels(data[,Y])[2]), X])
    if(any(param=="param") | any(param=="Test paramétrique")){
      large$diff<--large$t2-large$t1
      Resultats$"Tests de normalité"<-.normalite(data=large, X="diff", Y=NULL)
      t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)->ttest
      ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
      cohensD(x= large[,1], y=large[,2], method="paired")->dc
      data.frame("t test"= round(ttest$statistic,3), "ddl"= ttest$parameter, "valeur.p"= round(ttest$p.value,4), "Lim.inf.IC"= ttest$conf.int[[1]], 
                 "Lim.sup.IC"=ttest$conf.int[[2]], "R.carre"=round(R_carre,4), "D de Cohen"=round(dc,3))->ttest
      dimnames(ttest)[1]<-" "
      ttest->Resultats$"Test de Student - comparaison de deux échantillons appariés"}
    if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
      if(all(param!="param") & all(param!="Test paramétrique")) Resultats$"Tests de normalité"<-.normalite(data=data, X=X, Y=Y)
      BF<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale)
      BF<-extractBF(BF, onlybf=F)
      BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
      dimnames(BF)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle")
      Resultats$"Facteurs Bayesiens"<-BF
      
      samples<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
      plot(samples[,1:4])
      
      
      bfs<-c()
      for (i in 5:(length(data[,X])/2)) {
        bfm <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE, rscale=0.707)
        bfl <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1)
        bful <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1.41)
        bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
      }
      
      SBF<-data.frame("n"=rep(5:(length(data[,X])/2), each=3 ),"BF"= bfs, 
                      "rscale"=rep(c("moyen", "large", "ultra large"), length.out= 3*((length(data[,X])/2)-4) ))
      names(SBF)<-c("n", "BF", "rscale")
      reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
      .plotSBF(SBF)
      
      ##### Début du graphique  Bayes Factor Robustness Check     
      
      # what is the t-value for the data?
      tVal <-  t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)$statistic
      # how many points in the prior should be explored?
      nPoints <- 1000
      # what Cauchy rates should be explored?
      cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
      # what effect sizes should be plotted?
      effSize <- seq(from = -2, to = 2, length.out = 1000)
      
      # get the Bayes factor for each prior value
      bayesFactors <- sapply(cauchyRates, function(x) 
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
      
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
      exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
      plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
      # do the Bayes factor plot
      plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
           ylim = c(0, max(bayesFactors)), xaxt = "n", 
           xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
      abline(h = 0, lwd = 1)
      abline(h = 6, col = "black", lty = 2, lwd = 2)
      axis(1, at = seq(0, 1.5, 0.25))
      # add the BF at the default Cauchy point
      points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
      points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
      points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
      # add legend
      legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
             pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
             col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
      
    }
    if(any(param=="non param")| any(param=="Test non paramétrique")) {
      WT<-wilcox.test(as.formula(paste0(X, "~",Y)), paired=T,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
      if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
      r<-z/(length(data[,X]))^0.5
      Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                      "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
    }
    
    if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
      try(WRS::yuend(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=.2),silent=T)->moy.tr
      if(class(moy.tr)!="try-error"){
        round(unlist(moy.tr),3)->moy.tr
        names(moy.tr)<-c("IC Inf","IC Sup", "valeur.p", "Moyenne1", "Moyenne2", "Difference","se", "Stat", "n", "ddl") 
        WRS::ydbt(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=0.2, nboot=n.boot)->moy.tr.bt
        moy.tr->Resultats$Robustes$"Comparaison basée sur les moyennes tronquées"
        round(unlist(moy.tr.bt),4)->Resultats$Robustes$"bootstrap studentisé sur les moyennes tronquées"
        if(length(data[,1])>20) {WRS::bootdpci(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], 
                                               nboot=n.boot, BA=T)$output[,2:6]->Mest
          names(Mest)<-c("statistique", "valeur.p", "p.crit", "CI inf", "CI sup")
          Mest->Resultats$Robustes$"Bootstrap de type BCa sur le M-estimator"}} else Resultats$Robustes<-"Les statistiques robustes n'ont pas pu être réalisées"
    }
    # realisation du graphique
    
    nonaj<-ggplot(data, aes(data[,Y], data[,X[1]]))+labs(x=Y, y=X)+
      stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
    nonaj<-nonaj+theme(plot.title = element_text(size = 12))+ggtitle("Données non ajustées")
    # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
    
    large$meanD2<-(large[ ,1]+large[ ,2])/2
    mean(large$meanD2)->GMean
    GMean-large$meanD2->large$adj
    large$adjM1<-large[ ,1]+large$adj
    large$adjM2<-large[ ,2]+large$adj
    data[,paste0(X, ".ajustée")]<-c(large$adjM1,large$adjM2)
    
    aj<-ggplot(data, aes(data[,Y], data[,length(data)]))+labs(x=Y, y=X)+stat_summary(fun.y=mean, geom="bar", 
                                                                                     fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
    aj<-aj+theme(plot.title = element_text(size = 12))+ggtitle("Données ajustées (Loftus & Masson, 1994)")
    multiplot(nonaj,aj, cols=2 )
    # print(nonaj)
    
    # print(aj)
    return(Resultats)                                                                               
  }  
  indpdts<-function(X, Y, data, param=c("param", "non param","robustes"),alternative="two.sided", n.boot=NULL, rscale=0.707){
    Resultats<-list()
    .e <- environment()
    Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
    as.formula(paste0(X," ~ ",Y))->modele
    if(any(param=="param") | any(param=="Test paramétrique")){
      Resultats$"Tests de normalité"<-.normalite(data=data, X=X, Y=Y)
      car::leveneTest(data[ ,X], data[ ,Y])->Levene # test de Levene pour homogeneite des variances
      round(unlist(Levene)[c(1,2,3,5)],3)->Levene
      names(Levene)<-c("ddl1","ddl2","F","valeur.p")
      Levene->Resultats$"Test de Levene vérifiant l'homogénéité des variances"
      t.test(modele, data=data, alternative=alternative,  var.equal=TRUE, conf.level=0.95)->student
      round(student$statistic^2/(student$statistic^2+student$parameter),3)->R.deux
      d_cohen<-round(cohensD(modele , data=data, method = "pooled"),3)
      data.frame(student[9], round(student$statistic,3), student$parameter, round(student$p.value,3), round(student$conf.int[1],4),
                 round(student$conf.int[2],4),  R.deux, d_cohen)->student
      t.test(modele, data=data, alternative=alternative,  var.equal=FALSE, conf.level=0.95)->corrige
      corrige$statistic^2/(corrige$statistic^2+corrige$parameter)->R.deux.corr
      d_cohen.corr<-cohensD(modele , data=data, method = "unequal")
      data.frame(corrige[9], round(corrige$statistic,3), round(corrige$parameter,3), round(corrige$p.value,3), round(corrige$conf.int[1],4),
                 round(corrige$conf.int[2],4),  R.deux, d_cohen)->corrige
      names(student)<-c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
      names(corrige)<- c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
      student<-rbind(student, corrige)
      dimnames(student)[[1]]<-c("sans correction de Welch","avec correction de Welch")
      student->Resultats$"t de student pour échantillons indépendants"
    }
    if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
      if(all(param!="param") & all(param!="Test paramétrique")) Resultats$"Tests de normalité"<-.normalite(data=data, X=X, Y=Y)
      BF<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale)
      BF<-extractBF(BF, onlybf=F)
      BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
      dimnames(BF)[[1]]<-c("En faveur de l'hypothèse alternative", "En faveur de l'hypothèse nulle")
      Resultats$"Facteurs Bayesiens"<-BF
      
      samples<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
      plot(samples[,1:4])
      
      
      bfs<-c()
      tab<-table(data[,Y])
      data1<-data.frame(X=c(data[which(data[,Y]==levels(data[,Y])[1] ),X], data[which(data[,Y]==levels(data[,Y])[2] ),X]), id=c(1:tab[1],1:tab[2]), 
                        Y=c(rep(levels(data[,Y])[1], tab[1]), rep(levels(data[,Y])[2], tab[2])))
      data1<-data1[order(data1$id),]
      for (i in 5:length(data[,X])) {
        bfm <- ttestBF(formula=X~Y,data=data1[1:i,], paired=FALSE, rscale=0.707)
        bfl <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1)
        bful <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1.41)
        bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
      }
      
      SBF<-data.frame("n"=rep(5:(length(data[,X])), each=3 ),"BF"= bfs, 
                      "rscale"=rep(c("moyen", "large", "ultra large"), length.out= 3*(length(data[,X])-4) ))
      names(SBF)<-c("n", "BF", "rscale")
      reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
      .plotSBF(SBF)
      
      ##### Début du graphique  Bayes Factor Robustness Check     
      
      # what is the t-value for the data?
      tVal <-  t.test(formula=modele, data=data, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
      # how many points in the prior should be explored?
      nPoints <- 1000
      # what Cauchy rates should be explored?
      cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
      # what effect sizes should be plotted?
      effSize <- seq(from = -2, to = 2, length.out = 1000)
      
      # get the Bayes factor for each prior value
      
      bayesFactors <- sapply(cauchyRates, function(x) 
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = x)[['bf']]))
      
      exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 0.707)[['bf']])->r1
      exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1)[['bf']])->r2
      exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1.41)[['bf']])->r3
      plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
      # do the Bayes factor plot
      plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
           ylim = c(0, max(bayesFactors)), xaxt = "n", 
           xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
      abline(h = 0, lwd = 1)
      abline(h = 6, col = "black", lty = 2, lwd = 2)
      axis(1, at = seq(0, 1.5, 0.25))
      # add the BF at the default Cauchy point
      points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
      points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
      points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
      # add legend
      legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
             pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
             col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
      
    }
    if(any(param=="non param")| any(param=="Test non paramétrique")) {
      WT<-wilcox.test(modele, paired=F,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
      if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
      r<-z/(length(data[,X]))^0.5
      Resultats$"test de Mann-Whitney - Wilcoxon"<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                                               "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
    }
    
    if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
      data[which(data[,Y]==levels(data[,Y])[1]),]->g1 # on cree une base de Données avec le groupe 1 uniquement (sans valeur aberrantes)
      data[which(data[,Y]==levels(data[,Y])[2]),]->g2 # on cree une base de Données avec le groupe 2 uniquement (sans valeur aberrantes)
      try(WRS::yuen(g1[,X],g2[,X]), silent=T)->yuen.modele### fournit la probabilite associee a des moyennes tronquees.Par defaut, la troncature est de 0.20
      if(class(yuen.modele)!="try-error"){
        round(unlist(yuen.modele),4)->yuen.modele
        cbind(yuen.modele[1:2], yuen.modele[3:4])->yuen.desc
        dimnames(yuen.desc)[[1]]<-levels(data[,Y])
        dimnames(yuen.desc)[[2]]<-c("n", "moyennes tronquees")
        yuen.desc->Resultats$Robustes$"statistiques descriptives"
        
        yuen.modele[c(5,6,8,9,10,11,12,7)]->yuen.modele
        names(yuen.modele)<-c("lim.inf.IC", "lim.sup.IC", 
                              "Difference","Err-type","Stat", "Seuil", "ddl","valeur.p")
        yuen.modele->Resultats$Robustes$"Analyse sur les moyennes tronquees"
        WRS::yuenbt(g1[,X],g2[,X], nboot=n.boot, side=T)->yuen.bt.modele ### fournit la probabilite associee a des moyennes tronquees apres un bootstrap.
        round(unlist(yuen.bt.modele)[1:4],4)->yuen.bt.modele
        names(yuen.bt.modele)<-c("lim.inf.IC", "lim.sup.IC", "Stat", "valeur.p")
        yuen.bt.modele->Resultats$Robustes$"Bootstrap utilisant la methode t sur les moyennes tronquees"
        WRS::pb2gen(g1[,X],g2[,X], nboot=n.boot)->pb2gen.modele### calcule le bootstrap sur le M-estimateur et fournit l intervalle de confiance. 
        round(unlist(pb2gen.modele)[1:6],4)->pb2gen.modele
        names(pb2gen.modele)<-c("M.estimaror.G1", "M.estimator.G2", "diff", "lim.inf.IC", "lim.sup.IC", "valeur.p")
        pb2gen.modele->Resultats$Robustes$"Percentile bootstrap sur les M-estimator"
        Resultats$Robustes$Informations<-c("la methode du percentile bootstrap doit etre preferee pour les petits echantillons",
                                           "Pour des echantillons plus importants, les boostrap utilisant la methode t doit etre preferee.")
        WRS::ks(g1[,X],g2[,X],w=F,sig=T)->KS
        round(unlist(KS),4)->KS
        names(KS)<-c("KS", "Seuil.critique","valeur.p")
        KS->Resultats$Robustes$"Test de Kolmogorov-Smirnov comparant deux distributions"
      }else Resultats$"Statistiques robustes"<-"Les statistiques robustes n'ont pas pu être réalisées. Vérifiez l'installation du package WRS"
      
      p<-ggplot(data, aes(y= data[,X[1]],x=data[,Y]))+labs(x=Y, y=X)+
        stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
      print(p)
      
    }
    
    return(Resultats)
  }
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  #### 5 fonctions qui seront appelées pour réaliser l'analyse
  options (warn=-1) 
  # chargement des packages
  packages<-c("BayesFactor", "svDialogs", "outliers", "nortest","psych", "lsr","ggplot2", "reshape2", "car", "plyr")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  try(library("WRS"),silent=T)
  .e <- environment()
  Resultats<-list()
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data 
  test.t.options<-test.t.in(X=X, Y=Y, data=data, choix=choix, param=param, outlier=outlier, sauvegarde=sauvegarde, info=info, group=group,alternative=alternative, 
                            formula=formula,n.boot=n.boot, rscale=rscale, mu=mu) 
  if(is.null(test.t.options)) return(analyse())
  
  choix<-test.t.options$choix
  X<-test.t.options$X
  Y<-test.t.options$Y
  mu<-test.t.options$mu
  group<-test.t.options$group
  data<-test.t.options$data
  alternative<-test.t.options$alternative
  group<-test.t.options$group
  param<-test.t.options$options$choix
  rscale<-test.t.options$options$rscale
  n.boot<-test.t.options$options$n.boot
  sauvegarde<-test.t.options$options$sauvegarde
  outlier<-test.t.options$options$desires
  
  for(i in 1 : length(X)) {
    
    
    if(choix=="Deux échantillons appariés"){
      diffs<-data[which(is.na(data[,X])), "IDeasy"]
      if(length(diffs)==0) data->data1 else data[which(data$IDeasy!=diffs), ]->data1 
    } else  {
      data1<-data[complete.cases(data[,c(Y,X[i])]),]
    }
    
    
    
    
    X1<-X[i]
    R1<-list()
    if(any(outlier==  "Données complètes")){
      switch(choix,  "Comparaison à une norme"=  R1$"Données complètes"<-norme(X=X1, mu=mu, data=data1, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
             "Deux échantillons appariés"=R1$"Données complètes"<-apparies(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
             "Deux échantillons indépendants"= R1$"Données complètes"<-indpdts(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
    }
    
    if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Données sans valeur influente")){
      if(choix=="Comparaison à une norme") data1$residu<-data1[,X1] else data1$residu<-unlist(tapply(data1[,X1], data1[,Y], scale, center=T, scale=F))
      critere<-ifelse(is.null(z), "Grubbs", "z")
      valeurs.influentes(X="residu", critere=critere,z=z, data=data1)->influentes
    }
    if(any(outlier== "Identification des valeurs influentes")){influentes->R1$"Valeurs influentes"}
    if(any(outlier== "Données sans valeur influente")) {
      if(length(influentes$"observations influentes")!=0 | all(outlier!="Données complètes")){
        
        if(choix=="Deux échantillons appariés"){
          setdiff(data$IDeasy,influentes$"observations influentes"$IDeasy)->diffs
          data[which(data$IDeasy%in%diffs), ]->nettoyees
        } else  get("nettoyees", envir=.GlobalEnv)->nettoyees
        
        ### Régler le souci pour les échantillons appariés
        switch(choix,  "Comparaison à une norme"=  R1$"Données sans valeur influente"<-norme(X=X1, mu=mu, data=nettoyees, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
               "Deux échantillons appariés"=R1$"Données sans valeur influente"<-apparies(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
               "Deux échantillons indépendants"= R1$"Données sans valeur influente"<-indpdts(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
      }
    }
    Resultats[[i]]<-R1
  }
  
  names(Resultats)<-paste("Analyse sur la variable", X)
  
  paste(unique(X), collapse="','", sep="")->X
  paste(outlier,  collapse="','", sep="")->outlier
  paste(param,  collapse="','", sep="")->param
  Resultats$Call<-paste0("test.t(X=c('", X,
                         "'), Y=", ifelse(!is.null(Y),paste0("'",Y,"'"), "NULL"), 
                         ",group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), 
                         ", choix='", choix, 
                         "', sauvegarde = ", sauvegarde, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                         ", data=", test.t.options$nom, ",alternative='", alternative, "', mu=", ifelse(!is.null(mu),mu, "NULL"),
                          ",formula =NULL, n.boot=", n.boot, ",param=c('", param, "'),info=T, rscale=", rscale, ")"
  )
  .add.history(data=data, command=Resultats$Call, nom=test.t.options$nom)
  .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
  
  if(sauvegarde){save(Resultats=Resultats ,choix =choix, env=.e)}
  
  ref1(packages)->Resultats$Références
  ### Obtenir les Resultats
  return(Resultats) 
  
  }





#### corrélations tétrachoriques et polychoriques (+ mixtes)
# devrait bugger ... Ã  vérifier
tetrapoly<-function(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator="two.step", output="cor", imp=NULL){
  # data : dataframe
  # X : vector of variables names 
  # sauvegarde : bolean. Should analysis be saved ? 
  # ord : Character. names of variables considered as ordinal. The other are considered as continuous.
  # info : bolean. Should information be printed in the console during analysis ? 
  # group : character. Name of the factor variable 
  # estimator : see ?lavCor for information 
  # output : see ?lavCor for information
  options (warn=-1) 
  c("lavaan", "svDialogs")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  .e<- environment()
  Resultats<-list()

  if(is.null(data) | is.null(X))  {dial<-TRUE
  if(info) writeLines("Veuillez choisir le type de corrélations que vous désirez réaliser. Pour les variables dichotomiques, les corrélations seront des corrélations tétrachoriques")
  dlgList(c("corrélations polychoriques", "corrélations mixtes"), preselect=NULL, multiple = FALSE, title="Type de corrélations ?")$res->method
  if(length(method)==0) return(choix.corr())
  } else dial<-F
  
  
  if(dial || class(data)!="data.frame"){
    data<-choix.data(data=data, info=info, nom=T)
    if(length(data)==0) return(choix.corr())
    nom<-data[[1]]
    data<-data[[2]]  
  }else{
    deparse(substitute(data))->nom  
  }
  
  
  msg3<-"Veuillez choisir les variables dont il faut réaliser les corrélations polychorique/tétrachorique/mixte."
  X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variable-s ", out=NULL)
  if(is.null(X)) {
    Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
    return(Resultats)}
  data<-X$data
  X<-X$X
  
  if(!is.null(ord) & any(ord %in%X==F)||(dial && method=="corrélations mixtes" ) ){
    if(info) writeLines("Veuillez choisir les variables ordinales.")
    ord<-dlgList(X, preselect=X, multiple = TRUE, title="Variables ordinales ?")$res
    if(length(ord)==0){
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
  } else ord<-X
  if(any(is.na(data[,X]))) {
  if(is.null(imp))  {msgBox("Des valeurs manquantes ont été détectées. Comment voulez-vous les traiter ? Garder l'ensemble des observations peut biaiser les résultats.")
    imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes","Remplacer par la médiane","Multiple imputation - Amelia"), 
                  preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes ?")$res}
    if(length(imp)==0){
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
    data1<-ez.imp(data[, X], imp=imp, ord= ord)
    data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),group])
  }  
  if(dial || !is.logical(sauvegarde)){
    sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Voulez-vous sauver les résultats ?")$res
    if(length(sauvegarde)==0) {
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
  }
  Resultats$"Matrice de corrélation tétrachorique/polychorique ou mixte"<-lavCor(data[,c(X,group)], ordered=ord,estimator=estimator, group=group,  missing="default", output=output)
  paste(X, collapse="','", sep="")->X
  if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
  Resultats$Call<-paste0("tetrapoly(data=", nom,",X=c('", X,"'),sauvegarde=", sauvegarde, ",ord=", ifelse(!is.null(ord),paste0("c('",ord,"')"), "NULL"),
                         ",info=T, group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), ",estimator='", estimator, "',output='", output, "')")
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))  
  
  
  if(sauvegarde) save(Resultats=Resultats, choix="cor.polychorique", env=.e)
  ref1(packages)->Resultats$Références
  return(Resultats) }


#### permet d'identifier et enlever les valeurs influentes ####
# pas encore intÃÂ©grÃÂ© ÃÂ  l'interface graphique mais dans les fonctions. Il faut rajouter l'interace graphique pour la faire fonctionnerdirectement de easier
valeurs.influentes<-function(X, critere="Grubbs", z=3.26, data=NULL){options (warn=-1)
  c("outliers")->packages
  .inf <- environment()
  list()->Resultats.valeurs.influentes
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  if(class(data[,X])=="integer") as.numeric(data[,X])->data[,X]
  if(class(data[,X])!="numeric") return("la variable n est pas numerique")
  if(critere=="z" && class(z)!="numeric") return("z doit etre un nombre")
  if(any(match(c("Grubbs","z"), critere))==FALSE) return("Les valeurs admises pour critere sont  z  et  Grubbs ")
  length(data[,1])->i
  if(critere=="Grubbs"){
    grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)->outliers # test de Grubbs permettant de savoir s il y a des valeurs aberrantes
    names(data[X])->outliers$data.name
    # on realise un boucle du type: tant y est inferieure a 0.05, continue. 
    data.frame()->valeur.influentes
    while(grubbs.test(data[,X], type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  { 
      which.max(abs(data[,X]))->max #cherche la valeur maximale qu on stocke dans l objet max                                                                                                
      rbind(valeur.influentes,data[max, ])->valeur.influentes
      data<-data[ -max, ] # supprime la valeur maximmal de data
    }  
    data.frame(G=outliers$statistic[1], U=outliers$statistic[2], valeur.p=round(outliers$p.value,4))->Resultats.valeurs.influentes$"Test de Grubbs"
  Resultats.valeurs.influentes$"Valeur la plus élevée"<-outliers$alternative

    }
  
  
  if(critere=="z"){
    # on centre reduit les residus et on stocke la valeur absolue du z dans la variable "Var_centree_abs" dans l objet data2
    abs(scale(data[,X], center = TRUE, scale = TRUE))->data$Var_centree_abs 
    valeur.influentes<-data[which(data$Var_centree_abs>z),]
    data<-data[which(data$Var_centree_abs<=z),]  
  }
  length(data[,1])->iso
  i-iso->n # nombre d observations supprimees
  round((n/i)*100,2)-> pourcentage_N # proportions d observations supprimees (nombre / taille de l echantillon)
  rbind(n, paste(pourcentage_N, "%"))->synthese_aberrant # on combine le nombre et le pourcentage. 
  data.frame(information=c("Nombre d'observations retirées", "% d'observations considérées comme influentes"), Synthese=synthese_aberrant)->synthese_aberrant # on cree un data.frame 
  if(all(dim( valeur.influentes)!=0))    Resultats.valeurs.influentes$"observations influentes"<-valeur.influentes
  Resultats.valeurs.influentes$"Synthèse des observations influentes" <-synthese_aberrant
  data->>nettoyees
  return(Resultats.valeurs.influentes) 
}


VI.multiples<-function(data){ require("pych") 
  Resultats<-list()
  nvar<-length(data)
  try(psych::outlier(data, bad=T, na.rm=T,plot=T),silent=T)->essai
  if(class(essai)=="try-error"){
    msgBox("Votre matrice est singulière, ce qui pose souci. Nous tentons de  de résoudre le souci. Si possible, la distance de Mahalanobis sera alors calculée sur le maximum d'information tout en évitant la singularité.")
    data->data2
    rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
    which(rankifremoved == max(rankifremoved))->rangs
    if(length(rangs)==length(data2)){ 
      sample(rangs,1)->rang2
      data2[,-rang2]->data2
    } else {
      while(length(rangs)!=length(data2)){
        sample(rangs,1)->rang2
        data2[,-rang2]->data2
        rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
        which(rankifremoved == max(rankifremoved))->rangs
      }
    }
    try(psych::outlier(data2), silent=T)->essai
    if(class(essai)=="try-error") {
      corr.test(data2)$r->matrice
      if(any(abs(matrice)==1)) {
        msgBox("vous tenter de faire une matrice de corrélations avec des variables parfaitement corrélées. Cela pose souci pour le calcul de la distance de Mahalanobis. Nous tentons de résoudre le souci")
        which(abs(matrice)==1, arr.ind=TRUE)->un
        un<-un[-which(un[,1]==un[,2]),]
        data2[,-un[,2]]->data2
        try(psych::outlier(data2), silent=T)->essai
        if(class(essai)=="try-error") {
          writeLines("Désolé, nous ne pouvons pas calculer la distance de Mahalanobis sur vos données. Les analyses seront résalisées sur les données complètes")
          0->data$D.Mahalanobis  }
      }else{essai-> data$D.Mahalanobis}
    } else{ essai-> data$D.Mahalanobis
    }
  }else{
    essai-> data$D.Mahalanobis  
  }
  
  qchisq(p=0.001, df=nvar, ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
  data[which(data$D.Mahalanobis>seuil),]->outliers
  length(outliers[,1])/length(data[,1])*100->pourcent
  
  msgBox(paste(round(pourcent,2), "% des observations sont considérées comme outliers."))
  
  
  if(pourcent!=0){
    writeLines("Supprimer l'ensemble des outliers supprime l'ensemble des valeurs au-delà p(chi.deux)< 0.001.   
               Supprimer une observation à la fois permet de faire une analyse détaillée de chaque observation  
               considerée comme influente en partant de la valeur la plus extrême. La procédure s'arrête  
               quand plus aucune observation n'est considérée comme influente")  
    
    suppr<- dlgList(c("Suppression de l'ensemble des outliers", "Suppression manuelle"), 
                    preselect=c("Suppression de l'ensemble des outliers"), multiple = FALSE, title="Comment voulez-vous les supprimer?")$res
    if(length(suppr)==0) return(NULL)
    if(suppr=="Suppression de l'ensemble des outliers") {data[which(data$D.Mahalanobis<seuil),]->data 
      outliers->Resultats$"Valeurs considérées comme influentes"}else{
        suppression<-"yes"
        outliers<-data.frame()
        while(suppression=="yes"){
          print(data[which.max(data$D.Mahalanobis),])
          cat ("Appuyez [entrée] pour continuer")
          line <- readline()
          dlgMessage("Voulez-vous supprimer cette observation ?", "yesno")$res->suppression
          if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
            data[-which.max(data$D.Mahalanobis),]->data
            
          }
        }
        Resultats$"Valeurs considérées comme influentes"<-outliers
      }
  }
  Resultats$data<-data
  return(Resultats)
}




#############################################
####                                     ####
####     Fonctions non commentées        ####
####                                     ####
#############################################

### permet de choisir les données sur lesquelles on veut faire l'analyse
choix.data<-function(data=NULL, info=TRUE, nom=FALSE) {
  # data : character corresponding to the object name representing data. 
  # info : donne une explication sur les arguments
  # nom : logique. Spécifie si le nom de la base de données doit être importé en même temps. Dans ce cas, l'objet renvoyé est une liste
  list()->Resultats
  Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
  if(length(nom1)==0) {
    writeLines("il n'y a pas de données dans la mémoire de R, veuillez importer les donnnées sur lesquelles réaliser l'analyse")
    import()
    choix.data(data=NULL,info=T, nom=nom)->Resultats
    return(Resultats)}
  if(!is.null(data) && data%in% nom1) data->nom1
  if(length(nom1)==1)  data<-get(nom1) else{
    if(info=="TRUE") writeLines("Veuillez choisir la base de données")
    nom1 <- dlgList(nom1, multiple = FALSE, title="Données ?")$res
    if(length(nom1)==0) {nom1<-NULL
    data<-NULL}
    if(!is.null(nom1))  data<-get(nom1)
  }
  if(nom==TRUE){
    nom1->Resultats[[1]]
    data->Resultats[[2]]}else data->Resultats
  return(Resultats)
}


### permet de faire une liste de contrastes pour l'ensemble des variables d'un modèle
contrastes.ez<-function(longdata, inter=NULL, intra=NULL){
  Resultats<-list()
  writeLines("Les contrastes a priori correspondent aux contrastes sans correction de la probabilité en suivant les règles de contrastes.
             Les contrastes 2 à 2 permettent de faire toutes les comparaisons 2 à 2 en appliquant ou non une correction à la probabilité")
  type.cont<- dlgList(c("a priori",  "Comparaison 2 à 2", "aucun"), preselect="a priori",multiple = FALSE, title="Quel types de contraste voulez-vous ?")$res
  if(length(type.cont)==0) return(NULL)
  Resultats$type.cont<-type.cont
  c(inter, unlist(intra))->interintra
  if(type.cont=="a priori") {
    contrastes<-list()
    writeLines("Vous pouvez choisir les contrastes que vous souhaitez. Néanmoins les règles concernant l'application des contrastes doivent être respectées.
               Les contrastes peuvent etre specifiés manuellement. Dans ce cas, veuillez choisir spécifier les contrastes")
    cont.exemple<-list()
    contr.helmert(3)->cont.exemple$Orthogonaux
    apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inversés
    contr.poly(3)->cont.exemple$Polynomiaux
    contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
    print(cont.exemple)
    
    for (i in 1:length(interintra)){
      if(i>1) {
        type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", "spécifier les contrastes"), 
                             preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res} else {
                               type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", 
                                                      "spécifier les contrastes"),preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res                      
                             }
      if(length(type.cont2)==0) return(contrastes.ez()) 
      if(type.cont2=="orthogonaux") contr.helmert(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
      if(type.cont2=="orthogonaux inversés") apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrastes[[i]]
      if(type.cont2=="polynomiaux")  contr.poly(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
      if(type.cont2=="comparaison à une ligne de base") { 
        base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                       multiple = FALSE, title="Quelle est la ligne de base?")$res
        which(levels(longdata[, interintra[i]])==base)->base
        contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
      } 
      if(type.cont2=="spécifier les contrastes"){
        ortho<-FALSE
        while(ortho!=TRUE){
          matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrastes3
          dimnames(contrastes3)[[1]]<-levels(longdata[,interintra[i]])
          dimnames(contrastes3)[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
          fix(contrastes3)->contrastes3
          if(any(colSums(contrastes3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrastes3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
            test.out<-rep(1, length(contrastes3[,1]))
            for(j in 1:length(contrastes3[1,])) {contrastes3[,j]*test.out->test.out}
            if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
          if(ortho==FALSE) {dlgMessage("Les contrastes doivent respecter l orthogonalité. Voulez-vous continuer ?", "yesno")$res->cont
            if(cont=="no") return(contrastes.ez(longdata=longdata, inter=inter, intra=intra ))  }
          contrastes[[i]]<-contrastes3
          
        }
        
      }
      
      dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
    }
    names(contrastes)<-interintra
    Resultats$contrastes<-contrastes
    
  }
  if(type.cont== "Comparaison 2 à 2"){
    list()->p.adjust
    writeLines("Quelle correction de la probabilité voulez-vous appliquer ? Pour ne pas appliquer de correction, choisir +none+")
    dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
    if(length(p.adjust)==0) return(contrastes.ez())
    Resultats$p.adjust<-p.adjust
  }
  return(Resultats)
}

## permet d'ajouter les références des packages chargés pour une fonction
ref1<-function(packages){
  require("bibtex")
  c("base", packages, "bibtex")->packages
  write.bib(packages, file='references')
  bibtex::read.bib('references.bib')->Resultats
  file.remove('references.bib')
  return(Resultats)
}

#### Fonction de sauvegarde. fonction qui n'apparaÃÂ®t pas directement dans easieR mais est inclut dans le reste des fonctions

save<-function(Resultats, choix, env=.GlobalEnv){options (warn=-1)
  # Resultats = object that must be saved
  # choix = name of the file 
  # env = environment in which to find the object
  require(rtf)
  gsub(":",".",date())->date
  output<-paste(choix,date, ".doc")
  rtf<-RTF(output,width=30,height=20,font.size=12,omi=c(1,1,1,1))
  
  to.rtf<-function(Resultats, X=1){
    for(i in 1:length(Resultats)){
      names(Resultats)[[i]]->titres
      addHeader(rtf,title=titres, font.size=(22-2*X), TOC.level=i)
      if(any(class(Resultats[[i]])=="chr")|any(class(Resultats[[i]])=="character")) {addText.RTF(rtf, Resultats[[i]])
        addNewLine(rtf, n=2)
      }
      if(any(class(Resultats[[i]])=="matrix")) {
        data.frame(Resultats[[i]]) ->essai
        round(essai,4)->essai
        addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
        addNewLine(rtf, n=2)
      }
      
      if(any(class(Resultats[[i]])=='bibentry')){"voir console"->console
        addText.RTF(rtf, console)
        addNewLine(rtf, n=2)
      }
      
      
      
      if(any(class(Resultats[[i]])=="data.frame") && length(Resultats[[i]])!=0) {
        if(any(sapply(Resultats[[i]], class)=="numeric")) Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"]<-lapply(Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"],round,4)
        addTable(rtf,Resultats[[i]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]]))))
        addNewLine(rtf, n=2)
      }
      if(any(class(Resultats[[i]])=="matrix") && any(class(Resultats[[i]])=="table")) class(Resultats[[i]])<-"matrix"
      
      if(any(class(Resultats[[i]])=="table")) {
        matrix(Resultats[[i]], ncol=ncol(Resultats[[i]]))->essai
        data.frame(essai)->essai
        dimnames(Resultats[[i]])[[2]]->names(essai)
        unlist(lapply(names(test[[1]]), function(x) max(nchar(x))))->largeur
        addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=largeur/10 )
        addNewLine(rtf, n=2)
      }
      if(any(class(Resultats[[i]])=="numeric") ) {
        if(length(Resultats[[i]])==1) {
          addText.RTF(rtf, Resultats[[i]])
          addNewLine(rtf, n=2)}else{
            
            round(matrix(Resultats[[i]],nrow=1),4)->essai
            dimnames(essai)[[2]]<-names(Resultats[[i]])
            dimnames(essai)[[1]]<-list()
            addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]])) ))
            addNewLine(rtf, n=2)
          }
      }
      
      if(any(class(Resultats[[i]])=="list") ){
        Resultats[[i]]->Y
        to.rtf(Y, X=X+1)
      }
    }
  }
  to.rtf(Resultats, 1)
  done(rtf) 
  
  data<-get("data", env=env)
  data->Resultats$donnees
  date()->date
  gsub(":",".",date)->date
  dput(Resultats, file=paste(choix, date,".txt"))
  Resultats[[length(Resultats)]]<-NULL
  Resultats$SAUVEGARDE<-paste("les donnees sont sauvegardees dans", getwd())
  
}


################################
####                        ####
####       Graphique        ####
####                        ####
################################


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} 

.plotSBF<-function(SBF){
  min.y<-min(log(SBF$BF))
  max.y<-max(log(SBF$BF))
  etend.y<-max.y-min.y
  y_breaks<-c(min.y, min.y+1/4*etend.y ,min.y+1/2*etend.y ,min.y+3/4*etend.y , max.y )
  y_labs<-as.character(round(exp(y_breaks),2))
  reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
  p1 <- ggplot(SBF, aes(x = as.factor(n), y = log(BF), group=rscale)) + ylab("Facteur bayesiens 10") + 
    xlab("n")+ geom_line(aes(linetype=rscale))+ geom_point()
  p1<-p1+theme(plot.title = element_text(size = 12))+ggtitle("Facteurs bayesiens sequentiels - Analyse de robustesse")
  p1<-p1+scale_y_continuous(breaks = y_breaks, labels =y_labs )
  print(p1) 
}

# fonction corrigÃÂ©e du package mbess permettant de faire des barres d'effets sur les mÃÂ©diations. Ne pas modifier. 
mediation.effect.bar.plot2<-function (x, mediator, dv, main = "Mediation Effect Bar Plot", 
                                      width = 1, left.text.adj = 0, right.text.adj = 0, rounding = 3, 
                                      file = "", save.pdf = FALSE, save.eps = FALSE, save.jpg = FALSE, 
                                      ...) 
{
  Mediation.Results <- mediation(x = x, mediator = mediator, 
                                 dv = dv, conf.level = 0.95,complete.set=TRUE)
  observed.c <- Mediation.Results$Y.on.X$Regression.Table[2, 
                                                          1]
  observed.c.prime <- Mediation.Results$Y.on.X.and.M$Regression.Table[2, 
                                                                      1]
  max.possible.c <- sqrt(var(dv))/sqrt(var(x))
  if (observed.c < 0) 
    max.possible.c <- -max.possible.c
  if (width < 1) {
    width <- 0.5 * (1 - width)
  }
  if (width > 1) {
    width <- 0.5 * (1 + width)
  }
  if (save.pdf == TRUE) {
    if (save.eps == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both PDF and EPS specified).")
    if (save.jpg == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both PDF and JPG specified).")
  }
  if (save.eps == TRUE) {
    if (save.jpg == TRUE) 
      stop("Only one file format for saving figure may be used at a time (you have both EPS and JPG specified).")
  }
  if (save.pdf == TRUE | save.eps == TRUE | save.jpg == TRUE) {
    no.file.name <- FALSE
    if (file == "") {
      file <- "mediation.effect.bar.plot"
      no.file.name <- TRUE
    }
  }
  if (save.pdf == TRUE) 
    pdf(file = paste(file, ".pdf", sep = ""), ...)
  if (save.eps == TRUE) 
    jpeg(filename = paste(file, ".eps", sep = ""), ...)
  if (save.jpg == TRUE) 
    jpeg(filename = paste(file, ".jpg", sep = ""), ...)
  plot(c(-2, 2), seq(0, 1), ylab = "", xlab = "", xaxt = "n", 
       yaxt = "n", bty = "n", type = "n", main = main, ...)
  segments(x0 = -0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = 0, x1 = 0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 0)
  segments(x0 = 0.5 * width, y0 = 1, x1 = -0.5 * width, y1 = 1)
  segments(x0 = 0.5 * width, y0 = observed.c/max.possible.c, 
           x1 = -0.5 * width, y1 = observed.c/max.possible.c)
  segments(x0 = 0.5 * width, y0 = observed.c.prime/max.possible.c, 
           x1 = -0.5 * width, y1 = observed.c.prime/max.possible.c)
  rect(xleft = -0.5 * width, ybottom = 0, xright = 0.5 * width, 
       ytop = observed.c.prime/max.possible.c, density = 10, 
       angle = 45, border = NA)
  rect(xleft = -0.5 * width, ybottom = observed.c.prime/max.possible.c, 
       xright = 0.5 * width, ytop = observed.c/max.possible.c, 
       density = 10, angle = 135, border = NA)
  if (left.text.adj == 0) {
    left.text.adj <- -0.5 * width - (0.5 * width/3)
  }
  if (left.text.adj != 0) {
    left.text.adj <- -0.5 * width - (0.5 * width/3) + left.text.adj
  }
  if (right.text.adj == 0) {
    right.text.adj <- 0.5 * width + (0.5 * width/20)
  }
  if (right.text.adj != 0) {
    right.text.adj <- 0.5 * width + (0.5 * width/20) + right.text.adj
  }
  use.this <- round(max.possible.c, rounding)
  text(x = right.text.adj * 1.3, y = 1, bquote(paste(plain("max possible"), 
                                                     phantom(x), italic(c) == .(use.this))))
  use.this <- round(observed.c, rounding)
  text(x = left.text.adj, y = observed.c/max.possible.c, bquote(paste(plain(observed), 
                                                                      phantom(x), italic(c) == .(use.this))))
  use.this <- round(observed.c.prime, rounding)
  text(x = left.text.adj, y = observed.c.prime/max.possible.c, 
       bquote(paste(plain(observed), phantom(x), italic(c), 
                    phantom(x), plain(prime) == .(use.this))))
  use.this <- round(observed.c - observed.c.prime, rounding)
  text(x = right.text.adj, y = observed.c/max.possible.c - 
         observed.c.prime/max.possible.c, bquote(italic(ab) == 
                                                   .(use.this)))
  segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
           x1 = right.text.adj * 0.6, y1 = observed.c.prime/max.possible.c)
  segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
           x1 = right.text.adj * 0.55, y1 = observed.c/max.possible.c)
  segments(x0 = right.text.adj * 0.6, y0 = observed.c.prime/max.possible.c, 
           x1 = right.text.adj * 0.55, y1 = observed.c.prime/max.possible.c)
  text(x = right.text.adj * 0.8, y = 0, "zero")
  if (save.pdf == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.pdf' file saved at the directory", 
                  getwd()))
  }
  if (save.eps == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.eps' file saved at the directory", 
                  getwd()))
  }
  if (save.jpg == TRUE) {
    dev.off()
    if (no.file.name == TRUE) 
      print(paste("'mediation.effect.bar.plot.jpg' file saved at the directory", 
                  getwd()))
  }
}


########################################
####                                ####
####     Manipulation dataframe     ####
####                                ####
########################################


########################################
####                                ####
####         En construction        ####
####                                ####
########################################


ez.imp<-function(data=NULL, imp="median", ord=NULL, id=NULL, noms=NULL, info=T){
  # data : data.frame 
  # imp : one among "rm", "mean", "median", "amelia"
  # ord : if imp is amelia, names of ordinal variables
  # id : if imp is amelia, names of id variables
  # noms : if imp is amelia, names of nominal variables
  
  packages<-c("Amelia",  "svDialogs")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  if(is.null(data)) { dial<-T
  data<-choix.data(data=data, info=info, nom=T)
  if(length(data)==0) return(NULL)
  nom<-data[[1]]
  data<-data[[2]]
  } else {dial<-F
  deparse(substitute(data))->nom  }
  nom<-paste0(nom,".complet")
  
  if(dial || imp%in% c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes", "Remplacer par la moyenne",
                       "Remplacer par la médiane","Multiple imputation - Amelia","rien","rm", "mean","median", "amelia") == FALSE){
    writeLines("Nombre de valeurs manquantes par variable. Comment voulez-vous les traiter ?")
    print(sapply(data, function(x) sum(length(which(is.na(x))))) )
    
    imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes", "Remplacer par la moyenne",
                    "Remplacer par la médiane","Multiple imputation - Amelia"), preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes")$res
    if(length(imp)==0){
      return(NULL)
    }
  }
  if(length(imp)==0) return(NULL)
  if(imp == "Ne rien faire - Garder l'ensemble des observations" || imp=="rien") return(data)
  if(imp== "Suppression des observations avec valeurs manquantes"|| imp=="rm"){
    data<-data[complete.cases(data),]
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp=="Remplacer par la moyenne"|| imp=="mean"){
    for(i in 1 : length(data)) {data[which(is.na(data[,i])),i]<-mean(data[,i], na.rm=T)}
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp== "Remplacer par la médiane"|| imp=="median"){
    for(i in 1 : length(data)) {data[which(is.na(data[,i])),i]<-median(data[,i], na.rm=T)}
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp== "Multiple imputation - Amelia"|| imp=="amelia"){
    amelia(x=data, m = 1, p2s = 0,frontend = FALSE, idvars = id,
           ts = NULL, cs = NULL, polytime = NULL, splinetime = NULL, intercs = FALSE,
           lags = NULL, leads = NULL, startvals = 0, tolerance = 0.0001,
           logs = NULL, sqrts = NULL, lgstc = NULL, noms = noms, ords = ord,
           incheck = TRUE, collect = FALSE, arglist = NULL, empri = NULL,
           priors = NULL, autopri = 0.05, emburn = c(0,0), bounds = NULL,
           max.resample = 100, overimp = which(is.na(data), arr.ind = T), boot.type = "ordinary",
           parallel = c("no", "multicore", "snow"),
           ncpus = getOption("amelia.ncpus", 1L), cl = NULL)->data.am
    data.am$imputations$imp1->data
    
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  return(data)
}

ez.rank<-function(data=NULL, X=NULL, ties.method="average", info=T){
  options (warn=-1)
  c("svDialogs")->packages
  lapply(packages, require, character.only=T)
  list()->Resultats
  .e <- environment()
  if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data
  choix.data(data=data, info=TRUE, nom=T)->data 
  if(length(data)==0) { return(preprocess())} else {
    data[[1]]->nom1
    data[[2]]->data}
  if(!is.null(X)) dial<-FALSE
  msg.pre1<-"Veuillez préciser les variables dont vous souhaiter faire les rangs"
  .var.type(X=X, info=T, data=data, type="numeric", message=msg.pre1,multiple=T, title="Variable-s")->X1
  if(is.null(X1)) return(preprocess())
  if(!is.null(X) && X1$X!=X) dial<-TRUE 
  X1$X->X
  if(dial){
    if(info) writeLines("Comment voulez-vous traiter les ex-aequo ? La méthode *average* fait la moyenne entre les ex aequo (le plus habituel),
                        *first* attribue le premier rang ex aequo à la première valeur dans les données, *laste* à la dernière, *min* attribue la
                        valeur minimale à l'ensemble des ex aequo et *max* la valeur maximale.")
    ties.method<-dlgList(c("average", "first", "last", "random", "max", "min"), multiple = F, preselect="average", title="Spécifier effectifs ?")$res
  }
  sapply(data[,X], rank, ties.method=ties.method, na.last="keep")->rangs
  if(length(X)==1) data.frame(rangs)->rangs
  dimnames(rangs)[[2]]<-paste0(X, ".rangs")
  data.frame(data, rangs)->data
  assign(nom1,data,envir=.GlobalEnv)
  paste(X, collapse="','", sep="")->X
  Resultats$call<-paste0("ez.rank(data=", nom1, ", X=c('",X, "'), ties.method='",ties.method, "', info=T)")  
  .add.history(data=data, command=Resultats$Call, nom=nom1)
  ref1(packages)->Resultats$References
  return(Resultats)
}




.var.type<-function(X=NULL, info=T, data=NULL, type=NULL, check.prod=T, message=NULL, multiple=F, title="Variable", out=NULL){
  # permet de sélectionner des variables
  # vérifie les conditions pour les variables qui doivent respecter certaines conditions 
  # data : data.frame name which allow to check whether the variable is the data.frame
  # X : character. Name of the variable X (or vector allow to determine whether the selected variable belongs to data.frame)
  # info : logical. Should information be printed in the console ? 
  # liste
  # out : character or vector of names for variables of the data.frame which cannot be choosen (e.g. has already be choosen earlier). 
  # type : character. Class of variables which can be selected. One or several among "factor", "integer", "numeric". (see details). NULL means that all types are allowed
  # check.prod : logical. Should the product of the levels of factor variables be inferior to the number of rows?
  # message : message which should be printed if info is true
  # multiple : logical. Does the selection of several variables be allowed ? 
  # title : character. Title of the dialog box
  
  setdiff(names(data), out)->diff
  listes<-data.frame(paste(diff, "(format :", sapply(data[diff], class), ")", sep=" "), diff)
  
  if(is.null(X) | any(X %in% diff==F)) {
    if(info==T) writeLines(message)
    if(length(diff)>1){
      X<-dlgList(paste(diff, "(format :", sapply(data[,diff], class), ")", sep=" "), multiple = multiple, 
                 title=title)$res 
    } else {X<-dlgList(paste(diff, "(format :", class(data[,diff]), ")", sep=" "), multiple = multiple, 
                       title=title)$res}
    
    if(length(X)==0) return(NULL)
    subset(listes, listes[,1] %in% X)[,2]->X 
    as.character(X)->X}
  
  if(!is.null(type) && type=="factor"){
    if(all(sapply(data[,X], class)%in% c("factor", "character"))!=T ) {
      res<-okCancelBox("Vous devez utiliser des variables catégorielles. Voulez-vous transformer les variables numériques en variables catégorielles ?")
      if(res==F) {X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)}
    }
    if(length(X)==1) factor(data[,X])->data[,X] else lapply(data[, X], factor)->data[, X] 
    if((length(X)==1 && nlevels(data[,X])<2) | (length(X)>1 && any(sapply(data[, X], nlevels)<2))) {
      okCancelBox("Une variable catégorielle doit avoir au moins 2 modalités différentes. Veuillez choisir une variable avec au moins deux modalités")  
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
      return(Resultats)
    }
    if(check.prod){
      if(length(X)>1 && sapply(data[,X],nlevels)>length(data[,1])) {
        msgBox("Le produit des modalités des variables définissant les groupes est supérieur au nombre de vos observations. Il faut au moins une observation par combinaison de modalités de vos variables. Veuillez redéfinir votre analyse") 
        .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
        return(Resultats)
      }
      
    }
    
    
  }
  if(!is.null(type) && type=="integer"){
    if((any(data[,X]%%1==0) %in% c(FALSE, NA)) || min(data[,X])<0) {
      okCancelBox("la variable doit être un entier *integer* positif")
      X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)
    }
  }
  if(!is.null(type) && type=="numeric"){
    if(length(X)==1) moy<-is.na(mean(data[,X],na.rm=T)) else moy<-any(is.na(sapply(data[,X], mean, na.rm=T)))
    if(moy || var(data[,X],na.rm=T)==0){
    okCancelBox("la variable doit être numérique et avoir une variance non nulle.")
    X<-NULL
    .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
    return(Resultats)
  }
  }
  Resultats<-list()
  Resultats$X<-X
  Resultats$data<-data 
  
  
  return(Resultats)
}

# save : logical. Should the output be saved in rtf and R file ? 
.ez.options<-function(options="choix", n.boot=NULL,param=T, non.param=T, robust=T, Bayes=T, msg.options1=NULL, msg.options2=NULL, info=T, dial=T, 
                      choix=NULL,sauvegarde=F, outlier=NULL, rscale=NULL){
  # options : character or vector. List of options that must be used ("choix", "outlier") 
  # n.boot : Positive integer. Number of bootstrap that must be performed. 1 for no bootstrap
  # param : Logical. Is the parametric analysis  an option ? 
  # non.param : Logical. Is the non.parametric analysis  an option ? 
  # robust : Logical. Are robuste statistics  an option ? 
  # Bayes : Logical. are Bayes factors  an option ? 
  # msg.options1 : message that must be printed for the parametric analysis if info is true
  # msg.options2 : message that must be printed for the non-parametric analysis if info is true
  # info : logical. Must information be printed in the console ? 
  # dial = logical. Should dialog box be used ? 
  # choix = character or list of analyses that must be done c("parametric", "non parametric", "robust" or/and "bayesian")
  # sauvegarde = Logical. Must the results be saved ? 
  Resultats<-list()
  if(any(options=="choix") & dial==T){
    choix<-c()
    if(param==T){
      if(info) writeLines(msg.options1)
      choix<-c(choix, "Test paramétrique")
    } 
    if(non.param==T) {
      if(info) writeLines(msg.options2)
      choix<-c(choix, "Test non paramétrique")
    }
    if(robust==T) {
      if(info) writeLines("Les statistiques robustes sont des analyses alternatives à l'analyse principale, impliquant le plus souvent des bootstraps. Ces analyses sont souvent plus lentes")
      choix<-c(choix, "Test robustes - impliquant des bootstraps")
    }
    if(Bayes==T) {
      if(info) writeLines("Facteurs bayesiens : calcule l'équivalent du test d'hypothèse nulle en adoptant une approche bayesienne.")
      choix<-c(choix, "Facteurs bayesiens")
    }
    
    choix<- dlgList(choix, preselect=choix, multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
    if(length(choix)==0) return(NULL)
  } 
  Resultats$choix<-choix 
  
  
  if(exists("choix") && any(choix== "Test robustes - impliquant des bootstraps") || !is.null(n.boot)){{
    if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
      msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
      n.boot<-NULL
    }
    while(is.null(n.boot)){
      writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
      
      n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
      if(length(n.boot)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                             Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                             choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
        return(Resultats)}
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->n.boot
        if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
          msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
          n.boot<-NULL
        }
      }
    }
    Resultats$n.boot<-n.boot
  }
  if(!is.null(rscale)){
      if(dial & any(choix=="Facteurs bayesiens")|| (is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("moyen", "large", "ultralarge")==F)) {
    if(info) writeLines("Veuillez préciser la distribution a priori de Cauchy")
    rscale<-dlgList(c("moyen", "large", "ultralarge"), preselect="moyen", multiple = F, title="Quelle distribution voulez-vous  ?")$res 
    if(length(rscale)==0) {
      .ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                  Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                  choix=choix,sauvegarde=F, outlier=NULL, rscale=rscale)->Resultats
    }
      }
    if(is.character(rscale)) {
      ifelse(rscale=="moyen", rscale<-2^0.5/2, ifelse(rscale=="large", rscale<-1, ifelse(rscale=="ultralarge", rscale<-2^0.5, rscale<-rscale)))
      Resultats$rscalei<-T
    } else Resultats$rscalei<-F
    
    Resultats$rscale<-rscale
  }

  
  if(any(options=="outlier")){
    if(dial || is.null(outlier)|| (dial==F & any(outlier %in%c("Données complètes", "Identification des valeurs influentes","Données sans valeur influente"))==F)) {
      if(info==TRUE) writeLines("les données complètes représentent l'analyse classique sur toutes les données utilisables, l'identification des valeurs influentes
permet d'identifier les observations qui sont considérees statistiquement comme influençant les résultats.
les analyses sur les données sans les valeurs influentes réalise l'analyse après suppression des valeurs influentes. 
Cette option stocke dans la mémoire de R une nouvelle base de données sans valeur influente dans un objet portant le nom *nettoyees*")
     Resultats$desires<- dlgList(c("Données complètes", "Identification des valeurs influentes","Données sans valeur influente"), 
                      preselect=c("Données complètes","Identification des valeurs influentes", "Données sans valeur influente"),
                      multiple = TRUE, title="Quelles analyse voulez-vous ?")$res
     if(length(Resultats$desires)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                                     Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                                     choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
      return(Resultats)}
     } else Resultats$desires<-outlier
  }
  
  if( dial==T) {Resultats$sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Enregistrer les résultats ?")$res 
                if(length(Resultats$sauvegarde)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                                                       Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                                                       choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
                return(Resultats)}
  }else Resultats$sauvegarde<-sauvegarde
  
  return(Resultats)
  
}

# crée l'historique des commande (pour knitr)
.add.history<-function(data, command, nom){
  require(dplyr)
  try(get("ez.history", envir=.GlobalEnv),silent=T)->ez.history
  if(class(ez.history)=="try-error") {ez.history<-list()
  ez.history$Analyse[[1]]<-data
  names(ez.history)[length(ez.history)]<-paste("analyse sur",nom)
  names(ez.history[[length(ez.history)]])[1]<-nom  
  ez.history[[length(ez.history)]]$historique<-command 
  }else{
    if(nom==names(ez.history[[length(ez.history)]])[1] && all.equal(target=ez.history[[length(ez.history)]][[1]], current=data, ignore_col_order=T, ignore_row_order=T )!=TRUE){
      ez.history[[length(ez.history)]]$historique<-rbind(ez.history[[length(ez.history)]]$historique,command)
    }else {
      ez.history$Analyse[[1]]<-data
      names(ez.history)[length(ez.history)]<-paste("analyse sur",nom)
      names(ez.history[[length(ez.history)]])[1]<-nom  
      ez.history[[length(ez.history)]]$historique<-command 
    }
    
  }
  
  assign("ez.history",ez.history, envir=.GlobalEnv)  
}





# crée la liste avec tous les résultats
.add.result<-function(Resultats, name){
  
  try(get("ez.results", envir=.GlobalEnv),silent=T)->ez.results
  if(class(ez.results)=="try-error") {ez.results<-list()
  ez.results[[1]]<-Resultats
  }else{
    ez.results[[length(ez.results)+1]]<-Resultats
  }
  names(ez.results)[length(ez.results)]<-name
  assign("ez.results",ez.results, envir=.GlobalEnv)  
}


### test de normalité
.normalite<-function(data=NULL, X=NULL, Y=NULL){
  # data : dataframe in which data are stored
  # X : character. Name or list of the variables for the numerical values.Multinormality is prefered if X>1
  # Y : character. Name or list of the variabes which are used as groups. 
  packages<-c("outliers", "nortest","psych","ggplot2")
  if(length(X)==1){
    if(is.null(Y)){
      scale(data[,X], center=T, scale=F)->res
      res[1:length(res),]->data$res
    } else {
      tapply(data[,X], data[,Y], scale, center=T, scale=F)->res
      data$res<-unlist(res)
    }
    if(length(data[,"res"])<5000){
      shapiro.test(data[,"res"])->Shapiro_Wilk # realise le Shapiro-Wilk
      lillie.test(data[,"res"])->Lilliefors  # realise le Lilliefors
      round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
      names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
      dimnames(normalite)[1]<-" "
      format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->normalite}
    p1<-ggplot(data, aes(x=data[,"res"]))+geom_histogram(aes(y=..density..))
    p1<-p1+ stat_function(fun = dnorm, colour = "red",
                          args = list(mean = mean(data[,"res"], na.rm = TRUE),
                                      sd = sd(data[,"res"], na.rm = TRUE)))
    p1<-p1+theme(plot.title = element_text(size = 12))+ggtitle("Distribution du résidu")
    p1<-p1+ labs(x = names(data)[X])
    #print(p1)
    p2<-ggplot(data, aes(sample=res))+stat_qq() 
    p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("QQplot")
    
    p3<-multiplot(p1,p2,cols=2)
    print(p3)
  } else {
    try(mardia(data[,X],na.rm = TRUE, plot=TRUE), silent=TRUE)->mardia.results 
    
    if(any(class(mardia.results)=="mardia")) {
      data.frame("n"=mardia.results$n.obs, "N.var"=mardia.results$n.obs, "b1p"=mardia.results$b1p,"b2p"=mardia.results$b2p,
                 "skew"=mardia.results$skew,"p.skew"=mardia.results$p.skew,"small.skew"= mardia.results$small.skew,"p.small"= mardia.results$p.small,
                 "kurtosis"=mardia.results$kurtosis,"p.kurtosis"=mardia.results$p.kurt )->normalite
    } else {
      msgBox("La matrice est singulière et le test de Mardia ne peut être réalisé. Seules les analyses univariées peuvent être réalisées")
      normalite<-data.frame("W de Shapiro-Wilk"=NULL, "valeur.p SW"=NULL, "D de Lilliefors"=NULL, "valeur.p Llfrs"=NULL)
      for(i in 1:length(X)){
        X[i]->Z
        .normalite(data=data, X=Z,Y=Y)->nor1
        normalite<-rbind(normalite, nor1)
      }
      dimnames(normalite)[[1]]<-X
    }
  }
  return(normalite)
  
}


# crée la liste avec tous les résultats
.stat.desc.out<-function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3, plot=T){
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  Resultats<-list()
  if(length(X)==1 && class(data[, X])=="factor"){X->categ 
    X<-NULL} else if(any(sapply(data[,X], class)=="factor")) {
      X[which(sapply(data[,X], class)=="factor")]->categ
      setdiff(X, categ)->X
    }else categ<-NULL
  
  if(length(X)!=0){
    if(is.null(groupes)) NULL->groupes2 else data.frame(data[,groupes])->groupes2
    try(  psych::describeBy(data[,X], group=groupes2,mat=(!is.null(groupes)),type=type,digits=4, check=FALSE,skew = TRUE, 
                            ranges = TRUE,trim=tr, fast=FALSE), silent=T)->psych.desc
    if(class(psych.desc)=="try-error") {
      psych::describeBy(data[,X], group=groupes2,mat=F,type=type,digits=15, check=FALSE,skew = TRUE, 
                        ranges = TRUE,trim=tr)->psych.desc
      expand.grid(sapply(groupes2, levels))->modalites
      for(i in 1:length(modalites[,1])) {
        if(is.null(psych.desc[[i]])) paste("pas d'observations pour la combinaison", paste(unlist(modalites[i,]), collapse=" & "))->Resultats[[i]] else   psych.desc[[i]]->Resultats[[i]]
        paste(unlist(modalites[i,]), collapse=" & ")->names(Resultats)[i]}
    } else psych.desc-> Resultats$'Variables numériques'
    
    
    
    if(plot){
      for(j in 1:length(X)){
        if(is.null(groupes)){p <- ggplot(data, aes(x=factor(0), y=data[, X[j]])) + geom_violin()
        p+ labs( y=X[j])->p
        p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))->p
        p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)->p
        p+scale_fill_brewer(palette="Dark2")->p
        p + theme(legend.position="none")->p
        }
        if(!is.null(groupes) && length(groupes)==1){
          p <- ggplot(data, aes(x=data[, groupes], y=data[, X[j]], fill=data[, groupes])) + geom_violin()
          p+scale_fill_brewer(palette="PRGn")->p
          p+ labs(x = groupes, y=X[j])->p
          p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))->p
          p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)->p
          p + theme(legend.position="none")->p
        }
        if(!is.null(groupes) && length(groupes)==2){
          which.max(sapply(data[,groupes], nlevels))->pr
          which.min(sapply(data[,groupes], nlevels))->sec
          ggplot(data, aes(x=data[, groupes[pr]], y=data[, X[j]], fill=data[, groupes[sec]])) +geom_violin()->p
          p+ labs(x = groupes[pr], y=X[j])->p
          p+scale_fill_discrete(name=groupes[sec])->p
          p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))->p
          p+ geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)->p
          p+scale_fill_brewer(palette="PRGn")->p
        }
        if(!is.null(groupes) && length(groupes)>2){
          which.max(sapply(data[,groupes], nlevels))->pr
          which.min(sapply(data[,groupes], nlevels))->sec
          setdiff(groupes, c(pr,sec))->diff
          
          for(i in 1:length(diff)){
            if(i==1) paste0(".~", diff[1])->panneau
            if(i==2) paste0(diff[2],"~", diff[1])->panneau
            if(i>2 & i%%2!=0) paste0(panneau, "+", diff[i])->panneau 
            if(i>2 & i%%2==0) paste0(diff[i], "+", panneau)->panneau
          }
          ggplot(data, aes(x=data[,groupes[pr]], y=data[, X[j]], fill=data[, groupes[sec]])) +geom_violin()->p
          p+ facet_grid(as.formula(panneau))->p
          p+ labs(x = groupes[pr], y=X[j])->p
          p+scale_fill_discrete(name=groupes[sec])->p
          #  p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))->p
          p+ geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)->p
          p+scale_fill_brewer(palette="PRGn")->p
        }
        
        print(p)
        Resultats$"Informations sur les graphiques"[[1]]<-"L'épaisseur du graphique donne la densité, permettant de mieux cerner la distribution."
        Resultats$"Informations sur les graphiques"[[2]]<-"Le point rouge est la moyenne. La barre d'erreur est l'écart-type"
      }
    }
    
  }
  if(!is.null(categ)) {
    for(i in 1:length(categ)) {
      Resultats$'Variables catégorielles'[[categ[i]]] <-ftable(data[, c(categ[i], groupes)]) 
      }
  }
  
  return(Resultats)
}



blank.data<-function(){
  options (warn=1)
  options(scipen=999)
  # 3. choix du groupe de fonctions
  c("svDialogs", "RGtk2Extras")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)=="try-error") {try(install.packages("RGtk2Extras"),silent=T)->test2
    if(class(test2)=="try-error") msgBox("Désolé, vous devez utiliser une version 3.4 ou supérieure de R pour utiliser cette fonction")
  }
  fichier <- dlgInput("Quel nom voulez-vous donner à vos données ?", "data1")$res
  if(length(fichier)==0) return(imp.exp())
  fichier <- strsplit(fichier, ":")
  fichier <- tail(fichier[[1]],n=1)
  if(grepl("[^[:alnum:]]", fichier)) {
    writeLines("Des caractères non autorisés ont été utilisés pour le nom. Ces caractères ont été remplacés par des points")
    gsub("[^[:alnum:]]", ".", fichier)->fichier
  }
  data1<-data.frame()
  
  
  win <- gtkWindowNew()
  obj <- gtkDfEdit(data1, dataset.name=deparse(substitute(fichier)))
  win$add(obj)
  
  # assign(fichier, win$add(obj))
  return(ref1(packages))
  
  
}




view.results<-function(){
  c("svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  list()->Resultats
  Resultats$Call<-"view.results()"
  ref1(packages)->Resultats$"Packages des utilisés pour cette fonction"
  if(!exists("ez.results")) return("Aucune analyse sauvegardée n'a pu être trouvée") else get("ez.results")
  TkListView(ez.results)
  return(Resultats)
}



