shinyUI(fluidPage(theme = "custom.css",
  sidebarLayout(
    sidebarPanel(
      
      h3("A vous de jouer !"),
      
      br(),
      
      selectInput("demande", 
        label = "Niveau de demande (incluant les exportations)",
        choices = c("Forte baisse",
                    "Baisse",
                    "Augmentation",
                    "Forte augmentation" ),
        selected = "Baisse"),
      
      helpText("Ces valeurs de demande s'appuient sur les scénarios officiels du Débat National sur la Transition Energétique (DNTE)."),
      
      br(),
    
      sliderInput("coutRenovation", 
        label = "Coût du nucléaire rénové (€/MWh)",
        min = 40, max = 90, value = c(40), step=10),
      helpText("Les estimations actuelles d'EDF sont de 40 €/MWh, mais certains jugent cette estimation optimiste."),

      br(),
      
      selectInput("coutNouveauNucleaire",
                  label = "Trajectoire de coût du nucléaire nouvelle génération",
                  choices = c("Basse","Moyenne","Haute"),
                  selected = "Haute"),
      helpText("Coût constant à 120 €/MWh dans le scénario haut; décroissance à 90 et 70 €/MWh dans les scénarios médian et bas. L'EPR de Hinkley Point C est aujourd'hui à 120 €/MWh."),

      br(),

      selectInput("coutENR",
                  label = "Trajectoire de coût des ENR",
                  choices = c("Basse","Haute"),
                  selected = "Basse"),
      helpText("Scénario haut : 60 et 50 €/MWh respectivement pour l'éolien et le solaire en 2050 ;
                Scénario bas : 40 et 25 €/MWh respectivement."),

      br(),

      selectInput("CO2",
        label = "Trajectoire de prix du CO2",
        choices = c("Basse","Moyenne","Haute"),
        selected = "Moyenne"),
      helpText("Haute: Prix officiels du rapport Quinet. 56 €/tCO2 en 2020, 100 €/tCO2 en 2030 et 200 €/tCO2 en 2050.
      Basse : on divise par deux"),

      br()

    ),
  
    mainPanel(
      
      br(),
      
      br(),
    
      plotOutput("plotGene"),
      
      br(),
      
      tags$b(textOutput("textShare")),
      
      br(),
      
      h3("Paramètres utilisés pour cette simulation"),
      
      plotOutput("plotCO2", width = "80%", height = "200px"),
      
      br(),
      
      plotOutput("LCOE", width = "87%", height = "300px"),
      
      # textOutput("text1"),
      # 
      br()


      )
  )
))