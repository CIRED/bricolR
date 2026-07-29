library(tidyverse)

db_prod <- readRDS("data/generation.rds")
db_CO2price <- readRDS("data/CO2_price.rds")
db_LCOE <- readRDS("data/LCOE.rds")
db_shareRetrofit <- readRDS("data/db_shareRetrofit.rds")

shinyServer(
  function(input, output) {
    
    demInput <- reactive({
      dem <- switch(input$demande, 
                    "Forte baisse" = "SOB",
                    "Baisse" = "EFF",
                    "Augmentation" = "DIV",
                    "Forte augmentation" = "DEC")
    })

    retrofitInput <- reactive({
      cost <- input$coutRenovation
    })
    
    newInput <- reactive({
      new <- switch(input$coutNouveauNucleaire,
                    "Haute" = "high",
                    "Moyenne" = "medium",
                    "Basse" = "low")
    })

    enrInput <- reactive({
      enr <- switch(input$coutENR,
                    "Haute" = "high",
                    "Basse" = "low")
    })
    
    CO2Input <- reactive({
      CO2 <- switch(input$CO2,
                    "Haute" = "high",
                    "Moyenne" = "medium",
                    "Basse" = "low")
    })

    output$text1 <- renderText({
      paste("Vous avez choisi une demande", demInput(),
            ", un coût du nucléaire rénové de ", retrofitInput(),
            ", un coût du nucléaire nouveau de ", newInput(),
            ", un coût de renouvelables de ", enrInput(),
            "et un prix du CO2 de ", CO2Input())
    })
    
    generation <- reactive({
      mix <- db_prod %>% filter(demand==demInput(),
                                enr==enrInput(),
                                retrofit==retrofitInput(),
                                new==newInput(),
                                CO2==CO2Input())
      return(mix)
    })
    
    CO2price <- reactive({
      CO2 <- db_CO2price %>% filter(demand==demInput(),
                            enr==enrInput(),
                            retrofit==retrofitInput(),
                            new==newInput(),
                            CO2==input$CO2)
      return(CO2)
    })
    
    LCOE <- reactive({
      LCOE <- db_LCOE %>% filter(demand==demInput(),
                                    enr==enrInput(),
                                    retrofit==retrofitInput(),
                                    new==newInput(),
                                    CO2==CO2Input()) 
      return(LCOE)
    })
    


    output$plotGene <- renderPlot({
      ggplot(data=generation(),aes(x = year, y = value)) + 
        geom_area(aes(colour=tec,fill=tec)) +
        theme_bw() + theme(legend.title = element_blank(), text=element_text(size=16, family="Arial")) +
        labs(x="Année", y="Production annuelle (TWh)") +
        scale_fill_manual(values=c("#a5723e", "#dded36", "#a2d6ef", "#7ad38f","#3d9150","#225e30", "#7ba0fc"))+
        scale_colour_manual(values=c("#a5723e", "#fff23d", "#a2d6ef", "#7ad38f","#3d9150","#225e30","#7ba0fc"))+
        ggtitle("Mix électrique optimal") +
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", hjust=0.5))
   })
    
    output$plotCO2 <- renderPlot({
      db_CO2price %>% 
      ggplot(aes(x = year, y = value, colour = CO2)) +
      geom_line(size = 1.5) + theme_bw() +
      labs(x = "Année", y = "Prix du CO2 \n ", colour = "Scénario")
    })
    
    output$LCOE <- renderPlot({
      ggplot(data = LCOE(), aes(x = year, y = value, group = tec, colour = tec)) + 
      geom_line(size = 1.5) + theme_bw() +
      labs(x = "Année", 
           y = "LCOE \n (incluant le cout du CO2)", 
           colour = "Scénario")
    })
    
    output$textShare <- renderText({
      shareRetrofit <- db_shareRetrofit %>% 
        filter(demand==demInput(),
               enr==enrInput(),
               retrofit==retrofitInput(),
               new==newInput(),
               CO2==CO2Input())  %>% 
        select(value) %>% 
        unlist()
      nb_retrofit <- round(58 - (shareRetrofit * 58 / 100), digits = 0)
      return(paste0("Avec vos hypothèses, l'optimum consiste à fermer ", nb_retrofit, " réacteurs nucléaires."))
    })
    
    # fermeture <- renderText({
    #   fermeture <- db_fermeture %>% filter(Demand==demInput(),Cost==retrofitInput(),CO2==CO2Input())
    #   fermeture <- sum(fermeture$Decom)
    #   return(fermeture)
    # })
  
  }
)
