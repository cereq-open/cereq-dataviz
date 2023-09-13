


library(downloader)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)
library(ggpubr)
library(cowplot)
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(shinydashboard)
library(paletteer)
library(rnaturalearth)
library(readr)
library(ggmap)
library(viridis)
library(shinyjs)
library(leaflet)
library(DT)
library(shinydashboardPlus)
library(rsconnect)

library(readr)
library(tidyverse)
library(sf)

library(reactable)
server <- function(input, output,session) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("base_load_europe", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/base_load_europe.xlsx", file)
    }
  )
  #filtre bouton 

  
 
  
 
  
  observe({
    shinyjs::toggleState("taille", input$taille_secteur == 'Taille')

    shinyjs::toggleState("secteur", input$taille_secteur == 'Secteur')
  })
  observeEvent(input$taille_secteur, {
    shinyjs::reset("taille")
    shinyjs::reset("secteur")
  })    
  observe({
  shinyjs::toggle(id='taille',condition = {"Taille" %in% input$taille_secteur})
  shinyjs::toggle(id='secteur',condition = {"Secteur" %in% input$taille_secteur})
    })
  
  
  

  
  x<-reactive({input$taille_secteur })
  y<-reactive({input$taille })
  z<-reactive({input$secteur_bis })
  
  
   output$phrase<-renderText({
     if( x()=="Taille" & input$taille=="Ensemble")
       {
    paste0(h3("Indicateurs pour l'ensemble des secteurs, l'ensemble des tailles, et l'année ", input$annee) )
     } 
     else if ( ( x()=="Taille" & input$taille!="Ensemble") )
       {
   paste0(h3("Indicateurs pour l'ensemble des secteurs, la taille ",input$taille,"et l'année ", input$annee) )
       }
     
    else if( x()=="Secteur" & input$secteur_bis=="Ensemble des secteurs")
      {
       paste0(h3("Indicateurs pour l'ensemble des secteurs, l'ensemble des tailles, et l'année ", input$annee)  )
     } 
     else if ( ( x()=="Secteur" & input$secteur_bis!="Ensemble des secteurs") )
       {
       paste0(h3("Indicateurs pour le secteur ",input$secteur_bis, ", l'ensemble des tailles ","et l'année ", input$annee))
       }

     })
  
 
 
 
  
  
  filtre_UE <- reactive({
    
    europe %>% dplyr::filter(taille %in% input$taille & secteur %in% input$secteur_bis & Année %in% input$annee )
  })
  
 

  
  #TAUX ACCES



  X=1/5
  bins<- c(0 , quantile(europe$tx_acc1,X) , quantile(europe$tx_acc1,X*2) , quantile(europe$tx_acc1,X*3),quantile(europe$tx_acc1,X*4), 100) 

 
  
  output$taux_acces <- renderLeaflet({
  
    
    pal   <- colorNumeric(  palette = "Blues", domain = filtre_UE()$tx_acc1)
    class(europe)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_UE()$NAME_FREN.x, filtre_UE()$tx_acc1
    ) %>%
      lapply(htmltools::HTML)
    
    

    
    leaflet(filtre_UE()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken="8C2gU8pSutHVOkE3id0L7olcMCYjc2Aoh3GdmmneYDBw6bX4m1gBzw9t3JMM0EU9")) %>%
        addPolygons(data=filtre_UE(),fillColor = ~pal(tx_acc1),
                                                weight = 0.3,
                                                opacity = 1,
                                                smoothFactor = 0.5,
                                                color = "black",
                                                dashArray = "",
                                                fillOpacity = 0.7,
                                                highlight = highlightOptions(weight = 3,
                                                                             color = "white",
                                                                             dashArray = "",
                                                                             fillOpacity = 0.7,
                                                                             bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                             padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")) %>%
      addLegend( pal = pal, values = ~tx_acc1,
                 title = element_blank(),
                 labFormat = labelFormat(suffix =   "%", between = " à "),
                 opacity = 1 )
 
  })
  
  
  output$legende1<- renderUI(HTML('<div class="legende"> 
                                   <span style="color:#008B99;">Champ : </span>',
                                  '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                  "<br>",
                                  '<span style="color:#008B99;">Source : </span>',
                                  ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  # PART D ENTREPRISE FORMATRICE
  
  X=1/5
  bins_form<- c(0 , quantile(europe$tx_form,X) , quantile(europe$tx_form,X*2) , quantile(europe$tx_form,X*3),quantile(europe$tx_form,X*4)) 
  
  

  output$part_form <- renderLeaflet({
    
    
    pal_form <- colorNumeric(  palette = "Blues", domain = filtre_UE()$tx_form)
    class(europe)
    
    labels_form <- sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_UE()$NAME_FREN.x, filtre_UE()$tx_form
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    
    leaflet(filtre_UE()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=filtre_UE(),fillColor = ~pal_form(tx_form),
                  weight = 0.3,
                  opacity = 1,
                  smoothFactor = 0.5,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 3,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels_form,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) 
    
    
  })
  
  
  
  output$legende2 <- renderUI(HTML('<div class="legende"> 
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  
  # TAUX DE PARTICIPATION FINANCIERE
  
  
    
    

  bins_tpf<- c(0 , quantile(europe$tx_tpf,X) , quantile(europe$tx_tpf,X*2) , quantile(europe$tx_tpf,X*3),quantile(europe$tx_tpf,X*4), max(europe$tx_tpf)) 

  
  output$TPF <- renderLeaflet({
    
    
    pal_tpf <- colorBin(palette = "Blues", domain =filtre_UE()$tx_tpf, bins = round(bins_tpf, digits = 0))
    class(europe)
    
    labels_tpf <- sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_UE()$NAME_FREN.x, filtre_UE()$tx_tpf
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    
    leaflet(filtre_UE()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=filtre_UE(),fillColor = ~pal_tpf(tx_tpf),
                  weight = 0.3,
                  opacity = 1,
                  smoothFactor = 0.5,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 3,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels_tpf,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend( pal= pal_tpf, values = ~tx_tpf,
                 title = element_blank(),
                 labFormat = labelFormat(suffix =   "%", between = " à "),
                 opacity = 1 )
    
    
  })
  output$legende3 <- renderUI(HTML('<div class="legende"> 
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  
  
  
    # NOMBRE D HEURE DE COURS ET STAGE
  
  
  
  bins_nb_h<- c(0 , quantile(europe$heurstag,X) , quantile(europe$heurstag,X*2) , quantile(europe$heurstag,X*3),quantile(europe$heurstag,X*4), max(europe$heurstag)) 
  
  
  

  
  output$nb_h <- renderLeaflet({
    
    
    pal_heurstag <- colorBin(palette = "Blues", domain =filtre_UE()$heurstag, bins = round(bins_nb_h, digits = 0))
    class(europe)
    
    labels_heurstag <- sprintf(
      "<strong>%s</strong><br/>%g heures",
      filtre_UE()$NAME_FREN.x, filtre_UE()$heurstag
    ) %>%
      lapply(htmltools::HTML)
    
    
    
    
    leaflet(filtre_UE()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=filtre_UE(),fillColor = ~pal_heurstag(heurstag),
                  weight = 0.3,
                  opacity = 1,
                  smoothFactor = 0.5,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 3,
                                               color = "white",
                                               dashArray = "",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = labels_heurstag,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addLegend( pal= pal_heurstag, values = ~heurstag,

                 title = element_blank(),
                 labFormat = labelFormat(suffix =   " heures" , between = " à "),
                 opacity = 1 )
    
    
    
    

  }) 
  
  output$legende4 <- renderUI(HTML('<div class="legende"> 
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
                                  ))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


