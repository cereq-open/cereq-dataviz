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
  
  
  output$phrase<-renderText({
    if( x()=="Taille" & input$taille=="Ensemble des tailles")
    {
      paste0(h1("Indicateurs pour tous les secteurs d'activité, toutes les tailles, en ", input$annee) )
    } 
    else if ( ( x()=="Taille" & input$taille!="Ensemble des tailles") )
    {
      paste0(h1("Indicateurs pour tous les secteurs d'activité, la taille ",input$taille,"en", input$annee) )
    }
    
    else if( x()=="Secteur" & input$secteur_bis=="Ensemble des secteurs")
    {
      paste0(h1("Indicateurs pour tous les secteurs d'activité, toutes les tailles, en ", input$annee)  )
    } 
    else if ( ( x()=="Secteur" & input$secteur_bis!="Ensemble des secteurs") )
    {
      paste0(h1("Indicateurs pour le secteur ",input$secteur_bis, ", toutes les tailles ","en ", input$annee))
    }
    
  })
  
  
  #Création du filtre
  filtre_Europe <- reactive({
    
    Europe %>% dplyr::filter(taille %in% input$taille & secteur %in% input$secteur_bis & Année %in% input$annee )
  })
  
  # Carte TAUX ACCES
  
  
  output$taux_acces <- renderLeaflet({
    
    pal   <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_acc1)
    pal_2   <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_acc1, reverse = TRUE)
    class(Europe)
    
   labels <- ifelse(is.na(filtre_Europe()$tx_acc1),  "données manquantes", sprintf(
     "<strong>%s</strong><br/>%g %%",
     filtre_Europe()$NAME_FREN, filtre_Europe()$tx_acc1
   ) ) %>%
     lapply(htmltools::HTML)
       
       
   
    
    leaflet(filtre_Europe()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken=token)) %>%
      addPolygons(data=filtre_Europe(),fillColor = ~pal(tx_acc1),
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
      addLegend( pal = pal_2, values = ~tx_acc1,
                 title = element_blank(),
                 position = "bottomright",
                 labFormat = labelFormat(suffix =   " %", transform = function(tx_acc1)  sort(tx_acc1, decreasing = TRUE)),
                 na.label = paste0("données","<br>","manquantes"),
                 opacity = 1 )

  })
  
  
  output$legende1<- renderUI(HTML('<div class="legende">
                                  <br>
                                   <span style="color:#008B99;">Champ : </span>',
                                  '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                  "<br>",
                                  '<span style="color:#008B99;">Sources : </span>',
                                  ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  
  
  # Carte PART D ENTREPRISE FORMATRICE
  
  
  output$part_form <- renderLeaflet({
    
    pal_form <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_form)
    pal_form_2 <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_form, reverse = TRUE)
    class(Europe)
    
    
    
    
    labels_form <- ifelse(is.na(filtre_Europe()$tx_form),  "données manquantes", sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_Europe()$NAME_FREN, filtre_Europe()$tx_form
    ) ) %>%
      lapply(htmltools::HTML)
    
  
    
    
    leaflet(filtre_Europe()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken=token)) %>%
      addPolygons(data=filtre_Europe(),fillColor = ~pal_form(tx_form),
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
                                              direction = "auto")) %>%
    addLegend( pal = pal_form_2, values = ~tx_form,
               title = element_blank(),
               position = "bottomright",
               labFormat = labelFormat(suffix =   " %", transform = function(x)  sort(x, decreasing = TRUE)),
               na.label = paste0("données","<br>","manquantes"),
               opacity = 1 )
    
                  
  })
  
  
  
  output$legende2 <- renderUI(HTML('<div class="legende">
                                   <br>
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Sources : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  
  # Carte TAUX DE PARTICIPATION FINANCIERE
  
  
  
  output$TPF <- renderLeaflet({
    
    pal_tpf <-      colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_tpf)
    pal_tpf_2 <-      colorNumeric(  palette = "Blues", domain = filtre_Europe()$tx_tpf, reverse = TRUE)
    class(Europe)
 
    
    
    labels_tpf <- ifelse(is.na(filtre_Europe()$tx_tpf),  "données manquantes", sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_Europe()$NAME_FREN, filtre_Europe()$tx_tpf
    ) ) %>%
      lapply(htmltools::HTML)
    
    leaflet(filtre_Europe()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken=token)) %>%
      addPolygons(data=filtre_Europe(),fillColor = ~pal_tpf(tx_tpf),
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
      addLegend( pal= pal_tpf_2, values = ~tx_tpf,
                 title = element_blank(),
                 position = "bottomright",
                 labFormat = labelFormat(suffix =   " %",transform = function(x)  sort(x, decreasing = TRUE)),
                 na.label = paste0("données","<br>","manquantes"),
                 opacity = 1 )
    
    
  })
  output$legende3 <- renderUI(HTML('<div class="legende">
                                   <br>
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Sources : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
  
  
  
  # Carte NOMBRE D HEURE DE COURS ET STAGE
  
  
  output$nb_h <- renderLeaflet({
    
    pal_heurstag <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$heurstag)
    pal_heurstag_2 <- colorNumeric(  palette = "Blues", domain = filtre_Europe()$heurstag, reverse = TRUE)
    class(Europe)
    
    
    
    
    labels_heurstag <- ifelse(is.na(filtre_Europe()$heurstag),  "données manquantes", sprintf(
      "<strong>%s</strong><br/>%g %%",
      filtre_Europe()$NAME_FREN, filtre_Europe()$heurstag
    ) ) %>%
      lapply(htmltools::HTML)
    
    leaflet(filtre_Europe()) %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles() %>%
      addProviderTiles("Jawg.Light", options = providerTileOptions(accessToken=token)) %>%
      addPolygons(data=filtre_Europe(),fillColor = ~pal_heurstag(heurstag),
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
      addLegend( pal= pal_heurstag_2, values = ~heurstag,
                 
                 title = element_blank(),
                 position = "bottomright",
                 labFormat = labelFormat(suffix =   " heures" ,transform = function(x)  sort(x, decreasing = TRUE)),
                 na.label = paste0("données","<br>","manquantes"),
                 opacity = 1 )
    
    
    
    
    
  }) 
  
  output$legende4 <- renderUI(HTML('<div class="legende"> 
                                   <br>
                                   <span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#808080;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Sources : </span>',
                                   ' <span style="color:#808080;">Eurostat, enquête CVTS 5</span> 
                                   </div>'
  ))
}

