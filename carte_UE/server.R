


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

library(leaflet)
library(DT)
library(shinydashboardPlus)
library(rsconnect)
library(leaflet)
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
  
  #TAUX ACCES
  
  filtre_UE <- reactive({
    europe %>% filter(taille==input$taille & secteur==input$secteur_bis) 
 })
  X=1/5
  bins<- c(0 , quantile(europe$tx_acc1,X) , quantile(europe$tx_acc1,X*2) , quantile(europe$tx_acc1,X*3),quantile(europe$tx_acc1,X*4), 100) 

 
  
  output$taux_acces <- renderLeaflet({
  
    
    pal <- colorBin( palette = "Blues", domain =filtre_UE()$tx_acc1, bins = round(bins,digits = 0))
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
      addProviderTiles(providers$CartoDB.Positron) %>%
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
                 labFormat = labelFormat(suffix =   "%", between = "% à "),
                 opacity = 1 )
 
  })
  
  
  output$legende1 <- renderUI(HTML('<span style="color:#008B99;">Champ : </span>',
                                  '<span style="color:#C0C0C2;">Entreprise de 3 salariés et plus blablabla</span>',
                                             "<br>",
                                             '<span style="color:#008B99;">Source : </span>',
                                            ' <span style="color:#C0C0C2;">Eurostat, enquête CVTS 5</span>'))
  # PART D ENTREPRISE FORMATRICE
  
  X=1/5
  bins_form<- c(0 , quantile(europe$tx_form,X) , quantile(europe$tx_form,X*2) , quantile(europe$tx_form,X*3),quantile(europe$tx_form,X*4)) 
  
  
  filtre_UE <- reactive({
    europe %>% filter(taille==input$taille & secteur==input$secteur_bis) })
  
  output$part_form <- renderLeaflet({
    
    
    pal_form <- colorBin(palette = "Blues", domain =filtre_UE()$tx_form, bins = round(bins_form, digits = 0))
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
                                              direction = "auto")) %>%
      addLegend( pal= pal_form, values = ~tx_form,
                 title = element_blank(),
                 labFormat = labelFormat(suffix =   "%", between = "% à "),
                 opacity = 1 )
    
    
  })
  
  
  
  output$legende2 <- renderUI(HTML('<span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#C0C0C2;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#C0C0C2;">Eurostat, enquête CVTS 5</span>'))
  
  # TAUX DE PARTICIPATION FINANCIERE
  
  
    
    

  bins_tpf<- c(0 , quantile(europe$tx_tpf,X) , quantile(europe$tx_tpf,X*2) , quantile(europe$tx_tpf,X*3),quantile(europe$tx_tpf,X*4), max(europe$tx_tpf)) 
  
  
  filtre_UE <- reactive({
    europe %>% filter(taille==input$taille & secteur==input$secteur_bis) })
  
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
                 labFormat = labelFormat(suffix =   "%", between = "% à "),
                 opacity = 1 )
    
    
  })
  output$legende3 <- renderUI(HTML('<span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#C0C0C2;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#C0C0C2;">Eurostat, enquête CVTS 5</span>'))
  
  
  
    # NOMBRE D HEURE DE COURS ET STAGE
  
  
  
  bins_nb_h<- c(0 , quantile(europe$heurstag,X) , quantile(europe$heurstag,X*2) , quantile(europe$heurstag,X*3),quantile(europe$heurstag,X*4), max(europe$heurstag)) 
  
  
  
  
  filtre_UE <- reactive({
    europe %>% filter(taille==input$taille & secteur==input$secteur_bis) })
  
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
                 labFormat = labelFormat(suffix =   " heures" , between = " heures à "),
                 opacity = 1 )
    
    
    
    

  }) 
  
  output$legende4 <- renderUI(HTML('<span style="color:#008B99;">Champ : </span>',
                                   '<span style="color:#C0C0C2;">Entreprise de 3 salariés et plus blablabla</span>',
                                   "<br>",
                                   '<span style="color:#008B99;">Source : </span>',
                                   ' <span style="color:#C0C0C2;">Eurostat, enquête CVTS 5</span>'))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


