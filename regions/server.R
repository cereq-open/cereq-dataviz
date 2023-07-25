# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Define Server ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # Download Data --------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('tab_region', '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(db_region, file)
    }
  )
  
  # Create Title ---------------------------------------------------------------
  
  # Message contenu dans les bulles informatives
  
  info_bulle_reactive <- reactive({
    if(input$col_name == "tx_chmg") {
      return("Le taux de chômage correspond à la part des  individus sans emploi et à la recherche d'un emploi parmi les actifs (individus en emploi ou au chômage).")
      
    } else if(input$col_name == "traj_1" | input$col_name == "traj_2" | input$col_name == "traj_3" | input$col_name == "traj_7") {
      return("Cette information résulte d'une typologie des trajectoires construite par le Céreq par des méthodes statistiques.")
      
    } else if(input$col_name == "taux_ed") {
      return("Proportion d'individus en emploi non-salarié (personne à son compte ou aide familial), en contrat à durée indéterminée (CDI) ou avec le statut de fonctionnaire.")
      
    } else if(input$col_name == "rvn_trv") {
      return("Niveau de salaire ou traitement mensuel net primes incluses médian. Le revenu médian est la valeur telle que la moitié des individus de la population considérée gagne plus, l'autre moitié gagne moins.")
      
    }
  })
  
  # Titre
  
  output$titre1 <- renderUI({
    if(input$col_name %in% noms_colonnes_info_bulle) {
      labellize_row_i(noms_colonnes[input$col_name], info_bulle_reactive())
      
    } else {
      
      labellize_row_i(noms_colonnes[input$col_name])
      
    }
    
  })
  
  # Create Map -----------------------------------------------------------------
  
  # Code pour créer la carte avec ggplot
  output$carte <- renderPlot({
    tab_region <- concatenate_columns(tab_region, input$col_name)
    plot_map(tab_region, input$col_name, "value")
  })
  
  # Code pour créer la carte avec ggplot
  output$carte2 <- renderPlot({
    ggplot() +
      geom_sf(data = tab_region, aes(fill = tax_mpl)) +
      scale_fill_viridis_c() +
      theme_void() +
      theme(legend.position = "none")
  })
  
})