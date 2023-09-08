

library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)
library(ggpubr)
library(cowplot)


server <- function(input, output,session) {
  
  
  # Download Data --------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("excel_tx_accès", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/excel_tx_accès.xlsx", file)
    }
  )
  
  
  ################################### CL #################### CL ############################ CL #########################################################
  ################################### CL #################### CL ############################ CL #########################################################
  
  
  EFE_1 <- read_parquet("data/indicateur_JC_0409.parquet")
  EFE_1$secteur_ensemble <-as.character(EFE_1$secteur_ensemble)
  
  
  
  EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "1"] <- "Ensembles des secteurs"
  EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "0"] <- "Secteur choisi"
  
  EFE_1$taille <- fct_relevel(EFE_1$taille, c("1 à 3", "4 à 9", "10 à 19","20 à 49","50 à 249","250 à 499", "500 à 999", "1000 et plus","Ensemble" ))
  
  EFE_1_nodupkey <- EFE_1 %>% distinct(secteur, .keep_all = TRUE)
  
  ensemble = list('Ensemble des secteurs')
  liste_secteur <- as.list(sort(EFE_1_nodupkey$secteur))
  liste_secteur[17] <- NULL
  liste_secteur2 <- c(ensemble, liste_secteur)
  
  filtered_testpivot_long <- reactive({
    dplyr::filter(EFE_1, secteur  %in% c("Ensemble des secteurs", input$secteur ))
  })
  
  
  
  EFE_1_nodupkey_taille <- EFE_1 %>% distinct(taille, .keep_all = TRUE)
  
  ensemble_liste_taille = list('Ensemble')
  liste_taille <- as.list((EFE_1_nodupkey_taille$taille))
  liste_taille[1] <- NULL
  liste_taille2 <- c(ensemble_liste_taille, liste_taille)
  
  
  # TAUX ACCES
  output$plot_tx_acc1 <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_acc1", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  
  ##############################################
  
  
  # PART ENTREPRISE FORMATRICE
  
  output$plot_part_formatrice <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_form", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj = gg2, height_svg = 5, width_svg  = 6)
  })
  
  
  #TPF
  output$plot_TPF <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_tpf", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj = gg2, height_svg = 5, width_svg  = 6)
  })
  
  
  
  # HEURE DE STAGE
  output$plot_H_stage <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "heurstag", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj = gg2, height_svg = 5, width_svg  = 6)
  })
  #################################################################
  
  output$titre_secteur <- renderText({
    paste0(  "<font color=\"#008b99\">",filtered()$secteur)
    })

  
  filtered <- reactive({
    EFE_1 %>% filter(taille==input$taille & secteur==input$secteur) 
  })
  
  
  output$domaine<-   renderUI({
    top3(filtered()$domaine_form_1,filtered()$domaine_form_2,filtered()$domaine_form_3)
    
  })   
  
  
  output$raison<-   renderUI({ 
    top3(filtered()$raison_non_form1,filtered()$raison_non_form2,filtered()$raison_non_form3)
    
  })   
  
  output$frein<-   renderText({ 
    paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$frein_non_form1,"<font color=\"#C0C0C2\">"," (",filtered()$percent_frein1," %)","<br>",
           "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$frein_non_form2,"<font color=\"#C0C0C2\">"," (",filtered()$percent_frein2," %)","<br>",
           "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">",filtered()$frein_non_form3,"<font color=\"#C0C0C2\">"," (",filtered()$percent_frein3," %)","<br>"
          )
    
  })   
  
  
  
  
  
}
# Download Data --------------------------------------------------------------












# })




#}


