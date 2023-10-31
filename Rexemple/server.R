
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
  output$plot_tx_acc <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_acc", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  
  ##############################################
  # PART ENTREPRISE FORMATRICE TTES FORMES
  
  output$plot_part_formatrice_courses <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_courses", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj = gg2, height_svg = 5, width_svg  = 6)
  })
  
  
  
  # PART ENTREPRISE FORMATRICE TTES FORMES
  
  output$plot_part_formatrice_tte <- renderGirafe({
    
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
  
  
  # TAUX ACCES
  output$plot_heure_stage_sal <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "heurstag_sal", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  #################################################################
  
  output$titre_secteur <- renderText({
    paste0("<strong>","<font size=5px>","Chiffres clés par taille d'entreprises pour le secteur : "  ,"<font color=\"#008b99\">",filtered()$secteur)
    })
  
  output$titre_formatrice <- renderText({
    paste0("<strong>","<font size=5px>","Parmi les "  ,"<font color=\"#008b99\">",filtered()$tx_courses, " % ","<strong>","<font size=5px>","<font color=\"#000000\">","d'entreprises formatrices : " )
  })
  
  output$titre_non_formatrice <- renderText({
    paste0("<strong>","<font size=5px>","Parmi les "  ,"<font color=\"#008b99\">",100-filtered()$tx_courses, " % ","<strong>","<font size=5px>","<font color=\"#000000\">","d'entreprises non formatrices : " )
  })

  
  filtered <- reactive({
    EFE_1 %>% filter(taille==input$taille & secteur==input$secteur) 
  })
  
  
  output$raison<- renderText({ 
    if (!is.na(filtered()$top3_e1)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_e1,"<font color=\"#008b99\">"," (",filtered()$top1_e1_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_e1,"<font color=\"#008b99\">"," (",filtered()$top2_e1_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">",filtered()$top3_e1,"<font color=\"#008b99\">"," (",filtered()$top3_e1_tx,"&#xA0;%)","<br>")
    }
    
    if (is.na(filtered()$top3_e1)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_e1,"<font color=\"#008b99\">"," (",filtered()$top1_e1_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_e1,"<font color=\"#008b99\">"," (",filtered()$top2_e1_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    if (is.na(filtered()$top2_e1)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_e1,"<font color=\"#008b99\">"," (",filtered()$top1_e1_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    
    if (is.na(filtered()$top1_e1)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    
    
    
    
    out
    
  })
  
  
  
  output$domaine<-   renderText({
    if (!is.na(filtered()$top3_c5)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_c5,"<font color=\"#008b99\">"," (",filtered()$top1_c5_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_c5,"<font color=\"#008b99\">"," (",filtered()$top2_c5_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">",filtered()$top3_c5,"<font color=\"#008b99\">"," (",filtered()$top3_c5_tx,"&#xA0;%)","<br>")
    }
    
    if (is.na(filtered()$top3_c5)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_c5,"<font color=\"#008b99\">"," (",filtered()$top1_c5_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_c5,"<font color=\"#008b99\">"," (",filtered()$top2_c5_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    if (is.na(filtered()$top2_c5)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_c5,"<font color=\"#008b99\">"," (",filtered()$top1_c5_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    
    if (is.na(filtered()$top1_c5)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    
    
    
    
    out
    
  })
  
  output$frein<-   renderText({  
    
    
    if (!is.na(filtered()$top3_d3)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_d3,"<font color=\"#008b99\">"," (",filtered()$top1_d3_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_d3,"<font color=\"#008b99\">"," (",filtered()$top2_d3_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">",filtered()$top3_d3,"<font color=\"#008b99\">"," (",filtered()$top3_d3_tx,"&#xA0;%)","<br>")
    }
    
    if (is.na(filtered()$top3_d3)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_d3,"<font color=\"#008b99\">"," (",filtered()$top1_d3_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">",filtered()$top2_d3,"<font color=\"#008b99\">"," (",filtered()$top2_d3_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    if (is.na(filtered()$top2_d3)){
      out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">",filtered()$top1_d3,"<font color=\"#008b99\">"," (",filtered()$top1_d3_tx,"&#xA0;%)","<br>",
                  "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                  "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
    }
    
    
    if (is.na(filtered()$top1_d3)){
    out<-paste0("<font color=\"#008b99\">","#1 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                "<font color=\"#008b99\">","#2 ", "<font color=\"#00000\">","Données non disponibles","<br>",
                "<font color=\"#008b99\">","#3 ", "<font color=\"#00000\">","Données non disponibles","<br>")
  }
    
    
  
   
    
    out
    
  })
  
  
  
  
}
# Download Data --------------------------------------------------------------












# })




#}



