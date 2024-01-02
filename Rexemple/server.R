
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
  
  


  

  
  
  filtered_testpivot_long <- reactive({
    dplyr::filter(EFE_1, secteur  %in% c("Ensemble des secteurs", input$secteur ))
  })
  
  
  # TAUX ACCES
  output$plot_tx_acc <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_acc","ns_txacc",caption_part_1
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
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_courses","ns_txcourses" ,caption_part_1
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
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_form","ns_txform", caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj = gg2, height_svg = 5, width_svg  = 6)
  })
  
  
  #autres formes
  output$plot_autres_formes <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    #  titre <- tab_variables_evolution %>% filter(Nom_colonne == "taux_emploi") %>% pull(Titre_graphique)
    gg2 <- plot_barchart(filtered_testpivot_long(), "tx_autres","ns_txautres", caption_part_1
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
    gg2 <- plot_barchart(filtered_testpivot_long(), "heurstag","ns_heurstag" ,caption_part_1
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
    gg2 <- plot_barchart(filtered_testpivot_long(), "heurstag_sal","ns_heurstag_sal" ,caption_part_1
                         #                       , generateTitle(titre)
    )
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  #################################################################
  
  output$titre_secteur <- renderText({
    paste0("<strong>","<font size=5px>","Chiffres clés par taille d'entreprises pour le secteur : "  ,"<font color=\"#008b99\">",filtered()$secteur)
    })
  
  output$titre_formatrice_CS <- renderText({
    paste0("<strong>","<font size=3px>","Parmi les "  ,"<font color=\"#008b99\">",filtered()$tx_courses, " % ","<strong>","<font size=3px>","<font color=\"#000000\">","d'entreprises formatrices en cours et stages" )
  })
  output$sous_titre_formatrice_CS <- renderText({
    paste0("Secteur : ","<font color=\"#008b99\">",filtered()$secteur,"<font size=3px>","<font color=\"#000000\">"," Taille : ",  "<font color=\"#008b99\">",filtered()$taille,"<font size=3px>" )
  })
  
  output$titre_formatrice <- renderText({
    paste0("<strong>","<font size=3px>","Parmi les "  ,"<font color=\"#008b99\">",filtered()$tx_form, " % ","<strong>","<font size=3px>","<font color=\"#000000\">","d'entreprises formatrices toutes formes" )
  })
  output$sous_titre_formatrice <- renderText({
    paste0("Secteur : ","<font color=\"#008b99\">",filtered()$secteur,"<font size=3px>","<font color=\"#000000\">"," Taille : ",  "<font color=\"#008b99\">",filtered()$taille,"<font size=3px>" )
  })
  output$titre_non_formatrice <- renderText({
    paste0("<strong>","<font size=3px>","Parmi les "  ,"<font color=\"#008b99\">",100-filtered()$tx_form, " % ","<strong>","<font size=3px>","<font color=\"#000000\">","d'entreprises non formatrices" )
  })

  output$sous_titre_non_formatrice <- renderText({
    paste0("Secteur : ","<font color=\"#008b99\">",filtered()$secteur,"<font size=3px>","<font color=\"#000000\">"," Taille : ",  "<font color=\"#008b99\">",filtered()$taille,"<font size=3px>" )
  })
  
  filtered <- reactive({
    EFE_1 %>% filter(taille==input$taille & secteur==input$secteur) 
  })
  
  
  output$raison<- renderText({ 
    if (100-filtered()$tx_form!=0){
    if (!is.na(filtered()$top3_e1)){
     out<-paste0("<font color=\"#008b99\">","<font-size=\"40px\">","#1 ", "<font color=\"#00000\">",filtered()$top1_e1,"<font color=\"#008b99\">"," (",filtered()$top1_e1_tx,"&#xA0;%)","<br>",
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
    
    
    }
    
    else {out<-paste0("")}
    out
    
  })
  
  
  
  output$domaine<-   renderText({
    if (filtered()$tx_courses!=0){
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
    
    
    
    }
    else {out<-paste0("")}
    
    out
    
  })
  
  output$frein<-   renderText({  
    
    if (filtered()$tx_form!=0){
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
    
    
  
    }
    else {out<-paste0("")}
    out
    
  })
  
  
  
  
}
# Download Data --------------------------------------------------------------












# })




#}



