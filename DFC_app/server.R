
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
    dplyr::filter(EFE_1, secteur  %in% c("Ensemble des secteurs", input$Secteur_fin ) )
  })

  
  
  
  filtered_testpivot_long_fin <- reactive({
    dplyr::filter(EFE_1_fin, secteur  %in% c("Ensemble des secteurs",input$Secteur_fin ) )
  })
  
  
  filtered_testpivot_long_large <- reactive({
    dplyr::filter(EFE_1_large, secteur  %in% c("Ensemble des secteurs", input$Secteur_fin ) )
  })
  
  
  
  
  output$Secteur_large <- renderUI({
    radioGroupButtons("Secteur_large",  choices = c("Ensemble","Commerce", "Industrie","Service"),
                 status = "primary",
                 selected = "Ensemble",
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon"),
                   no = icon("remove",
                             lib = "glyphicon")))
  })
  
  
  

  
  
  
  output$Secteur_fin <- renderUI({
  
   if  (input$Secteur_large == "Ensemble")
     pickerInput("Secteur_fin", "Ensemble des secteurs", choices =c("Ensemble des secteurs"), selected = "Ensemble des secteurs")
    
    
    else if  (input$Secteur_large == "Service")
      pickerInput("Secteur_fin", h1("Service secteur fin"), choices =choix_service, selected = "Service ensemble" 
       )
    
    
    else  if  (input$Secteur_large == "Commerce")
      pickerInput("Secteur_fin", "Commerce secteur fin", choices =choix_commerce, selected = "Commerce ensemble")
    
    
    else  if  (input$Secteur_large == "Industrie")
      pickerInput("Secteur_fin", "Industrie secteur fin", choices =choix_industrie, selected = "Industrie ensemble")
  })
  
  
  
  ##############################################
  # PART ENTREPRISE FORMATRICE TTES FORMES
  
  output$plot_part_formatrice_courses <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
    { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_courses", caption_part_1)
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
    else {
      gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "tx_courses", caption_part_1) 
      girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
 
  })
  
  
  # TAUX ACCES
  output$plot_tx_acc <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
    { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_acc", caption_part_1)
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
    else {
      gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "tx_acc", caption_part_1)
      girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
    
    
    
    
    
  })
  
  # PART ENTREPRISE FORMATRICE TTES FORMES
  
  output$plot_part_formatrice_tte <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
    { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_form", caption_part_1)
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
    else {
      gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "tx_form", caption_part_1)
      girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
    
  })
  
  
  #TPF
  output$plot_TPF <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
    { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_tpf", caption_part_1)
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
    else {
      gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "tx_tpf", caption_part_1)
      girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
    
  })
  
  
  
  # HEURE DE STAGE
  output$plot_H_stage <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
    { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "heurstag", caption_part_1)
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
    else {
      gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "heurstag", caption_part_1)
      girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
    
  })
  
  
  # TAUX ACCES
  output$plot_heure_stage_sal <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)

   
   if (input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) 
   { gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "heurstag_sal", caption_part_1)
   girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
   else {
     gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "heurstag_sal", caption_part_1)
     girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
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
    EFE_1 %>% filter(taille==input$taille & secteur==input$Secteur_fin) 
  })
  
  
  output$raison<- renderText({ 
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



