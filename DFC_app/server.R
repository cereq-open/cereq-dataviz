
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
  
  
  filtered <- reactive({
    EFE_1 %>% filter(taille %in% input$taille & secteur %in% input$Secteur_fin) 
  })

  
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
  
  
  
  
  
  
  output$taille <- renderUI({
    
    if  (isTRUE(input$secteur_large  %in% "Ensemble" || input$Secteur_fin  %in% "Ensemble des secteurs" || input$Secteur_fin  %in% "Service ensemble" ||input$Secteur_fin  %in% "Commerce ensemble" || input$Secteur_fin  %in% "Industrie ensemble"))
      pickerInput("taille", choices =c("1 à 3 salariés", "4 à 9 salariés", "10 à 19 salariés","20 à 49 salariés","50 à 249 salariés","250 à 499 salariés", "500 à 999 salariés", "1000 salariés et plus", "Ensemble"), selected = "Ensemble")
    
    else 
      pickerInput("taille", choices =c("Moins de 50 salariés", "50 et plus", "Ensemble"), selected = "Ensemble")
  })
  
  
  
  output$Secteur_fin <- renderUI({
  
   if  (isTRUE(input$Secteur_large == "Ensemble"))
     pickerInput("Secteur_fin", "Ensemble des secteurs", choices =c("Ensemble des secteurs"), selected = "Ensemble des secteurs")
    
    
    else if  (isTRUE(input$Secteur_large == "Service"))
      pickerInput("Secteur_fin", h1("Service secteur fin"), choices =choix_service, selected = "Service ensemble" 
       )
    
    
    else  if  (isTRUE(input$Secteur_large == "Commerce"))
      pickerInput("Secteur_fin", "Commerce secteur fin", choices =choix_commerce, selected = "Commerce ensemble")
    
    
    else  if  (isTRUE(input$Secteur_large == "Industrie"))
      pickerInput("Secteur_fin", "Industrie secteur fin", choices =choix_industrie, selected = "Industrie ensemble")
  })
  
  
  
  ##############################################
  # PART ENTREPRISE FORMATRICE TTES FORMES
  
  output$plot_part_formatrice_courses <- renderGirafe({
    
    gg <- plot_only_legend(filtered_testpivot_long())
    legende <- get_legend(gg)
    gg1 <- as_ggplot(legende)
    
    if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
    {     
      theme_replace(
    
        axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
  
      )
      gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_courses", caption_part_1)
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
    
    if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
    { 
      theme_replace(
        
        axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
        
      )
      gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_acc", caption_part_1)
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
    
    if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
    { 
      theme_replace(
        
        axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
        
      )
      gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_form", caption_part_1)
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
    
    if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
    { 
      theme_replace(
        
        axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
        
      )
      gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "tx_tpf", caption_part_1)
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
    
    if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
    {
      theme_replace(
        
        axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
        
      )
      gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "heurstag", caption_part_1)
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

   
   if (isTRUE(input$Secteur_fin== "Service ensemble"  || input$Secteur_fin== "Industrie ensemble" || input$Secteur_fin== "Commerce ensemble"   || input$Secteur_fin== "Ensemble des secteurs"  ) )
   { 
     theme_replace(
       
       axis.text.x = element_text(color = "#008B99", size = 9, hjust = 0.5, vjust = 4.5),
       axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
       
     )
     gg2 <- plot_barchart_large(filtered_testpivot_long_large(), "heurstag_sal", caption_part_1)
   girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)}
   else {
     gg2_bis <- plot_barchart_fin(filtered_testpivot_long_fin(), "heurstag_sal", caption_part_1)
     girafe(ggobj =gg2_bis , height_svg = 5, width_svg  = 6)}
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
  
  
  output$raison<- renderText({ 
    if (isTRUE(100-filtered()$tx_form!=0)){
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
    if (isTRUE(filtered()$tx_courses!=0)){
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
    
    if (isTRUE(filtered()$tx_form!=0)){
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



