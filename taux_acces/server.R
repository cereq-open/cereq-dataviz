
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
  
  EFE_2$tx_acc <- round(EFE_2$tx_acc, digits = 0)

  
  EFE_age <-  subset.data.frame(x =EFE_2, age %in% c("Moins de 25 ans", "De 25 à 54 ans","55 ans et plus")
                                |( age =="Ensemble" & sexe=="Ensemble" & cs=="Ensemble"))
  
  
  EFE_sexe <-  subset.data.frame(x =EFE_2, sexe %in% c("Homme", "Femme","Ensemble")
                                 & ( age =="Ensemble" & cs=="Ensemble"))
  
  
  EFE_cs <-  subset.data.frame(x =EFE_2, cs %in% c("Ouvrier", "Employé","Profession intermédiaire","Cadre", "Ensemble")
                               &( age =="Ensemble" & sexe=="Ensemble"))
  
  
  EFE_sexe_cs <-  subset.data.frame(x =EFE_2, 
                                   ( age =="Ensemble" ))
  
  EFE_sexe_cs2 <-  subset.data.frame(x =EFE_sexe_cs, 
                                    ( age !="Ensemble" | cs !="Ensemble" | sexe!="Ensemble" ))
  
  
  
  EFE_sexe_cs3 <-  subset.data.frame(x =EFE_sexe_cs2, 
                                    ( sexe!="Ensemble"))
  
  
  EFE_age$age <- fct_relevel(EFE_age$age , c("Moins de 25 ans", "De 25 à 54 ans","55 ans et plus","Ensemble" ))
  
  EFE_sexe$sexe <- fct_relevel(EFE_sexe$sexe , c("Homme", "Femme","Ensemble" ))
  
  EFE_cs$cs <- fct_relevel(EFE_cs$cs, c("Ouvrier", "Employé","Profession intermédiaire","Cadre", "Ensemble"))
  
  
  EFE_sexe_cs3$cs <- fct_relevel(EFE_sexe_cs3$cs, c("Ouvrier", "Employé","Profession intermédiaire","Cadre", "Ensemble"))
  
  
  
  
  EFE_sexe_cs3$sexe2 = as.character(EFE_sexe_cs3$sexe )
  EFE_sexe_cs3$sexe2[ EFE_sexe_cs3$sexe == "Homme"  & EFE_sexe_cs3$cs == "Ensemble"] = "Homme Ensemble"
  EFE_sexe_cs3$sexe2[ EFE_sexe_cs3$sexe == "Femme"  & EFE_sexe_cs3$cs == "Ensemble"] = "Femme Ensemble"
  
  
  
  
   output$plot_age <- renderGirafe({
    

   
    gg2 <-   plot_barchart(EFE_age,y_col =  "tx_acc", x_col=EFE_age$age,color_bar=colors_age, caption_part_1) 
                         #                   
    
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  
  
  
  
  output$plot_sexe <- renderGirafe({
    
    
    
    gg2 <-   plot_barchart(EFE_sexe,y_col =  "tx_acc", x_col=EFE_sexe$sexe,color_bar=colors_sexe, caption_part_1)
    #                   
    
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  
  
  
  
  output$plot_cs <- renderGirafe({
    
    
    
    gg2 <-   plot_barchart(EFE_cs,y_col =  "tx_acc", x_col=EFE_cs$cs,color_bar=colors_cs, caption_part_1)
    #                   
    
    girafe(ggobj =gg2 , height_svg = 5, width_svg  = 6)
  })
  
  
  
  output$plot_sexe_CS <- renderGirafe({
    
    
    p <- ggplot(EFE_sexe_cs3, aes(y = tx_acc, x = cs ,fill=sexe2, tooltip =paste0(sexe2," : ",tx_acc, " %" )))+

       geom_bar_interactive(stat="identity", position="stack") +
      scale_fill_manual(breaks=c('Femme', 'Homme', 'Femme Ensemble', 'Homme Ensemble'),values=colors_sexe_cs)+
    
      geom_text(aes(label = paste0(tx_acc, " %" )), position = position_stack(vjust = 0.5), color="white")  +  theme(legend.position = "top") +
      labs(caption = caption_part_1)
    
    
     
    
    
    girafe(ggobj =p , height_svg = 5, width_svg  = 6)
  })
  

  
  
}