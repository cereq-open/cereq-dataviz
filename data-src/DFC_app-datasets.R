library(NestedMenu)
library(shiny)
library(readr)
library(readxl)
library(arrow)

library(tidyverse)
# Load data --------------------------------------------------------------------

#EFE_1 <-  read_excel("data/EFE_1.xlsx")

EFE_1_large <- read_excel("DFC_app/data/EFE_1_secteur_large.xlsx")
EFE_1_fin <- read_excel("DFC_app/data/EFE_1_secteur_fin.xlsx")
EFE_1 <- read_excel("DFC_app/data/EFE_1.xlsx")


EFE_1 <- EFE_1 %>% 
  rename( secteur =lib_secteur)

EFE_1 <- EFE_1 %>% 
  rename(  taille= lib_taille)



#FIN
EFE_1_fin <- EFE_1_fin %>% 
  rename( secteur =lib_secteur)

EFE_1_fin <- EFE_1_fin %>% 
  rename(  taille= lib_taille)

EFE_1_fin$secteur[EFE_1_fin$secteur == 'Ensemble'] <- 'Ensemble des secteurs'

EFE_1_fin$tx_acc <- round(EFE_1_fin$tx_acc, digits = 0)
EFE_1_fin$heurstag <- round(EFE_1_fin$heurstag, digits = 0)
EFE_1_fin$tx_form <- round(EFE_1_fin$tx_form, digits = 0)
EFE_1_fin$tx_tpf <- round(EFE_1_fin$tx_tpf, digits = 1)
EFE_1_fin$heurstag_sal <- round(EFE_1_fin$heurstag_sal, digits = 0)
EFE_1_fin$tx_courses <- round(EFE_1_fin$tx_courses, digits = 0)
EFE_1_fin$top1_c5_tx <- round(EFE_1_fin$top1_c5_tx, digits = 0)
EFE_1_fin$top2_c5_tx <- round(EFE_1_fin$top2_c5_tx, digits = 0)
EFE_1_fin$top3_c5_tx <- round(EFE_1_fin$top3_c5_tx, digits = 0)
EFE_1_fin$top1_d3_tx <- round(EFE_1_fin$top1_d3_tx, digits = 0)
EFE_1_fin$top2_d3_tx <- round(EFE_1_fin$top2_d3_tx, digits = 0)
EFE_1_fin$top3_d3_tx <- round(EFE_1_fin$top3_d3_tx, digits = 0)
EFE_1_fin$top1_e1_tx <- round(EFE_1_fin$top1_e1_tx, digits = 0)
EFE_1_fin$top2_e1_tx <- round(EFE_1_fin$top2_e1_tx, digits = 0)
EFE_1_fin$top3_e1_tx <- round(EFE_1_fin$top3_e1_tx, digits = 0)

#large

EFE_1_large <- EFE_1_large %>% 
  rename( secteur =lib_secteur)

EFE_1_large <- EFE_1_large %>% 
  rename(  taille= lib_taille)

EFE_1_large$secteur[EFE_1_large$secteur == 'Ensemble'] <- 'Ensemble des secteurs'

EFE_1_large$tx_acc <- round(EFE_1_large$tx_acc, digits = 0)
EFE_1_large$heurstag <- round(EFE_1_large$heurstag, digits = 0)
EFE_1_large$tx_form <- round(EFE_1_large$tx_form, digits = 0)
EFE_1_large$tx_tpf <- round(EFE_1_large$tx_tpf, digits = 1)
EFE_1_large$heurstag_sal <- round(EFE_1_large$heurstag_sal, digits = 0)
EFE_1_large$tx_courses <- round(EFE_1_large$tx_courses, digits = 0)
EFE_1_large$top1_c5_tx <- round(EFE_1_large$top1_c5_tx, digits = 0)
EFE_1_large$top2_c5_tx <- round(EFE_1_large$top2_c5_tx, digits = 0)
EFE_1_large$top3_c5_tx <- round(EFE_1_large$top3_c5_tx, digits = 0)
EFE_1_large$top1_d3_tx <- round(EFE_1_large$top1_d3_tx, digits = 0)
EFE_1_large$top2_d3_tx <- round(EFE_1_large$top2_d3_tx, digits = 0)
EFE_1_large$top3_d3_tx <- round(EFE_1_large$top3_d3_tx, digits = 0)
EFE_1_large$top1_e1_tx <- round(EFE_1_large$top1_e1_tx, digits = 0)
EFE_1_large$top2_e1_tx <- round(EFE_1_large$top2_e1_tx, digits = 0)
EFE_1_large$top3_e1_tx <- round(EFE_1_large$top3_e1_tx, digits = 0)
vec_secteur <- unique(EFE_1_large$secteur)



EFE_1$secteur[EFE_1$secteur == 'Ensemble'] <- 'Ensemble des secteurs'


EFE_1_nodupkey <- EFE_1 %>% distinct(secteur, .keep_all = TRUE)
ensemble = list('Ensemble')
liste_secteur <- as.list(sort(EFE_1_nodupkey$secteur))
liste_secteur[17] <- NULL
liste_secteur2 <- c(ensemble, liste_secteur)


EFE_1_nodupkey_taille <- EFE_1 %>% distinct(taille, .keep_all = TRUE)

ensemble_liste_taille = list('Ensemble')
liste_taille <- as.list((EFE_1_nodupkey_taille$taille))
liste_taille[1] <- NULL
liste_taille2 <- c(ensemble_liste_taille, liste_taille)


source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Source : Céreq-Dares-France compétences, Enquête Formation Employeur – européenne (EFE-e, Données 2020)"
)


caption_part_1 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Ensemble des entreprises de 1 salarié et plus du secteur privé (Hors activités des ménages et extraterritoriales)",
  "<br>",
  source
)


EFE_1$secteur  <- fct_relevel(EFE_1$secteur, c(
  "Service ensemble"   ,                                         "Activités auxiliaires de services financiers et d'assurance",
  "Activités créatives, artistiques et de spectacle"    ,        "Activités de location"    ,                                  
  "Activités des organisations associatives" ,                   "Activités des services financiers"   ,                       
  "Activités immobilières"              ,                        "Activités juridiques et comptables"  ,                       
  "Administration publique et santé" ,                           "Agriculture, sylviculture et pêche"    ,                    
  "Commerce ensemble"      ,                                     "Cockefaction et raffinage"      ,                            
  "Commerce de détail"      ,                                    "Commerce de gros"          ,
  "Commerce et réparation d'automobiles"     ,                   "Construction"         ,                                      
  "Edition, Production de films cinématographiques"    ,         "Enseignement"          ,                                     
  "Fabrication de meubles"                          ,           
  "Industrie ensemble"                                       ,   "Fabrication de produits informatiques"               ,       
  "Fabrication de textiles"                                   ,  "Hébergement, Restauration"                            ,      
  "Industrie automobile"                                       , "Industrie du papier et du carton"                      ,     
  "Industries alimentaire"                                      ,"Industries extractives"                                 ,    
  "Métallurgie"                                                 ,"Production et distribution d'électricité"  ,
  "Transports"  ,"Ensemble des secteurs"  ))  

EFE_1$secteur_large  <- fct_relevel(EFE_1$secteur_large, c( 
  "Ensemble",  "Commerce" , "Industrie" , "Service"  ))








EFE_1_fin$taille <- fct_relevel(EFE_1_fin$taille, c("moins de 50", "plus de 50" , "Ensemble" ))
EFE_1_large$taille <- fct_relevel(EFE_1_large$taille, c("1 à 3 salariés", "4 à 9 salariés", "10 à 19 salariés","20 à 49 salariés","50 à 249 salariés","250 à 499 salariés", "500 à 999 salariés", "1000 salariés et plus", "Ensemble" ))
        

EFE_1_fin$secteur  <- fct_relevel(EFE_1_fin$secteur, c(
  "Service ensemble"   ,                                         "Activités auxiliaires de services financiers et d'assurance",
  "Activités créatives, artistiques et de spectacle"    ,        "Activités de location"    ,                                  
  "Activités des organisations associatives" ,                   "Activités des services financiers"   ,                       
  "Activités immobilières"              ,                        "Activités juridiques et comptables"  ,                       
  "Administration publique et santé" ,                           "Agriculture, sylviculture et pêche"    ,                    
  "Commerce ensemble"      ,                                     "Cockefaction et raffinage"      ,                            
  "Commerce de détail"      ,                                    "Commerce de gros"          ,
  "Commerce et réparation d'automobiles"     ,                   "Construction"         ,                                      
  "Edition, Production de films cinématographiques"    ,         "Enseignement"          ,                                     
  "Fabrication de meubles"                          ,           
  "Industrie ensemble"                                       ,   "Fabrication de produits informatiques"               ,       
  "Fabrication de textiles"                                   ,  "Hébergement, Restauration"                            ,      
  "Industrie automobile"                                       , "Industrie du papier et du carton"                      ,     
  "Industries alimentaire"                                      ,"Industries extractives"                                 ,    
  "Métallurgie"                                                 ,"Production et distribution d'électricité"  ,
  "Transports"  ,"Ensemble des secteurs"  ))  

EFE_1_large$secteur_large  <- fct_relevel(EFE_1_large$secteur_large, c( 
  "Service ensemble" , "Industrie ensemble" ,  "Commerce ensemble"  , "Ensemble des secteurs" ))



EFE_1_large$secteur  <- fct_relevel(EFE_1_large$secteur, c( 
  "Service ensemble" , "Industrie ensemble" ,  "Commerce ensemble"  , "Ensemble des secteurs" ))



EFE_1$secteur_large  <- fct_relevel(EFE_1$secteur_large, c( 
  "Ensemble", "Commerce"  ,"Industrie" , "Service" ))



EFE_1$secteur  <- fct_relevel(EFE_1$secteur, c( 
  "Ensemble des secteurs",  "Service ensemble", "Commerce ensemble",   "Industrie ensemble",  
  "Activités auxiliaires de services financiers et d'assurance",
  "Activités créatives, artistiques et de spectacle",
  "Activités de location",                                     
  "Activités des organisations associatives",   
  "Activités des services financiers"   ,                       
  "Activités immobilières",                                     
  "Activités juridiques et comptables"   ,                      
  "Administration publique et santé" ,
  "Agriculture, sylviculture et pêche",
  "Cockefaction et raffinage"   ,                               
  "Commerce de détail",                    
  "Commerce de gros"  ,                                         
  "Commerce et réparation d'automobiles" ,         
  "Construction" ,                                             
  "Edition, Production de films cinématographiques"  ,  
  "Enseignement",   
  "Fabrication de meubles"     ,                                
  "Fabrication de produits informatiques"    ,                  
  "Fabrication de textiles"    ,   
  "Hébergement, Restauration"      ,                            
  "Industrie automobile"    ,   
  "Industrie du papier et du carton" ,                          
  "Industries alimentaire" ,                
  "Industries extractives"   ,                                  
  "Métallurgie"  ,                     
  "Production et distribution d'électricité",              
  "Transports"))

colors_fin<-setNames(c("#046B76","#046B76","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB",
                       "#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#046B76","#7FC4CB",
                       "#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB",
                       "#7FC4CB","#046B76"),unique(EFE_1$secteur))

colors_large<-setNames(c("#7FC4CB","#7FC4CB","#046B76","#7FC4CB"),unique(EFE_1_large$secteur))


choix_service <- c( 
  "Service ensemble",
  "Activités auxiliaires de services financiers et d'assurance",
  "Activités créatives, artistiques et de spectacle",
  "Activités de location",
  "Activités des organisations associatives",
  "Activités des services financiers",
  "Activités immobilières",
  "Activités juridiques et comptables",
  "Administration publique et santé",
  "Edition, Production de films cinématographiques",
  "Enseignement",
  "Hébergement, Restauration")

choix_industrie <- c( 
  "Industrie ensemble", 
  "Cockefaction et raffinage",
  "Construction",
  "Fabrication de meubles",
  "Fabrication de produits informatiques",
  "Fabrication de textiles",
  "Industrie automobile",
  "Industrie du papier et du carton",
  "Industries alimentaire",
  "Industries extractives",
  "Métallurgie",
  "Production et distribution d'électricité")

choix_commerce <- c( 
  "Commerce ensemble", 
  "Agriculture, sylviculture et pêche",
  "Commerce de détail",
  "Commerce de gros",
  "Commerce et réparation d'automobiles",
  "Transports")














plot_barchart_large <- function(df, y_col, caption_texte, titre = NULL) {
  DT <- concat_value(df, y_col)
  ggplot(data = DT, aes(x = taille, y = !!sym(y_col), fill = as.factor(secteur), tooltip =tooltip_value, data_id = taille )) + geom_bar_interactive(stat = "identity", position = "dodge")  +
    geom_text(aes(x= taille, label = taux_str),  position = position_dodge(width = 0.9),vjust= 2, size= 2, color = "white") + scale_fill_manual(values = colors_large) + 
    labs(caption = caption_texte, title = titre,x = "Taille de l'entreprise en nombre de salariés") +
    scale_x_discrete(labels = c("1 à 3", "4 à 9", "10 à 19","20 à 49","50 à 249","250 à 499", "500 à 999", "1000 et +", expression(bold("Ensemble")) )) 
}
    
    
 theme_large<- theme_set(
      theme(
        line = element_line(colour = "black", linewidth = 0.1),
        title = element_text(family = "Arimo", size = 6),
        text = element_text(size = 11, family = "Arimo"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "#008B99", size = 10, hjust = 0.4, vjust = 2.5),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
        plot.title = element_textbox_simple(hjust = 0, size = 17, color = "#008B99"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.justification = "center",
        plot.caption = element_textbox_simple( hjust = 100, color = "#808080", size = 10 , margin = margin(t = -1)) 
      )
    )



plot_barchart_fin <- function(df, y_col, caption_texte, titre = NULL) {
  DT <- concat_value(df, y_col)
  ggplot(data = DT, aes(x = taille, y = !!sym(y_col), fill = as.factor(secteur), tooltip =tooltip_value, data_id = taille )) + geom_bar_interactive(stat = "identity", position = "dodge")  +
    geom_text(aes(x= taille, label = taux_str),  position = position_dodge(width = 0.9),vjust= 2, size= 2, color = "white") + scale_fill_manual(values = colors_fin) + 
    labs(caption = caption_texte, title = titre,x = "Taille de l'entreprise en nombre de salariés") +
    scale_x_discrete(labels = c("moins de 50", "50 et plus"  , expression(bold("Ensemble")) )) }
  
  
  theme_fin<-  theme_set(
    theme(
      line = element_line(colour = "black", linewidth = 0.1),
      title = element_text(family = "Arimo", size = 6),
      text = element_text(size = 11, family = "Arimo"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(color = "#008B99", size = 8 , hjust = 0.4, vjust = 5),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
      plot.title = element_textbox_simple(hjust = 0, size = 17, color = "#008B99"),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.caption.position = "plot",
      legend.position = "top",
      legend.justification = "center",
      plot.caption = element_textbox_simple( hjust = 100, color = "#808080", size = 10 , margin = margin(t = -1)) 
    )
  )
  
     
       
  



saveRDS(plot_barchart_fin, file = "DFC_app/data/plot_barchart_fin.RDS")
saveRDS(plot_barchart_large, file = "DFC_app/data/plot_barchart_large.RDS")


saveRDS(EFE_1_large, file = "DFC_app/data/EFE_1_large.RDS")
saveRDS(EFE_1_fin, file = "DFC_app/data/EFE_1_fin.RDS")
saveRDS(liste_secteur2, file = "DFC_app/data/liste_secteur2.RDS")
saveRDS(liste_taille2, file = "DFC_app/data/liste_taille2.RDS")


saveRDS(source, file = "DFC_app/data/source.RDS")
saveRDS(caption_part_1, file = "DFC_app/data/caption_part_1.RDS")


saveRDS(choix_commerce, file = "DFC_app/data/choix_commerce.RDS")

saveRDS(choix_industrie, file = "DFC_app/data/choix_industrie.RDS")

saveRDS(choix_service, file = "DFC_app/data/choix_service.RDS")



saveRDS(colors, file = "DFC_app/data/colors.RDS")

saveRDS(colors_large, file = "DFC_app/data/colors_large.RDS")

saveRDS(colors_fin, file = "DFC_app/data/colors_fin.RDS")


saveRDS(theme_fin, file = "DFC_app/data/theme_fin.RDS")

saveRDS(theme_large, file = "DFC_app/data/theme_large.RDS")


