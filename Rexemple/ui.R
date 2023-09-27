library(readr)
library(ggmap)
library(viridis)
library(shinyWidgets)
library(DT)
library(shinydashboardPlus)

library(cli)
library(rsconnect)
library(leaflet)
library(readr)
library(tidyverse)
library(sf)
library(arrow)
library(reactable)
library(bslib)
library(htmltools)
library(ggiraph)
library(gdtools)
library(ggtext)
library(ggthemes)
library(glue)
library(gfonts)

url <-"https://twitter.com/intent/tweet?text=La%20super%20dataviz%20du%20cereq&url=https://cereq-data-visualisation.shinyapps.io/Rexemple/"
url_link<-"https://www.linkedin.com/shareArticle?mini=true&url=https://cereq-data-visualisation.shinyapps.io/Rexemple/&title=dataviz"

EFE_1 <- read_parquet("data/base_JC_2709.parquet")

EFE_1$secteur_ensemble <-as.character(EFE_1$secteur_ensemble)






EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "1"] <- "Ensembles des secteurs"
EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "0"] <- "Secteur choisi"

EFE_1_nodupkey <- EFE_1 %>% distinct(secteur, .keep_all = TRUE)

ensemble = list('Ensemble des secteurs')
liste_secteur <- as.list(sort(EFE_1_nodupkey$secteur))
liste_secteur[17] <- NULL
liste_secteur2 <- c(ensemble, liste_secteur)


EFE_1_nodupkey_taille <- EFE_1 %>% distinct(taille, .keep_all = TRUE)

ensemble_liste_taille = list('Ensemble')
liste_taille <- as.list((EFE_1_nodupkey_taille$taille))
liste_taille[1] <- NULL
liste_taille2 <- c(ensemble_liste_taille, liste_taille)


ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # CSS personnalisé
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"), # Librairie font-awesome
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"), # Web Font Loader
    tags$script(
      "
  WebFont.load({
    google: {
      families: ['Arimo']
    }
  });
  "
    ) # Pour que Arimo soit toujours disponible dans le navigateur de l'utilisateur
  ),
  
 
  br(),
  gfontHtmlDependency(family = "Arimo"),
 
  br(),
  fluidRow(
    column(width=6),
    column(
      align = "right",
      width = 6,
      div(
        style = "text-align:right;",
        tags$img(src = "logo-cereq.svg")#,
        #tags$p(
          #style = "font-size:14px;",
         # "Données : ",
         # tags$img(src = "logo-generation.png")
       # )
      ))),
    fluidRow(column( width = 12, class='h1')),
    fluidRow(
      column(
        align ="right", 
        width = 12,
     # tags$img(src = "logo-download.svg", height = "50px", width = "50px"),
      tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
      DownloadButton("downloadData", ".xlsx"),
      actionButton("downloadPDF", ".pdf", onclick = "window.print();"),
     actionButton("linkedin_share",
                  label = "Linkedin",
                  icon = icon("linkedin"),
                  onclick = sprintf("window.open('%s')", url_link))
      )
      ),
  
  
 
  br(),
  gfontHtmlDependency(family = "Arimo"),
  fluidRow(
    column(
      width = 4,
      pickerInput(
        width = "fit",
        inline = TRUE,
        label = tags$h1("Choisir le secteur :"),
        inputId = "secteur",
        choices = liste_secteur2,
        options= list( `live-search` = TRUE),
        choicesOpt = list(
          style = c(
            "font-weight: bold;"
            
          )
        )))
      ,
    
    column(
      width = 3,
      pickerInput(
        width = "fit",
        inline = TRUE,
        label = tags$h1("Choisir la taille :"),
        inputId = "taille",
        choices = liste_taille2,
        selected = "Ensemble"
        
      ))),
  
  br(),
 
  
  fluidRow(
    column(
      
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$h1(
           
            "Les trois premiers domaines de formation :"),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            uiOutput("domaine", style="#008b99")),
          tags$head(tags$style("#domaine{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
        ) )),
    # column(width =1),
    column(
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$h1(
            
            "Les trois premiers freins à la formation :"),
          
          htmlOutput("frein"),
          tags$head(tags$style("#frein{color: #00000;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
        ) )),
    
    # column(   width =1 ),
    
    column(
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$h1(
            
            "Les trois premières raisons de non formation :"),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            uiOutput("raison", style="#008b99")),
          tags$head(tags$style("#raison{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                 }" ))
        )  ))
    
  ),

  
  br(),
  fluidRow(
    column(
      width = 12,
      align = "left",
   

    
       htmlOutput("titre_secteur"))
    )
  ,
  

  
    #TAUX ACCES
    br(),
    fluidRow(
      column(
        width = 6,
        div(
          class = "custom-border-box",
          tags$p(
            class = "texte-stat-info",
            
            "Taux d'acces :"
            ,
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Les secteurs"
            )
          ),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            girafeOutput("plot_tx_acc1", height = NULL)
          )
        )
      ), 
      
      
      #PART FROMATRICE
      br(),
      column(
        width = 6,
        div(
          class = "custom-border-box",
          tags$p(
            class = "texte-stat-info",
           
              "Part d'entreprise formatrice"
            ,
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Les secteurs"
            )
          ),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            girafeOutput("plot_part_formatrice", height = NULL)
          )
        )
      )
    ),
  
  
    #TPF
    br(),
    fluidRow(
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Taux de participation financière des entreprises"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Les secteurs"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_TPF", height = NULL)
        )
      )
    
  ),
  #HEURES DE STAGES
  br(),
  column(
    width = 6,
    div(
      class = "custom-border-box",
      tags$p(
        class = "texte-stat-info",
        
        "Moyenne des heures de stages :"
        ,
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99; font-size: 16px;",
          title = "Les secteurs"
        )
      ),
      div(
        style = "max-width:800px; margin-left:auto; margin-right:auto;",
        girafeOutput("plot_H_stage", height = NULL)
      )
    )
  
  ))

)
        



