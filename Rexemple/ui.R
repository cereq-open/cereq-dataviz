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
          class = "texte-stat-info",
          paste0("Parmi les entreprises formatrices en cours et stages","<br>",
            "Les trois premiers domaines de formation"),
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Liste des domaines de formation :
              1.	Des compétences générales en technologie de l’information et de la communication
2.	Des compétences spécialisées en technologie de l’information et de la communication
3.	Des compétences en management
4.	Des compétences de travail en équipe
5.	Des compétences commerciales
6.	Une capacité à résoudre des problèmes
7.	Des compétences administratives
8.	Des compétences en langues étrangères
9.	Des compétences techniques et spécifiques à un métier
10.	Des compétences en communication écrite ou orale
11.	Savoir lire, écrire et compter
12.	Des compétences liées à la transition écologique
13.	D’autres compétences non mentionnées   
              "
            )),
        htmlOutput("domaine"),
        tags$head(tags$style("#domaine{color: #00000;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
      ) ),
    # column(width =1),
    column(
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
       
            
            "Les raisons qui ont limité l'effort de formation",
          
          
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "1.	Des compétences générales en technologie de l’information et de la communication
2.	Des compétences spécialisées en technologie de l’information et de la communication
3.	Des compétences en management
4.	Des compétences de travail en équipe
5.	Des compétences commerciales
6.	Une capacité à résoudre des problèmes
7.	Des compétences administratives
8.	Des compétences en langues étrangères
9.	Des compétences techniques et spécifiques à un métier
10.	Des compétences en communication écrite ou orale
11.	Savoir lire, écrire et compter
12.	Des compétences liées à la transition écologique
13.	D’autres compétences non mentionnées                       
"
          )),
          
          htmlOutput("frein"),
          tags$head(tags$style("#frein{color: #00000;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
        ) ),
    
    # column(   width =1 ),
    
    column(
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
      
            
            "Les trois premières raisons de non formation",
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Pour les entreprises non formatrices"
          )),
        htmlOutput("raison"),
        tags$head(tags$style("#raison{color: #00000;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
          ))
    
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
            
            "Taux d'accès"
            ,
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Part de salariés formés"
            )
          ),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            girafeOutput("plot_tx_acc", height = NULL)
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
           
              "Part d'entreprises formatrices en cours et stages"
            ,
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Part d'entreprises qui ont organisé au moins un cours et stages pour au moins un de leurs salariés"
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
            title = "Part de la masse salariale consacrée aux dépenses de formation"
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
        
        "Durée moyenne par stagiaire"
        ,
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99; font-size: 16px;",
          title = "Nombre d'heures de cours et stages par stagiaire"
        )
      ),
      div(
        style = "max-width:800px; margin-left:auto; margin-right:auto;",
        girafeOutput("plot_H_stage", height = NULL)
      )
    )
  
  ))

)
        

# Espérance : I = Nombre d'heures de cours et stages par salarié;
# Taux d'entreprise formatrice toutes formes de formation. 
#I = Part d'entreprises qui ont organisé au moins une formation (cours et stages, formation en situation de trav, colloque...) pour au moins un de leurs salariés


