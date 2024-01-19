library(readr)
library(ggmap)
library(viridis)
library(shinyWidgets)
library(DT)
library(shinydashboardPlus)

library(cli)
library(rsconnect)
library(leaflet)
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
library(NestedMenu)

url <-"https://twitter.com/intent/tweet?text=La%20super%20dataviz%20du%20cereq&url=https://cereq-data-visualisation.shinyapps.io/Rexemple/"
url_link<-"https://www.linkedin.com/shareArticle?mini=true&url=https://cereq-data-visualisation.shinyapps.io/Rexemple/&title=dataviz"




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
      width = 8,
      headerPanel("Choisir un secteur"),
      
      # Sidebar with a slider input for number of observations
   
        uiOutput("Secteur_large"),
        uiOutput("Secteur_fin")
      
      
      
    )
    ,
    
    column(
      width = 3,
      headerPanel("Choisir une taille"),
      
      # Sidebar with a slider input for number of observations
      
      uiOutput("taille"),
 
      
      
      
    )),
  
  br(),
  
  fluidRow(
    column(
      
      width =4,
      
      div(
        class = "custom-border-box",
        tags$p(
          class= "texte-stat-info",
          htmlOutput("titre_formatrice_CS"),
          htmlOutput("sous_titre_formatrice_CS"),
          tags$p(
            class = "texte-stat-info",
            
            "Les principaux domaines de formation",
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Liste des domaines de formation :
- Technologie de l’information et de la communication
- Management
- Travail en équipe
- Commercial
- Résolution de problèmes 
- Administratif 
- Langues étrangères
- Techniques spécifiques à un métier 
- Communication écrite ou orale
- Savoir lire écrire compter 
- Transition écologique
- Autres domaines ")),
          htmlOutput("domaine"),
          tags$head(tags$style("#domaine{color: #00000;
                                 font-size: 16px;
                                 font-style: bold;
                                }" ) )))),
    
    column(
      width =4,
      div(
        class = "custom-border-box",
        tags$p(
          class= "texte-stat-info",
          htmlOutput("titre_formatrice"),
          htmlOutput("sous_titre_formatrice"),
          tags$p(
            class = "texte-stat-info",
            
            "Les principales raisons qui ont limité l'effort de formation",
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99; font-size: 16px;",
              title = "Liste des raisons limitantes :
- Effort de formation approprié aux besoins
- Recrutement de personnes ayant les qualifications et compétences requises
- Difficulté d’évaluation des besoins en formation
- Absence de cours ou stages adaptés aux besoins 
- Coûts des cours ou stages trop élevés 
- Priorité donnée à l’alternance 
- Efforts de formation importants antérieurment 
- Charge de travail trop lourde ou manque de temps 
- Crise sanitaire
- Autres raisons ")),
          
          htmlOutput("frein"),
          tags$head(tags$style("#frein{color: #00000;
                                 font-size: 16px;
                                 font-style: bold;
                                }" ) ))))
    ,
    
    
    
    column(
      width =4,
      div(
        class = "custom-border-box",
        htmlOutput("titre_non_formatrice"),
        htmlOutput("sous_titre_non_formatrice"),
        tags$p(
          class = "texte-stat-info",
          
          
          "Les principales raisons de non formation",
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Liste des raisons de non formation :
- Qualifications et compétences appropriées aux besoins de l’entreprise
- Recrutement de personnes ayant les qualifications et compétences requises 
- Difficulté d’évaluation des besoins en formation 
- Absence de cours ou stages adaptés aux besoins 
- Coûts des cours ou stages trop élevés 
- Priorité donnée à la formation initiale 
- Efforts de formation importants antérieurement 
- Charge de travail trop lourde ou manque de temps 
- Crise sanitaire
- Autres raisons 
"
          )),
        
        htmlOutput("raison"),
        tags$head(tags$style("#raison{color: #00000;
                                 font-size: 16px;
                                 font-style: bold;
                                }" ) )
      )
      
    )),
  
  
  br(),
  fluidRow(
    column(
      width = 12,
      align = "left",
      
      
      
      htmlOutput("titre_secteur"))
  )
  ,
  
  
  
  #PART FORMATRICE COURS ET STAGE 
  br(),
  fluidRow(
    
  
    
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
          girafeOutput("plot_part_formatrice_courses", height = NULL)
        )
      )
    ), 

    
    #PART FROMATRICE TOUTES FORMES 

    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Part d'entreprises formatrices cours et stages et autres formes"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Part d'entreprises qui ont organisé au moins une formation pour au moins un de leurs salariés"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_part_formatrice_tte", height = NULL)
        )
      )
    )
 
  ),
  br(),
  #TPF

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
    
    
    #TAUX ACCES

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
    )
  ),
  br(),
  #DURE MOYENNE PAR STAGIAIRE

  fluidRow(
   
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Durée moyenne de formation par stagiaire"
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
      
    ),
    #NOMBRE D HEURE DE FORMATION PAR SALARIE
 
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "texte-stat-info",
          
          "Durée moyenne de formation par salarié"
          ,
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99; font-size: 16px;",
            title = "Nombre d'heures de cours et stages par salarié"
          )
        ),
        div(
          style = "max-width:800px; margin-left:auto; margin-right:auto;",
          girafeOutput("plot_heure_stage_sal", height = NULL)
        )
      )
      
    ))
  
)


# Espérance : I = Nombre d'heures de cours et stages par salarié;
# Taux d'entreprise formatrice toutes formes de formation. 
#I = Part d'entreprises qui ont organisé au moins une formation (cours et stages, formation en situation de trav, colloque...) pour au moins un de leurs salariés


