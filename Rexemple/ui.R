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



EFE_1 <- read_parquet("data/indicateur_JC_bis.parquet")



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
    column(
      align = "right",
      width = 6,
      div(
        style = "text-align:right;",
        tags$img(src = "logo-cereq.svg"),
        tags$p(
          style = "font-size:14px;",
          "Données : ",
          tags$img(src = "logo-generation.png")
        )
      ),
      tags$img(src = "logo-download.svg", height = "50px", width = "50px"),
      tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
      DownloadButton("downloadData", ".xlsx"),
      actionButton("downloadPDF", ".pdf", onclick = "window.print();")
    )
  ),
  
  br(),
  fluidRow(
    column(
      width = 12,
      tags$h1("Chiffres clefs par secteur :")
    )
  ),
  
  br(),
  fluidRow(
    tags$head(
      tags$style(type = "text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row; }")
    ),
    column(
      width = 12,
      tags$table(
        width = "100%",
        tags$tr(
          width = "100%",
          tags$td(
            width = "40%",
            div(style = "font-size:24px; text-align: center;", "Choisir le secteur :")
          ),
          tags$td(
            width = "60%",
            pickerInput(
              width = "fit",
              inputId = "secteur",
              choices = liste_secteur2,
              choicesOpt = list(
                style = c(
                  "font-weight: bold;"
                  
                )
              )
            )
          )
        )
      )
    ),
  
    #TAUX ACCES
    br(),
    fluidRow(
      column(
        width = 6,
        div(
          class = "custom-border-box",
          tags$p(
            class = "stat_info",
            tags$span(
              style = "color: #008B99;font-size: 30px;font-style: bold",
              "Taux acces"
            ),
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99;",
              title = "Le taux d'acces est blallalalalalallalalalalalalalalal"
            )
          ),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            girafeOutput("plot_tx_acc1", height = NULL)
          )
        )
      ), #PART FROMATRICE
      br(),
      column(
        width = 6,
        div(
          class = "custom-border-box",
          tags$p(
            class = "stat_info",
            tags$span(
             style = "color: #008B99;font-size: 30px;font-style: bold",
              "Part d'entreprise formatrice"
            ),
            tags$i(
              class = "fas fa-info-circle",
              style = "color: #008B99;",
              title = "Les secteurs"
            )
          ),
          div(
            style = "max-width:800px; margin-left:auto; margin-right:auto;",
            girafeOutput("plot_part_formatrice", height = NULL)
          )
        )
      )
    )),
    #TPF
    br(),
    fluidRow(
    column(
      width = 6,
      div(
        class = "custom-border-box",
        tags$p(
          class = "stat_info",
          tags$span(
            style = "color: #008B99;font-size: 30px;font-style: bold",
            "Taux de participation financière"
          ),
          tags$i(
            class = "fas fa-info-circle",
            style = "color: #008B99;",
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
        class = "stat_info",
        tags$span(
          style = "color: #008B99;font-size: 30px;font-style: bold",
          "Moyenne des heures de stage"
        ),
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99;",
          title = "Les secteurs "
        )
      ),
      div(
        style = "max-width:800px; margin-left:auto; margin-right:auto;",
        girafeOutput("plot_H_stage", height = NULL)
      )
    )
  
  ))
,
br(),
fluidRow(
  column(
    width = 12,
    tags$h1("TROUVER TITRE POUR LES TOP3")
  )
),
br(),
gfontHtmlDependency(family = "Arimo"),
fluidRow(
  column(
    width = 6,
    
      pickerInput(
        width = "fit",
        inline = TRUE,
        label = tags$h1("Choisir le secteur :"),
        inputId = "secteur_bis",
        choices = liste_secteur2,
        selected = "Ensemble des secteurs"
        
      )),
    column(
      width = 6,
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
    width =3,
    class = "custom-border-box",
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #00000;font-size: 25px;font-style: bold",
        "Trois principaux domaines de formation :"),
  uiOutput("domaine", style="#008b99"),
  tags$head(tags$style("#domaine{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
  ) ),
  column(
    width =1
  ),
  column(
    width =3,
    class = "custom-border-box",
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #000000;font-size: 25px;font-style: bold",
        "Trois principaux freins à la formation :"),
      
    uiOutput("frein", style="#008b99"),
    tags$head(tags$style("#frein{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
  ) ),
  
  column(
    width =1
  ),
  
  column(
    width =3,
    class = "custom-border-box",
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #000000;font-size: 25px;font-style: bold",
        "Trois principales raisons de non formation :"),
  uiOutput("raison", style="#008b99"),
  tags$head(tags$style("#raison{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                 }" ))
  )  )
        
      ))
        
        
   

