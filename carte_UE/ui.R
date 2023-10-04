url_link<-"https://www.linkedin.com/shareArticle?mini=true&url=https://cereq-data-visualisation.shinyapps.io/carte_UE/&title=dataviz"





ui <- 
  
  fluidPage(
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
        align = "left",
        width = 12,
        div(
          style = "text-align:right;",
          tags$img(src = "logo-cereq.svg"),
          #tags$p(
          #  style = "font-size:14px;",
          #  "Données : ",
          # tags$img(src = "logo-generation.png")
          # )
        )),
      
      column(
        
        width=12,
        div( style = "text-align:right;",
             tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
             DownloadButton("downloadData", ".xlsx"),
             
             actionButton("downloadPDF", ".pdf", onclick = "window.print();"),
             
             actionButton("linkedin_share",
                          label = "Linkedin",
                          icon = icon("linkedin"),
                          onclick = sprintf("window.open('%s')", url_link))
             
        ))
    ),
    
    br(),
    fluidRow(
      column(
        width = 12,
        tags$h1("Indicateurs européens")
      )
    ),
    br(),
    fluidRow(column (width=6,
                     radioGroupButtons(
                       inputId = "annee",
                       label = h1("Choisir l'année :"), 
                       choices = c("2010", "2015", "2020"),
                       status = "primary"
                     )),
                     
                     
                    
    
    column(
      
      width = 6,
      div(id="masque",
          style = "text-align:left;",
          radioGroupButtons(
            label=h1("Choisir le secteur ou la taille :"),
            inputId = "taille_secteur",
            
            choices = c( "Secteur","Taille"),
            status = "primary",
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          )
      ))),
    
    #Bouton TAILLE SECTEUR
    
    gfontHtmlDependency(family = "Arimo"),
    fluidRow(
      column(
        width = 6,     
        htmlOutput("phrase")),
      column(
        width = 3,
        
        shinyjs::useShinyjs(),
        
        div(id="secteur",
            pickerInput(
              width = "fit",
              inline = TRUE,
              label = tags$h1("Secteur :"),
              inputId = "secteur_bis",
              choices = liste_secteur2,
              selected = "Ensemble des secteurs",
              
              
            ))),
      
      
      column(
        width = 3,
        shinyjs::useShinyjs(),
        
        div(id="taille",
            pickerInput(
              width = "fit",
              inline = TRUE,
              label = tags$h1("Taille :"),
              inputId = "taille",
              choices = liste_taille2,
              selected = "Ensemble des tailles"
              
              
              
            ))),
      
      
      
      br(),
      br(),
      br(),
      br(),
      
      
      fluidRow(
        #TAUX ACCES
        column(
          width =6,
          div( 
            class = "custom-border-box",
            tags$p(
              class = "texte-stat-info",
              
              "Taux d'accès à la formation continue"
              ,
              tags$i(
                class = "fas fa-info-circle",
                style = "color: #008B99; font-size: 16px;",
                title = "Les secteurs"
              )
            ),
            
            
            
            
            leafletOutput("taux_acces"),
            uiOutput("legende1"),
            tags$head(tags$style("#taux_acces{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
          )),
        
        # ESPACE 
        # column(
        #  width =1),
        
        #PART D ENTREPRISE FORMATRICE 
        
        column(
          width =6,
          div(
            class = "custom-border-box",
            tags$p(
              class = "texte-stat-info",
              
              "Part d'entreprises formatrices"
              ,
              tags$i(
                class = "fas fa-info-circle",
                style = "color: #008B99; font-size: 16px;",
                title = "Les secteurs"
              )
            ),
            
            
            
            
            
            leafletOutput("part_form"),
            uiOutput("legende2"),
            tags$head(tags$style("#part_form{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
          ) )),
      
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      ),
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      ),
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      ),
      
      #TAUX DE PARTICIPATION FINANCIERE
      
      fluidRow(
        column(
          width =6,
          div(  
            class = "custom-border-box",
            tags$p(
              class = "texte-stat-info",
              
              "Taux de participations financières des entreprises"
              ,
              tags$i(
                class = "fas fa-info-circle",
                style = "color: #008B99; font-size: 16px;",
                title = "Les secteurs"
              )
            ),
            leafletOutput("TPF"),
            uiOutput("legende3"),
            tags$head(tags$style("#TPF{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
          ) ),
        
        
        #NOMBRE D HEURE DE COURS ET STAGES
        #  column( width =1),
        column(
          width =6,
          div(
            class = "custom-border-box",
            tags$p(
              class = "texte-stat-info",
              
              "Nombre d'heures de cours et stages"
              ,
              tags$i(
                class = "fas fa-info-circle",
                style = "color: #008B99; font-size: 16px;",
                title = "Les secteurs"
              )
            ),
            
            leafletOutput("nb_h"),
            uiOutput("legende4"),
            tags$head(tags$style("#nb_h{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
          ) )),
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      ),
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      ),
      #saut de ligne qui marche pas
      fluidRow(
        column(width= 12,  class = "h1")
      )))





