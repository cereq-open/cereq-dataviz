
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
      tags$h1("Indicateur Européen : ")
    )
  ),
  

          
          
          #Bouton TAILLE SECTEUR
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
    #TAUX ACCES
    column(
      width =5,
      div( style = "max-width:800px; margin-left:auto; margin-right:auto;",
      class = "custom-border-box",
      tags$p(
        class = "stat_info",
        tags$span(
          style = "color: #008B99;font-size: 30px;font-style: bold",
          "Taux acces"),
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99;",
          title = "Le taux d'acces est blallalalalalallalalalalalalalalal"),
        leafletOutput("taux_acces"),
        uiOutput("legende1"),
        tags$head(tags$style("#taux_acces{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
      ))),
    
    # ESPACE 
    column(
      width =1),
    
    #PART D ENTREPRISE FORMATRICE 
    
    column(
      width =5,
      class = "custom-border-box",
      tags$p(
        class = "stat_info",
        tags$span(
          style = "color: #008B99;font-size: 30px;font-style: bold",
          "Part d'entreprise formatrice"),
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99;",
          title = "la part d'entreprise formatrice blallalalalalallalalalalalalalalal"),
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
      width =5,
      div(  style = "max-width:800px; margin-left:auto; margin-right:auto;",
      class = "custom-border-box",
      tags$p(
        class = "stat_info",
        tags$span(
          style = "color: #008B99;font-size: 30px;font-style: bold",
          "Taux de participation financière"),
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99;",
          title = "Le taux de participation blallalalalalallalalalalalalalalal"),
        leafletOutput("TPF"),
        uiOutput("legende3"),
        tags$head(tags$style("#TPF{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
      ) )),
    
    
    #NOMBRE D HEURE DE COURS ET STAGES
    column(
      width =1),
    column(
      width =5,
      class = "custom-border-box",
      tags$p(
        class = "stat_info",
        tags$span(
          style = "color: #008B99;font-size: 30px;font-style: bold",
          "Nombre d'heure de cours et stages"),
        tags$i(
          class = "fas fa-info-circle",
          style = "color: #008B99;",
          title = "Le nombre d'heure est blallalalalalallalalalalalalalalal"),
        
        leafletOutput("nb_h"),
        uiOutput("legende4"),
        tags$head(tags$style("#nb_h{color: #008b99;
                                 font-size: 20px;
                                 font-style: bold;
                                }" ) )
      ) )))
  
  

          
        
