

library(readr)
library(ggmap)
library(viridis)
library(shinyWidgets)
library(DT)
library(shinydashboardPlus)
library(bootstrap)

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

#Lecture fichier : 

#1- fichier indicateur
UE <- read_parquet("data/Carte_UE.parquet", 
                   col_types = cols(tx_form = col_number(),heurstag = col_number(),
                                    tx_tpf = col_number()))

 



UE$tx_tpf <-as.numeric(UE$tx_tpf)
UE$tx_form <-as.numeric(UE$tx_form)
UE$heurstag <-as.numeric(UE$heurstag)

UE$heurstag[is.na(UE$heurstag)] <- sample(x = 1:100)
UE$tx_tpf[is.na(UE$tx_tpf)] <- 0
UE$tx_form[is.na(UE$tx_form)] <-0
UE$tx_acc1[is.na(UE$tx_acc1)] <- 0




#2- fichier Shp pour carte 

Monde <- st_read("data/CNTR_RG_10M_2020_4326.shp", quiet = TRUE)



UE[is.na(UE$taille),"taille"]<-"Ensemble"

UE[is.na(UE$secteur),"secteur"]<-"ensemble des secteurs"
#creation base Europe par merge de Monde et UE
europe <-sp::merge(Monde,UE , by.x = "NAME_FREN", by.y = "NAME_FREN")

#3 création liste pour menu déroulants

UE_nodupkey <- UE %>% distinct(secteur, .keep_all = TRUE)
liste_secteur <- na.omit(UE_nodupkey$secteur)

liste_secteur <- as.list(sort(liste_secteur))

ensemble_liste_secteur = list('ensemble des secteurs')
liste_secteur[4] <- NULL
liste_secteur2 <- c(ensemble_liste_secteur, liste_secteur)


UE_nodupkey_taille <- UE %>% distinct(taille, .keep_all = TRUE)
liste_taille <- na.omit(UE_nodupkey_taille$taille)
liste_taille <- as.list(sort(liste_taille))

ensemble_liste_taille = list('Ensemble')

liste_taille[4] <- NULL
liste_taille2 <- c(ensemble_liste_taille, liste_taille)







#CREATION DE LA BASE

filtre_UE <- reactive({
  europe %>% filter(taille==input$taille & secteur==input$secteur_bis) })



#4 FONCTION DE CREATION CARTE


caption <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Ensemble de la Génération 2017",
  "<br>",
  '<span style="color:#008B99;">Source : </span>',
  "Céreq, enquête Génération 2017 à trois ans."
)


GENE_CARTE <- function( DF, nom_colonne) {
  
  
 
  
#CREATION DE L ECHELLE
  X=1/6

bins<- c( 0, quantile(DF$nom_colonne,X),quantile(DF$nom_colonne,X*2),quantile(DF$nom_colonne,X*3),quantile(DF$nom_colonne,X*4),max(DF$nom_colonne) )

#PALETTE DE COULEUR EN FCT DE L ECHELLE
pal <- colorBin("YlOrRd", domain =DF$nom_colonne, bins = bins)
class(europe)

labels <- sprintf(
  "<strong>%s</strong><br/>%g %%",
  DF$NAME_FREN.x, DF$nom_colonne
) %>%
  lapply(htmltools::HTML)

  
 #LA CARTE 
  leaflet(DF) %>%
    setView(lng=12.766277, lat=55,zoom = 3,8) %>%
    # fitBounds(-20,65,20,40) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=DF,fillColor = ~pal(nom_colonne),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(weight = 1,5,
                                             color = "Black",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
    addLegend( pal= pal, values = ~nom_colonne,
               title = paste("Pour la taille ", taille, "et le secteur ",secteur_bis),
               labFormat = labelFormat(suffix =   "%"),
               opacity = 1 )
  
  
}


DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )}

