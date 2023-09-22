library(readr)
library(ggmap)
library(viridis)
library(shinyWidgets)
library(DT)
library(shinydashboardPlus)
library(bootstrap)
library(sfheaders)
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
UE <- read_parquet("data/Base_UE.parquet")

#2 - lecture du fichier shp pour la carte 
Monde <- st_read("data/CNTR_RG_10M_2020_4326.shp", quiet = TRUE)


#3 - merge du fichier carte et de la base pour obtenir la base Europe
Europe <- sp::merge(Monde , UE, by.x = "NAME_FREN", by.y = "NAME_FREN")

#4 - Nettoyage de la base 
Europe <- transform(Europe, tx_form = as.numeric(tx_form),
                    heurstag = as.numeric(heurstag),
                    tx_tpf = as.numeric(tx_tpf),
                    tx_acc1 = as.numeric(tx_acc1))

Europe$heurstag[is.na(Europe$heurstag)] <- 0
Europe$tx_tpf[is.na(Europe$tx_tpf)] <- 0
Europe$tx_form[is.na(Europe$tx_form)] <- 0
Europe$tx_acc1[is.na(Europe$tx_acc1)] <- 0

Europe$CNTR_ID <- NULL
Europe$CNTR_NAME <- NULL
Europe$NAME_ENGL <- NULL
Europe$SVRG_UN <- NULL
Europe$CAPT <- NULL
Europe$EU_STAT <- NULL
Europe$EFTA_STAT <- NULL
Europe$CC_STAT <- NULL
Europe$NAME_GERM <- NULL
Europe$FID <- NULL
Europe$iso3_code <- NULL


# Création liste pour menus déroulants

Europe_nodupkey <- Europe %>% distinct(secteur, .keep_all = TRUE)
liste_secteur <- na.omit(Europe_nodupkey$secteur)

liste_secteur <- as.list(sort(liste_secteur))

ensemble_liste_secteur = list('Ensemble des activités')
liste_secteur[4] <- NULL
liste_secteur2 <- c(ensemble_liste_secteur, liste_secteur)


Europe_nodupkey_taille <- Europe %>% distinct(taille, .keep_all = TRUE)
liste_taille <- na.omit(Europe_nodupkey_taille$taille)
liste_taille <- as.list(sort(liste_taille))

ensemble_liste_taille = list('Total','10 à 49','50 à 249','250 ou plus')

liste_taille[4] <- NULL
liste_taille2 <- c(ensemble_liste_taille)


DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )}

