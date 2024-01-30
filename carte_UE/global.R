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

Europe <- readRDS("data/Europe.RDS")
liste_secteur2 <- readRDS("data/liste_secteur2.RDS")
liste_taille2 <- readRDS("data/liste_taille2.RDS")

Europe$taille <- fct_recode(Europe$taille,
                             "10 à 49 salariés" = "10 à 49")


Europe$taille <- fct_recode(Europe$taille,
                            "50 à 249 salariés" = "50 à 249")

Europe$taille <- fct_recode(Europe$taille,
                            "250 salariés ou plus" = "250 ou plus")


#TOOLTIP
concat_value <- function(df, nom_colonne) {
  if (nom_colonne != "heurstag" & nom_colonne!="heurstag_sal") {
    df["taux_str"] <- paste0(df[[nom_colonne]], "%")
  } else {
    df["taux_str"] <- paste0(df[[nom_colonne]], "h")
  }
  
  df[["tooltip_value"]] <- paste0(df[["secteur"]], " : ", df[["taux_str"]], df[["taille"]] )
  
  return(df)
}


###########################################################################################
token<-"8C2gU8pSutHVOkE3id0L7olcMCYjc2Aoh3GdmmneYDBw6bX4m1gBzw9t3JMM0EU9"

DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )}

