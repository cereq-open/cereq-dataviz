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

###########################################################################################
token<-"8C2gU8pSutHVOkE3id0L7olcMCYjc2Aoh3GdmmneYDBw6bX4m1gBzw9t3JMM0EU9"

DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )}

