# Load packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(ggiraph)
  library(gdtools)
  library(arrow)
  library(readxl)
  library(forcats)
})

# Define Global ----------------------------------------------------------------

options(shiny.useragg = TRUE)

set_girafe_defaults(
  opts_hover_inv = opts_hover_inv(css = "stroke-width:2px; opacity:.5;"),
  opts_hover = opts_hover(css = ""),
  opts_selection = opts_selection(type = "none"),
  opts_toolbar = opts_toolbar(saveaspng = FALSE)
)

if (!gdtools::font_family_exists("Arimo")) {
  systemfonts::register_font(
    name = "Arimo",
    plain = "www/arimo/fonts/arimo-v28-latin_latin-ext-regular.ttf",
    bold = "www/arimo/fonts/arimo-v28-latin_latin-ext-700.ttf",
    italic = "www/arimo/fonts/arimo-v28-latin_latin-ext-italic.ttf",
    bolditalic = "www/arimo/fonts/arimo-v28-latin_latin-ext-700italic.ttf"
  )
}

linebreaks <- function(n){HTML(strrep(br(), n))}


EFE_1 <- readRDS("data/EFE_1.RDS")
EFE_1_fin <- readRDS("data/EFE_1_fin.RDS")
EFE_1_large <- readRDS("data/EFE_1_large.RDS")

liste_secteur2 <- readRDS("data/liste_secteur2.RDS")
liste_taille2 <- readRDS("data/liste_taille2.RDS")
source <- readRDS("data/source.RDS")
caption_part_1  <- readRDS("data/caption_part_1.RDS")

colors2 <- readRDS("data/colors2.RDS")
colors  <- readRDS("data/colors.RDS")
colors_large  <- readRDS("data/colors_large.RDS")
colors_fin  <- readRDS("data/colors_fin.RDS")


choix_industrie <-readRDS("data/choix_industrie.RDS")


choix_service<-readRDS("data/choix_service.RDS")


choix_commerce<-readRDS("data/choix_commerce.RDS")

plot_barchart_fin<-readRDS("data/plot_barchart_fin.RDS")
plot_barchart_large<-readRDS("data/plot_barchart_large.RDS")
theme_large <- readRDS("data/theme_large.RDS")
theme_fin <- readRDS("data/theme_fin.RDS")

# Couleurs des barplots




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



#TITRE
generateTitle <- function(title, infobulle_str = NULL) {
  
  tagList(
    tags$p(
      class = "texte-stat-info",
      title,
      if (!is.null(infobulle_str)) {
        tags$i(
          style = "color: #008B99; font-size: 16px;",
          class = "fas fa-info-circle",
          title = infobulle_str,
        )
      }
    )
  )
}














top3 <-function(y1 , y2, y3) {
  str1<- paste(y1 , y2, y3 , sep='<br/>')
  HTML(paste(str1))
}

plot_only_legend <- function(df) {
  ggplot(data = df, aes(x = taille, y = tx_acc , fill = secteur)) +
    geom_col_interactive(mapping = aes(data_id = taille)) +
    scale_fill_manual(values = colors) +
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.box.spacing = unit(0, "pt"),
      legend.text = element_text(size = 11, face = "plain")
    )
}


# Download data ----------------------------------------------------------------

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}
