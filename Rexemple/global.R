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

# Load data --------------------------------------------------------------------






plot_barchart_moins10<- readRDS(file = "data/plot_barchart_moins10.RDS")
plot_barchart_plus10<- readRDS(file = "data/plot_barchart_plus10.RDS")

EFE_1<-readRDS( file = "data/EFE_1.RDS")


liste_secteur2 <- readRDS( file = "data/liste_secteur2.RDS")

liste_taille2 <- readRDS( file = "data/liste_taille2.RDS")

source <- readRDS( file = "data/source.RDS")
caption_part_1 <- readRDS( file = "data/caption_part_1.RDS")
caption_part_10plus<- readRDS( file = "data/caption_part_10plus.RDS")
concat_value <- readRDS( file = "data/concat_value.RDS")

colors <- readRDS( file = "data/colors.RDS")
colors2 <- readRDS( file = "data/colors2.RDS")








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
      legend.justification = "center",
      legend.box.spacing = unit(0, "pt"),
      legend.text = element_text(size = 11, face = "plain")
    )
}


# Download data ----------------------------------------------------------------

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}
