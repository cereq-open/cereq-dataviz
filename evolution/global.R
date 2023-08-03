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
})

# Load data --------------------------------------------------------------------

tab_evolution <- read_parquet("data/tab_evolution.parquet") %>%
  mutate(
    Année = factor(Année),
    Libelle_Menu = str_trim(Libelle_Menu) # Supprime les espaces en début et fin de chaînes de caractères
    ) %>%
  mutate(
    Année = case_when(
      Année == "2010" ~ "Sortis en 2010",
      Année == "2013" ~ "Sortis en 2013",
      Année == "2017" ~ "Sortis en 2017",
      TRUE ~ Année)
  )

tab_variables_evolution <- read_excel("~/Desktop/Work/GitHub/cereq-dataviz/evolution/data/variables EVOLUTIONS.xlsx")

# Supprime les valeurs manquantes
tab_evolution <- na.omit(tab_evolution)

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

niveau_facteur <- c(
  "Ensemble des sortants", "Non diplômés ", "Diplômés du secondaire",
  "Diplômés du supérieur court", "Diplômés du supérieur long", "Non diplômés"
)

vec_indicateurs <- unique(tab_evolution$Libelle_Menu)

# Couleurs des barplots
colors <- c("#F8AC00", "#EF5350", "#008B99")

source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Céreq, enquêtes Génération 2010, Génération 2013 et Génération 2017."
  )

caption_part_1 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Ensemble des sortants.",
  "<br>",
  source
  )

caption_part_2 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Ensemble des sortants en emploi trois ans après leur sortie de formation initiale.",
  "<br>",
  source
  )

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

concat_value <- function(df, nom_colonne) {
  if (nom_colonne != "revenu_travail") {
    df["taux_str"] <- paste0(df[[nom_colonne]], "%")
  } else {
    df["taux_str"] <- paste0(df[[nom_colonne]], " €")
  }

  df[["tooltip_value"]] <- paste0(df[["Année"]], " : ", df[["taux_str"]])

  return(df)
}

plot_barchart <- function(df, y_col, caption_texte, legend = NULL) {
  DT <- concat_value(df, y_col)

  ggplot(data = DT, aes(x = Année, y = !!sym(y_col), fill = Année)) +
    geom_col_interactive(mapping = aes(data_id = Année, tooltip = tooltip_value)) +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      color = "white"
    ) +
    scale_fill_manual(values = colors) +
    labs(caption = caption_texte)
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo"),
    text = element_text(size = 11, family = "Arimo"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.title = element_blank(),
    legend.position = "none",
    plot.caption = element_textbox_simple(
      hjust = 0,
      color = "#C0C0C2",
      size = 12
    )
  )
)

# Download data ----------------------------------------------------------------

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}