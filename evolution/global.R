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

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

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
      TRUE ~ Année
    )
  )

tab_variables_evolution <- read_excel("data/variables EVOLUTIONS.xlsx")

# Supprime les valeurs manquantes
tab_evolution <- na.omit(tab_evolution)


vec_indicateurs <- unique(tab_evolution$Libelle_Menu)

sortis <- unique(tab_evolution$Année)

# Couleurs des barplots
colors <- c("#F8AC00", "#EF5350", "#008B99")

# Pour la hauteur et la largeur des graphiques ggiraph
hauteur_bar_chart <- 2
largeur_bar_chart <- 3

champ <- '<span style="color:#008B99;">Champ : </span>'

source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Céreq, enquêtes Génération 2010, Génération 2013 et Génération 2017"
)

caption_part_1 <- paste0(
  champ,
  "Ensemble des sortants.",
  "<br>",
  source
)

caption_part_2 <- paste0(
  champ,
  "Ensemble des sortants en emploi trois ans après leur sortie de formation initiale.",
  "<br>",
  source
)

labellize_row_i <- function(titre, infobulle_str = NULL) {
  tagList(
    tags$p(
      class = "d-inline",
      titre
    ),
    if (!is.null(infobulle_str)) {
      tags$i(
        class = "fas fa-info-circle",
        style = "font-size:18px;",
        title = infobulle_str
      )
    },
    tags$p(
      class = "d-inline",
      "selon la région de résidence à la fin des études"
    )
  )
}

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

generateStyledBlocks <- function(class, sortis) {
  tagList(
    tags$div(
      style = "display: flex; align-items: center;",
      tags$div(
        class = class,
        style = "background-color: #F8AC00;"
      ),
      tags$span(
        style = "font-size:18px;font-weight:300;",
        sortis[1]
      )
    ),
    tags$div(
      style = "display: flex; align-items: center;",
      tags$div(
        class = class,
        style = "background-color: #EF5350;"
      ),
      tags$span(
        style = "font-size:18px;font-weight:300;",
        sortis[2]
      )
    ),
    tags$div(
      style = "display: flex; align-items: center;",
      tags$div(
        class = class,
        style = "background-color: #008b99;"
      ),
      tags$span(
        style = "font-size:18px;font-weight:300;",
        sortis[3]
      )
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

plot_barchart <- function(df, y_col, caption_texte) {
  x_col <- "Année"
  DT <- concat_value(df, y_col)

  ggplot(data = DT, aes(x = !!sym(x_col), y = !!sym(y_col), fill = !!sym(x_col))) +
    geom_col_interactive(mapping = aes(data_id = !!sym(x_col), tooltip = tooltip_value)) +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      color = "white"
    ) +
    scale_fill_manual(values = colors) +
    labs(caption = caption_texte) +
    theme(legend.position = "none")
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
    plot.title = element_textbox_simple(hjust = 0, size = 12, color = "#008B99"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.position = "none",
    plot.caption = element_textbox_simple(
      hjust = 0,
      color = "#C0C0C2",
      size = 8
    )
  )
)
