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
  library(sf)
  library(rgdal)
  library(readxl)
})

# Load data --------------------------------------------------------------------

# Table permettant d'afficher les cartes
tab_region <- st_read("data/tab_region.shp", quiet = TRUE) %>%
  mutate(
    Libellé = ifelse(Libellé == "Corse", "Provence-Alpes-Cote-d'Azur et Corse",
      ifelse(Libellé == "Provence-Alpes-Cote d'Azur", "Provence-Alpes-Cote-d'Azur et Corse", Libellé)
    ),
    Libellé = toupper(Libellé)
  )


# Table permettant d'afficher les stats du type "France : XX%
#                                               (Dont DROM : xx%)"
db_region <- read_parquet("data/tab_region.parquet")

# Variables region
variables_region <- read_excel("data/variables_region.xlsx")

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

variables_residence <- variables_region[1:13, ]
variables_niveau <- variables_region[-c(1:13), ]

# Titre des cartes
titre_map_residence <- variables_residence$Titre_graphique
titre_map_niveau <- variables_niveau$Titre_graphique

# Longeur et largeur de la carte ggiraph
longeur_map <- 20
largeur_map <- 20

noms_colonnes_residence <- c(
  "tax_mpl"       = "taux_emploi",
  "prt_chm"       = "part_chomage",
  "tx_chmg"       = "taux_chomage",
  "traj_1"        = "traj_1",
  "traj_2"        = "traj_2",
  "traj_3"        = "traj_3",
  "traj_7"        = "traj_7",
  "taux_ed"       = "taux_edi",
  "rvn_trv"       = "revenu_travail",
  "ps_cdrs"       = "pos_cadres",
  "ps_prf_"       = "pos_prof_int",
  "ps_mp_v_q"     = "pos_emp_ouv_q",
  "ps_mp_v_n"     = "pos_emp_ouv_nq"
)

inv_noms_colonnes_residence <- setNames(names(noms_colonnes_residence), noms_colonnes_residence)

noms_colonnes_niveau <- c(
  "nondplm"       = "nondiplome",
  "secondr"       = "secondaire",
  "sprr_cr"       = "superieur_court",
  "sprr_ln"       = "superieur_long"
)

inv_noms_colonnes_niveau <- setNames(names(noms_colonnes_niveau), noms_colonnes_niveau)

# Ligne et colonne pour extraire les informations de la table "db_region"
ligne_fr <- 1
ligne_drom <- 3

# Fonction pour tracer la carte
plot_map <- function(df, nom_colonne, col_name_text, caption_texte) {
  ggplot(df) +
    geom_sf_interactive(aes(fill = !!sym(nom_colonne), data_id = !!sym(nom_colonne), tooltip = tooltip_value)) +
    geom_sf_text(
      aes(label = !!sym(col_name_text)),
      check_overlap = TRUE,
      size = 6,
      color = "white",
      nudge_x = c(0, .15, rep(0, 10), 0, 0),
      nudge_y = c(0, -.2, rep(0, 10), -.15, 0),
      fun.geometry = sf::st_centroid
    ) +
    labs(caption = caption_texte) +
    theme(legend.position = "none")
}

concatenate_columns <- function(df, col_name) {
  if(col_name != "rvn_trv") {
    df[["label"]] <- paste0(df[["Libellé"]], "\n" , paste0("(", df[[col_name]], "%)"))
    df[["tooltip_value"]] <- paste0(df[["Libellé"]], " : " ,df[[col_name]], "%")
  } else {
    df[["label"]] <- paste0(df[["Libellé"]], "\n" , paste0("(", df[[col_name]], " €)"))
    df[["tooltip_value"]] <- paste0(df[["Libellé"]], " : " ,df[[col_name]], " €")
  }
  # Masque le texte pour la Corse
  df$label[12] <- str_extract(df$label[12], "\\(.*?\\)")
  return(df)
}

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
    }
  )
}

labellize_stat <- function(stat1, stat2) {
  tagList(
    tags$p(
      class = "texte-stat-info",
      stat1,
      tags$span(
        style = "color: #C0C0C2; font-size: 18px;",
        stat2
      )
    )
  )
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_markdown(family = "Arimo", size = 30, hjust = 0),
  )
)