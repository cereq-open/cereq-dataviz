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
})

# Load data --------------------------------------------------------------------

tab_region <- st_read("data/tab_region.shp", quiet = TRUE) %>%
  mutate(Libellé = toupper(Libellé))

db_region <- readxl::read_excel("data/tab_region.xls")

# Define Global ----------------------------------------------------------------

options(shiny.useragg = TRUE)

set_girafe_defaults(
  opts_hover_inv = opts_hover_inv(css = "stroke-width:2px; opacity:.5;"),
  opts_hover = opts_hover(css = ""),
  opts_selection = opts_selection(type = "none"),
  opts_toolbar = opts_toolbar(saveaspng = FALSE)
)

# if (!gdtools::font_family_exists("Arimo")) {
#   systemfonts::register_font(
#     name = "Arimo",
#     plain = "www/arimo/fonts/arimo-v28-latin_latin-ext-regular.ttf",
#     bold = "www/arimo/fonts/arimo-v28-latin_latin-ext-700.ttf",
#     italic = "www/arimo/fonts/arimo-v28-latin_latin-ext-italic.ttf",
#     bolditalic = "www/arimo/fonts/arimo-v28-latin_latin-ext-700italic.ttf"
#   )
# }

# Longeur et largeur de la carte ggiraph
longeur_map <- 6
largeur_map <-6

# Vecteur associant chaque nom de colonne à sa description
noms_colonnes_residence <- c(
  "tax_mpl"          = "Taux d’emploi à trois ans",
  "prt_chm"          = "Proportion de sortants au chômage à trois ans",
  "tx_chmg"          = "Taux de chômage à trois ans",
  "traj_1"           = "Proportion de sortants ayant connu un accès rapide à l’emploi durable",
  "traj_2"           = "Proportion de sortants ayant connu un accès différé à l’emploi durable",
  "traj_3"           = "Proportion de sortants ayant connu une succession d'emplois courts",
  "traj_7"           = "Proportion de sortants ayant connu un chômage persistant ou récurrent",
  "taux_ed"          = "Proportion de sortants en emploi à durée indéterminé à trois ans",
  "rvn_trv"          = "Revenu mensuel médian à trois ans",
  "ps_cdrs"          = "Proportion de sortants cadres à trois ans",
  "ps_prf_"          = "Proportion de sortants professions intermédiaires à trois ans",
  "ps_mp_v_q"        = "Proportion de sortants employés ou ouvriers qualifiés à trois ans",
  "ps_mp_v_n"        = "Proportion de sortants employés ou ouvriers non qualifiés à trois ans"
)

noms_colonnes_residence_inverse <- setNames(names(noms_colonnes_residence), noms_colonnes_residence)

noms_colonnes_residence_info_bulle <- c("tx_chmg", "traj_1", "traj_2", "traj_3", "traj_7", "taux_ed", "rvn_trv")

# Vecteur associant chaque nom de colonne à sa description
noms_colonnes_niveau <- c(
  "nondplm"          = "Proportion de non diplômés parmi les sortants de formation initiale",
  "secondr"          = "Proportion de sortants ayant un diplôme du secondaire comme plus haut diplôme",
  "sprr_cr"          = "Proportion de sortants ayant un diplôme de l'enseignement supérieur court comme plus haut diplôme",
  "sprr_ln"          = "Proportion de sortants ayant un diplôme de l'enseignement supérieur long comme plus haut diplôme"
)

noms_colonnes_niveau_inverse <- setNames(names(noms_colonnes_niveau), noms_colonnes_niveau)

noms_colonnes_db_region <- c(
  "taux_emploi"         = "tax_mpl",
  "part_chomage"        = "prt_chm",
  "taux_chomage"        = "tx_chmg",
  "traj_1"              = "traj_1",
  "traj_2"              = "traj_2",
  "traj_3"              = "traj_3",
  "traj_7"              = "traj_7",
  "taux_edi"            = "taux_ed",
  "revenu_travail"      = "rvn_trv",
  "pos_cadres"          = "ps_cdrs",
  "pos_prof_int"        = "ps_prf_",
  "pos_emp_ouv_q"       = "ps_mp_v_q",
  "pos_emp_ouv_nq"      = "ps_mp_v_n"
)

inv_noms_colonnes_db_region <- setNames(names(noms_colonnes_db_region), noms_colonnes_db_region)

# Fonction pour tracer la carte
plot_map <- function(df, nom_colonne, col_name_text, caption_texte) {
  ggplot(df) +
    geom_sf_interactive(aes(fill = !!sym(nom_colonne), data_id = !!sym(nom_colonne), tooltip = tooltip_value)) +
    geom_sf_text(
      aes(label = !!sym(col_name_text)),
      check_overlap = TRUE,
      size = 3,
      color = "white"
    ) +
    scale_fill_viridis_c() +
    theme(legend.position = "none") +
    labs(caption = caption_texte)
}

concatenate_columns <- function(df, col_name) {
  df[["label"]] <- paste0(df[["Libellé"]], " : " ,df[[col_name]],  "%")
  df[["tooltip_value"]] <- paste0(df[["Libellé"]], " : " ,df[[col_name]], "%")
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
        style = "font-size:2em;",
        title = infobulle_str
      )
    },
    tags$p(
      class = "d-inline",
      "selon la région de résidence à la fin des études"
    )
  )
}

labellize_stat <- function(stat1, stat2) {
  tagList(
    tags$p(
      class = "texte-stat-info",
      stat1,
      tags$p(
        class = "stat-info",
        stat2
      )
    )
  )
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo"),
    text = element_text(family = "Arimo", size = 5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_markdown(size = 5, color = "#008B99", family = "Arimo"),
    plot.caption = element_markdown(family = "Arimo", size = 5),
    legend.text = element_markdown(family = "Arimo", size = 5),
    legend.title = element_blank()
  )
)
