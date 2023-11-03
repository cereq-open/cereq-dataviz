suppressPackageStartupMessages({
  library(sf)
  library(viridis)
})

source("graphics-settings.R")
source("shiny-elements.R")

theme_replace(
  legend.key.width = unit(15, "pt"),
  legend.key.height = unit(.5, "in"),
  
  legend.position = "right"
)

# Load data -----
liste_titre_diplome<- readRDS("data/liste_titre_diplome.RDS")
valeurs_indicateurs <- readRDS("data/valeurs_indicateurs.RDS")
liste_titre_indicateurs <- readRDS("data/liste_titre_indicateurs.RDS")
liste_label_indicateurs <- readRDS("data/liste_label_indicateurs.RDS")
valeurs_niveaux_diplomes <- readRDS("data/valeurs_niveaux_diplomes.RDS")
liste_label_niveaux_diplomes <- readRDS("data/liste_label_niveaux_diplomes.RDS")
db_stats_par_regions <- readRDS("data/db_stats_par_regions.RDS")

niveaux_diplomes<- as.list(valeurs_niveaux_diplomes)
# functions to prepare data ----
generateCaption <- function(variable) {
  if (variable %in% c("taux_emploi", "taux_chomage", "traj_1", "traj_2", "traj_3", "traj_7")) {
    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  } else {
    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017 en emploi en octobre 2020.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  }
  caption
}

# functions to prepare data ----

   
    caption2 <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
   
  

# Longeur et largeur de la carte ggiraph
longeur_map <- 8
largeur_map <- 8


# Fonction pour tracer la carte
region_map <- function(.data, column_stat_name, column_label_name, .caption, .title, .tooltip) {

  gg <- ggplot(data = .data) +
    geom_sf_interactive(aes(fill = !!sym(column_stat_name), data_id = `Libellé`, tooltip = tooltip_value)) +
    scale_fill_gradient(low = "#85C1E9", high = "#154360")+
    geom_sf_text(
      aes(label = !!sym(column_label_name)),
      check_overlap = FALSE,
      family = "Arimo",
      size = fs_default / .pt,
      color = "white",
      fun.geometry = sf::st_centroid
    ) +
    labs(
      caption = .caption,
      title = paste(
        .title,
        if (!is.null(.tooltip)) {
          "\u24D8"
        }
      )
    )

  if (!is.null(.tooltip)) {
    info_bulle <- sprintf("<div style=\"max-width:200px;\">%s</div>", .tooltip)
    theme_replace(
      plot.title = element_text_interactive(
        tooltip = info_bulle,
        hjust = 0,
        size = fs_title,
        margin = margin(t = 20, r = 5, b = 20, l = 5),
        face = "bold"
      )
    )
  } else {
    theme_replace(
      plot.title = element_textbox_simple(
        size = fs_title,
        margin = margin(t = 20, r = 5, b = 20, l = 5),
        face = "bold"
      )
    )
  }

  gg
}

concatenate_columns <- function(.data, col_name) {
  if (col_name != "revenu_travail") {
    .data[["label"]] <- paste0(.data[[col_name]], " %")
    .data[["tooltip_value"]] <- paste0(.data[["Libellé"]], " : ", .data[[col_name]], " %")
  } else {
    .data[["label"]] <- paste0(.data[[col_name]], " €")
    .data[["tooltip_value"]] <- paste0(.data[["Libellé"]], " : ", .data[[col_name]], " €")
  }
  # Masque le texte pour la Corse
  .data$label[12] <- ""
  .data
}
