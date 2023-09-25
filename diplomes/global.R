suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
})

source("graphics-settings.R")
source("shiny-elements.R")

# load data --------

menus_data <- readRDS("data/menus.RDS")
barchart_datasets_subsets <- readRDS("data/barchart_datasets_subsets.RDS")
barchart_captions_subsets <- readRDS("data/barchart_captions_subsets.RDS")
donut_profession_datasets_subsets <- readRDS("data/donut_profession_datasets_subsets.RDS")
donut_secteur_datasets_subsets <- readRDS("data/donut_secteur_datasets_subsets.RDS")
donuts_captions_subsets <- readRDS("data/donuts_captions_subsets.RDS")
tx_en_emploi_labels <- readRDS("data/tx_en_emploi_labels.RDS")
tx_chomage_labels <- readRDS("data/tx_chomage_labels.RDS")
taux_edi_labels <- readRDS("data/taux_edi_labels.RDS")
part_tps_partiel_labels <- readRDS("data/part_tps_partiel_labels.RDS")
revenu_travail_labels <- readRDS("data/revenu_travail_labels.RDS")
correspondance_ok_labels <- readRDS("data/correspondance_ok_labels.RDS")
competence_ok_labels <- readRDS("data/competence_ok_labels.RDS")

# misc. -----

# Le seuil des valeurs à afficher (ici on affiche donc toutes les valeurs supérieures ou égales à 2.5%)
seuil_donut_chart <- 2.5

# Les couleurs des catégories pour le bar chart
couleurs_bar_chart <- c("En emploi" = "#008B99", "Au chômage" = "#EF5350", "Autres situations" = "#F8AC00")

# Les couleurs des catégories pour les donut charts
couleurs_donut_chart <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

# barchart -----
generatePlotSpec <- function(.data, .caption) {

  ggplot(.data, aes(fake_axis, taux, fill = emploi)) +
    geom_col_interactive(width = 1, color = "white", mapping = aes(
      data_id = emploi,
      tooltip = tooltip_value
    )) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      colour = "white", size = fs_default / .pt
    ) +
    scale_fill_manual(values = couleurs_bar_chart) +
    scale_y_continuous(trans = "reverse", expand = expansion(mult = c(0, 0))) +
    facet_wrap(~Libelle_complet, ncol = 1) +
    labs(
      caption = .caption,
      title = "Répartition des sortants selon leur situation d'activité"
    )
}

# donut charts -----
generateDonutProfession <- function(.data, .caption, .donut_col_legend) {
  ggplot(.data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
    geom_rect_interactive(mapping = aes(data_id = profession, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(
      x = 3.5, size = fs_default / .pt,
      mapping = aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")),
      color = "white"
    ) +
    scale_fill_manual(
      values = couleurs_donut_chart,
      guide = guide_legend(label.vjust = 1)
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(
      caption = .caption,
      title = "Répartition par profession"
    ) +
    guides(fill = guide_legend(ncol = .donut_col_legend, byrow = TRUE))
}

generateDonutSecteur <- function(.data, .caption, .donut_col_legend) {
  ggplot(.data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
    geom_rect_interactive(mapping = aes(data_id = secteur, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(
      x = 3.5,
      size = fs_default / .pt,
      mapping = aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")), color = "white"
    ) +
    scale_fill_manual(
      values = couleurs_donut_chart, 
      guide = guide_legend(label.vjust = 1)
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = .caption, title = "Répartition par secteur") +
    guides(fill = guide_legend(ncol = .donut_col_legend, byrow = TRUE)) 
}
