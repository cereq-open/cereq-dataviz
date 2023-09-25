suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
})

source("graphics-settings.R")
source("shiny-elements.R")
theme_replace(
  axis.text.x = element_text(family = "Arimo", size = fs_default, hjust = .5,
                             margin = margin(0, 0, b = 5, 0))
)

# Load data --------------------------------------------------------------------
data_by_type_diplome <- readRDS("data/data_by_type_diplome.RDS")
type_diplome <- readRDS("data/type_diplome.RDS")
caption_part_1 <- readRDS("data/caption_part_1.RDS")
caption_part_2 <- readRDS("data/caption_part_2.RDS")
columns_titles <- readRDS("data/columns_titles.RDS")

# Couleurs des barplots
colors <- c("#F8AC00", "#EF5350", "#008B99")

augment_str_value <- function(.data, nom_colonne) {
  if (nom_colonne != "revenu_travail") {
    .data["taux_str"] <- paste0(.data[[nom_colonne]], " %")
  } else {
    .data["taux_str"] <- paste0(.data[[nom_colonne]], " €")
  }
  .data[["tooltip_value"]] <- paste0(.data[["annee"]], " : ", .data[["taux_str"]])
  return(.data)
}

evolution_barchart <- function(.data, colname, .caption, .title, .tooltip) {
  DT <- augment_str_value(.data, colname)
  gg <- ggplot(data = DT, aes(x = annee, y = !!sym(colname), fill = annee)) +
    geom_col_interactive(mapping = aes(data_id = annee, tooltip = tooltip_value)) +
    geom_text(
      size = fs_default / .pt, aes(label = taux_str),
      position = position_stack(vjust = .5),
      family = "Arimo",
      color = "white"
    ) +
    scale_fill_manual(values = colors) +
    labs(
      caption = .caption,
      title = paste(
        .title,
        if (!is.null(.tooltip)) {
          "\u24D8"
        }
      )
    ) +
    theme(legend.position = "none")

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
