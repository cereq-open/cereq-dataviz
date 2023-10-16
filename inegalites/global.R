source("graphics-settings.R")
source("shiny-elements.R")

# load data --------

data_par_facteur_analyse <- readRDS("data/data_par_facteur_analyse.RDS")
tab_facteurs <- readRDS("data/tab_facteurs.RDS")
tab_indicateurs <- readRDS("data/tab_indicateurs.RDS")
indicateur_choices <- readRDS("data/indicateur_choices.RDS")
indicateur_title <- readRDS("data/indicateur_title.RDS")
facteur_choices <- readRDS("data/facteur_choices.RDS")
indicateur_captions <- readRDS("data/indicateur_captions.RDS")
indicateur_titles <- readRDS("data/indicateur_titles.RDS")

# Function to create the caption
color_palette <- function(dat) {
  if (length(levels(dat$modalite)) == 2) {
    colors <- c("#F8AC00", "#256299")
  } else {
    colors <- c("#256299", "#F8AC00", "#008B99", "#EF5350", "#7B9A62", "#D6515C", "#C0C0C2")
  }
  colors
}


# Function to determine the height of the graph

generate_height <- function(dat) {
  n_lev <- length(levels(dat$modalite))
  height_single_unit <- .3
  n_rows <- (n_lev + 2) * 5 + n_lev + 2 # 5 diploma type * n_lvel + facet title + an empty row
  calculated_height <- n_rows * height_single_unit
  # message("height is estimated to ", calculated_height, " because it has ", n_rows, " rows")
  calculated_height
}

# Function to create the plot

generatePlot <- function(.data, colors, .caption = NULL, .title = NULL,
                         interactive_caption = TRUE) {
  non_percent <- all(.data[["statistique"]] > 100)
  xminlim <- if(non_percent) -500 else -18
  
  gg <- ggplot(data = .data, aes(x = statistique, y = modalite, fill = modalite)) +
    geom_col_interactive(mapping = aes(data_id = data_id, tooltip = tooltip_value)) +
    facet_wrap(~diplome, ncol = 1) +
    scale_fill_manual(
      values = colors,
      labels = scales::label_wrap(60),
      guide = guide_legend(
        reverse = TRUE,
        ncol = 1,
        byrow = TRUE
      )
    ) +
    labs(
      caption = .caption,
      title = paste(
        .title$title,
        if (!is.null(.title$tooltip) && interactive_caption) {
          "\u24D8"
        }
      )
    ) +
    geom_label_interactive(aes(label = taux_str, data_id = data_id, tooltip = tooltip_value),
      x = 0, show.legend = FALSE,
      hjust = 1.2,
      family = "Arimo",
      color = "white",
      size = fs_default / .pt
    ) +
    xlim(c(xminlim, NA))

  if (!is.null(.title$tooltip) && interactive_caption) {
    info_bulle <- sprintf("<div style=\"max-width:200px;\">%s</div>", .title$tooltip)
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
