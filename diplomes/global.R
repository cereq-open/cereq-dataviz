suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(ggiraph)
  library(gdtools)
  library(arrow)
})

options(shiny.useragg = TRUE)

set_girafe_defaults(
  opts_hover_inv = opts_hover_inv(css = "stroke-width:2px; opacity:.5;"),
  opts_hover = opts_hover(css = ""),
  opts_selection = opts_selection(type = "none"),
  opts_toolbar = opts_toolbar(saveaspng = FALSE),
  opts_sizing(rescale = FALSE)
)

# Pour la hauteur et la largeur des graphiques ggiraph
hauteur_1_barre <- 4
largeur_bar_chart <- 11

hauteur_donut_chart <- 9
largeur_donut_chart <- 9

# Le seuil des valeurs à afficher (ici on affiche donc toutes les valeurs supérieures ou égales à 2.5%)
seuil_donut_chart <- 2.5

# La police des notes de lecture des graphiques
fonts_arimo <- list(sans = "Arimo")

# Les couleurs des catégories pour le bar chart
couleurs_bar_chart <- c("En emploi" = "#008B99", "Au chômage" = "#EF5350", "Autres situations" = "#F8AC00")

# Les couleurs des catégories pour les donut charts
couleurs_donut_chart <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

# Symboles pour les stats
symbole_pourcentage <- "%"
symbole_euro <- " €"

caption_source <- paste0(
  '<span style="color:#008B99;">Source : </span>',
  "Céreq, enquête Génération 2017 à trois ans."
)

# Variable pour la valeur Ensemble des sortants
ensemble_des_sortants <- "Ensemble des sortants"

if (!gdtools::font_family_exists("Arimo")) {
  systemfonts::register_font(
    name = "Arimo",
    plain = "www/arimo/fonts/arimo-v28-latin_latin-ext-regular.ttf",
    bold = "www/arimo/fonts/arimo-v28-latin_latin-ext-700.ttf",
    italic = "www/arimo/fonts/arimo-v28-latin_latin-ext-italic.ttf",
    bolditalic = "www/arimo/fonts/arimo-v28-latin_latin-ext-700italic.ttf"
  )
}

tab_diplome <- read_parquet("data/tab_diplome.parquet") %>%
  rename(Libelle_complet = `Libelle complet`,
         Libelle_Menu = `Libelle Menu`)

# Keep only the levels whose code should not start with 0.
list_degre1_2 <- as.list(filter(tab_diplome, str_sub(Code, -1, -1) == "0") %>% pull(`Libelle_Menu`))

# Functions to create the data streams according to the levels selected

ensemble_de_sortants_data <- tab_diplome %>% filter(Libelle_Menu == ensemble_des_sortants)

generateDataForLevel <- function(tab_diplome, niveau) {
  filtered_data <- tab_diplome %>%
    filter(Libelle_Menu == niveau)
}

generateDataForLevel3 <- function(tab_diplome, code_niveau3, niveau3) {
  req(code_niveau3, code_niveau3)

  filtered_data <- tab_diplome %>%
    filter(Code %in% code_niveau3 & Libelle_Menu %in% niveau3)
}

generateDTBarChart <- function(tab_diplome, niveau, libelle = NULL) {
  data <- tab_diplome %>%
    select(Code, Libelle_Menu, Libelle_complet, taux_emploi, taux_chomage) %>%
    mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
    pivot_longer(
      cols = c("taux_emploi", "taux_chomage", "autre_situations"),
      names_to = "emploi",
      values_to = "taux"
    ) %>%
    mutate(
      emploi = case_when(
        emploi == "taux_emploi" ~ "En emploi",
        emploi == "taux_chomage" ~ "Au chômage",
        emploi == "autre_situations" ~ "Autres situations",
        TRUE ~ emploi
      ),
      emploi = factor(emploi, levels = c("En emploi", "Au chômage", "Autres situations")),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(emploi, " : ", taux_str)
    )

  if (!is.null(libelle)) {
    data <- data %>%
      filter(Code %in% c(niveau, 100) & Libelle_Menu %in% c(libelle, ensemble_des_sortants)) %>%
      mutate(Libelle_complet = sub("(\\-)([^\\-]*)$", "\n\\2", Libelle_complet))
  } else if (is.null(libelle)) {
    data <- data %>%
      filter(Libelle_Menu %in% c(ensemble_des_sortants, niveau))
  }

  return(data)
}

generateDTDonutChartProfession <- function(tab_diplome, niveau, libelle = NULL) {
  data <- tab_diplome %>%
    select(Code, Libelle_Menu, pos_cadres, pos_prof_int, pos_emp_ouv_q, pos_emp_ouv_nq, pos_autres) %>%
    mutate(
      pos_cadres = gsub(",", ".", pos_cadres),
      pos_prof_int = gsub(",", ".", pos_prof_int),
      pos_emp_ouv_q = gsub(",", ".", pos_emp_ouv_q),
      pos_emp_ouv_nq = gsub(",", ".", pos_emp_ouv_nq),
      pos_autres = gsub(",", ".", pos_autres)
    ) %>%
    pivot_longer(
      cols = c("pos_cadres", "pos_prof_int", "pos_emp_ouv_q", "pos_emp_ouv_nq", "pos_autres"),
      names_to = "profession",
      values_to = "taux"
    ) %>%
    mutate(
      taux = as.numeric(taux),
      fraction = taux / sum(taux),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      labelPosition = (ymax + ymin) / 2,
      label = paste0(profession, "\n ", taux),
      profession = case_when(
        profession == "pos_cadres" ~ "Cadres",
        profession == "pos_prof_int" ~ "Professions intermédiaires",
        profession == "pos_emp_ouv_q" ~ "Employés ou ouvriers qualifiés",
        profession == "pos_emp_ouv_nq" ~ "Employés ou ouvriers non qualifiés",
        profession == "pos_autres" ~ "Autres",
        TRUE ~ profession
      ),
      profession = factor(profession, levels = c(
        "Cadres", "Professions intermédiaires",
        "Employés ou ouvriers qualifiés",
        "Employés ou ouvriers non qualifiés",
        "Autres"
      )),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(profession, " : ", taux_str)
    )

  if (is.null(libelle)) {
    data <- data %>%
      filter(Libelle_Menu %in% niveau)
  } else if (!is.null(libelle)) {
    data <- data %>%
      filter(Code %in% niveau & Libelle_Menu %in% libelle)
  }

  return(data)
}

generateDTDonutChartSecteur <- function(tab_diplome, niveau, libelle = NULL) {
  data <- tab_diplome %>%
    select(Code, Libelle_Menu, sec_industries_btp, sec_commerce, sec_administration, sec_a_services, sec_autres) %>%
    mutate(
      sec_industries_btp = gsub(",", ".", sec_industries_btp),
      sec_commerce = gsub(",", ".", sec_commerce),
      sec_administration = gsub(",", ".", sec_administration),
      sec_a_services = gsub(",", ".", sec_a_services),
      sec_autres = gsub(",", ".", sec_autres)
    ) %>%
    pivot_longer(
      cols = c("sec_industries_btp", "sec_commerce", "sec_administration", "sec_a_services", "sec_autres"),
      names_to = "secteur",
      values_to = "taux"
    ) %>%
    mutate(taux = as.numeric(taux)) %>%
    mutate(
      fraction = taux / sum(taux), # Calculer les pourcentages
      ymax = cumsum(fraction), # Calculer les pourcentages cumulés (en haut de chaque rectangle)
      ymin = c(0, head(ymax, n = -1)), # Calculer le bas de chaque rectangle
      labelPosition = (ymax + ymin) / 2,
      label = paste0(secteur, "\n ", taux),
      secteur = case_when(
        secteur == "sec_industries_btp" ~ "Industries, bâtiment et travaux publics",
        secteur == "sec_commerce" ~ "Commerce",
        secteur == "sec_administration" ~ "Administrations, Education, Santé Action sociale",
        secteur == "sec_a_services" ~ "Services",
        secteur == "sec_autres" ~ "Autres",
        TRUE ~ secteur
      ),
      secteur = factor(secteur, levels = c(
        "Industries, bâtiment et travaux publics", "Commerce",
        "Administrations, Education, Santé Action sociale",
        "Services",
        "Autres"
      )),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(secteur, " : ", taux_str)
    )

  if (is.null(libelle)) {
    data <- data %>%
      filter(Libelle_Menu %in% niveau)
  } else if (!is.null(libelle)) {
    data <- data %>%
      filter(Code %in% niveau & Libelle_Menu %in% libelle)
  }

  return(data)
}

generateCaptionBarChart <- function(DT) {
  caption <- paste0(
    '<span style="color:#008B99;">Lecture : </span>',
    "Trois ans après leur sortie de formation initiale, ",
    DT$taux_str[1],
    " des jeunes de la Génération 2017 sont en emploi, ",
    DT$taux_str[2],
    " au chômage et ",
    DT$taux_str[3],
    " dans une autre situation.",
    "<br>",
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017.",
    "<br>",
    caption_source
  )
  return(caption)
}

generateCaptionDonutChart <- function(niveau, libelle = NULL) {
  champ <- paste0(
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation ayant atteint au plus le niveau de diplôme sélectionné : "
  )

  if (is.null(libelle)) {
    caption <- paste0(
      champ,
      niveau,
      "<br>",
      caption_source
    )
  } else {
    caption <- paste0(
      champ,
      niveau,
      " ",
      libelle,
      "<br>",
      caption_source
    )
  }

  return(caption)
}

# Function to generate the first plot when first and second levels are selected from the first SelectInput tool.
generatePlot <- function(tab_diplome, niveau) {
  DT <- generateDTBarChart(tab_diplome, niveau) %>%
    mutate(Libelle_Menu = factor(Libelle_Menu, levels = c(unique(Libelle_Menu)[1], unique(Libelle_Menu)[2])))

  caption <- generateCaptionBarChart(DT)

  ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
    geom_col_interactive(width = 1, color = "white", mapping = aes(
      data_id = emploi,
      tooltip = tooltip_value
    )) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      color = "white"
    ) +
    scale_fill_manual(values = couleurs_bar_chart) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.box.spacing = unit(0, "cm"),
      legend.margin = margin(0, 0, 10, 0)
    )
}

# Function to generate the plot when the third levels are selected from the second SelectInput tool.
generatePlotSpec <- function(tab_diplome, niveau, libelle) {
  DT <- generateDTBarChart(tab_diplome, niveau, libelle) %>%
    mutate(Libelle_complet = factor(Libelle_complet, levels = c(unique(Libelle_complet)[1], unique(Libelle_complet)[2])))

  caption <- generateCaptionBarChart(DT)

  ggplot(DT, aes(Libelle_complet, taux, fill = emploi)) +
    geom_col_interactive(width = 1, color = "white", mapping = aes(
      data_id = emploi,
      tooltip = tooltip_value
    )) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      colour = "white"
    ) +
    scale_fill_manual(values = couleurs_bar_chart) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.box.spacing = unit(0, "cm"),
      legend.margin = margin(0, 0, 10, 0)
    )
}

######### Create Donut charts ########################

generateDonutProfession <- function(tab_diplome, niveau, caption_texte) {
  DT <- generateDTDonutChartProfession(tab_diplome, niveau)

  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
    geom_rect_interactive(mapping = aes(data_id = profession, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")), color = "white") +
    scale_fill_manual(
      values = couleurs_donut_chart, labels = scales::label_wrap(20),
      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption_texte) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank(),
      legend.key.height = unit(3,"line"),
      legend.box.spacing = unit(-1, "cm")
      ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

generateDonutProfessionSpec <- function(tab_diplome, niveau, libelle, caption_texte) {
  DT <- generateDTDonutChartProfession(tab_diplome, niveau, libelle)

  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
    geom_rect_interactive(mapping = aes(data_id = profession, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")), color = "white") +
    scale_fill_manual(
      values = couleurs_donut_chart, labels = scales::label_wrap(20),
      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption_texte) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank(),
      legend.key.height = unit(3,"line"),
      legend.box.spacing = unit(-1, "cm")
      ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

generateDonutSecteur <- function(tab_diplome, niveau, caption_texte) {
  DT <- generateDTDonutChartSecteur(tab_diplome, niveau)

  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
    geom_rect_interactive(mapping = aes(data_id = secteur, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")), color = "white") +
    scale_fill_manual(
      values = couleurs_donut_chart, labels = scales::label_wrap(20),
      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption_texte) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank(),
      legend.key.height = unit(3,"line"),
      legend.box.spacing = unit(-1, "cm")
      ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

generateDonutSecteurSpec <- function(tab_diplome, niveau, libelle, caption_texte) {
  DT <- generateDTDonutChartSecteur(tab_diplome, niveau, libelle)

  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
    geom_rect_interactive(mapping = aes(data_id = secteur, tooltip = tooltip_value), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = ifelse(taux >= seuil_donut_chart, taux_str, "")), color = "white") +
    scale_fill_manual(
      values = couleurs_donut_chart, labels = scales::label_wrap(20),
      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))
    ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption_texte) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank(),
      legend.key.height = unit(3,"line"),
      legend.box.spacing = unit(-1, "cm")
      ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo"),
    text = element_text(size = 12, family = "Arimo"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14, face = "plain"),
    plot.title = element_textbox_simple(size = 8, color = "#008B99"),
    plot.caption = element_textbox_simple(
      hjust = 0,
      color = "#C0C0C2",
      size = 12
    ),
    plot.caption.position = "plot",
    legend.title = element_blank()
  )
)

labellize_stats_end_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(
      class = "texte-stat-info",
      tags$span(
        style = "color: #008B99;",
        info_str
      ),
      tags$i(
        class = "fas fa-info-circle",
        title = infobulle_str,
        style = "color: #008B99;"
      )
    ),
    tags$p(
      class = "stat-info",
      tags$span(
        style = "color: #008B99;",
        stat1_str
      ),
      if (!is.null(stat2_str)) {
        tags$span(
          style = "color: #C0C0C2; font-size: 16px;",
          stat2_str
        )
      }
    )
  )
}

labellize_stats_no_i <- function(stat1_str, stat2_str = NULL, info_str) {
  tagList(
    tags$p(
      class = "texte-stat-info",
      tags$span(
        style = "color: #008B99;",
        info_str
      )
    ),
    tags$p(
      class = "stat-info",
      tags$span(
        style = "color: #008B99;",
        stat1_str
      ),
      if (!is.null(stat2_str)) {
        tags$span(
          style = "color: #C0C0C2; font-size: 16px;",
          stat2_str
        )
      }
    )
  )
}

labellize_stats_row <- function(stat1_str, stat2_str = NULL, info_str) {
  tagList(
    tags$p(
      class = "d-inline",
      style = "font-weight: 300;",
      stat1_str
    ),
    tags$p(
      class = "d-inline",
      info_str
    ),
    if (!is.null(stat2_str)) {
      tags$p(
        class = "d-inline",
        style = "color: #C0C0C2; font-size: 16px; font-weight: 300;",
        stat2_str
      )
    }
  )
}

DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )
}

as_code <- function(tab_diplome, niveau) {
  as.numeric(filter(tab_diplome, Libelle_Menu %in% niveau) %>% pull(Code))
}