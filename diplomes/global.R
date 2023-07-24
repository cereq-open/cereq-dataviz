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

hauteur_2_barres <- 3
hauteur_1_barre <- 2

register_gfont("Arimo")

db_diplome <- read_parquet("data/diplome.parquet") %>%
  rename(Libelle_complet = `Libelle complet`)

# Keep only the levels whose code should not start with 0.
list_degre1_2 <- as.list(filter(db_diplome, str_sub(Code, -1, -1) == "0") %>% pull(`Libelle_Menu`))

# Functions to create the data streams according to the levels selected

ensemble_de_sortants_data <- db_diplome %>% filter(Libelle_Menu == "Ensemble des sortants")

generateDataForLevel <- function(db_diplome, niveau) {
  filtered_data <- db_diplome %>%
    filter(Libelle_Menu == niveau)
}

generateDataForLevel3 <- function(db_diplome, code_niveau3, niveau3) {
  req(code_niveau3, code_niveau3)

  filtered_data <- db_diplome %>%
    filter(Code %in% code_niveau3 & Libelle_Menu %in% niveau3)
}

# Function to generate the first plot when first and second levels are selected from the first SelectInput tool.
generatePlot <- function(db_diplome, niveau) {
  DT <- db_diplome %>%
    select(Libelle_Menu, taux_emploi, taux_chomage) %>%
    mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
    filter(Libelle_Menu %in% c("Ensemble des sortants", niveau)) %>%
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
      taux_str = paste0(taux, "%")
    )
  
  DT$Libelle_Menu = factor(DT$Libelle_Menu, levels = c(unique(DT$Libelle_Menu)[1], unique(DT$Libelle_Menu)[2]))

  colors <- c("En emploi" = "#008B99", "Au chômage" = "#EF5350", "Autres situations" = "#F8AC00")

  if (sum(!str_detect(DT$Libelle_Menu, "Ensemble des sortants")) == 0) {
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
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  } else {
    caption <- paste0(
      '<span style="color:#008B99;">Lecture : </span>',
      "Trois ans après leur sortie de formation initiale, ",
      DT$taux_str[4],
      " des jeunes de la Génération 2017 sont en emploi, ",
      DT$taux_str[5],
      " au chômage et ",
      DT$taux_str[6],
      " dans une autre situation.",
      "<br>",
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  }

  ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
    geom_col_interactive(width = 0.5, color = "white", mapping = aes(data_id = emploi)) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      color = "white"
    ) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top", # Place la légende en bas
      legend.justification="left",
      legend.box.spacing = unit(0, "pt"),
      legend.margin=margin(0,0,0,0)
        
    ) 
}

# Function to generate the plot when the third levels are selected from the second SelectInput tool.
generatePlotSpec <- function(db_diplome, niveau, libelle) {
  DT <- db_diplome %>%
    filter(Code %in% c(niveau, 100) & Libelle_Menu %in% c(libelle, "Ensemble des sortants")) %>%
    select(Code, Libelle_Menu, Libelle_complet, taux_emploi, taux_chomage) %>%
    mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
    pivot_longer(
      cols = c("taux_emploi", "taux_chomage", "autre_situations"),
      names_to = "emploi",
      values_to = "taux"
    ) %>%
    mutate(
      Libelle_complet = sub("(\\-)([^\\-]*)$", "\n\\2", Libelle_complet),
      emploi = case_when(
        emploi == "taux_emploi" ~ "En emploi",
        emploi == "taux_chomage" ~ "Au chômage",
        emploi == "autre_situations" ~ "Autres situations",
        TRUE ~ emploi
      ),
      emploi = factor(emploi, levels = c("En emploi", "Au chômage", "Autres situations")),
      taux_str = paste0(taux, "%")
    )
  
  DT$Libelle_complet = factor(DT$Libelle_complet, levels = c(unique(DT$Libelle_complet)[1], unique(DT$Libelle_complet)[2]))

  colors <- c("En emploi" = "#008B99", "Au chômage" = "#EF5350", "Autres situations" = "#F8AC00")

  if (sum(!str_detect(DT$Libelle_Menu, "Ensemble des sortants")) == 0) {
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
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  } else {
    caption <- paste0(
      '<span style="color:#008B99;">Lecture : </span>',
      "Trois ans après leur sortie de formation initiale, ",
      DT$taux_str[4],
      " des jeunes de la Génération 2017 sont en emploi, ",
      DT$taux_str[5],
      " au chômage et ",
      DT$taux_str[6],
      " dans une autre situation.",
      "<br>",
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération 2017.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  }

  ggplot(DT, aes(Libelle_complet, taux, fill = emploi)) +
    geom_col_interactive(width = 0.5, color = "white", mapping = aes(data_id = emploi)) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      colour = "white"
    ) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top", # Place la légende en bas
      legend.box.spacing = unit(0, "pt"),
      legend.margin=margin(0,0,0,0)
    )
}

######### Create Pie charts ########################

generateDonutProfession <- function(db_diplome, niveau) {
  DT <- db_diplome %>%
    select(Libelle_Menu, pos_cadres, pos_prof_int, pos_emp_ouv_q, pos_emp_ouv_nq, pos_autres) %>%
    filter(Libelle_Menu %in% niveau) %>%
    mutate(across(everything(), ~ gsub(",", ".", .))) %>%
    pivot_longer(
      cols = c("pos_cadres", "pos_prof_int", "pos_emp_ouv_q", "pos_emp_ouv_nq", "pos_autres"),
      names_to = "profession",
      values_to = "taux"
    ) %>%
    mutate(taux = as.numeric(taux)) %>%
    mutate(
      fraction = taux / sum(taux), # Calculer les pourcentages
      ymax = cumsum(fraction), # Calculer les pourcentages cumulés (en haut de chaque rectangle)
      ymin = c(0, head(ymax, n = -1)), # Calculer le bas de chaque rectangle
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
      taux_str = paste0(taux, "%")
    )

  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

  caption <- paste0(
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
    "<br>",
    '<span style="color:#008B99;">Source : </span>',
    "Céreq, enquête Génération 2017 à trois ans."
  )
  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
    geom_rect_interactive(mapping = aes(data_id = profession), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = taux_str), color = "white") +
    scale_fill_manual(values = colors, labels = scales::label_wrap(20),
                      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

generateDonutSecteur <- function(db_diplome, niveau) {
  DT <- db_diplome %>%
    select(Libelle_Menu, sec_industries_btp, sec_commerce, sec_administration, sec_a_services, sec_autres) %>%
    filter(Libelle_Menu %in% niveau) %>%
    mutate(across(everything(), ~ gsub(",", ".", .))) %>%
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
      secteur = factor(secteur, levels = c("Industries, bâtiment et travaux publics", "Commerce",
                                           "Administrations, Education, Santé Action sociale",
                                           "Services",
                                           "Autres")),
      taux_str = paste0(taux, "%")
    )

  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")

  caption <- paste0(
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
    "<br>",
    '<span style="color:#008B99;">Source : </span>',
    "Céreq, enquête Génération 2017 à trois ans."
  )

  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
    geom_rect_interactive(mapping = aes(data_id = secteur), color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    geom_text(x = 3.5, aes(y = labelPosition, label = taux_str), color = "white") +
    scale_fill_manual(values = colors, labels = scales::label_wrap(20),
                      guide = guide_legend(label.vjust = 1, override.aes = list(size = 0))) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top",
      axis.text.y = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo"),
    text = element_text(family = "Arimo", size = 11),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_markdown(size = 14, color = "#008B99", family = "Arimo"),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      family = "Open Sans",
      hjust = 0,
      color = "#303032"
    ),
    legend.text = element_text(family = "Arimo", size = 9),
    legend.title = element_blank()
  )
)

labellize_stats_middle_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        info_str
      ),
      tags$i(
        class = "fas fa-info-circle",
        title = infobulle_str
      )
    ),
    tags$p(
      class = "stat_info",
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

labellize_stats_end_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(
      class = "stat_info",
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
    ),
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        info_str
      ),
      tags$i(
        class = "fas fa-info-circle",
        title = infobulle_str
      )
    )
  )
}

labellize_stats_no_i <- function(stat1_str, stat2_str = NULL, info_str) {
  tagList(
    tags$p(
      class = "stat_info",
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
    ),
    tags$p(
      class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        info_str
      )
    )
  )
}

labellize_stats_row_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(
      class = "d-inline",
      stat1_str
    ),
    if (!is.null(stat2_str)) {
      tags$p(
        class = "d-inline",
        style = "color: #C0C0C2; font-size: 16px;",
        stat2_str
      )
    },
    tags$p(
      class = "d-inline",
      info_str
    )
  )
}
