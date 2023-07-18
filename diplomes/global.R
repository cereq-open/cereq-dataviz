suppressPackageStartupMessages({
library(readxl)
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
  opts_toolbar = opts_toolbar(saveaspng = FALSE)
)

register_gfont("Arimo")
 
db_diplome <- read_parquet("data/diplome.parquet") %>%
  rename(Libelle_complet = `Libelle complet`)

# Keep only the levels whose code should not start with 0.
list_degre1_2 <- as.list(filter(db_diplome, str_sub(Code, -1, -1)  == "0")  %>% pull(`Libelle_Menu`))

# Functions to create the data streams according to the levels selected

ensemble_de_sortants_data <- db_diplome %>% filter(Libelle_Menu == "Ensemble des sortants")

generateDataForLevel <- function(db_diplome, niveau) {
  
  filtered_data <- db_diplome %>%
    filter(Libelle_Menu == niveau)
}

generateDataForLevel3 <- function(db_diplome, code_niveau3, niveau3) {
  
  req(code_niveau3, code_niveau3)
  
  filtered_data <- db_diplome %>%
    filter(Code == code_niveau3 & Libelle_Menu %in% niveau3)
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
  
  colors <- c("En emploi"="#008B99", "Au chômage"="#EF5350", "Autres situations"="#F8AC00")
  
  caption <- paste0('<span style="color:#008B99;">Lecture : </span>',
                    "Trois ans après leur sortie de formation initiale ",
                    DT$taux_str[1],
                    " des jeunes de la Génération 2017 sont en emploi, ",
                    DT$taux_str[2],
                    " au chômage et ",
                    DT$taux_str[3],
                    " dans une autre situation.",
                    '<br>',
                    '<span style="color:#008B99;">Champ : </span>',
                    "Ensemble de la Génération 2017.",
                    '<br>',
                    '<span style="color:#008B99;">Source : </span>',
                    "Céreq, enquête Génération 2017 à trois ans."
                    )
  
  ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
    geom_col_interactive(width = 0.5, color = "white",mapping = aes(data_id = emploi)) + coord_flip() +
    geom_text(aes(label = taux),
              position = position_stack(vjust = .5),
              color = "white") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") + 
    ggtitle("Répartition des sortants selon leur situation d'activité") +
    labs(caption = caption) +
    theme(legend.position = "bottom",    # Place la légende en bas
          legend.direction = "horizontal",    # Orientation de la légende en ligne
          legend.box = "horizontal")    # Boîte de la légende en ligne
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
  
  colors <- c("En emploi"="#008B99", "Au chômage"="#EF5350", "Autres situations"="#F8AC00")
  
  caption <- paste0('<span style="color:#008B99;">Lecture : </span>',
                    "Trois ans après leur sortie de formation initiale ",
                    DT$taux_str[1],
                    " des jeunes de la Génération 2017 sont en emploi, ",
                    DT$taux_str[2],
                    " au chômage et ",
                    DT$taux_str[3],
                    " dans une autre situation.",
                    '<br>',
                    '<span style="color:#008B99;">Champ : </span>',
                    "Ensemble de la Génération 2017.",
                    '<br>',
                    '<span style="color:#008B99;">Source : </span>',
                    "Céreq, enquête Génération 2017 à trois ans."
  )
  
  ggplot(DT, aes(Libelle_complet, taux, fill = emploi)) +
    geom_col_interactive(width = 0.5, color = "white",mapping = aes(data_id = emploi)) + coord_flip() +
    geom_text(aes(label = taux),
              position = position_stack(vjust = .5),
              colour = "white") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(legend.position = "bottom",    # Place la légende en bas
          legend.direction = "horizontal",    # Orientation de la légende en ligne
          legend.box = "horizontal")    # Boîte de la légende en ligne
} 

######### Create Pie charts ########################

generateDonutProfession <- function(db_diplome, niveau) {
  DT <- db_diplome %>%
    select(Libelle_Menu, pos_cadres,	pos_prof_int,	pos_emp_ouv_q,	pos_emp_ouv_nq,	pos_autres) %>%
    filter(Libelle_Menu == niveau) %>%
    mutate(across(everything(), ~ gsub(",", ".", .))) %>% 
    pivot_longer(
      cols = c("pos_cadres",	"pos_prof_int",	"pos_emp_ouv_q",	"pos_emp_ouv_nq",	"pos_autres"),
      names_to = "profession",
      values_to = "taux") %>% 
    mutate(taux = as.numeric(taux)) %>% 
    mutate(fraction = taux / sum(taux), # Calculer les pourcentages
           ymax = cumsum(fraction),  # Calculer les pourcentages cumulés (en haut de chaque rectangle)
           ymin = c(0, head(ymax, n=-1)), # Calculer le bas de chaque rectangle
           labelPosition = (ymax + ymin) / 2,
           label = paste0(profession, "\n ", taux),
           profession = case_when(
             profession == "pos_cadres" ~ "Cadres",
             profession == "pos_prof_int" ~ "Professions intermédiaires",
             profession == "pos_emp_ouv_q" ~ "Ouvriers et employés qualifiés",
             profession == "pos_emp_ouv_nq" ~ "Ouvriers et employés non qualifiés",
             profession == "pos_autres" ~ "Autres professions",
             TRUE ~ profession
             ),
           profession = factor(profession, levels = c("Cadres", "Professions intermédiaires",
                                                      "Ouvriers et employés qualifiés",
                                                      "Ouvriers et employés non qualifiés",
                                                      "Autres professions"))
           )
  
  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
  
  caption <- paste0('<span style="color:#008B99;">Champ : </span>',
                    "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
                    '<br>',
                    '<span style="color:#008B99;">Source : </span>',
                    "Céreq, enquête Génération 2017 à trois ans."
  )
  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = profession)) +
    geom_rect_interactive(mapping = aes(data_id = profession), color = "gray") +
    coord_polar(theta = "y") + 
    xlim(c(2, 4)) + 
    geom_text(aes(x = 3.5, y = labelPosition, label = taux), color = "white") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(legend.position = "left",
          axis.text.y = element_blank())
}

generateDonutSecteur <- function(db_diplome, niveau) {
  
  DT <- db_diplome %>%
    select(Libelle_Menu, sec_industries_btp,	sec_commerce,	sec_administration,	sec_a_services,	sec_autres) %>%
    filter(Libelle_Menu == niveau) %>%
    mutate(across(everything(), ~ gsub(",", ".", .))) %>% 
    pivot_longer(
      cols = c("sec_industries_btp",	"sec_commerce",	"sec_administration",	"sec_a_services",	"sec_autres"),
      names_to = "secteur",
      values_to = "taux") %>% 
    mutate(taux = as.numeric(taux)) %>% 
    mutate(fraction = taux / sum(taux), # Calculer les pourcentages
           ymax = cumsum(fraction),  # Calculer les pourcentages cumulés (en haut de chaque rectangle)
           ymin = c(0, head(ymax, n=-1)), # Calculer le bas de chaque rectangle
           labelPosition = (ymax + ymin) / 2,
           label = paste0(secteur, "\n ", taux),
           secteur = case_when(
             secteur == "sec_industries_btp" ~ "Industrie et BTP",
             secteur == "sec_commerce" ~ "Commerce",
             secteur == "sec_administration" ~ "Administration",
             secteur == "sec_a_services" ~ "Autres services",
             secteur == "sec_autres" ~ "Autres secteurs",
             TRUE ~ secteur
           ),
           secteur = factor(secteur, levels = c("Industrie et BTP", "Commerce",
                                                "Administration",
                                                "Autres services",
                                                "Autres secteurs"))
           )
  
  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
  
  caption <- paste0('<span style="color:#008B99;">Champ : </span>',
                    "Ensemble de la Génération 2017 en emploi trois ans après leur sortie de formation initiale.",
                    '<br>',
                    '<span style="color:#008B99;">Source : </span>',
                    "Céreq, enquête Génération 2017 à trois ans."
  )
  
  ggplot(DT, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = secteur)) +
    geom_rect_interactive(mapping = aes(data_id = secteur), color = "gray") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) + 
    geom_text(aes(x = 3.5, y = labelPosition, label = taux), color = "white") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(legend.position = "left",
          axis.text.y = element_blank())
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
    plot.caption = element_textbox_simple(family = "Open Sans",
                                hjust = 0,
                                color="#303032",
                                margin = margin(t = 10),
                                size = 9),
    legend.text = element_text(family = "Arimo", size = 9),
    legend.title = element_blank()
  )
)

labellize_stats_middle_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        info_str
      ),
      tags$i(
        class = "fas fa-info-circle",
        style = "margin-left: 5px;",
        title = infobulle_str
      )
    ),
    tags$p(class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        stat1_str
      ),
      if (!is.null(stat2_str)) tags$span(
        style = "color: #C0C0C2;",
        stat2_str
      )
      
    )
  )
}

labellize_stats_end_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$p(class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        stat1_str
      ),
      if (!is.null(stat2_str)) tags$span(
        style = "color: #C0C0C2;",
        stat2_str
      )
      
    ),
    tags$p(class = "stat_info",
      tags$span(
        style = "color: #008B99;",
        info_str
      ),
      tags$i(
        class = "fas fa-info-circle",
        style = "margin-left: 5px;",
        title = infobulle_str
      )
    )
  )
}

labellize_stats_row_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str) {
  tagList(
    tags$span(
      style = "color: #008B99; font-size: 2em;",
      stat1_str
    ),
    if (!is.null(stat2_str)) tags$span(
      style = "color: #C0C0C2; font-size: 2em;",
      stat2_str
    ),
    tags$span(
      style = "color: #008B99; font-size: 2em;",
      info_str
    ),
    tags$i(
      class = "fas fa-info-circle",
      style = "margin-left: 5px; font-size: 2em;",
      title = infobulle_str
    )
  )
}
