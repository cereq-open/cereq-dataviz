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

options(shiny.useragg = TRUE)

set_girafe_defaults(
  opts_hover_inv = opts_hover_inv(css = "stroke-width:2px; opacity:.5;"),
  opts_hover = opts_hover(css = ""),
  opts_selection = opts_selection(type = "none"),
  opts_toolbar = opts_toolbar(saveaspng = FALSE),
  opts_sizing(rescale = FALSE)
)

if (!gdtools::font_family_exists("Arimo")) {
  systemfonts::register_font(
    name = "Arimo",
    plain = "www/arimo/fonts/arimo-v28-latin_latin-ext-regular.ttf",
    bold = "www/arimo/fonts/arimo-v28-latin_latin-ext-700.ttf",
    italic = "www/arimo/fonts/arimo-v28-latin_latin-ext-italic.ttf",
    bolditalic = "www/arimo/fonts/arimo-v28-latin_latin-ext-700italic.ttf")
}

tab_inegalites <- read_parquet("data/tab_inegalites.parquet") %>%
  mutate(Diplôme = str_trim(Diplôme)) %>%
  mutate(Diplôme = case_when(
    str_detect(Diplôme, "Diplômées") ~ str_replace(Diplôme, "Diplômées", "Diplômés"),
    str_detect(Diplôme, "diplômées") ~ str_replace(Diplôme, "diplômées", "diplômés"),
    TRUE ~ Diplôme  # on garde la même valeur pour toutes les autres valeurs
  )) %>% 
  mutate(across(c(taux_emploi,part_chomage,taux_chomage,taux_edi,revenu_travail,traj_1,traj_2,traj_3,traj_7,correspondance_ok,competence_ok), 
                ~ifelse(. == -900, 0, .)))

tab_variables_inegalites <- read_excel("data/variables_INEGALITES.xlsx")

# Values for both pickerInput tools

valeurs_facteur_analyse <- unique(na.omit(tab_inegalites$facteur_analyse))
valeurs_indicateurs <- unique(na.omit(tab_variables_inegalites$Titre_graphique))

# Function to create the data stream according to the factor selected

generateData <- function(tab_inegalites, facteur) {
  tab_inegalites %>% filter(facteur_analyse %in% facteur)
}

# Function to create the caption

generateCaption <- function(variable) {
  
if (variable %in% c("taux_emploi","part_chomage","taux_chomage","traj_1","traj_2","traj_3","traj_7")){
  caption <- paste0(
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la génération.",
    "<br>",
    '<span style="color:#008B99;">Source : </span>',
    "Céreq, enquête Génération 2017 à trois ans."
  )
  }
else {
 caption <- paste0(
   '<span style="color:#008B99;">Champ : </span>',
   "Ensemble de la génération en emploi trois ans après leur sortie de formation initiale.",
   "<br>",
   '<span style="color:#008B99;">Source : </span>',
   "Céreq, enquête Génération 2017 à trois ans."
 )
}
  return(caption)
}

# Function to create the caption
  
generateColors <- function(facteur) {
    
  tab_filtree <- tab_inegalites %>% group_by(facteur_analyse) %>% 
    summarise(levels_count = n_distinct(modalité)) %>% filter(facteur_analyse %in% facteur)
  
    if (tab_filtree$levels_count == 2){
      colors <- c("#F8AC00", "#256299")
    }
    else {
      colors <- c("#256299", "#F8AC00","#008B99","#EF5350","#7B9A62","#D6515C","#C0C0C2")
    }
    return(colors)
  }

generate_Nb_rows <- function(facteur) {
  
  tab_filtree <- tab_inegalites %>% group_by(facteur_analyse) %>% 
    summarise(levels_count = n_distinct(modalité)) %>% filter(facteur_analyse %in% facteur)
  
  if (tab_filtree$levels_count == 2){
    n <- 1
  }
  else {
    n <- 3
  }
  return(n)
}

# Function to create the plot
   
generatePlot <- function(tab_inegalites,indicateur,colors,caption,nb_row) {
  
  tab <- tab_inegalites %>%
    mutate(
      taux_str = paste0(indicateur, "%"),
      tooltip_value = paste0(modalité, " : " , taux_str)
    )
  
  tab$Diplôme = factor(tab$Diplôme, levels = c("Ensemble des sortants","Non-diplômés","Diplômés du secondaire",    
                                                "Diplômés du supérieur court","Diplômés du supérieur long"))
  tab$mod_2 <- gsub("'", " ", tab$modalité)
  ggplot(tab, aes(x = Diplôme, y = indicateur, fill = modalité)) +
    geom_col_interactive(width = 1, color = "white", mapping = aes(data_id = mod_2,
                                                                     tooltip = tooltip_value)) +
    coord_flip() +
    geom_text(aes(label = taux_str),
      position = position_stack(vjust = .5),
      color = "white",
      size = 2
    ) +
    scale_fill_manual(values = colors, 
                      labels = scales::label_wrap(20),
                      guide = guide_legend(nrow = nb_row, byrow = TRUE,
                                           override.aes = list(shape = 16))
                      ) +
    scale_y_continuous(trans = "reverse") +
    labs(caption = caption) +
    theme(
      legend.position = "top",
      legend.justification="center",
      legend.box.spacing = unit(0, "pt"),
      legend.margin=margin(0, 0, 10, 0),
      legend.text = element_text(size = 8, face = "plain"),
      plot.caption = element_markdown(
        hjust = 0,
        color = "#C0C0C2",
        size = 8
      )
    ) 
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo"),
    text = element_text(size = 10, family = "Arimo"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_markdown(size = 8, color = "#008B99"),
    plot.caption.position = "plot"
  )
)

labellize_stats_end_i <- function(info_str, infobulle_str = NULL) {
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
    )
}

labellize_stats_no_i <- function(info_str) {
    tags$p(
      class = "texte-stat-info",
      tags$span(
        style = "color: #008B99;",
        info_str
      )
      )
}

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}