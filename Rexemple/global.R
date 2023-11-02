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
  library(readxl)
  library(forcats)
})

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

linebreaks <- function(n){HTML(strrep(br(), n))}

# Load data --------------------------------------------------------------------

EFE_1 <- read_parquet("data/base_EFE_1.parquet")

EFE_1 <- EFE_1 %>% 
  rename( secteur =lib_secteur)

EFE_1 <- EFE_1 %>% 
  rename(  taille= lib_taille)

EFE_1$taille[EFE_1$taille == '1 à 3 salariés'] <- '1 à 3'
EFE_1$taille[EFE_1$taille == '4 à 9 salariés'] <- '4 à 9'
EFE_1$taille[EFE_1$taille == '10 à 19 salariés'] <- '10 à 19'
EFE_1$taille[EFE_1$taille == '20 à 49 salariés'] <- '20 à 49'
EFE_1$taille[EFE_1$taille == '50 à 249 salariés'] <- '50 à 249'
EFE_1$taille[EFE_1$taille == '250 à 499 salariés'] <- '250 à 499'
EFE_1$taille[EFE_1$taille == '500 à 999 salariés'] <- '500 à 999'
EFE_1$taille[EFE_1$taille == '1000 salariés et plus'] <- '1000 et plus'
EFE_1$secteur[EFE_1$secteur == 'Ensemble'] <- 'Ensemble des secteurs'

EFE_1$tx_acc <- round(EFE_1$tx_acc, digits = 0)
EFE_1$heurstag <- round(EFE_1$heurstag, digits = 0)
EFE_1$tx_form <- round(EFE_1$tx_form, digits = 0)
EFE_1$tx_tpf <- round(EFE_1$tx_tpf, digits = 1)
EFE_1$heurstag_sal <- round(EFE_1$heurstag_sal, digits = 0)
EFE_1$tx_courses <- round(EFE_1$tx_courses, digits = 0)
EFE_1$top1_c5_tx <- round(EFE_1$top1_c5_tx, digits = 0)
EFE_1$top2_c5_tx <- round(EFE_1$top2_c5_tx, digits = 0)
EFE_1$top3_c5_tx <- round(EFE_1$top3_c5_tx, digits = 0)
EFE_1$top1_d3_tx <- round(EFE_1$top1_d3_tx, digits = 0)
EFE_1$top2_d3_tx <- round(EFE_1$top2_d3_tx, digits = 0)
EFE_1$top3_d3_tx <- round(EFE_1$top3_d3_tx, digits = 0)
EFE_1$top1_e1_tx <- round(EFE_1$top1_e1_tx, digits = 0)
EFE_1$top2_e1_tx <- round(EFE_1$top2_e1_tx, digits = 0)
EFE_1$top3_e1_tx <- round(EFE_1$top3_e1_tx, digits = 0)


vec_secteur <- unique(EFE_1$secteur)


# Couleurs des barplots



col=rep("#7FC4CB",27)
colors2<-setNames(c("#046B76","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB"),unique(EFE_1$secteur))
colors <-setNames(c("#046B76",col),unique(EFE_1$secteur))


# Couleurs des barplots
source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Source : Céreq-Dares-France compétences, Enquête Formation Employeur – européenne (EFE-e, Données 2020)"
)


caption_part_1 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Ensemble des entreprises de 1 salarié et plus du secteur privé (Hors activités des ménages et extraterritoriales)",
  "<br>",
  source
)




caption_part_2 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "blaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  "<br>",
  caption_part_1,
  source
)
#TOOLTIP
concat_value <- function(df, nom_colonne) {
  if (nom_colonne != "heurstag") {
    df["taux_str"] <- paste0(df[[nom_colonne]], "%")
  } else {
    df["taux_str"] <- paste0(df[[nom_colonne]], "h")
  }
  
  df[["tooltip_value"]] <- paste0(df[["secteur"]], " : ", df[["taux_str"]] )
  
  return(df)
}



#TITRE
generateTitle <- function(title, infobulle_str = NULL) {
  
  tagList(
    tags$p(
      class = "texte-stat-info",
      title,
      if (!is.null(infobulle_str)) {
        tags$i(
          style = "color: #008B99; font-size: 16px;",
          class = "fas fa-info-circle",
          title = infobulle_str,
        )
      }
    )
  )
}


plot_barchart <- function(df, y_col, caption_texte, titre = NULL) {
  DT <- concat_value(df, y_col)
  ggplot(data = DT, aes(x = taille, y = !!sym(y_col), fill = as.factor(secteur), tooltip =tooltip_value, data_id = taille )) + geom_bar_interactive(stat = "identity", position = "dodge")  +
    geom_text(aes(x= taille, label = taux_str),  position = position_dodge(width = 0.9),vjust= 2, size= 2, color = "white") + scale_fill_manual(values = colors2) + 
    labs(caption = caption_texte, title = titre,x = "Taille de l'entreprise en nombre de salarié") +
    scale_x_discrete(labels = c("1 à 3", "4 à 9", "10 à 19","20 à 49","50 à 249","250 à 499", "500 à 999", "1000 et +",       expression(bold("Ensemble")) ))
}

top3 <-function(y1 , y2, y3) {
  str1<- paste(y1 , y2, y3 , sep='<br/>')
  HTML(paste(str1))
}

plot_only_legend <- function(df) {
  ggplot(data = df, aes(x = taille, y = tx_acc , fill = secteur)) +
    geom_col_interactive(mapping = aes(data_id = taille)) +
    scale_fill_manual(values = colors) +
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = "top",
      legend.justification = "center",
      legend.box.spacing = unit(0, "pt"),
      legend.text = element_text(size = 11, face = "plain")
    )
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo", size = 6),
    text = element_text(size = 11, family = "Arimo"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "#008B99", size = 8, hjust = 0.4, vjust = 5),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
    plot.title = element_textbox_simple(hjust = 0, size = 17, color = "#008B99"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.position = "top",
    legend.justification = "center",
    plot.caption = element_textbox_simple( hjust = 100, color = "#808080", size = 10 , margin = margin(t = -1)) 
  )
)

# Download data ----------------------------------------------------------------

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}
