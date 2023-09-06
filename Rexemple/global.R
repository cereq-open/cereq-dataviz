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

EFE_1 <- read_parquet("data/indicateur_JC_0409.parquet")






donnees_tx_acces <- read_excel("data/excel_tx_accès.xlsx")


vec_secteur <- unique(EFE_1$secteur)

# Couleurs des barplots

EFE_1$secteur_ensemble <-as.character(EFE_1$secteur_ensemble)






EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "1"] <- "Ensembles des secteurs"
EFE_1$secteur_ensemble[EFE_1$secteur_ensemble == "0"] <- "Secteur choisi"

col=rep("#F8AC00",27)
colors2<-setNames(c("#256299","#F8AC00"),unique(EFE_1$secteur_ensemblebis))
colors <-setNames(c("#256299",col),unique(EFE_1$secteur))


# Couleurs des barplots
source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Céreq, enquêtes CVTS_6"
)


caption_part_1 <- paste0(
  '<span style="color:#008B99;">Champ : </span>',
  "Entreprise de 3 salariés et plus",
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
  ggplot(data = DT, aes(x = taille, y = !!sym(y_col), fill = as.factor(secteur_ensemble), tooltip =tooltip_value, data_id = taille )) + geom_bar_interactive(stat = "identity", position = "dodge")  +
    geom_text(aes(x= taille, label = taux_str),  position = position_dodge(width = 1),vjust=-0.5, size= 2.5, color = "Black", fontface = "bold") + scale_fill_manual(values = colors2) + 
    labs(caption = caption_texte, title = titre,x = "Taille de l'entreprise en nombre de salarié") 
  
}

top3 <-function(y1 , y2, y3) {
  str1<- paste(y1 , y2, y3 , sep='<br/>')
  HTML(paste(str1))
}

plot_only_legend <- function(df) {
  ggplot(data = df, aes(x = taille, y = tx_acc1 , fill = secteur)) +
    geom_col_interactive(mapping = aes(data_id = taille)) +
    scale_fill_manual(values = colors) +
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = "top",
      legend.justification = "left",
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
    axis.text.x = element_text(color = "#008B99", size = 8, hjust = 0.4, vjust = 8),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "#008B99", size = 10, vjust = 4),
    plot.title = element_textbox_simple(hjust = 0, size = 17, color = "#008B99"),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    plot.caption = element_textbox_simple( hjust = 100, color = "#C0C0C2", size = 10 , margin = margin(t = -1)) 
  )
)

# Download data ----------------------------------------------------------------

DownloadButton <- function(outputId, label = label){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}
