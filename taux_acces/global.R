
library(sjPlot)  
library(arrow)
library(ggplot2)

library(ggtext)
library(ggiraph)
library(gdtools)

library(bslib)
EFE_2 <- read_parquet("data/base_EFE_2.parquet")
library(forcats)


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



colors_age<-setNames(c("#046B76","#7FC4CB","#7FC4CB","#7FC4CB"),unique(EFE_2$age))
colors_sexe<-setNames(c("#046B76","#7FC4CB","#7FC4CB"),unique(EFE_2$sexe))
colors_cs<-setNames(c("#046B76","#7FC4CB","#7FC4CB","#7FC4CB","#7FC4CB"),unique(EFE_2$cs))
colors_sexe_cs<-c("#7FC4CB","#e8d272","#046B76", "#F8AC00")



plot_barchart <- function(df, y_col,x_col, caption_texte, color_bar, titre = NULL) {


  ggplot(data = df, aes(x = x_col, y=!!sym(y_col),fill=x_col, tooltip =paste0(x_col," : ", tx_acc, " %" ))) + 
    scale_fill_manual(values = color_bar) + 

    geom_bar_interactive(stat = "identity", position = "dodge") + geom_text(aes(x= x_col, label = paste0(tx_acc, " %" )) ,position=position_stack(vjust = 0.5), size= 6, color = "white") +
    labs(caption = caption_texte, title = titre)
}


theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Arimo", size = 6),
    text = element_text(size = 11, family = "Arimo"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "#008B99", size = 10, hjust = 0.4, vjust = 5),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x =  element_blank(),
    plot.title =  element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption.position = "plot",
    legend.position = "none",
    legend.justification = "center",
    plot.caption = element_textbox_simple( hjust = 100, color = "#808080", size = 10 , margin = margin(t = -1)) 
  )
)

DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label  )}

