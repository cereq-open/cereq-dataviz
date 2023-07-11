library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

options(shiny.useragg = TRUE)

db_diplome <- read_excel("data/db_diplome.xls")

# Keep only the levels whose code should not start with 0.
list_degre1_2 <- as.list(filter(db_diplome, str_sub(Code, -1, -1)  == "0")  %>% pull(`Libelle_Menu`))

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
    )
  
  colors <- c("taux_emploi"="#008B99", "taux_chomage"="#4C9A9A", "autre_situations"="#C0C0C2")
  
  ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
    geom_bar(stat = "identity", width = 0.5) + coord_flip() +
    geom_text(aes(label = taux),
              position = position_stack(vjust = .5)) +
    scale_fill_manual(values = colors)
    
}

######### Create Pie charts ########################

generatePieProfession <- function(db_diplome, niveau) {
  DT <- db_diplome %>%
    select(Libelle_Menu, pos_cadres,	pos_prof_int,	pos_emp_ouv_q,	pos_emp_ouv_nq,	pos_autres) %>%
    filter(Libelle_Menu == niveau) %>%
    pivot_longer(
      cols = c("pos_cadres",	"pos_prof_int",	"pos_emp_ouv_q",	"pos_emp_ouv_nq",	"pos_autres"),
      names_to = "profession",
      values_to = "taux"
    )
  
  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
  
  ggplot(DT, aes(x = "", y = taux, fill = profession)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black") +
    scale_fill_manual(values = colors)
}

generatePieSecteur <- function(db_diplome, niveau) {
  
  DT <- db_diplome %>%
    select(Libelle_Menu, sec_industries_btp,	sec_commerce,	sec_administration,	sec_a_services,	sec_autres) %>%
    filter(Libelle_Menu == niveau) %>%
    pivot_longer(
      cols = c("sec_industries_btp",	"sec_commerce",	"sec_administration",	"sec_a_services",	"sec_autres"),
      names_to = "secteur",
      values_to = "taux"
    )
  
  colors <- c("#008B99", "#FAC05E", "#4C9A9A", "#FF7043", "#6E88A4")
  
  ggplot(DT, aes(x = "", y = taux, fill = secteur)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black") +
    scale_fill_manual(values = colors)
}

generateDataForLevel <- function(db_diplome, niveau) {
  
  filtered_data <- db_diplome %>%
    filter(Libelle_Menu == niveau)
}

generateDataForLevel3 <- function(db_diplome, code_niveau3, niveau3) {
  
  req(code_niveau3, code_niveau3)
  
  filtered_data <- db_diplome %>%
    filter(Code == code_niveau3 & Libelle_Menu %in% niveau3)
}

theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    title = element_text(family = "Open Sans"),
    text = element_text(family = "Open Sans"),
    panel.background = element_blank(),
    panel.grid = element_line(colour = "#D6D8DD", linewidth = 0.1),
    axis.ticks = element_line(colour = "#D6D8DD", linewidth = 0.1),
    axis.text.y = element_text(family = "Open Sans", size = 12),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_rect(color = "#D6D8DD", linewidth = 0.1),
    plot.title = element_text(face = "bold", size = 20),
    plot.caption = element_text(face = "bold", size = 11),
    legend.text = element_text(family = "Open Sans", size = 12),
    legend.title = element_blank()
  )
)