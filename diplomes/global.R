suppressPackageStartupMessages({library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
})

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
  
  DT$emploi[DT$emploi == "taux_emploi"] <- "En emploi"
  DT$emploi[DT$emploi == "taux_chomage"] <- "Au chômage"
  DT$emploi[DT$emploi == "autre_situations"] <- "Autres situations"
  DT$emploi <- factor(DT$emploi, levels = c("En emploi", "Au chômage", "Autres situations"))
  DT$taux_str <- paste0(DT$taux, "%")
  
  colors <- c("En emploi"="#008B99", "Au chômage"="#EF5350", "Autres situations"="#F8AC00")
  
  caption <- paste0("Lecture : ",
                    "Trois ans après leur sortie de formation initiale ",
                    DT$taux_str[1],
                    " des jeunes de la Génération 2017 sont en emploi, ",
                    DT$taux_str[2],
                    " au chômage et ",
                    DT$taux_str[3],
                    " dans une autre situation.",
                    "\n",
                    "Champ : ",
                    "Ensemble de la Génération 2017.",
                    "\n",
                    "Source : ",
                    "Céreq, enquête Génération 2017 à trois ans."
                    )
  
  ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
    geom_bar(stat = "identity", width = 0.5) + coord_flip() +
    geom_text(aes(label = taux),
              position = position_stack(vjust = .5),
              color = "white",
              size = 10) +
    scale_fill_manual(values = colors) +
    labs(caption = caption) +
    theme(legend.position = "bottom",    # Place la légende en bas
          legend.direction = "horizontal",    # Orientation de la légende en ligne
          legend.box = "horizontal",    # Boîte de la légende en ligne
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0,
                                      color="gray",
                                      #face = "bold",
                                      margin = margin(t = 10)))
}

generatePlotSpec <- function(db_diplome, niveau, libelle) {
  
  DT <- db_diplome %>%
    filter(Code %in% c(niveau, 100) & Libelle_Menu %in% c(libelle, "Ensemble des sortants")) %>%
    select(Code, Libelle_Menu, Libelle_complet, taux_emploi, taux_chomage) %>%
    mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
    pivot_longer(
      cols = c("taux_emploi", "taux_chomage", "autre_situations"),
      names_to = "emploi",
      values_to = "taux"
    )
  
  DT$emploi[DT$emploi == "taux_emploi"] <- "En emploi"
  DT$emploi[DT$emploi == "taux_chomage"] <- "Au chômage"
  DT$emploi[DT$emploi == "autre_situations"] <- "Autres situations"
  DT$emploi <- factor(DT$emploi, levels = c("En emploi", "Au chômage", "Autres situations"))
  DT$taux_str <- paste0(DT$taux, "%")
  
  colors <- c("En emploi"="#008B99", "Au chômage"="#EF5350", "Autres situations"="#F8AC00")
  
  caption <- paste0("Lecture : ",
                    "Trois ans après leur sortie de formation initiale ",
                    DT$taux_str[1],
                    " des jeunes de la Génération 2017 sont en emploi, ",
                    DT$taux_str[2],
                    " au chômage et ",
                    DT$taux_str[3],
                    " dans une autre situation.",
                    "\n",
                    "Champ : ",
                    "Ensemble de la Génération 2017.",
                    "\n",
                    "Source : ",
                    "Céreq, enquête Génération 2017 à trois ans."
  )
  
  ggplot(DT, aes(Libelle_complet, taux, fill = emploi)) +
    geom_bar(stat = "identity", width = 0.5) + coord_flip() +
    geom_text(aes(label = taux),
              position = position_stack(vjust = .5),
              colour = "white",
              size = 10) +
    scale_fill_manual(values = colors) +
    labs(caption = caption) +
    theme(legend.position = "bottom",    # Place la légende en bas
          legend.direction = "horizontal",    # Orientation de la légende en ligne
          legend.box = "horizontal",    # Boîte de la légende en ligne
          plot.caption = element_text(hjust = 0,
                                      color="#303032",
                                      margin = margin(t = 10)))
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
  
  colors <- c("#008B99", "#256299", "#EF5350", "#F8AC00", "#7B9A62")
  
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
    plot.caption.position = "plot",
    plot.caption = element_text(size = 11),
    legend.text = element_text(family = "Open Sans", size = 12),
    legend.title = element_blank()
  )
)