library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

options(shiny.useragg = TRUE)

data <- read_excel("data/db_diplome.xls")
data <- data %>% filter(!row_number() %in% c(35,36,37,38)) # We are going to get directly rid of these rows from the CSV file later.

# Keep only the levels whose code should not start with 0.
list_degre1_2 <- as.list(filter(data, str_sub(Code, -1, -1)  == "0")  %>% pull(`Libelle_Menu`)) 

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