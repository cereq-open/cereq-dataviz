library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

options(shiny.useragg = TRUE)

data <- read_excel("data/db_diplome.xls")
data <- data %>% filter(!row_number() %in% c(35,36,37,38)) # We are going to get directly rid of these rows from the CSV file later.

list_degre1 <- as.list(filter(data, str_sub(Code, -2, -1)  == "00")  %>% pull(`Libelle_Menu`)) # Filter the data based on the list of first levels whose codes should end up with 00 according to the dataset.

sequence <- seq(100,600,by=10) # Define a sequence of values from 100 to a rondom value i.e. 600
intersect <- as.numeric(intersect(sequence,data$Code))
level_1_2 <- split(intersect, cumsum(c(TRUE, diff(intersect)!=10))) # Split the sequence to sub groups.
names(level_1_2) <- list_degre1

# Function to replace list components
replace_list_components <- function(lst, df, column_name) {
  for (i in seq_along(lst)) {
    for (j in seq_along(lst[[i]])) {
      value_to_replace <- lst[[i]][[j]]
      matching_row <- df[df[[column_name]] == value_to_replace, "Libelle_Menu"]
      
      if (length(matching_row) > 0) {
        lst[[i]][[j]] <- matching_row
      }
    }
  }
  
  lst
}

# Call the function to replace list components
updated_list <- replace_list_components(level_1_2, data, "Code")

# Transformed list without tibbles at the second level
transformed_list <- lapply(updated_list, function(x) lapply(x, function(y) y$Libelle_Menu))

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