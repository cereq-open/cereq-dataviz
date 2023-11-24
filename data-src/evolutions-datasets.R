library(tidyverse)
library(arrow)
library(readxl)
library(htmltools)

# data preparation ----

## augment tab_inegalites  ----

tab_evolution <- read_excel("data/tab_evolution vf.xlsx") %>%
  rename(diplome = Libelle_Menu, annee = Année) |>
  mutate(
    annee = factor(annee),
    diplome = str_trim(diplome) # Supprime les espaces en début et fin de chaînes de caractères
  ) %>%
  mutate(
    annee = case_when(
      annee == "2010" ~ "Sortis en 2010",
      annee == "2013" ~ "Sortis en 2013",
      annee == "2017" ~ "Sortis en 2017",
      TRUE ~ annee
    )
  ) 


## captions ----
champ <- '<span style="color:#008B99;">Champ : </span>'
source <- paste0(
  '<span style="color:#008B99;">Sources : </span>',
  "Céreq, enquêtes Génération 2010, Génération 2013 et Génération 2017."
)
caption_part_1 <- paste0(
  champ,
  "Ensemble de la Génération.",
  "<br>",
  source
)

caption_part_2 <- paste0(
  champ,
  "Ensemble de la Génération en emploi trois ans après leur sortie de formation initiale.",
  "<br>",
  source
)


data_by_type_diplome <- split(tab_evolution, tab_evolution$diplome)

tab_variables_evolution <- read_excel("data/variables EVOLUTIONS_OJ.xlsx") |>
  filter(`Variable affichée` %in% "Oui") |>
  select(-`Variable affichée`)

columns_titles <- split(tab_variables_evolution, tab_variables_evolution$Nom_colonne) |>
  lapply(function(x) {
    z <- list(
      .title = x$Titre_graphique,
      .tooltip = x$Bulle
    )
    if (is.na(z$.tooltip)) {
      z$.tooltip <- NULL
    }
    z
  })

c("taux_emploi", "part_chomage", "taux_chomage", "taux_edi", "part_tps_partiel", "revenu_travail", "competence_ok")
## menus and submenus preparation ----

type_diplome <- tab_evolution |>
  select(1, diplome) |>
  setNames(c("ordre", "diplome")) |>
  arrange(ordre) |>
  distinct()
type_diplome <- type_diplome$diplome

# save data for reuse in shiny app ----
# with no calculation to do later
saveRDS(data_by_type_diplome, file = "evolution/data/data_by_type_diplome.RDS")
saveRDS(type_diplome, file = "evolution/data/type_diplome.RDS")
saveRDS(caption_part_1, file = "evolution/data/caption_part_1.RDS")
saveRDS(caption_part_2, file = "evolution/data/caption_part_2.RDS")
saveRDS(columns_titles, file = "evolution/data/columns_titles.RDS")

file.copy("src-templates/graphics-settings.R",
  "evolution/graphics-settings.R",
  overwrite = TRUE
)
file.copy("src-templates/shiny-elements.R",
  "evolution/shiny-elements.R",
  overwrite = TRUE
)
file.copy("src-templates/www",
  "evolution",
  recursive = TRUE,
  overwrite = TRUE
)
