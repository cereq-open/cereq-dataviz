library(tidyverse)
library(arrow)
library(readxl)
library(htmltools)

# functions to prepare data ----
generateCaption <- function(variable) {
  if (variable %in% c("taux_emploi", "part_chomage", "taux_chomage", "traj_1", "traj_2", "traj_3", "traj_7")) {
    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la Génération.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  } else {
    caption <- paste0(
      '<span style="color:#008B99;">Champ : </span>',
      "Ensemble de la génération en emploi trois ans \
      après leur sortie de formation initiale.",
      "<br>",
      '<span style="color:#008B99;">Source : </span>',
      "Céreq, enquête Génération 2017 à trois ans."
    )
  }
  caption
}


# data preparation ----

## menus

tab_variables_inegalites <- read_excel("data/variables_INEGALITES.xlsx")

tab_facteurs <- tab_variables_inegalites |>
  filter(!is.na(Ordre_menu2)) |>
  arrange(Ordre_menu2) |>
  select(facteur = libelle_menu2, label = libelle_menu2)

tab_indicateurs <- tab_variables_inegalites |>
  filter(!is.na(Nom_colonne)) |>
  arrange(Ordre_menu1) |>
  select(
    indicateur = Nom_colonne, label = Titre_graphique,
    tooltip = Bulle
  )

rm(tab_variables_inegalites)

indicateur_captions <- lapply(tab_indicateurs$indicateur, generateCaption) |>
  setNames(tab_indicateurs$indicateur)
indicateur_titles <- split(tab_indicateurs, tab_indicateurs$indicateur) |>
  lapply(function(x) {
    z <- list(title = x$label)
    if (!is.na(x$tooltip)) {
      z$tooltip <- x$tooltip
    }
    z
  })
indicateur_captions <- lapply(tab_indicateurs$indicateur, generateCaption) |>
  setNames(tab_indicateurs$indicateur)

## augment tab_inegalites  ----

tab_inegalites <- read_parquet("data/tab_inegalites.parquet") %>%
  rename(
    diplome = Diplôme, ordre_graphique = `Ordre graphique`,
    modalite = modalité
  ) |>
  mutate(diplome = str_trim(diplome)) %>%
  mutate(diplome = case_when(
    str_detect(diplome, "Diplômées") ~ str_replace(diplome, "Diplômées", "Diplômés"),
    str_detect(diplome, "diplômées") ~ str_replace(diplome, "diplômées", "diplômés"),
    TRUE ~ diplome # on garde la même valeur pour toutes les autres valeurs
  )) %>%
  mutate(across(
    c(
      taux_emploi, part_chomage, taux_chomage,
      taux_edi, part_tps_partiel, revenu_travail,
      traj_1, traj_2, traj_3, traj_7,
      correspondance_ok, competence_ok
    ),
    ~ ifelse(. == -900, -1, .)
  )) |>
  arrange(facteur_analyse, ordre_graphique) |>
  mutate(
    diplome = factor(diplome, levels = c(
      "Ensemble des sortants", "Non-diplômés", "Diplômés du secondaire",
      "Diplômés du supérieur court", "Diplômés du supérieur long"
    ))
  )
data_par_facteur_analyse <- split(tab_inegalites, tab_inegalites$facteur_analyse)
data_par_facteur_analyse <- lapply(data_par_facteur_analyse, function(x, indicateurs) {
  modalite_levels <- unique(x$modalite)
  data <- mutate(x, modalite = factor(modalite, levels = modalite_levels)) |>
    select(modalite, diplome, all_of(indicateurs)) |>
    pivot_longer(
      cols = all_of(indicateurs),
      names_to = "indicateur",
      values_to = "statistique"
    ) |>
    mutate(
      symbole = case_when(
        indicateur %in% "revenu_travail" ~ " €",
        TRUE ~ " %"
      ),
      data_id = gsub("'", "&apos;", modalite),
      taux_str = case_when(
        indicateur %in% "revenu_travail" ~ paste0(
          sprintf("%.0f", statistique),
          symbole
        ),
        TRUE ~ paste0(
          sprintf("%2.0f", statistique),
          symbole
        )
      ),
      taux_str = case_when(
        statistique < 1 ~ "% ns",
        TRUE ~ taux_str
      ),
      tooltip_value = paste0(modalite, " : ", taux_str),
      tooltip_value = case_when(
        statistique < 0 ~ paste0(modalite, " : ", "Nombre de répondants insuffisants"),
        TRUE ~ tooltip_value
      ),
      statistique = case_when(
        statistique < 0 ~ 0,
        TRUE ~ statistique
      ),
      symbole = NULL
    )
  split(data |> select(-indicateur), data$indicateur)
}, indicateurs = tab_indicateurs$indicateur)

indicateur_choices <- setNames(tab_indicateurs$indicateur, tab_indicateurs$label)
indicateur_title <- setNames(tab_indicateurs$label, tab_indicateurs$indicateur)
facteur_choices <- setNames(tab_facteurs$facteur, tab_facteurs$label)

# save data for reuse in shiny app ----
# with no calculation to do later
saveRDS(data_par_facteur_analyse, file = "inegalites/data/data_par_facteur_analyse.RDS")
saveRDS(tab_facteurs, file = "inegalites/data/tab_facteurs.RDS")
saveRDS(tab_indicateurs, file = "inegalites/data/tab_indicateurs.RDS")
saveRDS(indicateur_choices, file = "inegalites/data/indicateur_choices.RDS")
saveRDS(indicateur_title, file = "inegalites/data/indicateur_title.RDS")
saveRDS(facteur_choices, file = "inegalites/data/facteur_choices.RDS")
saveRDS(indicateur_captions, file = "inegalites/data/indicateur_captions.RDS")
saveRDS(indicateur_titles, file = "inegalites/data/indicateur_titles.RDS")

file.copy("src-templates/graphics-settings.R",
  "inegalites/graphics-settings.R",
  overwrite = TRUE
)
file.copy("src-templates/shiny-elements.R",
  "inegalites/shiny-elements.R",
  overwrite = TRUE
)
file.copy("src-templates/www",
  "inegalites",
  recursive = TRUE,
  overwrite = TRUE
)
