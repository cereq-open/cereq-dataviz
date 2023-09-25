library(tidyverse)
library(arrow)
library(readxl)
library(htmltools)
library(sf)

# functions to prepare data ----
labellize_stat <- function(info_str, stat1_str, stat2_str) {
  tags$p(
    tags$span(
      class = "stat-label",
      info_str
    ),
    tags$span(
      class = "stat-value",
      stat1_str
    ),
    if (!is.null(stat2_str)) {
      tags$span(
        class = "stat-other",
        style = "color: #C0C0C2;",
        stat2_str
      )
    }
  )
}

# data preparation ----

## geo processing ----
gadm <- readRDS("data/map/gadm36_FRA_1_sf.rds") |>
  st_as_sf() |> # Convertis en objet sf
  st_set_crs(4326) |>
  # Attribue le système de coordonnées géographiques WGS 84
  mutate(NAME_1 = stringi::stri_trans_general(NAME_1, id = "latin-ascii"))

## stats par regions ----
db_stats_par_regions <- arrow::read_parquet("data/tab_region.parquet") |>
  rename(pos_prof_inter = pos_prof_int)

db_stats_ens_et_drom <- db_stats_par_regions %>%
  filter(Libellé %in% c("Ensemble", "DROM"))

db_stats_par_regions <- db_stats_par_regions %>%
  filter(!Libellé %in% c("Ensemble", "DROM")) %>%
  mutate(Libellé = stringi::stri_trans_general(Libellé, id = "latin-ascii"))

# Remplace "Haut-de-France" par "Hauts-de-France" dans la colonne "Libellé"
db_stats_par_regions$Libellé <- gsub("Haut-de-France", "Hauts-de-France", db_stats_par_regions$Libellé)

# Remplace "Nouvelle Aquitaine" par "Nouvelle-Aquitaine" dans la colonne "Libellé"
db_stats_par_regions$Libellé <- gsub("Nouvelle Aquitaine", "Nouvelle-Aquitaine", db_stats_par_regions$Libellé)

# Duplique la ligne "Provence-Alpes-Cote-d'Azur et Corse"
ligne_a_dupliquer <- db_stats_par_regions[12, ]

# Remplace "Provence-Alpes-Cote-d'Azur et Corse" par "Provence-Alpes-Cote d'Azur" dans la colonne "Libellé"
ligne_a_dupliquer[[1]] <- "Provence-Alpes-Cote d'Azur"

db_stats_par_regions <- rbind(db_stats_par_regions, ligne_a_dupliquer)

# Remplace "Provence-Alpes-Cote-d'Azur et Corse" par "Corse" dans la colonne "Libellé"
db_stats_par_regions$Libellé <- gsub("Provence-Alpes-Cote-d'Azur et Corse", "Corse", db_stats_par_regions$Libellé)

# Jointure des deux tables
db_stats_par_regions <- left_join(db_stats_par_regions, gadm, by = c("Libellé" = "NAME_1")) |>
  st_as_sf() |> # Convertis le dataframe en objet spatial sf
  mutate(
    Libellé = ifelse(Libellé == "Corse", "P.A.C.A. et Corse",
      ifelse(Libellé == "Provence-Alpes-Cote d'Azur", "P.A.C.A. et Corse", Libellé)
    ),
    Libellé = toupper(Libellé)
  )


### indicateurs ----
db_indicateurs <- read_excel("data/variables REGION.xlsx") |>
  filter(!is.na(Ordre_menu1)) |>
  select(-Ordre_menu2) |>
  rename(
    indicateur = Nom_colonne,
    label = Titre_graphique, tooltip = Bulle
  )

valeurs_indicateurs <- setNames(db_indicateurs$indicateur, db_indicateurs$label)

liste_titre_indicateurs <- split(db_indicateurs, db_indicateurs$indicateur) |>
  lapply(function(x) {
    z <- list(.title = x$label)
    if (!is.na(x$tooltip)) {
      z$.tooltip <- x$tooltip
    }
    z
  })


liste_label_indicateurs <- lapply(db_indicateurs$indicateur, function(colonne, db_stat) {
  if (colonne != "revenu_travail") {
    symbole <- " %"
  } else {
    symbole <- " €"
  }

  stat_france <- paste0(db_stat[1, colonne], symbole)
  stat_drom <- paste0("(", paste0("dont ensemble des D.R.O.M. : ", db_stat[2, colonne], symbole), ")")
  labellize_stat(
    info_str = "France : ",
    stat1_str = stat_france,
    stat2_str = stat_drom
  )
}, db_stat = db_stats_ens_et_drom) |> setNames(db_indicateurs$indicateur)

### niveaux_diplomes ----
db_niveaux_diplomes <- read_excel("data/variables REGION.xlsx") |>
  filter(!is.na(Ordre_menu2)) |>
  select(-Ordre_menu1, -Bulle)

valeurs_niveaux_diplomes <- setNames(db_niveaux_diplomes$Nom_colonne, db_niveaux_diplomes$Titre_graphique)

liste_label_niveaux_diplomes <- lapply(db_niveaux_diplomes$Nom_colonne, function(colonne, db_stat) {
  symbole <- " %"

  stat_france <- paste0(db_stat[1, colonne], symbole)
  stat_drom <- paste0("(", paste0("dont ensemble des D.R.O.M. : ", db_stat[2, colonne], symbole), ")")
  labellize_stat(
    info_str = "France : ",
    stat1_str = stat_france,
    stat2_str = stat_drom
  )
}, db_stat = db_stats_ens_et_drom) |> setNames(db_niveaux_diplomes$Nom_colonne)


# save data for reuse in shiny app ----
# with no calculation to do later
saveRDS(valeurs_indicateurs, file = "regions/data/valeurs_indicateurs.RDS")
saveRDS(liste_titre_indicateurs, file = "regions/data/liste_titre_indicateurs.RDS")
saveRDS(liste_label_indicateurs, file = "regions/data/liste_label_indicateurs.RDS")
saveRDS(valeurs_niveaux_diplomes, file = "regions/data/valeurs_niveaux_diplomes.RDS")
saveRDS(liste_label_niveaux_diplomes, file = "regions/data/liste_label_niveaux_diplomes.RDS")
saveRDS(db_stats_par_regions, file = "regions/data/db_stats_par_regions.RDS")

file.copy("src-templates/graphics-settings.R",
  "regions/graphics-settings.R",
  overwrite = TRUE
)
file.copy("src-templates/shiny-elements.R",
  "regions/shiny-elements.R",
  overwrite = TRUE
)
file.copy("src-templates/www",
  "regions",
  recursive = TRUE,
  overwrite = TRUE
)
