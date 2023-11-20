library(tidyverse)
library(htmltools)

# functions to prepare data ----
symbole_pourcentage <- " %"

generateDTBarChart <- function(subset_df) {
  data <- subset_df %>%
    select(Code, Libelle_Menu, Libelle_complet, taux_emploi, part_chomage) %>%
    mutate(autre_situations = 100 - (taux_emploi + part_chomage)) %>%
    pivot_longer(
      cols = c("taux_emploi", "part_chomage", "autre_situations"),
      names_to = "emploi",
      values_to = "taux"
    ) %>%
    mutate(
      emploi = case_when(
        emploi == "taux_emploi" ~ "En emploi",
        emploi == "part_chomage" ~ "Au chômage",
        emploi == "autre_situations" ~ "Autres situations",
        TRUE ~ emploi
      ),
      emploi = factor(emploi, levels = c("En emploi", "Au chômage", "Autres situations")),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(emploi, " : ", taux_str),
      fake_axis = "A"
    )
  data
}

generateDTDonutChartProfession <- function(dat) {
  data <- dat %>%
    slice(1) |>
    select(Code, Libelle_Menu, pos_cadres, pos_prof_int, pos_emp_ouv_q, pos_emp_ouv_nq, pos_autres) %>%
    mutate(
      pos_cadres = gsub(",", ".", pos_cadres),
      pos_prof_int = gsub(",", ".", pos_prof_int),
      pos_emp_ouv_q = gsub(",", ".", pos_emp_ouv_q),
      pos_emp_ouv_nq = gsub(",", ".", pos_emp_ouv_nq),
      pos_autres = gsub(",", ".", pos_autres)
    ) %>%
    pivot_longer(
      cols = c("pos_cadres", "pos_prof_int", "pos_emp_ouv_q", "pos_emp_ouv_nq", "pos_autres"),
      names_to = "profession",
      values_to = "taux"
    ) %>%
    mutate(
      taux = as.numeric(taux),
      fraction = taux / sum(taux),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      labelPosition = (ymax + ymin) / 2,
      label = paste0(profession, "\n ", taux),
      profession = case_when(
        profession == "pos_cadres" ~ "Cadres",
        profession == "pos_prof_int" ~ "Professions intermédiaires",
        profession == "pos_emp_ouv_q" ~ "Employés ou ouvriers qualifiés",
        profession == "pos_emp_ouv_nq" ~ "Employés ou ouvriers non qualifiés",
        profession == "pos_autres" ~ "Autres",
        TRUE ~ profession
      ),
      profession = factor(profession, levels = c(
        "Cadres", "Professions intermédiaires",
        "Employés ou ouvriers qualifiés",
        "Employés ou ouvriers non qualifiés",
        "Autres"
      )),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(profession, " : ", taux_str)
    )

  data
}

generateDTDonutChartSecteur <- function(dat) {
  data <- dat %>%
    slice(1) |>
    select(Code, Libelle_Menu, sec_industries_btp, sec_commerce, sec_administration, sec_a_services, sec_autres) %>%
    mutate(
      sec_industries_btp = gsub(",", ".", sec_industries_btp),
      sec_commerce = gsub(",", ".", sec_commerce),
      sec_administration = gsub(",", ".", sec_administration),
      sec_a_services = gsub(",", ".", sec_a_services),
      sec_autres = gsub(",", ".", sec_autres)
    ) %>%
    pivot_longer(
      cols = c("sec_industries_btp", "sec_commerce", "sec_administration", "sec_a_services", "sec_autres"),
      names_to = "secteur",
      values_to = "taux"
    ) %>%
    mutate(taux = as.numeric(taux)) %>%
    mutate(
      fraction = taux / sum(taux), # Calculer les pourcentages
      ymax = cumsum(fraction), # Calculer les pourcentages cumulés (en haut de chaque rectangle)
      ymin = c(0, head(ymax, n = -1)), # Calculer le bas de chaque rectangle
      labelPosition = (ymax + ymin) / 2,
      label = paste0(secteur, "\n ", taux),
      secteur = case_when(
        secteur == "sec_industries_btp" ~ "Industries, bâtiment et travaux publics",
        secteur == "sec_commerce" ~ "Commerce",
        secteur == "sec_administration" ~ "Administrations, Education, Santé Action sociale",
        secteur == "sec_a_services" ~ "Services",
        secteur == "sec_autres" ~ "Autres",
        TRUE ~ secteur
      ),
      secteur = factor(secteur, levels = c(
        "Industries, bâtiment et travaux publics", "Commerce",
        "Administrations, Education, Santé Action sociale",
        "Services",
        "Autres"
      )),
      taux_str = paste0(taux, symbole_pourcentage),
      tooltip_value = paste0(secteur, " : ", taux_str)
    )

  data
}

caption_source <- paste0(
  '<span style="color:#008B99;">Source : </span>',
  "Céreq, enquête Génération 2017."
)
generateCaptionBarChart <- function(DT) {
  caption <- paste0(
    '<span style="color:#008B99;">Lecture : </span>',
    "Trois ans après leur sortie de formation initiale, ",
    DT$taux_str[1],
    " des jeunes de la Génération 2017 sont en emploi, ",
    DT$taux_str[2],
    " au chômage et ",
    DT$taux_str[3],
    " dans une autre situation.",
    "<br>",
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017.",
    "<br>",
    caption_source
  )
  return(caption)
}

generateCaptionDonutChart <- function(niveau) {
  caption <- paste0(
    '<span style="color:#008B99;">Champ : </span>',
    "Ensemble de la Génération 2017 en emploi en octobre 2020",
    "<br>",
    caption_source
  )
  caption
}
titre<-'Conditions d’emploi des jeunes trois ans après leur sortie de formation initiale pour : '
generateCaptionTitre <- function(niveau) {
  caption_titre <- paste0(
    titre,
    niveau,
    "."
  
  )
  caption_titre
}

labellize_stats_end_i <- function(stat1_str, stat2_str = NULL, info_str, infobulle_str = NULL) {
  tagList(
    tags$p(
      tags$span(
        class = "stat-label",
        `data-title` = if (!is.null(infobulle_str) && !is.na(infobulle_str)) {
          infobulle_str
        } else "",
        info_str,
        if (!is.null(infobulle_str) && !is.na(infobulle_str)) {
          "\u24D8"
        }
      )
    ),
    tags$p(
      tags$span(
        class = "stat-value",
        stat1_str
      ),
      if (!is.null(stat2_str)) {
        tags$span(
          class = "stat-other",
          stat2_str
        )
      }
    )
  )
}

generate_stat_comment_1 <- function(x, infobulle_str, info_str, column, symbole) {
  x <- x[rev(order(x$Code)), ] # make sure code 100 is the latest if more than a row
  text_info1 <- paste0(x[[column]][1], symbole)
  if (nrow(x) > 1) {
    text_info2 <- paste0("(", x[[column]][2], symbole, " pour l'ensemble des sortants)")
  } else {
    text_info2 <- NULL
  }

  labellize_stats_end_i(
    stat1_str = text_info1, stat2_str = text_info2, info_str = info_str, infobulle_str = infobulle_str
  )
}

labellize_stats_row <- function(stat1_str, stat2_str = NULL, info_str) {
  tags$p(
    tags$span(
      class = "stat-value",
      stat1_str
    ),
    tags$span(
      class = "stat-label",
      info_str
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
generate_stat_comment_2 <- function(x, info_str, column, symbole) {
  x <- x[rev(order(x$Code)), ] # make sure code 100 is the latest if more than a row
  text_info1 <- paste0(x[[column]][1], symbole)
  if (nrow(x) > 1) {
    text_info2 <- paste0("(", x[[column]][2], symbole, " pour l'ensemble des sortants)")
  } else {
    text_info2 <- NULL
  }
  labellize_stats_row(
    stat1_str = text_info1, stat2_str = text_info2,
    info_str = info_str
  )
}

# data preparation ----

## augment tab_diplome  ----
## with calculated columns that help distinguish what goes where

tab_diplome <- arrow::read_parquet("data/base_diplome.parquet") %>%
  rename(
    Libelle_complet = `Libelle complet`
  )

tab_diplome <- tab_diplome |>
  mutate(
    level1 = (tab_diplome$Code %% 10) %in% 0,
    level0 = (tab_diplome$Code %% 100) %in% 0,
    CodeParent = Code %/% 10 * 10
  ) |>
  relocate(level0, level1, Code, CodeParent)

## menus and submenus preparation ----
data_submenu <- tab_diplome |>
  filter(!level1) |>
  select(subkey = Code, CodeParent, Libelle_Menu) |>
  nest(.by = CodeParent, .key = "subkeys")

data_menu <- tab_diplome |>
  filter(level1) |>
  mutate(
    key = Code,
    key = case_when(
      level0 ~ Code,
      TRUE ~ Code + 1
    ),
    style = case_when(
      level0 ~ "font-weight: bold;",
      TRUE ~ ""
    )
  ) |>
  select(level0, key, style, Libelle_Menu, CodeParent) |>
  left_join(data_submenu, by = "CodeParent") |>
  mutate(
    key = case_when(
      !level0 & sapply(subkeys, is.null) ~ key %/% 10 * 10,
      TRUE ~ key
    )
  )

rm(data_submenu)

## datasets for barplots ----

code_empty_level1 <- data_menu |>
  filter(!level0, key == CodeParent) |>
  pull(key)

subset_dat_eds <- tab_diplome |> filter(Code %in% 100)

subset_dat_without_subchoice <- bind_rows(
  tab_diplome |> filter(level0, !Code %in% 100),
  tab_diplome |> filter(Code %in% code_empty_level1)
)
rm(code_empty_level1)

subset_dat_without_subchoice <- lapply(split(subset_dat_without_subchoice, subset_dat_without_subchoice$Code), function(x, x0) {
  z <- bind_rows(x, x0) |> arrange(Code)
  z$Libelle_Menu <- factor(z$Libelle_Menu, levels = z$Libelle_Menu)
  z
}, subset_dat_eds)

subset_dat_with_subchoice <- tab_diplome |>
  filter(!level0, !level1) |>
  mutate(CodeParent = Code %/% 10 * 10)

subset_dat_with_subchoice <- lapply(split(subset_dat_with_subchoice, subset_dat_with_subchoice$Code), function(x, x0) {
  z <- bind_rows(x, x0)
  z$Libelle_Menu <- factor(z$Libelle_Menu, levels = z$Libelle_Menu)
  z
}, subset_dat_eds)

datasets_subsets <- append(subset_dat_without_subchoice, subset_dat_with_subchoice)

datasets_subsets[["100"]] <- subset_dat_eds
rm(subset_dat_eds, subset_dat_without_subchoice, subset_dat_with_subchoice)

### datasets for barchart plots ----
barchart_datasets_subsets <- lapply(datasets_subsets, generateDTBarChart)
rm(generateDTBarChart)

### captions for barchart ----
barchart_captions_subsets <- lapply(barchart_datasets_subsets, generateCaptionBarChart)


## datasets for donuts ----
### datasets for profession donuts ----
donut_profession_datasets_subsets <- lapply(datasets_subsets, generateDTDonutChartProfession)
### datasets for secteur donuts ----
donut_secteur_datasets_subsets <- lapply(datasets_subsets, generateDTDonutChartSecteur)
### captions for donuts ----


donuts_captions_subsets <- tab_diplome |>
  mutate(donut_caption = generateCaptionDonutChart(Libelle_complet)) |>
  select(Code, donut_caption)
donuts_captions_subsets <- setNames(as.list(donuts_captions_subsets$donut_caption), donuts_captions_subsets$Code)

### TITRE ----
donuts_captions_titre <- tab_diplome |>
  mutate(donut_titre = generateCaptionTitre(Libelle_complet)) |>
  select(Code,donut_titre)
donuts_captions_titre <- setNames(as.list(donuts_captions_titre$donut_titre),donuts_captions_titre$Code)




# labels and stats preparation ----
variables_diplome <- readxl::read_excel("data/Variables_DIPLOME_OJ.xlsx")

## tx_en_emploi ----
tx_en_emploi_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_1,
  infobulle_str = variables_diplome %>%
    filter(Nom_colonne == "taux_emploi") %>% pull(Bulle),
  info_str = "En emploi",
  column = "taux_emploi",
  symbole = " %"
)

## taux_chomage ----
tx_chomage_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_1,
  infobulle_str = variables_diplome %>%
    filter(Nom_colonne == "taux_chomage") %>% pull(Bulle),
  info_str = "Taux de chômage",
  column = "taux_chomage",
  symbole = " %"
)

## taux_edi ----
taux_edi_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_1,
  infobulle_str = variables_diplome %>%
    filter(Nom_colonne == "taux_edi") %>% pull(Bulle),
  info_str = "En emploi à durée indéterminée",
  column = "taux_edi",
  symbole = " %"
)

## part_tps_partiel ----
part_tps_partiel_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_1,
  infobulle_str = variables_diplome %>%
    filter(Nom_colonne == "part_tps_partiel") %>% pull(Bulle),
  info_str = "À temps partiel",
  column = "part_tps_partiel",
  symbole = " %"
)
## revenu_travail ----
revenu_travail_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_1,
  infobulle_str = variables_diplome %>%
    filter(Nom_colonne == "revenu_travail") %>% pull(Bulle),
  info_str = "Revenu mensuel médian",
  column = "revenu_travail",
  symbole = " €"
)
## correspondance_ok ----
correspondance_ok_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_2,
  info_str = "jugent leur emploi cohérent avec leur formation initiale",
  column = "correspondance_ok",
  symbole = " %"
)
## competence_ok ----
competence_ok_labels <- lapply(
  datasets_subsets,
  generate_stat_comment_2,
  info_str = "estiment être employés sous leur niveau de compétence",
  column = "competence_ok",
  symbole = " %"
)

# save data for reuse in shiny app ----
# with no calculation to do later

saveRDS(as.data.frame(data_menu), "diplomes/data/menus.RDS")
saveRDS(barchart_datasets_subsets, "diplomes/data/barchart_datasets_subsets.RDS")
saveRDS(barchart_captions_subsets, "diplomes/data/barchart_captions_subsets.RDS")
saveRDS(donut_profession_datasets_subsets, "diplomes/data/donut_profession_datasets_subsets.RDS")
saveRDS(donut_secteur_datasets_subsets, "diplomes/data/donut_secteur_datasets_subsets.RDS")
saveRDS(donuts_captions_subsets, "diplomes/data/donuts_captions_subsets.RDS")
saveRDS(donuts_captions_titre, "diplomes/data/donuts_captions_titre.RDS")
saveRDS(tx_en_emploi_labels, "diplomes/data/tx_en_emploi_labels.RDS")
saveRDS(tx_chomage_labels, "diplomes/data/tx_chomage_labels.RDS")
saveRDS(taux_edi_labels, "diplomes/data/taux_edi_labels.RDS")
saveRDS(part_tps_partiel_labels, "diplomes/data/part_tps_partiel_labels.RDS")
saveRDS(revenu_travail_labels, "diplomes/data/revenu_travail_labels.RDS")
saveRDS(correspondance_ok_labels, "diplomes/data/correspondance_ok_labels.RDS")
saveRDS(competence_ok_labels, "diplomes/data/competence_ok_labels.RDS")

file.copy("src-templates/graphics-settings.R",
  "diplomes/graphics-settings.R",
  overwrite = TRUE
)
file.copy("src-templates/shiny-elements.R",
  "diplomes/shiny-elements.R",
  overwrite = TRUE
)
file.copy("src-templates/www",
  "diplomes",
  recursive = TRUE,
  overwrite = TRUE
)
