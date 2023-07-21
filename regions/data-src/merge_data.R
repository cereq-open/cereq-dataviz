# Load packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

# Load data --------------------------------------------------------------------

db_region <- readxl::read_excel("data/tab_region.xls") %>%
  filter(Libellé != "Ensemble") %>%
  mutate(Libellé = stringi::stri_trans_general(Libellé, id = "latin-ascii"))

# Remplace "Haut-de-France" par "Hauts-de-France" dans la colonne "Libellé"
db_region$Libellé <- gsub("Haut-de-France", "Hauts-de-France", db_region$Libellé)

# Remplace "Nouvelle Aquitaine" par "Nouvelle-Aquitaine" dans la colonne "Libellé"
db_region$Libellé <- gsub("Nouvelle Aquitaine", "Nouvelle-Aquitaine", db_region$Libellé)

# Remplace "Provence-Alpes-Cote-d'Azur et Corse" par "Provence-Alpes-Cote d'Azur" dans la colonne "Libellé"
db_region$Libellé <- gsub("Provence-Alpes-Cote-d'Azur et Corse", "Provence-Alpes-Cote d'Azur", db_region$Libellé)

gadm <- readRDS("data/gadm36_FRA_1_sf.rds")

gadm <- st_as_sf(gadm)  # Convertis en objet sf
gadm <- st_set_crs(gadm, 4326) %>% # Attribue le système de coordonnées géographiques WGS 84
  mutate(NAME_1 = stringi::stri_trans_general(NAME_1, id = "latin-ascii"))

# Remarques : Nous avons pas d'information sur les DROM dans la table gadm36_FRA_1_sf.rds
# Il est nécessaire de demander au client de scinder la colonne Provence-Alpes-Cote-d'Azur et Corse
# Si celui-ci souhaite afficher des informations sur la Corse sur la carte.

# Jointure des deux tables  
jointure_df <- merge(db_region, gadm, by.x = "Libellé", by.y = "NAME_1")

# Convertis le dataframe en objet spatial sf
jointure_sf <- st_as_sf(jointure_df)

# Exporte la table en shapefile
st_write(jointure_sf, "data/tab_region.shp")
