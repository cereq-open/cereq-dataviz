
library(tidyverse)
library(arrow)
library(readxl)
library(htmltools)
library(sf)


#Lecture fichier : 

#1- fichier indicateur
UE <- read_parquet("data/data_carte_UE/Base_UE.parquet")

#2 - lecture du fichier shp pour la carte 
Monde <- st_read("data/data_carte_UE/CNTR_RG_10M_2020_4326.shp", quiet = TRUE)


#3 - merge du fichier carte et de la base pour obtenir la base Europe
Europe <- sp::merge(Monde , UE, by.x = "NAME_FREN", by.y = "NAME_FREN")

#4 - Nettoyage de la base 
Europe <- transform(Europe, tx_form = as.numeric(tx_form),
                    heurstag = as.numeric(heurstag),
                    tx_tpf = as.numeric(tx_tpf),
                    tx_acc1 = as.numeric(tx_acc1))


Europe$CNTR_ID <- NULL
Europe$CNTR_NAME <- NULL
Europe$NAME_ENGL <- NULL
Europe$SVRG_UN <- NULL
Europe$CAPT <- NULL
Europe$EU_STAT <- NULL
Europe$EFTA_STAT <- NULL
Europe$CC_STAT <- NULL
Europe$NAME_GERM <- NULL
Europe$FID <- NULL
Europe$iso3_code <- NULL
Europe$secteur <- fct_recode(Europe$secteur,
                             "Ensemble des secteurs" = "Ensemble des activités")
Europe$taille <- fct_recode(Europe$taille,
                            "Ensemble des tailles" = "Total")

# Création liste pour menus déroulants

Europe_nodupkey <- Europe %>% distinct(secteur, .keep_all = TRUE)
liste_secteur <- na.omit(Europe_nodupkey$secteur)
liste_secteur <- as.character( liste_secteur )
liste_secteur <- as.list(sort(liste_secteur))
liste_secteur[5]<-NULL
ensemble_liste_secteur = list('Ensemble des secteurs')
liste_secteur2 <- c(ensemble_liste_secteur, liste_secteur)

########################################################################################


liste_taille <-  list('Ensemble des tailles',"10 à 49", "49 à 249", "250 ou plus" )












saveRDS(Europe, file = "carte_UE/data/Europe.RDS")
saveRDS(liste_secteur2, file = "carte_UE/data/liste_secteur2.RDS")
saveRDS(liste_taille, file = "carte_UE/data/liste_taille2.RDS")