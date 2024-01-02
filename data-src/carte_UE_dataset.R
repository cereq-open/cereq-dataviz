
library(tidyverse)
library(arrow)
library(readxl)
library(htmltools)
library(sf)


#Lecture fichier : 

#1- fichier indicateur

UE <- readxl::read_excel("data/UE.xlsx")

tx_form <- readxl::read_excel("data/rajout_autre_forme_2.xlsx")
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


Europe_2 <- sp::merge(Europe , tx_form, by.x = "KEY", by.y = "KEY")


Europe_3 <- sp::merge(UE , tx_form, by.x = "KEY", by.y = "KEY")
write.xlsx(Europe_3,"data/Europe_3.xlsx", sep=";")

# Création liste pour menus déroulants

Europe_nodupkey <- Europe %>% distinct(secteur, .keep_all = TRUE)
liste_secteur <- na.omit(Europe_nodupkey$secteur)
liste_secteur <- as.character( liste_secteur )
liste_secteur <- as.list(sort(liste_secteur))
liste_secteur[5]<-NULL
ensemble_liste_secteur = list('Ensemble des secteurs')
liste_secteur2 <- c(ensemble_liste_secteur, liste_secteur)

########################################################################################
Europe_nodupkey_taille <- Europe %>% distinct(taille, .keep_all = TRUE)
liste_taille <- na.omit(Europe_nodupkey_taille$taille)
liste_taille <- as.character( liste_taille )
liste_taille <- as.list(sort(liste_taille))
liste_taille[4]<-NULL
ensemble_liste_taille = list('Ensemble des tailles')
liste_taille2 <- c(ensemble_liste_taille, liste_taille)











saveRDS(Europe_2, file = "carte_UE/data/Europe.RDS")
saveRDS(liste_secteur2, file = "carte_UE/data/liste_secteur2.RDS")
saveRDS(liste_taille2, file = "carte_UE/data/liste_taille2.RDS")