

rm(list=ls())
library(sf)
library(dplyr)
library(rio)
library(rgdal)
setwd("~/Desktop/BioD/")
NigerShapeFile <- readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
dir.create("Migrations_Works/Products/Department", recursive = TRUE, showWarnings = FALSE)

# Data processing
dataIdps <- rio::import("Data/Niger VAS round 6-Jun 06 2023.xlsx - Main data.csv")
Data <- as.data.frame(matrix(ncol = 1, nrow = 632))
colnames(Data) <- c("region")
Data$region <- dataIdps$`A2. Région`
Data$department <- dataIdps$`A3. Département`
Data$commune <- dataIdps$`A4. Commune`
Data$Idps <- dataIdps$`B1. Personnes Déplacées interne/B1.a Nombre de ménages`
Data$IdpReturn <- dataIdps$`B2.a Retournés (anciennes PDIs)/B2.a.2  Nombre d'individus`
Data$ReturnFromOutsideBorder <- dataIdps$`B2.b Retournés (de l’étranger)/B2.b.b  Nombre d'individus`
Data$Refugees <- dataIdps$`B4. Refugiés/B4.b  Nombre d'individus`
Data$region <- ifelse(Data$region == "Tillaberi", "Tillabéry", Data$region)
Data$lat <- dataIdps$coordonnees_gps_site_latitude
Data$lon <- dataIdps$coordonnees_gps_site_longitude

# Replace "," with "."
Data$lon <- gsub(",", ".", Data$lon)
Data$lat <- gsub(",", ".", Data$lat)

Data$lon <- as.numeric(Data$lon)
Data$lat <- as.numeric(Data$lat)
missing_lon <- is.na(Data$lon)
missing_lat <- is.na(Data$lat)
Data <- Data[!missing_lon & !missing_lat, ]
