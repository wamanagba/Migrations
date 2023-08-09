
rm(list = ls())
library(sf)
library(dplyr)
library(rio)
library(tmap)
library(rgdal)
library(mapview)
setwd("~/Desktop/BioD/")
NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
dir.create("Data/Department",recursive = T,showWarnings = F)
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")

# Data processing
dataIdps=readxl::read_xlsx("Migrations_Works/Data/IOM/DTM_Niger_VAS_Round_5_Septembre_2022_final.xlsx")
dataIdps= dataIdps[-1,]
Data=as.data.frame(matrix(ncol=1, nrow=787))
colnames(Data)=c("dates")
Data$dates = as.Date("2022-31-09", format = "%Y-%d-%m")

Data$region=dataIdps$`A2. Région`
Data$department =dataIdps$`A3. Département`
Data$commune= dataIdps$`A4. Commune`
Data$Idps = dataIdps$`B1.b Nombre d'individus déplacés internes`
Data$IdpReturn = dataIdps$`B2.a.1  Nombre de ménages retournés (anciennes PDI)`
Data$autochtone = dataIdps$`B4.b  Nombre d'individus autochtone`
Data$MenaeIdp = dataIdps$`B1.a Nombre de ménages déplacés internes`
Data$MenaeReturnIdp = dataIdps$`B2.a.1  Nombre de ménages retournés (anciennes PDI)`
Data$region <- ifelse(Data$region == "Tillaberi", "Tillabéry", Data$region)
rio::export(Data,"Migrations_Works/Data/IOM/Data_09-2022.csv")

dataIdps=readxl::read_xlsx("Migrations_Works/Data/IOM/DTM_Niger_VAS_b3f_round-4_dataset_hdx.xlsx")
dataIdps= dataIdps[-1,]
Data=as.data.frame(matrix(ncol=1, nrow=409))
colnames(Data)=c("dates")
Data$dates = as.Date("2022-31-05", format = "%Y-%d-%m")

Data$region=dataIdps$`A2. Région`
Data$department =dataIdps$`A3. Département`
Data$commune= dataIdps$`A4. Commune`
Data$Idps = dataIdps$`B1. Personnes Déplacées interne - B1.b Nombre d'individus`
Data$MenaeIdp = dataIdps$`B1. Personnes Déplacées interne - B1.a Nombre de ménages`

Data$IdpReturn = dataIdps$`B2.a Retournés (anciennes PDIs) - B2.a.2  Nombre d'individus`
Data$MenaeReturnIdp = dataIdps$`B2.a Retournés (anciennes PDIs) - B2.a.1  Nombre de ménages`
Data$ReturnEtrangerMenages = dataIdps$`B2.b Retournés (de l’étranger) - B2.b.a  Nombre de ménages`
Data$ReturnEtrangerIndividus = dataIdps$`B2.a Retournés (anciennes PDIs) - B2.a.2  Nombre d'individus`

Data$autochtone = dataIdps$`B5. Population autochtone (sans compter les déplacés – à ne demander que si le site est une Communauté Hôte / Familles d’Accueil) - B5.b  Nombre d'individus`
Data$autochtoneMenages = dataIdps$`B5. Population autochtone (sans compter les déplacés – à ne demander que si le site est une Communauté Hôte / Familles d’Accueil) - B5.b  Nombre d'individus`

Data$RessortissantsPaysTiersMenage = dataIdps$`B3. Ressortissants de Pays Tiers - B3.a  Nombre de ménages`
Data$RessortissantsPaysTiersIndi = dataIdps$`B3. Ressortissants de Pays Tiers - B3.b  Nombre d'individus`

Data$RefugeesMenages = dataIdps$`B4. Refugiés - B4.a  Nombre de ménages`
Data$RefugeesIndi = dataIdps$`B4. Refugiés - B4.b  Nombre d'individus`

Data[, 5:15] <- sapply(Data[, 5:15], as.numeric)
Data$region <- ifelse(Data$region == "Tillaberi", "Tillabéry", Data$region)
rio::export(Data,"Migrations_Works/Data/IOM/Data_05-2022.csv")


dataIdps=rio::import("Migrations_Works/Data/IOM/dec2019.xlsx")

Data=as.data.frame(matrix(ncol=1, nrow=20))
colnames(Data)=c("dates")
Data$dates = as.Date("2019-12-12", format = "%Y-%d-%m")

Data$region=dataIdps$admin1
Data$department =dataIdps$admin_2
Data$commune= dataIdps$admin_3
Data$Idp2017= dataIdps$`IdpIn_2017/IdpIn_2017_Individus`
Data$Idp2018= dataIdps$`IdpIn_2018/IdpIn_2018_Individus`
Data$Idp2019 = dataIdps$`IdpIn_2019/IdpIn_2019_Individus`
Data$IdpTotalInsecurite= dataIdps$`# Total Ind PDIs (incidents sécurité)`
Data$IdpTotalMenageInsecurite = dataIdps$`# Total Mén PDIs (incidents sécurité)`

Data$region <- ifelse(Data$region == "Tillaberi", "Tillabéry", Data$region)
Data[, 5:9] <- sapply(Data[, 5:9], as.numeric)

rio::export(Data,"Migrations_Works/Data/IOM/Data_2019.csv")



# Data processing
dataIdps=rio::import("Data/Niger VAS round 6-Jun 06 2023.xlsx - Main data.csv")
Data=as.data.frame(matrix(ncol=1, nrow=632))
colnames(Data)=c("dates")
Data$dates = as.Date("2023-30-04", format = "%Y-%d-%m")

Data$region=dataIdps$`A2. Région`
Data$department =dataIdps$`A3. Département`
Data$commune= dataIdps$`A4. Commune`
Data$IdpsMenages = dataIdps$`B1. Personnes Déplacées interne/B1.a Nombre de ménages`
Data$IdpsInd = dataIdps$`B1. Personnes Déplacées interne/B1.b Nombre d'individus`
Data$RessortissantPaysTiers = dataIdps$`B3. Ressortissants de Pays Tiers/B3.b  Nombre d'individus`
Data$IdpReturn = dataIdps$`B2.a Retournés (anciennes PDIs)/B2.a.2  Nombre d'individus`
Data$ReturnFromOutsideBorder = dataIdps$`B2.b Retournés (de l’étranger)/B2.b.b  Nombre d'individus`
Data$Refugees = dataIdps$`B4. Refugiés/B4.b  Nombre d'individus`
Data$region <- ifelse(Data$region == "Tillaberi", "Tillabéry", Data$region)
Data$lat = dataIdps$coordonnees_gps_site_latitude
Data$lon =dataIdps$coordonnees_gps_site_longitude


#Ramplacer "," par ".."
Data$lon <- gsub(",", ".", Data$lon)
Data$lat <- gsub(",", ".", Data$lat)


Data$lon <- as.numeric(Data$lon)
Data$lat <- as.numeric(Data$lat)
missing_lon <- is.na(Data$lon)
missing_lat <- is.na(Data$lat)
Data <- Data[!missing_lon & !missing_lat, ]
rio::export(Data,"Migrations_Works/Data/IOM/Data_06-2023.csv")



data1=rio::import("Migrations_Works/Data/IOM/Data_09-2022.csv")
DATA001=as.data.frame(matrix(ncol=1, nrow=787))
colnames(DATA001)=c("dates")
DATA001$dates = as.Date("2022-30-09", format = "%Y-%d-%m")
DATA001$region= data1$region
DATA001$department= data1$department
DATA001$commune= data1$commune
DATA001$IdpsInd= data1$Idps
DATA001$IdpReturn= data1$IdpReturn
DATA001$ReturnFromOutsideBorder= NA
DATA001$Refugees= NA
DATA001$RessortissantPaysTiers= NA
dpt1= DATA001 %>%
      group_by(department) %>%
      summarise(IDP= sum(IdpsInd), na.rm = TRUE)

dpt2= de %>%
  group_by(department) %>%
  summarise(IDP= sum(IDP), na.rm = TRUE)

Data_Sums <- merge(de,DATA001, by = "commune", all = TRUE)



data2=rio::import("Migrations_Works/Data/IOM/Data_05-2022.csv")
DATA00=as.data.frame(matrix(ncol=1, nrow=409))
colnames(DATA00)=c("dates")
DATA00$dates = as.Date("2022-30-05", format = "%Y-%d-%m")
DATA00$region= data2$region
DATA00$department= data2$department
DATA00$commune= data2$commune
DATA00$IdpsInd= data2$Idps
DATA00$IdpReturn= data2$IdpReturn
DATA00$ReturnFromOutsideBorder= data2$ReturnEtrangerIndividus
DATA00$Refugees= data2$RefugeesIndi
DATA00$RessortissantPaysTiers= data2$RessortissantsPaysTiersIndi



data3=rio::import("Migrations_Works/Data/IOM/Data_2019.csv")
DATA01=as.data.frame(matrix(ncol=1, nrow=20))
colnames(DATA01)=c("dates")
DATA01$dates = as.Date("2017-31-12", format = "%Y-%d-%m")
DATA01$region= data3$region
DATA01$department= data3$department
DATA01$commune= data3$commune
DATA01$IdpsInd= data3$Idp2017
DATA01$IdpReturn= NA
DATA01$ReturnFromOutsideBorder= NA
DATA01$Refugees= NA
DATA01$RessortissantPaysTiers= NA

DATA02=as.data.frame(matrix(ncol=1, nrow=20))
colnames(DATA02)=c("dates")
DATA02$dates = as.Date("2018-31-12", format = "%Y-%d-%m")
DATA02$region= data3$region
DATA02$department= data3$department
DATA02$commune= data3$commune
DATA02$IdpsInd= data3$Idp2018
DATA02$IdpReturn= NA
DATA02$ReturnFromOutsideBorder= NA
DATA02$Refugees= NA
DATA02$RessortissantPaysTiers= NA

DATA03=as.data.frame(matrix(ncol=1, nrow=20))
colnames(DATA03)=c("dates")
DATA03$dates = as.Date("2019-31-12", format = "%Y-%d-%m")
DATA03$region= data3$region
DATA03$department= data3$department
DATA03$commune= data3$commune
DATA03$IdpsInd= data3$Idp2019
DATA03$IdpReturn= NA
DATA03$ReturnFromOutsideBorder= NA
DATA03$Refugees= NA
DATA03$RessortissantPaysTiers= NA




data4=rio::import("Migrations_Works/Data/IOM/Data_06-2023.csv")
DATA=as.data.frame(matrix(ncol=1, nrow=631))
colnames(DATA)=c("dates")
DATA$dates = data4$dates
DATA$region= data4$region
DATA$department= data4$department
DATA$commune= data4$commune
DATA$IdpsInd= data4$IdpsInd
DATA$IdpReturn= data4$IdpReturn
DATA$ReturnFromOutsideBorder= data4$ReturnFromOutsideBorder
DATA$Refugees= data4$Refugees
DATA$RessortissantPaysTiers= data4$RessortissantPaysTiers



DaTa= rbind(DATA00,DATA001)
DaTa =rbind(DaTa,DATA01)
DaTa =rbind(DaTa,DATA02)
DaTa =rbind(DaTa,DATA03)
DaTa =rbind(DaTa,DATA)


###########################################



# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(data4, coords = c("lon", "lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)
Data_Sums <- data.frame(NAME_2 = unique(agregation$NAME_2))

sum_by_region <- agregation %>%
  group_by(NAME_2) %>%
  summarise(IDP= sum(IdpsInd))

# Merge the results with the main dataframe
Data_Sums <- merge(Data_Sums, sum_by_region, by = "NAME_2", all = TRUE)
# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"
# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)

shapefile <- shapefile[order(shapefile$NAME_2), ]
shapefile$SumIdps <- Data_Sums$IDP

##################

DATA00


DD = DATA %>%
  group_by(dates,NAME_2) %>%
  summarise(Idp= sum(IdpsInd, na.rm = TRUE))
colnames(DaTa)[4] = "NAME_2"
ag =as.data.frame(agregation$geometry)
ag$NAME_2=agregation$commune
Data_Sums <- merge(DD, ag, by = "NAME_2", all = TRUE)

rio::export(DaTa,"Migrations_Works/Data/IOM/DataF.csv")
