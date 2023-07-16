library(googledrive)
library(dplyr)
library(terra)
library(sf)

# Raster (SpatRaster)
r <- rast(system.file("ex/elev.tif", package = "terra"))
# Polygons (SpatVector)
v <- vect(system.file("ex/lux.shp", package = "terra"))

points <- crds(centroids(v))
setwd("~/Desktop/BioD/")
library(rio)
library(rgdal)

df=data=rio::import("Data/Niger to west and Central Africa.csv")


# Supprimer les 10 premières lignes d'un dataframe (par exemple, df)
df <- slice(df, -(1:14))
colnames(df) <- df[1, ]

# Supprimer la première ligne du dataframe
df <- df[-1, ]

df <- rename(df, "origin"= "Country of origin" ,"asylum"="Country of asylum","refugees"="Refugees under UNHCR's mandate")

  
df$refugees <- as.numeric(df$refugees)
Pvalue<-df%>%
  group_by(origin,asylum)%>%
  summarise(val=sum(refugees)) 

westA<-rgdal::readOGR("Data/wca_admbnda_adm0_ocha_29062021/wca_admbnda_adm0_ocha_29062021.shp") 
plot(westA[1])
westA$admin0Name
v <- vect(system.file("SHP_AFRIQUE/Afrique_frontier_news.shp", package = "terra"))

setwd("~/Desktop/SIGP_training/R-packages_maps_spatial_data/Percentage_Of_Average/")


Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
africa <- st_read("Data/wca_admbnda_adm0_ocha_29062021/wca_admbnda_adm0_ocha_29062021.shp") 
coordonnees <- st_geometry(africa)
pays_coords <- st_coordinates(africa)
# Créer un dataframe des coordonnées des pays
coords_df <- data.frame(pays = africa$admin0Name, longitude = pays_coords[, "X"], latitude = pays_coords[, "Y"])


# Installer et charger les bibliothèques nécessaires

# Charger le fichier de formes (shapefile) de l'Afrique
africa_shapefile <- st_read("Data/wca_admbnda_adm0_ocha_29062021/wca_admbnda_adm0_ocha_29062021.shp")

# Extraire les coordonnées des centroïdes des pays
centroids <- st_centroid(africa_shapefile)

# Créer un dataframe des coordonnées des pays
coords_df <- data.frame(pays = africa_shapefile$admin0Name, 
                        longitude = st_coordinates(centroids)[, "X"], 
                        latitude = st_coordinates(centroids)[, "Y"])
#rio::export(coords_df,"Data/coord.csv")
cord=rio::import("Data/coord.csv")
cord <- cord[-18, ]
cord=rename(cord,"asylum"="pays")
merged_df <- merge(cord, Pvalue, by = "asylum")
merged_df$NigerLont= 9.3474509
merged_df$NigerLat = 17.4173044


# Créer une carte leaflet
library(leaflet)
map <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") # Vous pouvez choisir un autre fournisseur de tuiles si vous le souhaitez

# Ajouter les lignes de flux migratoires à la carte
for (i in 1:nrow(merged_df)) {
  map <- map %>%
    addPolylines(
      lng = c(merged_df[i, "NigerLont"], merged_df[i, "longitude"]),
      lat = c(merged_df[i, "Nigerlat"], merged_df[i, "latitude"]),
      weight = merged_df[i, "val"] / 100,  # Ajustez l'épaisseur de ligne en fonction du flux migratoire
      color = "red"  # Couleur de la ligne
    )
}

# Afficher la carte
print(map)
