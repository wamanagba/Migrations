rm(list=ls())
setwd("~/Desktop/BioD/")
library(rio)
library(dplyr)
library(rgdal)
library(sf)
library(leaflet)
library(ggplot2)
df=data=rio::import("Data/Niger to west and Central Africa.csv")


# Supprimer les 10 premières lignes d'un dataframe (par exemple, df)
df <- slice(df, -(1:14))
colnames(df) <- df[1, ]
df <- slice(df, -1)
df <- rename(df, "origin"= "Country of origin" ,"asylum"="Country of asylum","refugees"="Refugees under UNHCR's mandate")

df$refugees <- as.numeric(df$refugees)
Data<-df%>%
  group_by(origin,asylum)%>%
  summarise(flow=sum(refugees)) 

Africa<-st_read("Data/SHP_AFRIQUE/Afrique_frontier_news.shp") 
Westafrica <- st_read("Data/wca_admbnda_adm0_ocha_29062021/wca_admbnda_adm0_ocha_29062021.shp") 


# Extraire les coordonnées des centroïdes des pays
centroids <- st_centroid(Westafrica)

# Créer un dataframe des coordonnées des pays
coords_df <- data.frame(pays = Westafrica$admin0Name, 
                        longitude = st_coordinates(centroids)[, "X"], 
                        latitude = st_coordinates(centroids)[, "Y"])

rio::export(coords_df,"Migrations_Works/Data/coord.csv")
coords_df=rio::import("Migrations_Works/Data/coord.csv")
coords_df <- coords_df[-18, ]

coords_df=rename(coords_df,"asylum"="pays")
merged_df <- merge(coords_df, Data, by = "asylum")
merged_df$NigerLont= 9.3474509
merged_df$NigerLat = 17.4173044



# Créer la carte avec les frontières des pays de l'Afrique
map <- leaflet(data = Africa) %>%
  addTiles() %>%
  addPolygons(fill = FALSE, color = "gray", weight = 1)

# Ajouter les lignes de flux migratoires à la carte
for (i in 1:nrow(merged_df)) {
  map <- map %>%
    addPolylines(
      lng = c(merged_df[i, "NigerLont"], merged_df[i, "longitude"]),
      lat = c(merged_df[i, "NigerLat"], merged_df[i, "latitude"]),
      weight = 3,  # Épaisseur fixe de la ligne (ajustez la valeur selon vos préférences)
      color = "red"  # Couleur de la ligne
    )
}
map


# Créer la carte statique avec les frontières des pays de l'Afrique
map2 <- ggplot() +
  geom_sf(data = Africa, aes(fill = "gray")) +
  geom_segment(data = merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = flow), size = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Flux migratoires") +
  scale_fill_manual(values = setNames(c("gray", "blue", "green", "yellow", "orange", "red"), levels(Africa$admin0Name))) +
  theme_void()

# Afficher la carte
print(map2)





# Sauvegarder la carte statique en tant qu'image
ggsave(filename = "Migrations_Works/Products/flowMap.png", plot = map2, width = 10, height = 8, dpi = 300)
