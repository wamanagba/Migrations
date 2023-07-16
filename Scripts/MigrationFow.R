rm(list=ls())
setwd("~/Desktop/BioD/")
library(rio)
library(dplyr)
library(rgdal)
library(sf)
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

cord=rename(cord,"asylum"="pays")
merged_df <- merge(cord, Data, by = "asylum")
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



breakpoints <- c(-Inf, c(0, 100, 500, 2000, 3000, 5000, 10000), Inf)
cols <- colorRampPalette(c("blue", "yellow", "orange", "red", "darkred"))(length(breakpoints) - 1)

xx <- cut(c(t(as.numeric(merged_df$flow))), breaks = breakpoints)
levels(xx) <- cols
merged_df$couleur_val = xx


# Créer la carte statique avec les frontières des pays de l'Afrique
map <- ggplot() +
  geom_sf(data = africa_shapefile, aes(fill = "gray")) +
  geom_segment(data = merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = couleur_val), size = 1) +
  scale_color_manual(values = couleurs_valeurs, name = "Flux migratoires") +
  theme_void()

# Afficher la carte
print(map)





library(ggplot2)

# Créer la carte statique
map <- ggplot(merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = flow)) +
  geom_segment(linewidth = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Flux migratoires") +
  theme_void()

# Sauvegarder la carte statique en tant qu'image
ggsave(filename = "Migrations_Works/Products/carte_statique_ggplot.png", plot = map, width = 10, height = 8, dpi = 300)



# Afficher la carte
map






# Créer la carte statique avec les frontières des pays de l'Afrique
map <- ggplot() +
  geom_sf(data = Africa, color = "gray", fill = "white") +
  geom_segment(data = merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = flow), size = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Flux migratoires") +
  theme_void()

# Afficher la carte
print(map)


# Créer la carte statique avec les frontières des pays de l'Afrique
map <- ggplot() +
  geom_sf(data = Africa, aes(fill = "gray")) +
  geom_segment(data = merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = flow), size = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Flux migratoires") +
  scale_fill_manual(values = setNames(c("gray", "blue", "green", "yellow", "orange", "red"), levels(africa_shapefile$admin0Name))) +
  theme_void()

# Afficher la carte
print(map)


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


setwd("~/Desktop/BioD")
# Charger le fichier de formes (shapefile) de l'Afrique
africa_shapefile <- st_read("")

# Fusionner les données de forme de l'Afrique avec les données de flux migratoires
merged_map <- merge(africa_shapefile, merged_df, by.x = "admin0Name", by.y = "asylum")

# Créer la carte statique
map <- ggplot() +
  geom_sf(data = merged_map, color = "gray", fill = "white") +
  geom_segment(data = merged_df, aes(x = NigerLont, y = NigerLat, xend = longitude, yend = latitude, color = val), size = 1) +
  scale_color_gradient(low = "blue", high = "red", name = "Flux migratoires") +
  theme_void()

# Sauvegarder la carte statique en tant qu'image
ggsave(filename = "chemin/vers/carte_statique.png", plot = map, width = 10, height = 8, dpi = 300)
