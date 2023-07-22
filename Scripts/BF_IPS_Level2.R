
rm(list = ls())
library(rio)
library(rgdal)
library(dplyr)
library(sf)
library(mapview)
library(tmap)

library(stringi)
setwd("~/Desktop/BioD/")
df = rio::import("Migrations_Works/Data/situation-des-pdi-par-communes-accueil-du-28-fevrier-2023.xlsx")
# remove the first roows
df <- slice(df, -(1:5))
colnames(df) <- df[1, ]
df <- slice(df, -1)
df <- slice(df,-c((nrow(df) - 1):nrow(df)))
#df$COMMUNES <- tolower(df$COMMUNES)
df$PROVINCES = paste(toupper(substring(df$PROVINCES, 1, 1)), tolower(substring(df$PROVINCES, 2)), sep = "")


BFShapeFile<-readOGR("Data/gadm41_BFA_shp/gadm41_BFA_2.shp")
shapefile=st_read("Data/gadm41_BFA_shp/gadm41_BFA_2.shp")
DataShapefile= as.data.frame(shapefile)

# Supprimer les accents de la colonne "votre_colonne" dans le dataframe "df"
DataShapefile$NAME_2 <- stri_trans_general(DataShapefile$NAME_2, "Latin-ASCII")

colnames(df)[colnames(df) == "PROVINCES"] <- "NAME_2"
df$NAME_2 <- gsub("Komondjari", "Komandjoari", df$NAME_2)


File<-df%>%
  group_by(NAME_2)%>%
  summarise(TotalIDP=sum(as.numeric(`Nombre total de PDI`)) )
dd=merge(DataShapefile,File, by = "NAME_2",all.x = TRUE)

File<-df%>%
  group_by(NAME_2)%>%
  summarise(Femelle=sum(as.numeric(Femmes)) )
dd=merge(dd,File, by = "NAME_2",all.x = TRUE)

File<-df%>%
  group_by(NAME_2)%>%
  summarise(Man=sum(as.numeric(Hommes)) )
dd=merge(dd,File, by = "NAME_2",all.x = TRUE)

File<-df%>%
  group_by(NAME_2)%>%
  summarise(ChildrenUnder_5=sum(as.numeric(`Enfants de moins de 5 ans`)) )
dd=merge(dd,File, by = "NAME_2",all.x = TRUE)

File<-df%>%
  group_by(NAME_2)%>%
  summarise(ChildrenOver_5=sum(as.numeric(`Enfants de plus de 5 ans`)) )
dd=merge(dd,File, by = "NAME_2",all.x = TRUE)

File<-df%>%
  group_by(NAME_2)%>%
  summarise(TotalChildren=sum(as.numeric(`Total Enfants`)) )
dd=merge(dd,File, by = "NAME_2",all.x = TRUE)


#File$Man = as.numeric(dd$Hommes)
#File$Femelle = as.numeric(dd$Femmes)
#File$ChildrenUnder_5 = as.numeric(dd$`Enfants de moins de 5 ans`)
#File$ChildrenOver_5 = as.numeric(dd$`Enfants de plus de 5 ans`)
#File$TotalChildren = as.numeric(dd$`Total Enfants`)
#File$ = as.numeric(dd$`Nombre total de PDI`)
#File$geometry =dd$geometry

dd_sf <- st_as_sf(dd)

mapview(dd_sf, zcol = "Man")

tmap_mode("view")
p1 <- tm_shape(dd_sf) +
  tm_polygons(col = "Man", title = "Total Men", style = "quantile") 
p1

p2 <- tm_shape(dd_sf) +
  tm_polygons(col = "Femelle", title = "Total women", style = "quantile") 
p2

p3 <- tm_shape(dd_sf) +
  tm_polygons(col = "ChildrenUnder_5", title = "Children Under 5 year", style = "quantile") 
p3

p4 <- tm_shape(dd_sf) +
  tm_polygons(col = "ChildrenOver_5", title = "Children Over 5 year", style = "quantile") 
p4

p5 <- tm_shape(dd_sf) +
  tm_polygons(col = "TotalChildren", title = "Total Children", style = "quantile") 
p5


p6 <- tm_shape(dd_sf) +
  tm_polygons(col = "TotalIDP", title = "Total IDP's", style = "quantile") 
p6



tmap_arrange(p1, p2, p3, p4, p5, p6)
