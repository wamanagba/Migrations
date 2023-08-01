rm(list = ls())
library(rio)
library(rgdal)
library(dplyr)
library(sf)
library(mapview)
library(tmap)
setwd("~/Desktop/BioD/")
df = rio::import("Migrations_Works/Data/situation-des-pdi-par-communes-accueil-du-28-fevrier-2023.xlsx")
# remove the first roows
df <- slice(df, -(1:5))
colnames(df) <- df[1, ]
df <- slice(df, -1)
df <- slice(df,-c((nrow(df) - 1):nrow(df)))
#df$COMMUNES <- tolower(df$COMMUNES)
df$COMMUNES = paste(toupper(substring(df$COMMUNES, 1, 1)), tolower(substring(df$COMMUNES, 2)), sep = "")


BFShapeFile<-readOGR("Data/gadm41_BFA_shp/gadm41_BFA_3.shp")
shapefile=st_read("Data/gadm41_BFA_shp/gadm41_BFA_3.shp")
DataShapefile= as.data.frame(shapefile)
df$NAME_3
DataShapefile$NAME_3
colnames(df)[colnames(df) == "COMMUNES"] <- "NAME_3"
dd=merge(DataShapefile,df, by = "NAME_3",all.x = TRUE)

File= as.data.frame(dd$NAME_3)
colnames(File) = "NAME_3"
File$Man = as.numeric(dd$Hommes)
File$Femelle = as.numeric(dd$Femmes)
File$ChildrenUnder_5 = as.numeric(dd$`Enfants de moins de 5 ans`)
File$ChildrenOver_5 = as.numeric(dd$`Enfants de plus de 5 ans`)
File$TotalChildren = as.numeric(dd$`Total Enfants`)
File$TotalIDP = as.numeric(dd$`Nombre total de PDI`)
File$geometry =dd$geometry

dd_sf <- st_as_sf(File)

mapview(dd_sf, zcol = "Man")

tmap_mode("view")
p1 <- tm_shape(dd_sf) +
  tm_polygons(col = "Man", title = "Total Men", style = "quantile") 

p2 <- tm_shape(dd_sf) +
  tm_polygons(col = "Femelle", title = "Total women", style = "quantile") 

p3 <- tm_shape(dd_sf) +
  tm_polygons(col = "ChildrenUnder_5", title = "Children Under 5 year", style = "quantile") 

p4 <- tm_shape(dd_sf) +
  tm_polygons(col = "ChildrenOver_5", title = "Children Over 5 year", style = "quantile") 

p5 <- tm_shape(dd_sf) +
  tm_polygons(col = "TotalChildren", title = "Total Children", style = "quantile") 

p6 <- tm_shape(dd_sf) +
  tm_polygons(col = "TotalIDP", title = "Total IDP's", style = "quantile") 


tmap_arrange(p1, p2, p3, p4, p5, p6)
