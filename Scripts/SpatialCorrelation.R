

rm(list = ls())
library(rio)
library(rgdal)
library(dplyr)
library(sf)
library(mapview)
library(tmap)
library(spdep)
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



nb <- poly2nb(dd_sf, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")


dd_sf$TotalIDP[is.na(dd_sf$TotalIDP)] <- 0
lmoran <- localmoran(dd_sf$TotalIDP, nbw, alternative = "greater")
head(lmoran)

tmap_mode("view")

dd_sf$lmI <- lmoran[, "Ii"] # local Moran's I
dd_sf$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
dd_sf$lmp <- lmoran[, "Pr(z > E(Ii))"]

p1 <- tm_shape(dd_sf) +
  tm_polygons(col = "TotalIDP", title = "vble", style = "quantile") +
  tm_layout(legend.outside = TRUE)

p2 <- tm_shape(dd_sf) +
  tm_polygons(col = "lmI", title = "Local Moran's I",
              style = "quantile") +
  tm_layout(legend.outside = TRUE)

p3 <- tm_shape(dd_sf) +
  tm_polygons(col = "lmZ", title = "Z-score",
              breaks = c(-Inf, 1.65, Inf)) +
  tm_layout(legend.outside = TRUE)

p4 <- tm_shape(dd_sf) +
  tm_polygons(col = "lmp", title = "p-value",
              breaks = c(-Inf, 0.05, Inf)) +
  tm_layout(legend.outside = TRUE)

tmap_arrange(p1, p2, p3, p4)

mp <- moran.plot(as.vector(scale(dd_sf$TotalIDP)), nbw)

tm_shape(dd_sf) + tm_polygons(col = "lmZ",
                            title = "Local Moran's I", style = "fixed",
                            breaks = c(-Inf, -1.96, 1.96, Inf),
                            labels = c("Negative SAC", "No SAC", "Positive SAC"),
                            palette =  c("blue", "white", "red")) +
  tm_layout(legend.outside = TRUE)


dd_sf$quadrant <- NA
# high-high
dd_sf[(mp$x >= 0 & mp$wx >= 0) & (dd_sf$lmp <= 0.05), "quadrant"]<- 1
# low-low
dd_sf[(mp$x <= 0 & mp$wx <= 0) & (dd_sf$lmp <= 0.05), "quadrant"]<- 2
# high-low
dd_sf[(mp$x >= 0 & mp$wx <= 0) & (dd_sf$lmp <= 0.05), "quadrant"]<- 3
# low-high
dd_sf[(mp$x <= 0 & mp$wx >= 0) & (dd_sf$lmp <= 0.05), "quadrant"]<- 4
# non-significant
dd_sf[(dd_sf$lmp > 0.05), "quadrant"] <- 5




tm_shape(dd_sf) + tm_fill(col = "quadrant", title = "",
                        breaks = c(1, 2, 3, 4, 5, 6),
                        palette =  c("red", "blue", "lightpink", "skyblue2", "white"),
                        labels = c("High-High", "Low-Low", "High-Low",
                                   "Low-High", "Not significant")) +
  tm_legend(text.size = 1)  + tm_borders(alpha = 0.5) +
  tm_layout(frame = FALSE,  title = "Clusters")  +
  tm_layout(legend.outside = TRUE)
