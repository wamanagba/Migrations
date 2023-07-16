

rm(list=ls())
library(sf)
library(dplyr)
library(rio)
library(rgdal)
setwd("~/Desktop/BioD/")
NigerShapeFile <- readOGR("Data/gadm41_NER_shp/gadm41_NER_3.shp") 
dir.create("Migrations_Works/Products/Commune", recursive = TRUE, showWarnings = FALSE)

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

# Convert your dataframe to an sf object
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_3.shp")
Data_sf <- st_as_sf(Data, coords = c("lon", "lat"), crs = st_crs(shapefile))

# Perform aggregation
aggregation <- st_join(Data_sf, shapefile, join = st_within)
aggregation <- as.data.frame(aggregation)

# Columns to process
columns <- c("Idps", "Refugees", "IdpReturn", "ReturnFromOutsideBorder")

# Initialize an empty dataframe to store the results
Data_Sums <- data.frame(NAME_3 = unique(aggregation$NAME_3))

# Loop to calculate sums for different columns
for (col in columns) {
  sum_by_region <- aggregation %>%
    group_by(NAME_3) %>%
    summarise(!!paste0("Sum", col) := sum(!!sym(col), na.rm = TRUE))
  
  # Merge the results with the main dataframe
  Data_Sums <- merge(Data_Sums, sum_by_region, by = "NAME_3", all = TRUE)
}

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_3))
colnames(d) <- "NAME_3"

# Merge the data with the shape file Informations
Data_Sums <- merge(Data_Sums, d, by = "NAME_3", all = TRUE)

Data_Sums[is.na(Data_Sums)] <- 0

breakpoints <- c(-Inf, c(0, 100, 500, 2000, 3000, 5000, 10000), Inf)
cols <- colorRampPalette(c("lightgreen", "yellow", "orange", "red", "darkred"))(length(breakpoints) - 1)



# Loop over the columns
for (col in columns) {
  xx <- cut(c(t(as.numeric(Data_Sums[[paste0("Sum", col)]]))), breaks = breakpoints)
  levels(xx) <- cols
  
  # Create a temporary copy of NigerShapeFile
  tmpNigerShapeFile <- NigerShapeFile
  
  # Add the interval values to the temporary copy
  tmpNigerShapeFile[["SumIdps"]] <- c(as.vector(xx),"#90EE90")
  
  png(paste0("Migrations_Works/Products/Commune/Sum", col, ".png"), height = 800, width = 1200, type = "cairo")
  
  plot(tmpNigerShapeFile, col = tmpNigerShapeFile$SumIdps, main = paste("Sum of", col, "by Level 2"), cex = 3, cex.main = 3)
  # Add text labels for NAME_3
  #text(tmpNigerShapeFile, labels = tmpNigerShapeFile$NAME_3, col = "black", cex = 0.8)
  
  legend_title <- paste("Sum of", col)
  legend_labels <- c("[-Inf, 0]", "(0, 100]", "(100, 500]", "(500, 2000]", "(2000, 3000]", "(3000, 5000]", "(5000, 10000]", "(10000, Inf]")
  legend_colors <- cols
  
  # Create the legend with rectangles indicating the colors
  legend("topright", legend = legend_labels, fill = legend_colors, bty = "n")
  rect(par("usr")[2] + 0.2, par("usr")[4], par("usr")[2] + 0.5, par("usr")[4] - 0.5, col = cols, border = NA)
  
  dev.off()
}
