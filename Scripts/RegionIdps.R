
rm(list=ls())
library(sf)
library(dplyr)
library(rio)
setwd("~/Desktop/BioD/")
NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_1.shp") 
# Data processing
dataIdps=rio::import("Data/Niger VAS round 6-Jun 06 2023.xlsx - Main data.csv")
Data=as.data.frame(matrix(ncol=1, nrow=632))
colnames(Data)=c("region")
Data$region=dataIdps$`A2. Région`
Data$department =dataIdps$`A3. Département`
Data$commune= dataIdps$`A4. Commune`
Data$Idps = dataIdps$`B1. Personnes Déplacées interne/B1.a Nombre de ménages`
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

dir.create("Migrations_Works/Products/Region", recursive = TRUE, showWarnings = FALSE)


# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_3.shp")
Data_sf <- st_as_sf(Data, coords = c("lon", "lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)


# Columns to process
columns <- c("Idps", "Refugees", "IdpReturn", "ReturnFromOutsideBorder")

# Initialize an empty dataframe to store the results
Data_Sums <- data.frame(NAME_1 = unique(agregation$NAME_1))

# Loop to calculate sums for different columns
for (col in columns) {
  # Calculate the sum per region
  sum_by_region <- agregation %>%
    group_by(NAME_1) %>%
    summarise(!!paste0("Sum", col) := sum(!!sym(col), na.rm = TRUE))
  
  # Merge the results with the main dataframe
  Data_Sums <- merge(Data_Sums, sum_by_region, by = "NAME_1", all = TRUE)
}
# Convert the "department" column to a factor with sorted levels
#Data_Sums$department <- factor(Data_Sums$department, levels = unique(Data_Sums$department))



# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_1))
colnames(d) <- "NAME_1"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_1", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)

Data_Sums[is.na(Data_Sums)] <- 0

library(RColorBrewer)

# Définir le nombre de couleurs souhaité
n <- length(unique(Data_Sums$NAME_1))

# Définir la palette de couleurs pour la légende et les graphiques
legend_colors <- colorRampPalette(brewer.pal(n, "Set3"))(n)

# Boucle sur les colonnes
dir.create("Data/Region/",recursive = T,showWarnings = F)
breakpoints <- c(-Inf, c(0, 100, 500, 2000, 3000, 5000, 10000), Inf)
cols <- colorRampPalette(c("lightgreen", "yellow", "orange", "red", "darkred"))(length(breakpoints) - 1)



for (col in columns) {
  # Cut the values into intervals and assign corresponding colors
  xx <- cut(Data_Sums[[paste0("Sum", col)]], breaks = breakpoints)
  levels(xx) <- cols
  
  # Add the interval values to the NigerShapeFile
  NigerShapeFile[[col]] <- as.vector(xx)
  
  # Generate the plot
  png(paste0("Migrations_Works/Products/Region/Map_", col, ".png"), height = 800, width = 1200, type = "cairo")
  plot(NigerShapeFile, border = "black")
  plot(NigerShapeFile, col = NigerShapeFile[[col]], add = TRUE)
  
  # Add the legend with region values
  legend_title <- paste("Sum of", col)
  legend_values <- legend_labels <- c("[-Inf, 0]", "(0, 100]", "(100, 500]", "(500, 2000]", "(2000, 3000]", "(3000, 5000]", "(5000, 10000]", "(10000, Inf]")
  
  legend("topright", legend = legend_values, title = legend_title, fill = cols, bty = "n", cex = 1.5)
  
  dev.off()
}
