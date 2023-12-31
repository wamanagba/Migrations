rm(list = ls())
library(sf)
library(dplyr)
library(rio)
library(tmap)
library(rgdal)
library(mapview)
library(shiny)
setwd("~/Desktop/BioD/")
NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
dir.create("Data/Department",recursive = T,showWarnings = F)

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


# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(Data, coords = c("lon", "lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)


# Columns to process
columns <- c("Idps", "Refugees", "IdpReturn", "ReturnFromOutsideBorder")

# Initialize an empty dataframe to store the results
Data_Sums <- data.frame(NAME_2 = unique(agregation$NAME_2))

# Loop to calculate sums for different columns
for (col in columns) {
  # Calculate the sum per region
  sum_by_region <- agregation %>%
    group_by(NAME_2) %>%
    summarise(!!paste0("Sum", col) := sum(!!sym(col), na.rm = TRUE))
  
  # Merge the results with the main dataframe
  Data_Sums <- merge(Data_Sums, sum_by_region, by = "NAME_2", all = TRUE)
}


# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)

#Data_Sums[is.na(Data_Sums)] <- 0
shapefile = shapefile[,-c(1:6)]
shapefile =shapefile[,-c(2:7)]

shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumIdps <- Data_Sums$SumIdps
shapefile$SumRefugees <- Data_Sums$SumRefugees
shapefile$SumIdpReturn <- Data_Sums$SumIdpReturn
shapefile$SumReturnFromOutsideBorder <- Data_Sums$SumReturnFromOutsideBorder

mapview(shapefile, zcol = "SumIdps")
tmap_mode("view")
p1 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIdps", title = "Total IDPs --2023", style = "quantile") 
p1
p2 <- tm_shape(shapefile) +
  tm_polygons(col = "SumRefugees", title = "Total Refugees --2023") +
  tm_layout(legend.outside = TRUE)
p2
p3 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIdpReturn", title = "Total IDPs Returned --2023") +
  tm_layout(legend.outside = TRUE)
p3
p4 <- tm_shape(shapefile) +
  tm_polygons(col = "SumReturnFromOutsideBorder", title = "Total Person returning from outside Niger-2023") +
  tm_layout(legend.outside = TRUE)
p4
#tmap_arrange(p1, p2, p3, p4)



# Create a Shiny web application
shinyApp(
  ui = fluidPage(
    titlePanel("Map of Displaced Persons"),
    selectInput("mapInput", "Select a map:",
                choices = c("Internal Displaced Persons", "Refugees", "Internally displaced person Return", "Returned from outside the country")),
    tmapOutput("map")
  ),
  server = function(input, output) {
    # Render the selected map
    output$map <- renderTmap({
      if (input$mapInput == "Internal Displaced Persons") {
        p1
      } else if (input$mapInput == "Refugees") {
        p2
      } else if (input$mapInput == "Internally displaced person Return") {
        p3
      } else if (input$mapInput == "Returned from outside the country") {
        p4
      }
    })
  }
)


