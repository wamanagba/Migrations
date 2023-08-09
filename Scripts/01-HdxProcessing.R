
rm(list = ls())
library(sf)
library(dplyr)
library(rio)
library(tmap)
library(rgdal)
library(mapview)
library(
  shiny
)
setwd("~/Desktop/BioD/")

NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Data = rio::import("Migrations_Works/Data/HDX/DataOk/Sept2020.xlsx")


#Ramplacer "," par ".."
Data$Long <- gsub(",", ".", Data$Long)
Data$Lat <- gsub(",", ".", Data$Lat)


Data$Long <- as.numeric(Data$Long)
Data$Lat <- as.numeric(Data$Lat)
missing_Long <- is.na(Data$Long)
missing_Lat <- is.na(Data$Lat)
Data <- Data[!missing_Long & !missing_Lat, ]

#sum_by_region <- Data %>%
#  group_by(Département) %>%
#  summarise(summm= sum(`Nombre de personnes`), na.rm = TRUE)

# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(Data, coords = c("Long", "Lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)

# Columns to process
columns <- c("Nombre de ménages", "Nombre de personnes", "hommes", "femmes")


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

DataSept2020 = Data_Sums


# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumHH <- Data_Sums$`SumNombre de ménages`
shapefile$SumIDP <- Data_Sums$`SumNombre de personnes`
shapefile$Men <- Data_Sums$Sumhommes
shapefile$Females <- Data_Sums$Sumfemmes

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
p1_Sept2020 <- tm_shape(shapefile) +
         tm_polygons(col = "SumHH", title = "Number of households", style = "quantile") 

p2_Sept2020 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIDP", title = "Number of IDPs") +
  tm_layout(legend.outside = TRUE)

p3_Sept2020 <- tm_shape(shapefile) +
  tm_polygons(col = "Men", title = "Number of Men") +
  tm_layout(legend.outside = TRUE)

p4_Sept2020 <- tm_shape(shapefile) +
  tm_polygons(col = "Females", title = "Number of Females") +
  tm_layout(legend.outside = TRUE)

#sept2020=tmap_arrange(p1, p2, p3, p4)



######################

NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Data = rio::import("Migrations_Works/Data/HDX/DataOk/Jan2021.xlsx")


#Ramplacer "," par ".."
Data$Long <- gsub(",", ".", Data$Long)
Data$Lat <- gsub(",", ".", Data$Lat)


Data$Long <- as.numeric(Data$Long)
Data$Lat <- as.numeric(Data$Lat)
missing_Long <- is.na(Data$Long)
missing_Lat <- is.na(Data$Lat)
Data <- Data[!missing_Long & !missing_Lat, ]

#sum_by_region <- Data %>%
#  group_by(Département) %>%
#  summarise(summm= sum(`Nombre de personnes`), na.rm = TRUE)

# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(Data, coords = c("Long", "Lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)

# Columns to process
columns <- c("Nombre de ménages", "Nombre de personnes", "hommes", "femmes")


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

DataJan2021 = Data_Sums

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumHH <- Data_Sums$`SumNombre de ménages`
shapefile$SumIDP <- Data_Sums$`SumNombre de personnes`
shapefile$Men <- Data_Sums$Sumhommes
shapefile$Females <- Data_Sums$Sumfemmes



#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
p1_Jan2021 <- tm_shape(shapefile) +
  tm_polygons(col = "SumHH", title = "Number of households", style = "quantile") 

p2_Jan2021 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIDP", title = "Number of IDPs") +
  tm_layout(legend.outside = TRUE)

p3_Jan2021 <- tm_shape(shapefile) +
  tm_polygons(col = "Men", title = "Number of Men") +
  tm_layout(legend.outside = TRUE)

p4_Jan2021 <- tm_shape(shapefile) +
  tm_polygons(col = "Females", title = "Number of Females") +
  tm_layout(legend.outside = TRUE)

#Jan2021=tmap_arrange(p1, p2, p3, p4)






######################

NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Data = rio::import("Migrations_Works/Data/HDX/DataOk/sept2021.xlsx")


#Ramplacer "," par ".."
Data$Long <- gsub(",", ".", Data$Long)
Data$Lat <- gsub(",", ".", Data$Lat)


Data$Long <- as.numeric(Data$Long)
Data$Lat <- as.numeric(Data$Lat)
missing_Long <- is.na(Data$Long)
missing_Lat <- is.na(Data$Lat)
Data <- Data[!missing_Long & !missing_Lat, ]

#sum_by_region <- Data %>%
#  group_by(Département) %>%
#  summarise(summm= sum(`Nombre de personnes`), na.rm = TRUE)

# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(Data, coords = c("Long", "Lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)

# Columns to process
columns <- c("Nombre de ménages", "Nombre de pdi")


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

DataSept2021 =Data_Sums

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumHH <- Data_Sums$`SumNombre de ménages`
shapefile$SumIDP <- Data_Sums$`SumNombre de pdi`

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
p1_Sept2021 <- tm_shape(shapefile) +
  tm_polygons(col = "SumHH", title = "Number of households", style = "quantile") 

p2_Sept2021 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIDP", title = "Number of IDPs") +
  tm_layout(legend.outside = TRUE)





######################

NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Data = rio::import("Migrations_Works/Data/HDX/DataOk/may2022.xlsx")


#Ramplacer "," par ".."
Data$Long <- gsub(",", ".", Data$Long)
Data$Lat <- gsub(",", ".", Data$Lat)


Data$Long <- as.numeric(Data$Long)
Data$Lat <- as.numeric(Data$Lat)
missing_Long <- is.na(Data$Long)
missing_Lat <- is.na(Data$Lat)
Data <- Data[!missing_Long & !missing_Lat, ]

#sum_by_region <- Data %>%
#  group_by(Département) %>%
#  summarise(summm= sum(`Nombre de personnes`), na.rm = TRUE)

# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
Data_sf <- st_as_sf(Data, coords = c("Long", "Lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)

# Columns to process
columns <- c("Nombre de ménages", "Nombre de pdi")


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

DataMay2022 = Data_Sums

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumHH <- Data_Sums$`SumNombre de ménages`
shapefile$SumIDP <- Data_Sums$`SumNombre de pdi`

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
p1_may2022 <- tm_shape(shapefile) +
  tm_polygons(col = "SumHH", title = "Number of households", style = "quantile") 

p2_may2022 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIDP", title = "Number of IDPs") +
  tm_layout(legend.outside = TRUE)








######################

NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Data = rio::import("Migrations_Works/Data/HDX/DataOk/Jul-2022.xlsx")
#shapefile= st_read("Data/ner_adm02_feb2018/NER_adm02_feb2018.shp")


#Ramplacer "," par ".."
Data$Long <- gsub(",", ".", Data$Long)
Data$Lat <- gsub(",", ".", Data$Lat)


Data$Long <- as.numeric(Data$Long)
Data$Lat <- as.numeric(Data$Lat)
missing_Long <- is.na(Data$Long)
missing_Lat <- is.na(Data$Lat)
Data <- Data[!missing_Long & !missing_Lat, ]

#sum_by_region <- Data %>%
#  group_by(Département) %>%
#  summarise(summm= sum(`Nombre de personnes`), na.rm = TRUE)

# Convertissez votre dataframe en objet sf
shapefile <- st_read("Data/gadm41_NER_shp/gadm41_NER_2.shp")
#shapefile= st_read("Data/ner_adm02_feb2018/NER_adm02_feb2018.shp")

Data_sf <- st_as_sf(Data, coords = c("Long", "Lat"), crs = st_crs(shapefile))

# Effectuez l'agrégation
agregation <- st_join(Data_sf, shapefile, join = st_within)
agregation= as.data.frame(agregation)

# Columns to process
columns <- c("Nombre de ménages", "Nombre de personnes")


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

DataJul2022 = Data_Sums

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Data_Sums, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$SumHH <- Data_Sums$`SumNombre de ménages`
shapefile$SumIDP <- Data_Sums$`SumNombre de personnes`

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
p1_Jul2022 <- tm_shape(shapefile) +
  tm_polygons(col = "SumHH", title = "Number of households", style = "quantile") 

p2_Jul2022 <- tm_shape(shapefile) +
  tm_polygons(col = "SumIDP", title = "Number of IDPs") +
  tm_layout(legend.outside = TRUE)










library(shiny)
library(tmap)
library(leaflet)

# ... (Your data processing code here) ...

# Create a list to store the figures for each year
figures_list <- list(
  Sept2020 = list(
    p1 = p1_Sept2020,
    p2 = p2_Sept2020,
    p3 = p3_Sept2020,
    p4 = p4_Sept2020
  ),
  Jan2021 = list(
    p1 = p1_Jan2021,
    p2 = p2_Jan2021,
    p3 = p3_Jan2021,
    p4 = p4_Jan2021
  ),
  Sept2021 = list(
    p1 = p1_Sept2021,
    p2 = p2_Sept2021
  ),
  may2022 = list(
    p1=p1_may2022,
    p2 = p2_may2022
  ),
  Jul2022 = list(
    p1=p1_Jul2022,
    p2=p2_Jul2022
  )
)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Map of Displaced Persons"),
  selectInput("yearInput", "Select the year:",
              choices = c("September 2020", "January 2021", "September 2021", "May 2022", "July 2022")),
  selectInput("mapInput", "Select a map:",
              choices = c("Number of households", "Number of IDP", "Number of Men", "Number of females")),
  leafletOutput("map")
)

# Define the server function for the Shiny app
server <- function(input, output) {
  
  # Function to get the selected figure based on year and map type
  getSelectedFigure <- reactive({
    year <- switch(input$yearInput,
                   "September 2020" = "Sept2020",
                   "January 2021" = "Jan2021",
                   "September 2021" = "Sept2021",
                   "May 2022" ="may2022",
                   "July 2022" = "Jul2022"
    )
    
    map_type <- switch(input$mapInput,
                       "Number of households" = "p1",
                       "Number of IDP" = "p2",
                       "Number of Men" = "p3",
                       "Number of females" = "p4")
    
    return(figures_list[[year]][[map_type]])
  })
  
  # Render the selected map
  output$map <- renderLeaflet({
    selectedFigure <- getSelectedFigure()
    tmap_leaflet(selectedFigure)
  })
}

# Run the Shiny app
shinyApp(ui, server)



