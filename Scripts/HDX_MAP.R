library(sf)
library(dplyr)
df= rio::import("Migrations_Works/Data/HDX/DataOk/data2019-2022.xlsx")
df[, (ncol(df) - 25):ncol(df)] <- lapply(df[, (ncol(df) - 25):ncol(df)], as.numeric)
colnames(df)[1]="adm_02"
shapefile = st_read("Data/ner_adm02_feb2018/NER_adm02_feb2018.shp")
df <- df %>%
  mutate(adm_02 = recode(adm_02,
                       "Ayérou" = "Ayerou",
                       "Gotheye" = "Gothèye",
                       "Mainé Soroa" = "Maïné Soroa",
                       "Maradi" = "Ville de Maradi",
                       "N'Guiguimi" = "N'Guigmi",
                       "Niamey" = "Ville de Niamey"))

df2=rio::import("Migrations_Works/Data/HDX/DataOk/Apr2023.xlsx")
df2 =df2%>%
  group_by(DEPARTEMENT)%>%
  summarise(Apr2023 = sum(Personnes))
colnames(df2)[1]= "adm_02"
df2 <- df2 %>%
  mutate(adm_02 = recode(adm_02,
                         "Ayérou" = "Ayerou",
                         "Gotheye" = "Gothèye",
                         "Maine-Soroa" = "Maïné Soroa",
                         "Maradi" = "Ville de Maradi",
                         "N'Guiguimi" = "N'Guigmi",
                         "Niamey" = "Ville de Niamey",
                         "Tera"= "Téra"))

df3 <- merge(df, df2, by = "adm_02", all = TRUE)

Data_Sums <- merge(shapefile, df3, by = "adm_02", all = TRUE)
colnames(Data_Sums)[11:36] = c("t2016","t2017","t2018","t2019","t2020","Jan-2021","Feb2021","Mar-2021","Apr-2021","May-2021",
                               "Jun-2021","Jul-2021","Aug-2021","Sep-2021","Oct-2021","Nov-2021","Dec-2021",
                               "Jav-2022","Feb-2022","Mar-2022","Apr-2022","May-2022","Jun-2022","Jul-2022","Aug-2022","Sep-2022")


# Tenter de corriger les géométries invalides
Data_Sums_valid <- st_make_valid(Data_Sums)

# Vérifier à nouveau la validité des données après correction
st_is_valid(Data_Sums_valid)
tmap_mode("view")


library(tmap)
library(shiny)
library(leaflet)

# Create a list to store the figures for each year
figures_list <- list()

# List of years to loop through
years <- c("t2017", "t2018", "t2019", "t2020","Jan-2021","Feb2021","Mar-2021","Apr-2021","May-2021",
           "Jun-2021","Jul-2021","Aug-2021","Sep-2021","Oct-2021","Nov-2021","Dec-2021",
           "Jav-2022","Feb-2022","Mar-2022","Apr-2022","May-2022","Jun-2022","Jul-2022","Aug-2022","Sep-2022","Apr2023")

# Loop to create the figures for each year
for (year in years) {
  figure_name <- paste0("p", year)
  figure <- tm_shape(Data_Sums_valid) +
    tm_polygons(col = year, title = "Total IDP's", style = "quantile") 
  figures_list[[figure_name]] <- figure
}

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Map of Displaced Persons"),
  selectInput("yearInput", "Select the year:",
              choices = years),
  leafletOutput("map")
)

# Define the server function for the Shiny app
server <- function(input, output) {
  
  # Function to get the selected figure based on year
  getSelectedFigure <- reactive({
    year <- paste0("p", input$yearInput)
    return(figures_list[[year]])
  })
  
  # Render the selected map
  output$map <- renderLeaflet({
    selectedFigure <- getSelectedFigure()
    tmap_leaflet(selectedFigure)
  })
}

# Run the Shiny app
shinyApp(ui, server)












library(tmap)
p2017 <- tm_shape(Data_Sums_valid) +
  tm_polygons(col = "t2017", title = "Total Men", style = "quantile") 
p2017
