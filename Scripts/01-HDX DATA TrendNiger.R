library(rio)
library(dplyr)

library(rgdal)

library(sf)
library(tmap)
library(zoo)

rm(list=ls())

#DataSept2020 = DataSept2020[,c(1:3)]
#DataSept2020$Date= "Sept-2020"
#DataJan2021 =DataJan2021[,c(1:3)]
#DataJan2021$Date = "Jan-2021"
#colnames(DataSept2021)[3] ="SumNombre de personnes"
#colnames(DataMay2022)[3] ="SumNombre de personnes"

#DataSept2021$Date = "Sept-2021"
#DataMay2022$Date = "May-2022"
#DataJul2022$Date = "Jul-2022"

#Datac= rbind(DataSept2020,DataJan2021)
#Datac =rbind(Datac,DataSept2021)
#Datac =rbind(Datac,DataMay2022)
#Datac =rbind(Datac,DataJul2022)


#rio::export(Datac,"Migrations_Works/Data/Datamerge.csv")
NigerShapeFile<-readOGR("Data/gadm41_NER_shp/gadm41_NER_2.shp") 
shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
Datac=rio::import("Migrations_Works/Data/Datamerge.csv")
Trend<-Datac%>%
  group_by(NAME_2)%>%
  summarise(TrendValue=summary(lm(coredata(`SumNombre de personnes`)~index(Date)))$coefficients[2])

#Trend$val <- ifelse(Trend$TrendValue > 0, -1, 1)
Trend$val <- ifelse(Trend$TrendValue > 0, "Positive ", "Negative ")

CVr <- Datac %>%
  group_by(NAME_2) %>%
  summarise(
    Mean = mean(`SumNombre de personnes`),       # Calculer la moyenne
    SD = sd(`SumNombre de personnes`),           # Calculer l'écart-type
    CV = (SD / Mean) * 100    # Calculer le coefficient de variation (en pourcentage)
  )

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(Trend, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)



shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$Trend <- Data_Sums$val
shapefile$Trend_Coef <- Data_Sums$TrendValue

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")
color_palette <- c("green","red")
shapefile <- st_make_valid(shapefile)
p1_Trend <- tm_shape(shapefile) +
  tm_polygons(col = "Trend",palette=color_palette,  title = "Trend Coef") +
  tm_layout(panel.labels = "tm_scale_categorical", 
            legend.text.size = 0.5)
p1_Trend
###

# Merge with the region names from NigerShapeFile
d <- as.data.frame(unique(NigerShapeFile$NAME_2))
colnames(d) <- "NAME_2"

# Merge the data by region
Data_Sums <- merge(CVr, d, by = "NAME_2", all = TRUE)
#Data_Sums <- merge(Data_Sums, d, by = "region", all = TRUE)


shapefile <- st_read("Data/NER_adm/NER_adm2.shp")
shapefile <- shapefile[order(shapefile$NAME_2), ]

shapefile$Mean_Idps <- Data_Sums$Mean
shapefile$CV <- Data_Sums$CV

#mapview(shapefile, zcol = "")
shapefile = shapefile[,-c(1:6)]
tmap_mode("view")

p2_CV <- tm_shape(shapefile) +
  tm_polygons(col = "CV", title = "coefficient of variation ") +
  tm_layout(legend.outside = TRUE)
p2_Mean <- tm_shape(shapefile) +
  tm_polygons(col = "Mean_Idps", title = "Average IDPs-- From 2020 to 2022") +
  tm_layout(legend.outside = TRUE)
p2_CV
p2_Mean
