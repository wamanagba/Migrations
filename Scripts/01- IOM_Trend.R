rm(list = ls())

library(rio)
library(dplyr)
library(zoo)
library(sf)
library(tmap)
df=s = rio::import("Migrations_Works/Data/IOM/DataF.csv")
s=df <- df %>%
  mutate(adm_02 = recode(department,
                         "Ayérou" = "Ayerou",
                         "Gotheye" = "Gothèye",
                         "Mainé Soroa" = "Maïné Soroa",
                         "Maine_Soroa" = "Maïné Soroa",
                         "Ngourti" = 	"N'Gourti",
                         "Nguigmi"="N'Guigmi",
                         "Tera" = "Téra",
                         "Maradi" = "Ville de Maradi",
                         "N'Guiguimi" = "N'Guigmi",
                         "Niamey" = "Ville de Niamey",
                         "Tillaberi" = "Tillabéri",
                         "Birnin_Konni"="Birni N'Konni",
                         "Baleyara"= "Balleyara",
                         "Guidan_Roumdji" = "Guidan Roumdji"))

Trend<-s%>%
  group_by(adm_02)%>%
  summarise(TrendValue=summary(lm(coredata(IdpsInd)~index(dates)))$coefficients[2])

Trend$val <- ifelse(Trend$TrendValue > 0, "Positive ", "Negative ")

CVr <- s %>%
  group_by(adm_02) %>%
  summarise(
    Mean = mean(IdpsInd),       # calcul the average
    SD = sd(IdpsInd),           # standard deviation
    CV = (SD / Mean) * 100    #  coefficient of variation (in percentage)
  )

shapefile = st_read("Migrations_Works/Data/ner_adm02_feb2018/NER_adm02_feb2018.shp")
shapefile = merge(shapefile, Trend, by = "adm_02", all = TRUE)


tmap_mode("view")
shapefile <- st_make_valid(shapefile)
p1_Trend <- tm_shape(shapefile) +
  tm_polygons(col = "val", title = "Trend Coef") +
  tm_layout(panel.labels = "tm_scale_categorical", 
            legend.text.size = 0.5)
p1_Trend


shapefile <- merge(shapefile, CVr, by = "adm_02", all = TRUE)


p2_CV <- tm_shape(shapefile) +
  tm_polygons(col = "CV", title = "coefficient of variation ") +
  tm_layout(legend.outside = TRUE)
p2_Mean <- tm_shape(shapefile) +
  tm_polygons(col = "Mean", title = "Average IDPs per Level2") +
  tm_layout(legend.outside = TRUE)
p2_CV
p2_Mean

