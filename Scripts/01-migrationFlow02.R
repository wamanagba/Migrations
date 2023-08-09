


rm(list=ls())
setwd("~/Desktop/BioD/")
library(rio)
library(dplyr)
library(rgdal)
library(sf)
library(leaflet)
library(ggplot2)
library(odf)
library(donutmaps)
library(tmap)
df =data = rio::import("Migrations_Works/Data/population.csv")



# remove the first roows
df <- slice(df, -(1:14))
colnames(df) <- df[1, ]
df <- slice(df, -1)
df <- rename(df, "origin"= "Country of origin" ,"asylum"="Country of asylum","refugees"="Refugees under UNHCR's mandate")
df$origin <- ifelse(df$origin == "Cote d'Ivoire", "Côte d'Ivoire", df$origin)
df$origin <- ifelse(df$origin == "Central African Rep.", "Central African Republic", df$origin)
df$origin <- ifelse(df$origin == "Guinea-Bissau", "Guinea Bissau", df$origin)

#df <- df %>%
#  filter(Year == 2022)

Data_ISO = as.data.frame( df$`Country of origin (ISO)`)
colnames(Data_ISO) = "ISO"
Data_ISO$name = df$origin


# Remove duplicates using the "column_a_remove_duplicates" column
Data_ISO <- distinct(Data_ISO, ISO, .keep_all = TRUE)
Westafrica <- st_read("Data/wca_admbnda_adm0_ocha_29062021/wca_admbnda_adm0_ocha_29062021.shp") 
centroids <- st_centroid(Westafrica)
colnames(centroids)[1]= "name"


merged_df <- merge(centroids, Data_ISO, by = "name")
Data_ISO= merged_df[, -c(2:5)]

Data_flow= as.data.frame(df$`Country of origin (ISO)`)
colnames(Data_flow) = "ISO_origin"
Data_flow$ISO_to = df$`Country of asylum (ISO)`
Data_flow$value = df$refugees
Data_flow$value = as.numeric(Data_flow$value)

Data_flow$Asylum =df$`Asylum-seekers`
Data_flow$Asylum = as.numeric(Data_flow$Asylum)
####################### outside the code
niger_to= Data_flow %>%
  filter(ISO_origin == "NER")
Niger_to = niger_to%>%
  group_by(ISO_to)%>%
  summarise(ISO= sum(value))
sorted_data <- Niger_to %>%
  arrange(desc(ISO))
rio::export(sorted_data,"Migrations_Works/Data/Niger_to.csv")
###
niger_from= Data_flow %>%
  filter(ISO_to == "NER")
Niger_frm = niger_from%>%
  group_by(ISO_origin)%>%
  summarise(origin= sum(value))
rio::export(Niger_frm,"Migrations_Works/Data/to_Niger.csv")
########################

x = od(Data_flow, Data_ISO, col_orig = "ISO_origin", col_dest = "ISO_to", col_id = "ISO")

# Define color palette
CBS_pal = c("#d9328a", "#7d4791", "#da5914", "#53a31d", "#0581a2","#FF0000", "#B3B3B3","#00FFFF")


my_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#800080")

tm = bake_donuts(x,
                 var = "value",
                 groupname = "Netherlands",
                 highlight = c("Burkina Faso", "Niger", "Côte d'Ivoire", "Mali","Nigeria","Chad"),
                 pal = CBS_pal,
                 donut_size_min = 30000, donut_size_max = 400000,
                 flow_th = 50, flow_max = 2000, flow_buffer = 500, flow_scale = 10,
                 donut_scale = 1.75)


# The result is a tmap object
tmap_mode("view")
tm

m = bake_donuts(x,
                var = "Asylum",
                groupname = "Netherlands",
                highlight = c("Burkina Faso", "Niger", "Côte d'Ivoire", "Mali","Nigeria","Chad"),
                pal = CBS_pal,
                donut_size_min = 30000, donut_size_max = 400000,
                flow_th = 50, flow_max = 2000, flow_buffer = 500, flow_scale = 10,
                donut_scale = 1.75)


# The result is a tmap object
tmap_mode("view")
m
