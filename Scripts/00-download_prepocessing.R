library(dplyr)
library(tidync)
library(ncdf4)
library(rio)
library(rgdal)
library(metR)
library(raster)
setwd("~/Desktop/BioD/")
dir.create("Data/Chirps/csv",recursive = T,showWarnings = F)
dir.create("Data/Chirps/netcdf",recursive = T,showWarnings = F)

for (k in 1992:2022) {
  download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/(1%20Jan%20",k,")/(31%20Decc%20",k,")/RANGEEDGES/data.nc",sep=""),destfile =paste("Data/Chirps/netcdf/",k,".nc",sep = "" ))
  
}

#https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.daily/.est_prcp/?Set-Language=fr

rm(list = ls())

#k=2020

for (k in 1992:2022) {
  Data_NC<-nc_open(paste("Data/Chirps/netcdf/",k,".nc",sep=""))
  Data<-tidync(paste("Data/Chirps/netcdf/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  Date_All=sort(rep(Date,X*Y),decreasing = F)
  Data$T<-Date_All
  
  dir.create("Data/Chirps/csv/",recursive = T,showWarnings = F)
  rio::export(Data,paste("Data/Chirps/csv/",k,".csv",sep=""))  
}
