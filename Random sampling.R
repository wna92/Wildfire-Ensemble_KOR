###### Packages ########
list.of.packages <- c(
  "foreach",  "doParallel",  "ncdf4", "PCICt", "ggplot2", "viridis", "readr", "dplyr", "tidyr", "tidync",
  "stats", "rasterVis", "raster", "sf", "tidyverse", "rgdal", "magrittr", "maps", "terra", "zoo","chron",
  "matrixTests", "parallel","RcppRoll","s2dv", "readxl","writexl","psych"
)

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}


############ Random sampling * 1000 times #######
rand_n <- 30

##### NDVI ########
setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Data\\NDVI")

raw_ndvi <- nc_open("MOD13Q1.061_250m_2014-2023.nc")

ndvi <- ncvar_get(raw_ndvi, "_250m_16_days_NDVI")
ndvi <- ndvi[,,3:231]

lon_ndvi <- ncvar_get(raw_ndvi, "lon")
lat_ndvi <- ncvar_get(raw_ndvi, "lat")
time_ndvi <- ncvar_get(raw_ndvi, "time")
tunits_ndvi <- ncatt_get(raw_ndvi,"time","units")

ndvi_daily <- array(dim=c(38,29,3664))

# Regrid & daily
for (ii in seq(38)){
  for (jj in seq(29)){
    ndvi_daily[ii,jj,] <- rep(ndvi[ii,jj,],each = 16)
  }
}

ndvi_re <- ndvi_daily[c(9,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32,33,34),c(4,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25),]

ndvi_2014 <- ndvi_re[,,1:365]
ndvi_2015 <- ndvi_re[,,366:730]
ndvi_2016 <- ndvi_re[,,731:1096]
ndvi_2017 <- ndvi_re[,,1097:1461]
ndvi_2018 <- ndvi_re[,,1462:1826]
ndvi_2019 <- ndvi_re[,,1827:2191]
ndvi_2020 <- ndvi_re[,,2192:2557]
ndvi_2021 <- ndvi_re[,,2558:2922]
ndvi_2022 <- ndvi_re[,,2923:3287]
ndvi_2023 <- ndvi_re[,,3288:3652]

######### Load climate variables & index #########
year <- 2023
year.ant <- year-1
days <- 365

######### setting #############
setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Data\\ERA5-Land")
raw_tasmean <- nc_open(paste0("ERA5_Land_Daily_",year,"_tasmean.nc"))
raw_tasmax <- nc_open(paste0("ERA5_Land_Daily_",year,"_tasmax.nc"))
raw_dew <- nc_open(paste0("ERA5_Land_Daily_",year,"_dew.nc"))
raw_rh <- nc_open(paste0("ERA5_Land_Daily_",year,"_rh.nc"))
raw_pr <- nc_open(paste0("ERA5_Land_Daily_",year,"_prec.nc"))
raw_wind <- nc_open(paste0("ERA5_Land_Daily_",year,"_wind.nc"))
raw_vpd <- nc_open(paste0("ERA5_Land_Daily_",year,"_vpd.nc"))
raw_soil <- nc_open(paste0("ERA5_Land_Daily_",year,"_soil.nc"))
raw_pr.ant <- nc_open(paste0("ERA5_Land_Daily_",year.ant,"_prec.nc"))
raw_press <- nc_open(paste0("ERA5_Land_Daily_",year,"_press.nc"))

lon_era5 <- ncvar_get(raw_tasmean, "lon")
lat_era5 <- ncvar_get(raw_tasmean, "lat")
time_era5 <- ncvar_get(raw_tasmean, "time")
tunits_era5 <- ncatt_get(raw_tasmean,"time","units")

tasmean_C <- ncvar_get(raw_tasmean, "tasmean")
tasmax_C <- ncvar_get(raw_tasmax, "tasmax")
dew_C <- ncvar_get(raw_dew, "dew")
rh <- ncvar_get(raw_rh, "Relhum")
pr <- ncvar_get(raw_pr, "prec")
wind <- ncvar_get(raw_wind, "wind")  # meter/sec
vpd <- ncvar_get(raw_vpd, "vpd")
soil <- ncvar_get(raw_soil, "soil")
pr.ant <- ncvar_get(raw_pr.ant, "prec")
press <- ncvar_get(raw_press, "press")

setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Index")
raw_FFWI <- nc_open(paste0("FFWI_",year,".nc"))
raw_FPI <- nc_open(paste0("FPI_",year,".nc"))
raw_FWI <- nc_open(paste0("FWI.GEOS-5.Daily_Kor_",year,".nc"))
raw_HDWI <- nc_open(paste0("HDWI_",year,".nc"))
raw_MNI <- nc_open(paste0("MNI_",year,".nc"))

FFWI <- ncvar_get(raw_FFWI, "FFWI")
FPI <- ncvar_get(raw_FPI, "FPI")
FWI <- ncvar_get(raw_FWI, "FWI")
HDWI <- ncvar_get(raw_HDWI, "HDWI")
MNI <- ncvar_get(raw_MNI, "MNI")

lon_FWI <- ncvar_get(raw_FWI, "lon")
lat_FWI <- ncvar_get(raw_FWI, "lat")

####### Load inventory ###########
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis")
Inven <- read_excel("Wildfires_2014-2023_All.xlsx")

Inven_yr <- subset(Inven,Inven$Year == year)
Inven_yr <- Inven_yr[,-c(5:13)]

Inven_yr$Latitude <- round(Inven_yr$Latitude,1)
Inven_yr$Longitude <- round(Inven_yr$Longitude,1)

# Generate date order considering # of dates for each month
Inven_yr$date_order <- NA

for (ii in seq(nrow(Inven_yr))){
  if (Inven_yr$Month[ii] == 1){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]
  }
  if (Inven_yr$Month[ii] == 2){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+31
  }
  if (Inven_yr$Month[ii] == 3){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28)
  }
  if (Inven_yr$Month[ii] == 4){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31)
  }
  if (Inven_yr$Month[ii] == 5){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30)
  }
  if (Inven_yr$Month[ii] == 6){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31)
  }
  if (Inven_yr$Month[ii] == 7){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30)
  }
  if (Inven_yr$Month[ii] == 8){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30+31)
  }
  if (Inven_yr$Month[ii] == 9){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30+31+31)
  }
  if (Inven_yr$Month[ii] == 10){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30+31+31+30)
  }
  if (Inven_yr$Month[ii] == 11){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30+31+31+30+31)
  }
  if (Inven_yr$Month[ii] == 12){
    Inven_yr$date_order[ii] <- Inven_yr$Day[ii]+(31+28+31+30+31+30+31+31+30+31+30)
  }
}

# Generate lon/lat order considering # of dates for each month
Inven_yr$lat_order <- NA
Inven_yr$lon_order <- NA
Inven_yr$lon_order_FWI <- NA
Inven_yr$lat_order_FWI <- NA

for (ii in seq(nrow(Inven_yr))){
  
  Inven_yr$lon_order[ii] <- 10*(round(Inven_yr$Longitude[ii],digits = 1)-127.5)+1
  Inven_yr$lat_order[ii] <- 10*(38.5-round(Inven_yr$Latitude[ii],digits = 1))+1
  
  Inven_yr$lon_order_FWI[ii] <- order(abs(lon_FWI-Inven_yr$Longitude[ii]))[1]
  Inven_yr$lat_order_FWI[ii] <- order(abs(lat_FWI-Inven_yr$Latitude[ii]))[1]
  
}


######### Random sampling implementation ###########
for (jj in seq(nrow(Inven_yr))){
  
  seq_lon <- subset(seq(lon_era5), seq(lon_era5) > Inven_yr$lon_order[jj]+1.5 | seq(lon_era5) < Inven_yr$lon_order[jj]-1.5)
  seq_lat <- subset(seq(lat_era5), seq(lat_era5) > Inven_yr$lat_order[jj]+1.5 | seq(lat_era5) < Inven_yr$lat_order[jj]-1.5)
  
  Random <- Inven_yr[jj,c(1:4,13)]
  Random[1:rand_n,] <- Random
  
  Random$lon_order <- sample(seq_lon,rand_n,replace = T)
  Random$lat_order <- sample(seq_lat,rand_n,replace = T)
  
  Random$lon <- (Random$lon_order-1)/10+127.5
  Random$lat <- 38.5-(Random$lat_order-1)/10
  
  Random$lon_FWI <- NA
  Random$lon_FWI <- NA
  
  for (ii in seq(nrow(Random))){
    Random$lon_FWI[ii] <- order(abs(lon_FWI-Random$lon[ii]))[1]
    Random$lat_FWI[ii] <- order(abs(lat_FWI-Random$lat[ii]))[1]
  }
  
  Random$FWI <- NA
  Random$FFWI <- NA
  Random$FPI <- NA
  Random$HDWI <- NA
  Random$MNI <- NA
  
  for (ii in seq(nrow(Random))){
    Random$FWI[ii] <- FWI[Random$lon_FWI[ii],Random$lat_FWI[ii],Random$date_order[ii]]
    Random$FFWI[ii] <- FFWI[Random$lon_order[ii],Random$lat_order[ii],Random$date_order[ii]]
    Random$FPI[ii] <- FPI[Random$lon_order[ii],Random$lat_order[ii],Random$date_order[ii]]
    Random$HDWI[ii] <- HDWI[Random$lon_order[ii],Random$lat_order[ii],Random$date_order[ii]]
    Random$MNI[ii] <- MNI[Random$lon_order[ii],Random$lat_order[ii],Random$date_order[ii]]
  }
  
  
  setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Nofire")
  
  write_xlsx(Random,paste0("Nofire_",year,"_",jj,".xlsx"))
  
}
