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


############# Climate factors at wildfire spots ###############
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

######### Load climate variables #########
setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Data\\ERA5-Land")
year <- 2014
year.ant <- year-1
days <- 365

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

for (ii in seq(nrow(Inven_yr))){
  
  Inven_yr$lon_order[ii] <- 10*(round(Inven_yr$Longitude[ii],digits = 1)-127.5)+1
  Inven_yr$lat_order[ii] <- 10*(38.5-round(Inven_yr$Latitude[ii],digits = 1))+1
  
}

######## Add variables ##########
Inven_yr$tasmean <- NA
Inven_yr$tasmax <- NA
Inven_yr$dew <- NA
Inven_yr$rh <- NA
Inven_yr$pr <- NA
Inven_yr$pr.ant <- NA
Inven_yr$press <- NA
Inven_yr$soil <- NA
Inven_yr$wind <- NA
Inven_yr$vpd <- NA
Inven_yr$ndvi <- NA
Inven_yr$tasmean.5day <- NA
Inven_yr$tasmax.5day <- NA
Inven_yr$dew.5day <- NA
Inven_yr$rh.5day <- NA
Inven_yr$pr.5day <- NA
Inven_yr$press.5day <- NA
Inven_yr$soil.5day <- NA
Inven_yr$wind.5day <- NA
Inven_yr$vpd.5day <- NA
Inven_yr$ndvi.5day <- NA
Inven_yr$tasmean.10day <- NA
Inven_yr$tasmax.10day <- NA
Inven_yr$dew.10day <- NA
Inven_yr$rh.10day <- NA
Inven_yr$pr.10day <- NA
Inven_yr$press.10day <- NA
Inven_yr$soil.10day <- NA
Inven_yr$wind.10day <- NA
Inven_yr$vpd.10day <- NA
Inven_yr$ndvi.10day <- NA
Inven_yr$tasmean.15day <- NA
Inven_yr$tasmax.15day <- NA
Inven_yr$dew.15day <- NA
Inven_yr$rh.15day <- NA
Inven_yr$pr.15day <- NA
Inven_yr$press.15day <- NA
Inven_yr$soil.15day <- NA
Inven_yr$wind.15day <- NA
Inven_yr$vpd.15day <- NA
Inven_yr$ndvi.15day <- NA

for (ii in seq(nrow(Inven_yr))){
  Inven_yr$tasmean[ii] <- tasmean_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$tasmean.5day[ii] <- mean(tasmean_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$tasmean.10day[ii] <- mean(tasmean_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$tasmean.15day[ii] <- mean(tasmean_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$tasmax[ii] <- tasmax_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$tasmax.5day[ii] <- mean(tasmax_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$tasmax.10day[ii] <- mean(tasmax_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$tasmax.15day[ii] <- mean(tasmax_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$dew[ii] <- dew_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$dew.5day[ii] <- mean(dew_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$dew.10day[ii] <- mean(dew_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$dew.15day[ii] <- mean(dew_C[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$rh[ii] <- rh[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$rh.5day[ii] <- mean(rh[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$rh.10day[ii] <- mean(rh[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$rh.15day[ii] <- mean(rh[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$pr[ii] <- pr[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$pr.5day[ii] <- sum(pr[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$pr.10day[ii] <- sum(pr[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$pr.15day[ii] <- sum(pr[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  Inven_yr$pr.ant[ii] <- sum(pr.ant[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],])
  
  Inven_yr$wind[ii] <- wind[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$wind.5day[ii] <- mean(wind[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$wind.10day[ii] <- mean(wind[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$wind.15day[ii] <- mean(wind[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$vpd[ii] <- vpd[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$vpd.5day[ii] <- mean(vpd[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$vpd.10day[ii] <- mean(vpd[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$vpd.15day[ii] <- mean(vpd[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$soil[ii] <- soil[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$soil.5day[ii] <- mean(soil[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$soil.10day[ii] <- mean(soil[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$soil.15day[ii] <- mean(soil[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  Inven_yr$press[ii] <- press[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$press.5day[ii] <- mean(press[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$press.10day[ii] <- mean(press[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$press.15day[ii] <- mean(press[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
  
  }

# For each year
for (ii in seq(nrow(Inven_yr))){
  Inven_yr$ndvi[ii] <- ndvi_2023[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],Inven_yr$date_order[ii]]
  Inven_yr$ndvi.5day[ii] <- mean(ndvi_2023[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-4):Inven_yr$date_order[ii]])
  Inven_yr$ndvi.10day[ii] <- mean(ndvi_2023[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-9):Inven_yr$date_order[ii]])
  Inven_yr$ndvi.15day[ii] <- mean(ndvi_2023[Inven_yr$lon_order[ii],Inven_yr$lat_order[ii],(Inven_yr$date_order[ii]-14):Inven_yr$date_order[ii]])
}


######### result combine #################
result_2014 <- Inven_yr
result_2015 <- Inven_yr
result_2016 <- Inven_yr
result_2017 <- Inven_yr
result_2018 <- Inven_yr
result_2019 <- Inven_yr
result_2020 <- Inven_yr
result_2021 <- Inven_yr
result_2022 <- Inven_yr
result_2023 <- Inven_yr

result_all <- rbind(result_2014,result_2015,result_2016,result_2017,result_2018,
                    result_2019,result_2020,result_2021,result_2022,result_2023)

######### Output export #######
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis")

write_xlsx(result_all,"Clim_2014-2023.xlsx")



