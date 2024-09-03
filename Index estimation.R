###### Packages ########
list.of.packages <- c(
  "foreach",  "doParallel",  "ncdf4", "PCICt", "ggplot2", "viridis", "readr", "dplyr", "tidyr", "tidync",
  "stats", "rasterVis", "raster", "sf", "tidyverse", "rgdal", "magrittr", "maps", "terra", "zoo","chron",
  "matrixTests", "parallel"
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

######### Directory #########
setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Data\\ERA5-Land")

######### Load files #########
year <- 2020
days <- 366

raw_tasmean <- nc_open(paste0("ERA5_Land_Daily_",year,"_tasmean.nc"))
raw_tasmax <- nc_open(paste0("ERA5_Land_Daily_",year,"_tasmax.nc"))
raw_dew <- nc_open(paste0("ERA5_Land_Daily_",year,"_dew.nc"))
raw_rh <- nc_open(paste0("ERA5_Land_Daily_",year,"_rh.nc"))
raw_pr <- nc_open(paste0("ERA5_Land_Daily_",year,"_prec.nc"))
raw_wind <- nc_open(paste0("ERA5_Land_Daily_",year,"_wind.nc"))
raw_vpd <- nc_open(paste0("ERA5_Land_Daily_",year,"_vpd.nc"))

######### Load information #########
lon_era5 <- ncvar_get(raw_tasmean, "lon")
lat_era5 <- ncvar_get(raw_tasmean, "lat")
time_era5 <- ncvar_get(raw_tasmean, "time")
tunits_era5 <- ncatt_get(raw_tasmean,"time","units")

tasmean_C <- ncvar_get(raw_tasmean, "tasmean")
tasmax_C <- ncvar_get(raw_tasmax, "tasmax")
dew_C <- ncvar_get(raw_dew, "dew")
rh <- ncvar_get(raw_rh, "Relhum")
pr <- ncvar_get(raw_pr, "prec")
wind <- ncvar_get(raw_wind, "wind")
vpd <- ncvar_get(raw_vpd, "vpd")

# unit conversion
tasmean_F <- tasmean_C*1.8+32 # Celcius -> Farhenheit
wind_mph <- wind*2.237  # meter/sec -> mile/hour

########## Fosberg Fire Weather Index #########

###### Calculate m ########

m <- array(NA,dim=c(length(lon_era5),length(lat_era5),days))

for (ii in seq(length(lon_era5))){
  for (jj in seq(length(lat_era5))){
    for (kk in seq(days)){
      if(is.na(rh[ii,jj,kk]) == FALSE){
        if (is.na(rh[ii,jj,kk]) > 50){
          m[ii,jj,kk] <- 21.0606+0.005565*(rh[ii,jj,kk]^2)-0.00035*rh[ii,jj,kk]*tasmean_F[ii,jj,kk]-0.483199*rh[ii,jj,kk]
        }
        if (is.na(rh[ii,jj,kk]) < 50 && is.na(rh[ii,jj,kk]) > 10){
          m[ii,jj,kk] <- 2.22749+0.160107*rh[ii,jj,kk]-0.01478*tasmean_F[ii,jj,kk]
        }
        if (is.na(rh[ii,jj,kk]) < 10){
          m[ii,jj,kk] <- 0.03229+0.281073*rh[ii,jj,kk]-0.000578*rh[ii,jj,kk]*tasmean_F[ii,jj,kk]
        }       
      }
    }
  }
}

a <- m/30
FFWI <- sqrt(1+wind_mph^2)*((1-2*a+1.5*(a^2)-0.5*(a^3))/0.3002)

########## Hot-Dry-Windy Index ##########

HDW <- wind*vpd

######### Fire Potential Index #########

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

# Maximum value
ndvi_max <- matrix(nrow=length(lon_era5),ncol=length(lat_era5))
ndvi_min <- matrix(nrow=length(lon_era5),ncol=length(lat_era5))

for (ii in seq(length(lon_era5))){
  for (jj in seq(length(lat_era5))){
    ndvi_max[ii,jj] <- max(ndvi_re[ii,jj,])
    ndvi_min[ii,jj] <- min(ndvi_re[ii,jj,])
  }
}

ndvi_absmax <- max(ndvi_max,na.rm = TRUE)

vc_max <- 0.25+0.5*(ndvi_max/ndvi_absmax)

rg_ndvi <- array(dim=c(length(lon_era5),length(lat_era5),length(ndvi_re[1,1,])))
vc <- array(dim=c(length(lon_era5),length(lat_era5),length(ndvi_re[1,1,])))

for (ii in seq(length(lon_era5))){
  for (jj in seq(length(lat_era5))){
    for (kk in seq(length(ndvi_re[1,1,]))){
      rg_ndvi[ii,jj,kk] <- (ndvi_re[ii,jj,kk]-ndvi_min[ii,jj])/(ndvi_max[ii,jj]-ndvi_min[ii,jj])
      vc[ii,jj,kk] <- vc_max[ii,jj]*rg_ndvi[ii,jj,kk]
    }
  }
}

# for each year
vc_2014 <- vc[,,1:365]
vc_2015 <- vc[,,366:730]
vc_2016 <- vc[,,731:1096] 
vc_2017 <- vc[,,1097:1461]
vc_2018 <- vc[,,1462:1826]
vc_2019 <- vc[,,1827:2191]
vc_2020 <- vc[,,2191:2556]
vc_2021 <- vc[,,2557:2921]
vc_2022 <- vc[,,2922:3286]
vc_2023 <- vc[,,3287:3651]
  
fmc <- 1.28*m

# year change!!!!!!
fpi <- 100*(1-fmc/100)*(1-vc_2020)

########## Modified Nestrov Index (MNI) ##########

d <- tasmax_C-dew_C

# KN computation
KN <- array(dim=c(length(lon_era5),length(lat_era5),days))

for (ii in seq(length(lon_era5))){
  for (jj in seq(length(lat_era5))){
    for (kk in seq(days-1)){
      if (is.na(pr[ii,jj,kk]) == FALSE){
        if (pr[ii,jj,kk] < 0.1){
          KN[ii,jj,kk] <- 1.0
        }
        if (pr[ii,jj,kk] > 0 && pr[ii,jj,kk] < 1.0){
          KN[ii,jj,kk] <- 0.8
        }
        if (pr[ii,jj,kk] > 1.0 && pr[ii,jj,kk] < 3.0){
          KN[ii,jj,kk] <- 0.6
        }
        if (pr[ii,jj,kk] > 3.0 && pr[ii,jj,kk] < 6.0){
          KN[ii,jj,kk] <- 0.4
        }
        if (pr[ii,jj,kk] > 6.0 && pr[ii,jj,kk] < 15.0){
          KN[ii,jj,kk] <- 0.2
        }
        if (pr[ii,jj,kk] > 15.0 && pr[ii,jj,kk] < 20.0){
          KN[ii,jj,kk] <- 0.1
        }
        if (pr[ii,jj,kk] > 20.0){
          KN[ii,jj,kk] <- 0.0
        }
      }
    }
  }
}

mni <- array(dim=c(length(lon_era5),length(lat_era5),days))

mni[,,1] <- tasmean_C[,,1]*d[,,1]*KN[,,1]

for (ii in seq(length(lon_era5))){
  for (jj in seq(length(lat_era5))){
    for (kk in seq(days-1)){
      mni[ii,jj,kk+1] <- (mni[ii,jj,kk]+tasmean_C[ii,jj,kk+1]*d[ii,jj,kk+1])*KN[ii,jj,kk+1]
    }
  }
}

######### NC output ###########
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Index")

ncfname_FFWI <- paste("FFWI_",year,".nc", sep="")
ncfname_HDWI <- paste("HDWI_",year,".nc", sep="")
ncfname_FPI <- paste("FPI_",year,".nc", sep="")
ncfname_MNI <- paste("MNI_",year,".nc", sep="")

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon_era5))
latdim <- ncdim_def("lat","degrees_north",as.double(lat_era5))
timedim <- ncdim_def("time","day",as.double(seq(days)))

# define variables
fillvalue <- NA

var_FFWI_def <- ncvar_def("FFWI","",list(londim,latdim,timedim),fillvalue,prec="double")
var_HDWI_def <- ncvar_def("HDWI","",list(londim,latdim,timedim),fillvalue,prec="double")
var_FPI_def <- ncvar_def("FPI","",list(londim,latdim,timedim),fillvalue,prec="double")
var_MNI_def <- ncvar_def("MNI","",list(londim,latdim,timedim),fillvalue,prec="double")

# create netCDF file and put arrays
ncout_FFWI <- nc_create(ncfname_FFWI,var_FFWI_def)
ncout_HDWI <- nc_create(ncfname_HDWI,var_HDWI_def)
ncout_FPI <- nc_create(ncfname_FPI,var_FPI_def)
ncout_MNI <- nc_create(ncfname_MNI,var_MNI_def)

ncvar_put(ncout_FFWI,var_FFWI_def,FFWI)
ncvar_put(ncout_HDWI,var_HDWI_def,HDW)
ncvar_put(ncout_FPI,var_FPI_def,fpi)
ncvar_put(ncout_MNI,var_MNI_def,mni)

nc_close(ncout_FFWI)
nc_close(ncout_HDWI)
nc_close(ncout_FPI)
nc_close(ncout_MNI)


