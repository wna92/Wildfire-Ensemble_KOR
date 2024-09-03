###### Packages ########
list.of.packages <- c(
  "foreach",  "doParallel",  "ncdf4", "PCICt", "ggplot2", "viridis", "readr", "dplyr", "tidyr", "tidync",
  "stats", "rasterVis", "raster", "sf", "tidyverse", "rgdal", "magrittr", "maps", "terra", "zoo","chron",
  "matrixTests", "parallel","RcppRoll","s2dv"
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


######### For each year ###########
year <- 2023  #2014-2023

######### For FWI dataset ###########
######### Directory #################
setwd(paste0("Z:\\Data\\FWI\\GEOS-5\\",year))

######### Load files #########
raw_FWI_all <- nc_open(paste0("FWI.GEOS-5.Daily_",year,".nc"))

######### Load information #########
lon <- ncvar_get(raw_FWI_all, "lon")
lat <- ncvar_get(raw_FWI_all, "lat")
time <- ncvar_get(raw_FWI_all, "time")
tunits <- ncatt_get(raw_FWI_all,"time","units")

FWI_all <- ncvar_get(raw_FWI_all, "GEOS-5_FWI")

####### Region for Korea #########
lat_str <- 365
lat_end <- 393
lon_str <- 974
lon_end <- 993

FWI_kor <- FWI_all[lon_str:lon_end,lat_str:lat_end,]

# NC output
ncfname <- paste("FWI.GEOS-5.Daily_Kor_",year,".nc", sep="")

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon[lon_str:lon_end]))
latdim <- ncdim_def("lat","degrees_north",as.double(lat[lat_str:lat_end]))
timedim <- ncdim_def("time","day",as.double(seq(length(time))))

# define variables
fillvalue <- NA

var_def <- ncvar_def("FWI","",list(londim,latdim,timedim),fillvalue,prec="double")

# create netCDF file and put arrays
ncout <- nc_create(ncfname,var_def)
ncvar_put(ncout,var_def,FWI_kor)

nc_close(ncout)

######### For ERA5-Land dataset ###########
######### Directory #########
setwd ("Z:\\Data\\ERA5-Land\\Gangwon\\Hourly")

######### For each year ###########
year <- 2023  #2014-2023
days <- 365

##############################
raw_temp <- nc_open(paste0("ERA5_Land_",year,"_2m_temperature.nc"))
raw_dew <- nc_open(paste0("ERA5_Land_",year,"_2m_dewpoint_temperature.nc"))
raw_windu <- nc_open(paste0("ERA5_Land_",year,"_10m_u_component_of_wind.nc"))
raw_windv <- nc_open(paste0("ERA5_Land_",year,"_10m_v_component_of_wind.nc"))
raw_evap <- nc_open(paste0("ERA5_Land_",year,"_total_evaporation.nc"))
raw_prec <- nc_open(paste0("ERA5_Land_",year,"_total_precipitation.nc"))

######### Load information #########
lon <- ncvar_get(raw_temp, "longitude")
lat <- ncvar_get(raw_temp, "latitude")
time <- ncvar_get(raw_temp, "time")
tunits <- ncatt_get(raw_temp,"time","units")

temp <- ncvar_get(raw_temp, "t2m")
dew <- ncvar_get(raw_dew, "d2m")
windu <- ncvar_get(raw_windu, "u10")
windv <- ncvar_get(raw_windv, "v10")
evap <- ncvar_get(raw_evap, "e")
prec <- ncvar_get(raw_prec, "tp")

# unit conversion
temp <- temp-273.15   # Celcius
dew <- dew-273.15   # Celcius
evap <- evap*1000   # mm
prec <- prec*1000   # mm

# Considering UTC+8 hours
temp <- temp[,,-c(1:8)]
dew <- dew[,,-c(1:8)]
windu <- windu[,,-c(1:8)]
windv <- windv[,,-c(1:8)]
evap <- evap[,,-c(1:8)]
prec <- prec[,,-c(1:8)]

# wind speed vector
wind <- sqrt(windu^2+windv^2)

### Hourly to Daily ####
temp_max <- array(dim=c(length(lon),length(lat),days))
temp_min <- array(dim=c(length(lon),length(lat),days))
dew_mean <- array(dim=c(length(lon),length(lat),days))
wind_mean <- array(dim=c(length(lon),length(lat),days))
evap_mean <- array(dim=c(length(lon),length(lat),days))
prec_mean <- array(dim=c(length(lon),length(lat),days))

for (ii in 1:length(lon)){
  for (jj in 1:length(lat)){
    temp_max[ii,jj,] <- rollapply(temp[ii,jj,], 24, max, align = "left", partial = TRUE)[seq(1,length(temp[1,1,]),24)]
    temp_min[ii,jj,] <- rollapply(temp[ii,jj,], 24, min, align = "left", partial = TRUE)[seq(1,length(temp[1,1,]),24)]
    dew_mean[ii,jj,] <- rollapply(dew[ii,jj,], 24, mean, align = "left", partial = TRUE)[seq(1,length(dew[1,1,]),24)]
    wind_mean[ii,jj,] <- rollapply(wind[ii,jj,], 24, max, align = "left", partial = TRUE)[seq(1,length(wind[1,1,]),24)]
    evap_mean[ii,jj,] <- rollapply(evap[ii,jj,], 24, max, align = "left", partial = TRUE)[seq(1,length(evap[1,1,]),24)]
    prec_mean[ii,jj,] <- rollapply(prec[ii,jj,], 24, max, align = "left", partial = TRUE)[seq(1,length(prec[1,1,]),24)]
  }
}

temp_mean <- (temp_max+temp_min)/2

# Relative humidity
Rh1 <- 100-5*(temp_mean-dew_mean)
up <- exp((17.625*dew_mean)/(243.04+dew_mean))
down <- exp((17.625*temp_mean)/(243.04+temp_mean))
Rh2 <- 100*(up/down)
vpd <- down-up

# NC output
setwd("Z:\\Data\\ERA5-Land\\Gangwon\\Daily")

ncfname_tasmax <- paste("ERA5_Land_Daily_",year,"_tasmax.nc", sep="")
ncfname_tasmin <- paste("ERA5_Land_Daily_",year,"_tasmin.nc", sep="")
ncfname_tasmean <- paste("ERA5_Land_Daily_",year,"_tasmean.nc", sep="")
ncfname_dew <- paste("ERA5_Land_Daily_",year,"_dew.nc", sep="")
ncfname_wind <- paste("ERA5_Land_Daily_",year,"_wind.nc", sep="")
ncfname_prec <- paste("ERA5_Land_Daily_",year,"_prec.nc", sep="")
ncfname_Rh <- paste("ERA5_Land_Daily_",year,"_rh.nc", sep="")
ncfname_vpd <- paste("ERA5_Land_Daily_",year,"_vpd.nc", sep="")

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon))
latdim <- ncdim_def("lat","degrees_north",as.double(lat))
timedim <- ncdim_def("time","day",as.double(seq(days)))

# define variables
fillvalue <- NA

var_tasmax_def <- ncvar_def("tasmax","",list(londim,latdim,timedim),fillvalue,prec="double")
var_tasmin_def <- ncvar_def("tasmin","",list(londim,latdim,timedim),fillvalue,prec="double")
var_tasmean_def <- ncvar_def("tasmean","",list(londim,latdim,timedim),fillvalue,prec="double")
var_dew_def <- ncvar_def("dew","",list(londim,latdim,timedim),fillvalue,prec="double")
var_wind_def <- ncvar_def("wind","",list(londim,latdim,timedim),fillvalue,prec="double")
var_prec_def <- ncvar_def("prec","",list(londim,latdim,timedim),fillvalue,prec="double")
var_Rh_def <- ncvar_def("Relhum","",list(londim,latdim,timedim),fillvalue,prec="double")
var_vpd_def <- ncvar_def("vpd","",list(londim,latdim,timedim),fillvalue,prec="double")

# create netCDF file and put arrays
ncout_tasmax <- nc_create(ncfname_tasmax,var_tasmax_def)
ncout_tasmin <- nc_create(ncfname_tasmin,var_tasmin_def)
ncout_tasmean <- nc_create(ncfname_tasmean,var_tasmean_def)
ncout_dew <- nc_create(ncfname_dew,var_dew_def)
ncout_wind <- nc_create(ncfname_wind,var_wind_def)
ncout_prec <- nc_create(ncfname_prec,var_prec_def)
ncout_Rh <- nc_create(ncfname_Rh,var_Rh_def)
ncout_vpd <- nc_create(ncfname_vpd,var_vpd_def)

ncvar_put(ncout_tasmax,var_tasmax_def,temp_max)
ncvar_put(ncout_tasmin,var_tasmin_def,temp_min)
ncvar_put(ncout_tasmean,var_tasmean_def,temp_mean)
ncvar_put(ncout_dew,var_dew_def,dew_mean)
ncvar_put(ncout_wind,var_wind_def,wind_mean)
ncvar_put(ncout_prec,var_prec_def,prec_mean)
ncvar_put(ncout_Rh,var_Rh_def,Rh2)
ncvar_put(ncout_vpd,var_vpd_def,vpd)

nc_close(ncout_tasmax)
nc_close(ncout_tasmin)
nc_close(ncout_tasmean)
nc_close(ncout_dew)
nc_close(ncout_wind)
nc_close(ncout_prec)
nc_close(ncout_Rh)
nc_close(ncout_vpd)




