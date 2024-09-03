###### Packages ########
list.of.packages <- c(
  "foreach",  "doParallel",  "ncdf4", "viridis", "readr", "dplyr", "tidyr", "tidync", "MASS",
  "stats", "rasterVis", "raster", "sf", "tidyverse", "rgdal", "magrittr", "maps", "terra", "zoo","chron",
  "matrixTests", "RcppRoll","s2dv", "readxl","writexl","psych","abind","akima","fitdistrplus",
  "flexsurv", "mixtools","distr", "KScorrect","spatstat"
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

######## Load files #############
setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Ensemble")

raw_ens_forecast <- nc_open("Ensemble_forecast.nc")
raw_ens_train <- nc_open("Ensemble_training.nc")

lon <- ncvar_get(raw_ens_train, "lon")
lat <- ncvar_get(raw_ens_train, "lat")
time.train <- ncvar_get(raw_ens_train, "time")
tunits.train <- ncatt_get(raw_ens_train,"time","units")
time.forecast <- ncvar_get(raw_ens_forecast, "time")
tunits.forecast <- ncatt_get(raw_ens_forecast,"time","units")

FFWI.train <- ncvar_get(raw_ens_train, "FFWI")
FFWI.norm.train <- ncvar_get(raw_ens_train, "FFWI.norm")
FPI.train <- ncvar_get(raw_ens_train, "FPI")
FPI.norm.train <- ncvar_get(raw_ens_train, "FPI.norm")
FWI.train <- ncvar_get(raw_ens_train, "FWI")
FWI.norm.train <- ncvar_get(raw_ens_train, "FWI.norm")
HDWI.train <- ncvar_get(raw_ens_train, "HDWI")
HDWI.norm.train <- ncvar_get(raw_ens_train, "HDWI.norm")
MNI.train <- ncvar_get(raw_ens_train, "MNI")
MNI.norm.train <- ncvar_get(raw_ens_train, "MNI.norm")
Index_VC.All.train <- ncvar_get(raw_ens_train, "Index_VC.All")
Index_VC.Clim.train <- ncvar_get(raw_ens_train, "Index_VC.Clim")
Index_VC.noFWI.train <- ncvar_get(raw_ens_train, "Index_VC.noFWI")
Index_TC.FFWI.train <- ncvar_get(raw_ens_train, "Index_TC.FFWI")
Index_TC.HDWI.train <- ncvar_get(raw_ens_train, "Index_TC.HDWI")
Index_TC.MNI.train <- ncvar_get(raw_ens_train, "Index_TC.MNI")
Index_TC.FFWI.re.train <- ncvar_get(raw_ens_train, "Index_TC.rescale.FFWI")
Index_TC.HDWI.re.train <- ncvar_get(raw_ens_train, "Index_TC.rescale.HDWI")
Index_TC.MNI.re.train <- ncvar_get(raw_ens_train, "Index_TC.rescale.MNI")

FFWI.forecast <- ncvar_get(raw_ens_forecast, "FFWI")
FFWI.norm.forecast <- ncvar_get(raw_ens_forecast, "FFWI.norm")
FPI.forecast <- ncvar_get(raw_ens_forecast, "FPI")
FPI.norm.forecast <- ncvar_get(raw_ens_forecast, "FPI.norm")
FWI.forecast <- ncvar_get(raw_ens_forecast, "FWI")
FWI.norm.forecast <- ncvar_get(raw_ens_forecast, "FWI.norm")
HDWI.forecast <- ncvar_get(raw_ens_forecast, "HDWI")
HDWI.norm.forecast <- ncvar_get(raw_ens_forecast, "HDWI.norm")
MNI.forecast <- ncvar_get(raw_ens_forecast, "MNI")
MNI.norm.forecast <- ncvar_get(raw_ens_forecast, "MNI.norm")
Index_VC.All.forecast <- ncvar_get(raw_ens_forecast, "Index_VC.All")
Index_VC.Clim.forecast <- ncvar_get(raw_ens_forecast, "Index_VC.Clim")
Index_VC.noFWI.forecast <- ncvar_get(raw_ens_forecast, "Index_VC.noFWI")
Index_TC.FFWI.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.FFWI")
Index_TC.HDWI.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.HDWI")
Index_TC.MNI.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.MNI")
Index_TC.FFWI.re.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.rescale.FFWI")
Index_TC.HDWI.re.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.rescale.HDWI")
Index_TC.MNI.re.forecast <- ncvar_get(raw_ens_forecast, "Index_TC.rescale.MNI")

############# Simple averaging ################
Index_Simple.forecast <- (FFWI.norm.forecast+FPI.norm.forecast+FWI.norm.forecast+HDWI.norm.forecast+MNI.norm.forecast)/5
Index_Simple.train <- (FFWI.norm.train+FPI.norm.train+FWI.norm.train+HDWI.norm.train+MNI.norm.train)/5

############# Inventory ##################
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis")
Inven <- read_excel("Wildfires_2014-2023_All.xlsx")

Inven_out <- matrix(NA,nrow=1,ncol=20)
colnames(Inven_out) <-c("Year", "Month", "Day", "Time", "진화종료시간_년", "진화종료시간_월", "진화종료시간_일",
                        "진화종료시간_시간", "발생장소_관서", "발생장소_시도", "발생장소_시군구", "발생장소_읍면",
                        "발생장소_동리", "Latitude", "Longitude", "Area", "index", "date_order", "lat_order", "lon_order")
Inven_train <- Inven_out[-c(1), ]
Inven_forecast <- Inven_out[-c(1), ]

# For training period
for (year in 2014:2020){
  
  Inven_sub <- subset(Inven,Inven$Year == year)
  Inven_sub$Month <- as.numeric(Inven_sub$Month)
  Inven_sub$Day <- as.numeric(Inven_sub$Day)
  
  # Generate date order considering # of dates for each month
  Inven_sub$date_order <- NA
  
  for (ii in seq(nrow(Inven_sub))){
    if (Inven_sub$Month[ii] == 1){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]
    }
    if (Inven_sub$Month[ii] == 2){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+31
    }
    if (Inven_sub$Month[ii] == 3){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28)
    }
    if (Inven_sub$Month[ii] == 4){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31)
    }
    if (Inven_sub$Month[ii] == 5){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30)
    }
    if (Inven_sub$Month[ii] == 6){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31)
    }
    if (Inven_sub$Month[ii] == 7){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30)
    }
    if (Inven_sub$Month[ii] == 8){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31)
    }
    if (Inven_sub$Month[ii] == 9){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31)
    }
    if (Inven_sub$Month[ii] == 10){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30)
    }
    if (Inven_sub$Month[ii] == 11){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30+31)
    }
    if (Inven_sub$Month[ii] == 12){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30+31+30)
    }
  }
  
  # Generate lon/lat order considering # of dates for each month
  Inven_sub$lat_order <- NA
  Inven_sub$lon_order <- NA
  
  # for (ii in seq(nrow(Inven_sub))){
  #   
  #   Inven_sub$lon_order[ii] <- 10*(round(Inven_sub$Longitude[ii],digits = 1)-127.5)+1
  #   Inven_sub$lat_order[ii] <- 10*(38.5-round(Inven_sub$Latitude[ii],digits = 1))+1
  #   
  # }
  
  for (ii in seq(nrow(Inven_sub))){
    
    Inven_sub$lon_order[ii] <- order(abs(lon-Inven_sub$Longitude[ii]))[1]
    Inven_sub$lat_order[ii] <- order(abs(lat-Inven_sub$Latitude[ii]))[1]
    
  }
  
  Inven_train <- rbind(Inven_train,Inven_sub)
  
}

# For forecasting period
for (year in 2021:2023){
  
  Inven_sub <- subset(Inven,Inven$Year == year)
  Inven_sub$Month <- as.numeric(Inven_sub$Month)
  Inven_sub$Day <- as.numeric(Inven_sub$Day)
  
  # Generate date order considering # of dates for each month
  Inven_sub$date_order <- NA
  
  for (ii in seq(nrow(Inven_sub))){
    if (Inven_sub$Month[ii] == 1){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]
    }
    if (Inven_sub$Month[ii] == 2){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+31
    }
    if (Inven_sub$Month[ii] == 3){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28)
    }
    if (Inven_sub$Month[ii] == 4){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31)
    }
    if (Inven_sub$Month[ii] == 5){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30)
    }
    if (Inven_sub$Month[ii] == 6){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31)
    }
    if (Inven_sub$Month[ii] == 7){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30)
    }
    if (Inven_sub$Month[ii] == 8){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31)
    }
    if (Inven_sub$Month[ii] == 9){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31)
    }
    if (Inven_sub$Month[ii] == 10){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30)
    }
    if (Inven_sub$Month[ii] == 11){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30+31)
    }
    if (Inven_sub$Month[ii] == 12){
      Inven_sub$date_order[ii] <- Inven_sub$Day[ii]+(31+28+31+30+31+30+31+31+30+31+30)
    }
  }
  
  # Generate lon/lat order considering # of dates for each month
  Inven_sub$lat_order <- NA
  Inven_sub$lon_order <- NA
  
  # for (ii in seq(nrow(Inven_sub))){
  #   
  #   Inven_sub$lon_order[ii] <- 10*(round(Inven_sub$Longitude[ii],digits = 1)-127.5)+1
  #   Inven_sub$lat_order[ii] <- 10*(38.5-round(Inven_sub$Latitude[ii],digits = 1))+1
  #   
  # }
  
  for (ii in seq(nrow(Inven_sub))){
    
    Inven_sub$lon_order[ii] <- order(abs(lon-Inven_sub$Longitude[ii]))[1]
    Inven_sub$lat_order[ii] <- order(abs(lat-Inven_sub$Latitude[ii]))[1]
    
  }
  
  Inven_forecast <- rbind(Inven_forecast,Inven_sub)
  
}

# date ordering
Inven_train$date_order[Inven_train$Year == 2014] <- Inven_train$date_order[Inven_train$Year == 2014]
Inven_train$date_order[Inven_train$Year == 2015] <- Inven_train$date_order[Inven_train$Year == 2015] + 365
Inven_train$date_order[Inven_train$Year == 2016] <- Inven_train$date_order[Inven_train$Year == 2016] + 365 + 365
Inven_train$date_order[Inven_train$Year == 2017] <- Inven_train$date_order[Inven_train$Year == 2017] + 365 + 365 + 366
Inven_train$date_order[Inven_train$Year == 2018] <- Inven_train$date_order[Inven_train$Year == 2018] + 365 + 365 + 366 + 365
Inven_train$date_order[Inven_train$Year == 2019] <- Inven_train$date_order[Inven_train$Year == 2019] + 365 + 365 + 366 + 365 +365
Inven_train$date_order[Inven_train$Year == 2020] <- Inven_train$date_order[Inven_train$Year == 2020] + 365 + 365 + 366 + 365 +365 +365


Inven_forecast$date_order[Inven_forecast$Year == 2021] <- Inven_forecast$date_order[Inven_forecast$Year == 2021]
Inven_forecast$date_order[Inven_forecast$Year == 2022] <- Inven_forecast$date_order[Inven_forecast$Year == 2022] + 365
Inven_forecast$date_order[Inven_forecast$Year == 2023] <- Inven_forecast$date_order[Inven_forecast$Year == 2023] + 365 + 365

# setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Result")
# 
# write_xlsx(Inven_train,"Inven_training.xlsx")
# write_xlsx(Inven_forecast,"Inven_forecasting.xlsx")

############ Cap area ###############
area_lim <- 50

Inven_train <- subset(Inven_train,Inven_train$Area > area_lim)
Inven_forecast <- subset(Inven_forecast,Inven_forecast$Area > area_lim)

############## Threshold at each grid #################
# For thresholds: 0.9, 0.95, 0.98
prob=0.98

thr_FFWI.norm.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_FPI.norm.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_FWI.norm.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_HDWI.norm.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_MNI.norm.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_Simple.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.All.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.Clim.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.noFWI.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.FFWI.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.HDWI.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.MNI.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.FFWI.re.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.HDWI.re.train <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.MNI.re.train <- matrix(nrow=length(lon),ncol=length(lat))

thr_FFWI.norm.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_FPI.norm.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_FWI.norm.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_HDWI.norm.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_MNI.norm.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_Simple.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.All.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.Clim.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_VC.noFWI.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.FFWI.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.HDWI.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.MNI.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.FFWI.re.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.HDWI.re.forecast <- matrix(nrow=length(lon),ncol=length(lat))
thr_Index_TC.MNI.re.forecast <- matrix(nrow=length(lon),ncol=length(lat))

for (ii in seq(length(lon))){
  for (jj in seq(length(lat))){
    
    thr_FFWI.norm.train[ii,jj] <- as.numeric(quantile(FFWI.norm.train[ii,jj,],prob,na.rm = TRUE))
    thr_FPI.norm.train[ii,jj] <- as.numeric(quantile(FPI.norm.train[ii,jj,],prob,na.rm = TRUE))
    thr_FWI.norm.train[ii,jj] <- as.numeric(quantile(FWI.norm.train[ii,jj,],prob,na.rm = TRUE))
    thr_HDWI.norm.train[ii,jj] <- as.numeric(quantile(HDWI.norm.train[ii,jj,],prob,na.rm = TRUE))
    thr_MNI.norm.train[ii,jj] <- as.numeric(quantile(MNI.norm.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_Simple.train[ii,jj] <- as.numeric(quantile(Index_Simple.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.All.train[ii,jj] <- as.numeric(quantile(Index_VC.All.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.Clim.train[ii,jj] <- as.numeric(quantile(Index_VC.Clim.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.noFWI.train[ii,jj] <- as.numeric(quantile(Index_VC.noFWI.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.FFWI.train[ii,jj] <- as.numeric(quantile(Index_TC.FFWI.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.HDWI.train[ii,jj] <- as.numeric(quantile(Index_TC.HDWI.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.MNI.train[ii,jj] <- as.numeric(quantile(Index_TC.MNI.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.FFWI.re.train[ii,jj] <- as.numeric(quantile(Index_TC.FFWI.re.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.HDWI.re.train[ii,jj] <- as.numeric(quantile(Index_TC.HDWI.re.train[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.MNI.re.train[ii,jj] <- as.numeric(quantile(Index_TC.MNI.re.train[ii,jj,],prob,na.rm = TRUE))
    
    thr_FFWI.norm.forecast[ii,jj] <- as.numeric(quantile(FFWI.norm.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_FPI.norm.forecast[ii,jj] <- as.numeric(quantile(FPI.norm.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_FWI.norm.forecast[ii,jj] <- as.numeric(quantile(FWI.norm.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_HDWI.norm.forecast[ii,jj] <- as.numeric(quantile(HDWI.norm.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_MNI.norm.forecast[ii,jj] <- as.numeric(quantile(MNI.norm.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_Simple.forecast[ii,jj] <- as.numeric(quantile(Index_Simple.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.All.forecast[ii,jj] <- as.numeric(quantile(Index_VC.All.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.Clim.forecast[ii,jj] <- as.numeric(quantile(Index_VC.Clim.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_VC.noFWI.forecast[ii,jj] <- as.numeric(quantile(Index_VC.noFWI.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.FFWI.forecast[ii,jj] <- as.numeric(quantile(Index_TC.FFWI.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.HDWI.forecast[ii,jj] <- as.numeric(quantile(Index_TC.HDWI.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.MNI.forecast[ii,jj] <- as.numeric(quantile(Index_TC.MNI.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.FFWI.re.forecast[ii,jj] <- as.numeric(quantile(Index_TC.FFWI.re.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.HDWI.re.forecast[ii,jj] <- as.numeric(quantile(Index_TC.HDWI.re.forecast[ii,jj,],prob,na.rm = TRUE))
    thr_Index_TC.MNI.re.forecast[ii,jj] <- as.numeric(quantile(Index_TC.MNI.re.forecast[ii,jj,],prob,na.rm = TRUE))
    
  }
}

# For specific grid
# thr_FFWI.norm.forecast[ii,jj]

################### Wildfire case #############
Check.FFWI.norm.train <- list(Index = FFWI.norm.train, Detect = array(0,dim=dim(FFWI.norm.train)))
Check.FPI.norm.train <- list(Index = FPI.norm.train, Detect = array(0,dim=dim(FPI.norm.train)))
Check.FWI.norm.train <- list(Index = FWI.norm.train, Detect = array(0,dim=dim(FWI.norm.train)))
Check.HDWI.norm.train <- list(Index = HDWI.norm.train, Detect = array(0,dim=dim(HDWI.norm.train)))
Check.MNI.norm.train <- list(Index = MNI.norm.train, Detect = array(0,dim=dim(MNI.norm.train)))
Check.Index_Simple.train <- list(Index = Index_Simple.train, Detect = array(0,dim=dim(Index_Simple.train)))
Check.Index_VC.All.train <- list(Index = Index_VC.All.train, Detect = array(0,dim=dim(Index_VC.All.train)))
Check.Index_VC.Clim.train <- list(Index = Index_VC.Clim.train, Detect = array(0,dim=dim(Index_VC.Clim.train)))
Check.Index_VC.noFWI.train <- list(Index = Index_VC.noFWI.train, Detect = array(0,dim=dim(Index_VC.noFWI.train)))
Check.Index_TC.FFWI.train <- list(Index = Index_TC.FFWI.train, Detect = array(0,dim=dim(Index_TC.FFWI.train)))
Check.Index_TC.HDWI.train <- list(Index = Index_TC.HDWI.train, Detect = array(0,dim=dim(Index_TC.HDWI.train)))
Check.Index_TC.MNI.train <- list(Index = Index_TC.MNI.train, Detect = array(0,dim=dim(Index_TC.MNI.train)))
Check.Index_TC.FFWI.re.train <- list(Index = Index_TC.FFWI.re.train, Detect = array(0,dim=dim(Index_TC.FFWI.re.train)))
Check.Index_TC.HDWI.re.train <- list(Index = Index_TC.HDWI.re.train, Detect = array(0,dim=dim(Index_TC.HDWI.re.train)))
Check.Index_TC.MNI.re.train <- list(Index = Index_TC.MNI.re.train, Detect = array(0,dim=dim(Index_TC.MNI.re.train)))

Check.FFWI.norm.forecast <- list(Index = FFWI.norm.forecast, Detect = array(0,dim=dim(FFWI.norm.forecast)))
Check.FPI.norm.forecast <- list(Index = FPI.norm.forecast, Detect = array(0,dim=dim(FPI.norm.forecast)))
Check.FWI.norm.forecast <- list(Index = FWI.norm.forecast, Detect = array(0,dim=dim(FWI.norm.forecast)))
Check.HDWI.norm.forecast <- list(Index = HDWI.norm.forecast, Detect = array(0,dim=dim(HDWI.norm.forecast)))
Check.MNI.norm.forecast <- list(Index = MNI.norm.forecast, Detect = array(0,dim=dim(MNI.norm.forecast)))
Check.Index_Simple.forecast <- list(Index = Index_Simple.forecast, Detect = array(0,dim=dim(Index_Simple.forecast)))
Check.Index_VC.All.forecast <- list(Index = Index_VC.All.forecast, Detect = array(0,dim=dim(Index_VC.All.forecast)))
Check.Index_VC.Clim.forecast <- list(Index = Index_VC.Clim.forecast, Detect = array(0,dim=dim(Index_VC.Clim.forecast)))
Check.Index_VC.noFWI.forecast <- list(Index = Index_VC.noFWI.forecast, Detect = array(0,dim=dim(Index_VC.noFWI.forecast)))
Check.Index_TC.FFWI.forecast <- list(Index = Index_TC.FFWI.forecast, Detect = array(0,dim=dim(Index_TC.FFWI.forecast)))
Check.Index_TC.HDWI.forecast <- list(Index = Index_TC.HDWI.forecast, Detect = array(0,dim=dim(Index_TC.HDWI.forecast)))
Check.Index_TC.MNI.forecast <- list(Index = Index_TC.MNI.forecast, Detect = array(0,dim=dim(Index_TC.MNI.forecast)))
Check.Index_TC.FFWI.re.forecast <- list(Index = Index_TC.FFWI.re.forecast, Detect = array(0,dim=dim(Index_TC.FFWI.re.forecast)))
Check.Index_TC.HDWI.re.forecast <- list(Index = Index_TC.HDWI.re.forecast, Detect = array(0,dim=dim(Index_TC.HDWI.re.forecast)))
Check.Index_TC.MNI.re.forecast <- list(Index = Index_TC.MNI.re.forecast, Detect = array(0,dim=dim(Index_TC.MNI.re.forecast)))


for (ii in seq(length(lon))){
  for (jj in seq(length(lat))){
    Check.FFWI.norm.train$Detect[ii,jj,][Check.FFWI.norm.train$Index[ii,jj,] > thr_FFWI.norm.train[ii,jj]] <- 1
    Check.FPI.norm.train$Detect[ii,jj,][Check.FPI.norm.train$Index[ii,jj,] > thr_FPI.norm.train[ii,jj]] <- 1
    Check.FWI.norm.train$Detect[ii,jj,][Check.FWI.norm.train$Index[ii,jj,] > thr_FWI.norm.train[ii,jj]] <- 1
    Check.HDWI.norm.train$Detect[ii,jj,][Check.HDWI.norm.train$Index[ii,jj,] > thr_HDWI.norm.train[ii,jj]] <- 1
    Check.MNI.norm.train$Detect[ii,jj,][Check.MNI.norm.train$Index[ii,jj,] > thr_MNI.norm.train[ii,jj]] <- 1
    Check.Index_Simple.train$Detect[ii,jj,][Check.Index_Simple.train$Index[ii,jj,] > thr_Index_Simple.train[ii,jj]] <- 1
    Check.Index_VC.All.train$Detect[ii,jj,][Check.Index_VC.All.train$Index[ii,jj,] > thr_Index_VC.All.train[ii,jj]] <- 1
    Check.Index_VC.Clim.train$Detect[ii,jj,][Check.Index_VC.Clim.train$Index[ii,jj,] > thr_Index_VC.Clim.train[ii,jj]] <- 1
    Check.Index_VC.noFWI.train$Detect[ii,jj,][Check.Index_VC.noFWI.train$Index[ii,jj,] > thr_Index_VC.noFWI.train[ii,jj]] <- 1
    Check.Index_TC.FFWI.train$Detect[ii,jj,][Check.Index_TC.FFWI.train$Index[ii,jj,] > thr_Index_TC.FFWI.train[ii,jj]] <- 1
    Check.Index_TC.HDWI.train$Detect[ii,jj,][Check.Index_TC.HDWI.train$Index[ii,jj,] > thr_Index_TC.HDWI.train[ii,jj]] <- 1
    Check.Index_TC.MNI.train$Detect[ii,jj,][Check.Index_TC.MNI.train$Index[ii,jj,] > thr_Index_TC.MNI.train[ii,jj]] <- 1
    Check.Index_TC.FFWI.re.train$Detect[ii,jj,][Check.Index_TC.FFWI.re.train$Index[ii,jj,] > thr_Index_TC.FFWI.re.train[ii,jj]] <- 1
    Check.Index_TC.HDWI.re.train$Detect[ii,jj,][Check.Index_TC.HDWI.re.train$Index[ii,jj,] > thr_Index_TC.HDWI.re.train[ii,jj]] <- 1
    Check.Index_TC.MNI.re.train$Detect[ii,jj,][Check.Index_TC.MNI.re.train$Index[ii,jj,] > thr_Index_TC.MNI.re.train[ii,jj]] <- 1

    Check.FFWI.norm.forecast$Detect[ii,jj,][Check.FFWI.norm.forecast$Index[ii,jj,] > thr_FFWI.norm.forecast[ii,jj]] <- 1
    Check.FPI.norm.forecast$Detect[ii,jj,][Check.FPI.norm.forecast$Index[ii,jj,] > thr_FPI.norm.forecast[ii,jj]] <- 1
    Check.FWI.norm.forecast$Detect[ii,jj,][Check.FWI.norm.forecast$Index[ii,jj,] > thr_FWI.norm.forecast[ii,jj]] <- 1
    Check.HDWI.norm.forecast$Detect[ii,jj,][Check.HDWI.norm.forecast$Index[ii,jj,] > thr_HDWI.norm.forecast[ii,jj]] <- 1
    Check.MNI.norm.forecast$Detect[ii,jj,][Check.MNI.norm.forecast$Index[ii,jj,] > thr_MNI.norm.forecast[ii,jj]] <- 1
    Check.Index_Simple.forecast$Detect[ii,jj,][Check.Index_Simple.forecast$Index[ii,jj,] > thr_Index_Simple.forecast[ii,jj]] <- 1
    Check.Index_VC.All.forecast$Detect[ii,jj,][Check.Index_VC.All.forecast$Index[ii,jj,] > thr_Index_VC.All.forecast[ii,jj]] <- 1
    Check.Index_VC.Clim.forecast$Detect[ii,jj,][Check.Index_VC.Clim.forecast$Index[ii,jj,] > thr_Index_VC.Clim.forecast[ii,jj]] <- 1
    Check.Index_VC.noFWI.forecast$Detect[ii,jj,][Check.Index_VC.noFWI.forecast$Index[ii,jj,] > thr_Index_VC.noFWI.forecast[ii,jj]] <- 1
    Check.Index_TC.FFWI.forecast$Detect[ii,jj,][Check.Index_TC.FFWI.forecast$Index[ii,jj,] > thr_Index_TC.FFWI.forecast[ii,jj]] <- 1
    Check.Index_TC.HDWI.forecast$Detect[ii,jj,][Check.Index_TC.HDWI.forecast$Index[ii,jj,] > thr_Index_TC.HDWI.forecast[ii,jj]] <- 1
    Check.Index_TC.MNI.forecast$Detect[ii,jj,][Check.Index_TC.MNI.forecast$Index[ii,jj,] > thr_Index_TC.MNI.forecast[ii,jj]] <- 1
    Check.Index_TC.FFWI.re.forecast$Detect[ii,jj,][Check.Index_TC.FFWI.re.forecast$Index[ii,jj,] > thr_Index_TC.FFWI.re.forecast[ii,jj]] <- 1
    Check.Index_TC.HDWI.re.forecast$Detect[ii,jj,][Check.Index_TC.HDWI.re.forecast$Index[ii,jj,] > thr_Index_TC.HDWI.re.forecast[ii,jj]] <- 1
    Check.Index_TC.MNI.re.forecast$Detect[ii,jj,][Check.Index_TC.MNI.re.forecast$Index[ii,jj,] > thr_Index_TC.MNI.re.forecast[ii,jj]] <- 1
    
  }
}


################## Scoring #############
TP.FFWI.norm.train <- 0
TP.FPI.norm.train <- 0
TP.FWI.norm.train <- 0
TP.HDWI.norm.train <- 0
TP.MNI.norm.train <- 0
TP.Index_Simple.train <- 0
TP.Index_VC.All.train <- 0
TP.Index_VC.Clim.train <- 0
TP.Index_VC.noFWI.train <- 0
TP.Index_TC.FFWI.train <- 0
TP.Index_TC.HDWI.train <- 0
TP.Index_TC.MNI.train <- 0
TP.Index_TC.FFWI.re.train <- 0
TP.Index_TC.HDWI.re.train <- 0
TP.Index_TC.MNI.re.train <- 0

TP.FFWI.norm.forecast <- 0
TP.FPI.norm.forecast <- 0
TP.FWI.norm.forecast <- 0
TP.HDWI.norm.forecast <- 0
TP.MNI.norm.forecast <- 0
TP.Index_Simple.forecast <- 0
TP.Index_VC.All.forecast <- 0
TP.Index_VC.Clim.forecast <- 0
TP.Index_VC.noFWI.forecast <- 0
TP.Index_TC.FFWI.forecast <- 0
TP.Index_TC.HDWI.forecast <- 0
TP.Index_TC.MNI.forecast <- 0
TP.Index_TC.FFWI.re.forecast <- 0
TP.Index_TC.HDWI.re.forecast <- 0
TP.Index_TC.MNI.re.forecast <- 0

Map_train <- Inven_train[,c(1:4,14:16,22:24)]
Map_train$FFWI <- 0
Map_train$FPI <- 0
Map_train$FWI <- 0
Map_train$HDWI <- 0
Map_train$MNI <- 0
Map_train$Simple <- 0
Map_train$VC.All <- 0
Map_train$VC.Clim <- 0
Map_train$VC.noFWI <- 0
Map_train$TC.FFWI <- 0
Map_train$TC.HDWI <- 0
Map_train$TC.MNI <- 0
Map_train$TC.FFWI.re <- 0
Map_train$TC.HDWI.re <- 0
Map_train$TC.MNI.re <- 0

Map_forecast <- Inven_forecast[,c(1:4,14:16,22:24)]
Map_forecast$FFWI <- 0
Map_forecast$FPI <- 0
Map_forecast$FWI <- 0
Map_forecast$HDWI <- 0
Map_forecast$MNI <- 0
Map_forecast$Simple <- 0
Map_forecast$VC.All <- 0
Map_forecast$VC.Clim <- 0
Map_forecast$VC.noFWI <- 0
Map_forecast$TC.FFWI <- 0
Map_forecast$TC.HDWI <- 0
Map_forecast$TC.MNI <- 0
Map_forecast$TC.FFWI.re <- 0
Map_forecast$TC.HDWI.re <- 0
Map_forecast$TC.MNI.re <- 0

# Training period
for (ii in seq(nrow(Inven_train))){
  # TP
  if(Check.FFWI.norm.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.FFWI.norm.train <- TP.FFWI.norm.train + 1
    Map_train$FFWI[ii] <- Map_train$FFWI[ii] + 1
  }
  if(Check.FPI.norm.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.FPI.norm.train <- TP.FPI.norm.train + 1
    Map_train$FPI[ii] <- Map_train$FPI[ii] + 1
  }
  if(Check.FWI.norm.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.FWI.norm.train <- TP.FWI.norm.train + 1
    Map_train$FWI[ii] <- Map_train$FWI[ii] + 1
  }
  if(Check.HDWI.norm.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.HDWI.norm.train <- TP.HDWI.norm.train + 1
    Map_train$HDWI[ii] <- Map_train$HDWI[ii] + 1
  }
  if(Check.MNI.norm.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.MNI.norm.train <- TP.MNI.norm.train + 1
    Map_train$MNI[ii] <- Map_train$MNI[ii] + 1
  }
  if(Check.Index_Simple.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_Simple.train <- TP.Index_Simple.train + 1
    Map_train$Simple[ii] <- Map_train$Simple[ii] + 1
  }
  if(Check.Index_VC.All.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_VC.All.train <- TP.Index_VC.All.train + 1
    Map_train$VC.All[ii] <- Map_train$VC.All[ii] + 1
  }
  if(Check.Index_VC.Clim.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_VC.Clim.train <- TP.Index_VC.Clim.train + 1
    Map_train$VC.Clim[ii] <- Map_train$VC.Clim[ii] + 1
  }
  if(Check.Index_VC.noFWI.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_VC.noFWI.train <- TP.Index_VC.noFWI.train + 1
    Map_train$VC.noFWI[ii] <- Map_train$VC.noFWI[ii] + 1
  }
  if(Check.Index_TC.FFWI.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.FFWI.train <- TP.Index_TC.FFWI.train + 1
    Map_train$TC.FFWI[ii] <- Map_train$TC.FFWI[ii] + 1
  }
  if(Check.Index_TC.HDWI.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.HDWI.train <- TP.Index_TC.HDWI.train + 1
    Map_train$TC.HDWI[ii] <- Map_train$TC.HDWI[ii] + 1
  }
  if(Check.Index_TC.MNI.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.MNI.train <- TP.Index_TC.MNI.train + 1
    Map_train$TC.MNI[ii] <- Map_train$TC.MNI[ii] + 1
  }
  if(Check.Index_TC.FFWI.re.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.FFWI.re.train <- TP.Index_TC.FFWI.re.train + 1
    Map_train$TC.FFWI.re[ii] <- Map_train$TC.FFWI.re[ii] + 1
  }
  if(Check.Index_TC.HDWI.re.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.HDWI.re.train <- TP.Index_TC.HDWI.re.train + 1
    Map_train$TC.HDWI.re[ii] <- Map_train$TC.HDWI.re[ii] + 1
  }
  if(Check.Index_TC.MNI.re.train$Detect[Inven_train$lon_order[ii],Inven_train$lat_order[ii],Inven_train$date_order[ii]] == 1){
    TP.Index_TC.MNI.re.train <- TP.Index_TC.MNI.re.train + 1
    Map_train$TC.MNI.re[ii] <- Map_train$TC.MNI.re[ii] + 1
  }
}

# Forecasting period
for (ii in seq(nrow(Inven_forecast))){
  # TP
  if(Check.FFWI.norm.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.FFWI.norm.forecast <- TP.FFWI.norm.forecast + 1
    Map_forecast$FFWI[ii] <- Map_forecast$FFWI[ii] + 1
  }
  if(Check.FPI.norm.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.FPI.norm.forecast <- TP.FPI.norm.forecast + 1
    Map_forecast$FPI[ii] <- Map_forecast$FPI[ii] + 1
  }
  if(Check.FWI.norm.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.FWI.norm.forecast <- TP.FWI.norm.forecast + 1
    Map_forecast$FWI[ii] <- Map_forecast$FWI[ii] + 1
  }
  if(Check.HDWI.norm.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.HDWI.norm.forecast <- TP.HDWI.norm.forecast + 1
    Map_forecast$HDWI[ii] <- Map_forecast$HDWI[ii] + 1
  }
  if(Check.MNI.norm.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.MNI.norm.forecast <- TP.MNI.norm.forecast + 1
    Map_forecast$MNI[ii] <- Map_forecast$MNI[ii] + 1
  }
  if(Check.Index_Simple.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_Simple.forecast <- TP.Index_Simple.forecast + 1
    Map_forecast$Simple[ii] <- Map_forecast$Simple[ii] + 1
  }
  if(Check.Index_VC.All.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_VC.All.forecast <- TP.Index_VC.All.forecast + 1
    Map_forecast$VC.All[ii] <- Map_forecast$VC.All[ii] + 1
  }
  if(Check.Index_VC.Clim.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_VC.Clim.forecast <- TP.Index_VC.Clim.forecast + 1
    Map_forecast$VC.Clim[ii] <- Map_forecast$VC.Clim[ii] + 1
  }
  if(Check.Index_VC.noFWI.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_VC.noFWI.forecast <- TP.Index_VC.noFWI.forecast + 1
    Map_forecast$VC.noFWI[ii] <- Map_forecast$VC.noFWI[ii] + 1
  }
  if(Check.Index_TC.FFWI.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.FFWI.forecast <- TP.Index_TC.FFWI.forecast + 1
    Map_forecast$TC.FFWI[ii] <- Map_forecast$TC.FFWI[ii] + 1
  }
  if(Check.Index_TC.HDWI.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.HDWI.forecast <- TP.Index_TC.HDWI.forecast + 1
    Map_forecast$TC.HDWI[ii] <- Map_forecast$TC.HDWI[ii] + 1
  }
  if(Check.Index_TC.MNI.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.MNI.forecast <- TP.Index_TC.MNI.forecast + 1
    Map_forecast$TC.MNI[ii] <- Map_forecast$TC.MNI[ii] + 1
  }
  if(Check.Index_TC.FFWI.re.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.FFWI.re.forecast <- TP.Index_TC.FFWI.re.forecast + 1
    Map_forecast$TC.FFWI.re[ii] <- Map_forecast$TC.FFWI.re[ii] + 1
  }
  if(Check.Index_TC.HDWI.re.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.HDWI.re.forecast <- TP.Index_TC.HDWI.re.forecast + 1
    Map_forecast$TC.HDWI.re[ii] <- Map_forecast$TC.HDWI.re[ii] + 1
  }
  if(Check.Index_TC.MNI.re.forecast$Detect[Inven_forecast$lon_order[ii],Inven_forecast$lat_order[ii],Inven_forecast$date_order[ii]] == 1){
    TP.Index_TC.MNI.re.forecast <- TP.Index_TC.MNI.re.forecast + 1
    Map_forecast$TC.MNI.re[ii] <- Map_forecast$TC.MNI.re[ii] + 1
  }
}

# setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Result")
# 
# write_xlsx(Map_train,paste0("Map.training_",prob,".xlsx"))
# write_xlsx(Map_forecast,paste0("Map.forecasting_",prob,".xlsx"))


# Hit rate
HR.FFWI.norm.train <- TP.FFWI.norm.train/nrow(Inven_train)
HR.FPI.norm.train <- TP.FPI.norm.train/nrow(Inven_train)
HR.FWI.norm.train <- TP.FWI.norm.train/nrow(Inven_train)
HR.HDWI.norm.train <- TP.HDWI.norm.train/nrow(Inven_train)
HR.MNI.norm.train <- TP.MNI.norm.train/nrow(Inven_train)
HR.Index_Simple.train <- TP.Index_Simple.train/nrow(Inven_train)
HR.Index_VC.All.train <- TP.Index_VC.All.train/nrow(Inven_train)
HR.Index_VC.Clim.train <- TP.Index_VC.Clim.train/nrow(Inven_train)
HR.Index_VC.noFWI.train <- TP.Index_VC.noFWI.train/nrow(Inven_train)
HR.Index_TC.FFWI.train <- TP.Index_TC.FFWI.train/nrow(Inven_train)
HR.Index_TC.HDWI.train <- TP.Index_TC.HDWI.train/nrow(Inven_train)
HR.Index_TC.MNI.train <- TP.Index_TC.MNI.train/nrow(Inven_train)
HR.Index_TC.FFWI.re.train <- TP.Index_TC.FFWI.re.train/nrow(Inven_train)
HR.Index_TC.HDWI.re.train <- TP.Index_TC.HDWI.re.train/nrow(Inven_train)
HR.Index_TC.MNI.re.train <- TP.Index_TC.MNI.re.train/nrow(Inven_train)

HR.FFWI.norm.forecast <- TP.FFWI.norm.forecast/nrow(Inven_forecast)
HR.FPI.norm.forecast <- TP.FPI.norm.forecast/nrow(Inven_forecast)
HR.FWI.norm.forecast <- TP.FWI.norm.forecast/nrow(Inven_forecast)
HR.HDWI.norm.forecast <- TP.HDWI.norm.forecast/nrow(Inven_forecast)
HR.MNI.norm.forecast <- TP.MNI.norm.forecast/nrow(Inven_forecast)
HR.Index_Simple.forecast <- TP.Index_Simple.forecast/nrow(Inven_forecast)
HR.Index_VC.All.forecast <- TP.Index_VC.All.forecast/nrow(Inven_forecast)
HR.Index_VC.Clim.forecast <- TP.Index_VC.Clim.forecast/nrow(Inven_forecast)
HR.Index_VC.noFWI.forecast <- TP.Index_VC.noFWI.forecast/nrow(Inven_forecast)
HR.Index_TC.FFWI.forecast <- TP.Index_TC.FFWI.forecast/nrow(Inven_forecast)
HR.Index_TC.HDWI.forecast <- TP.Index_TC.HDWI.forecast/nrow(Inven_forecast)
HR.Index_TC.MNI.forecast <- TP.Index_TC.MNI.forecast/nrow(Inven_forecast)
HR.Index_TC.FFWI.re.forecast <- TP.Index_TC.FFWI.re.forecast/nrow(Inven_forecast)
HR.Index_TC.HDWI.re.forecast <- TP.Index_TC.HDWI.re.forecast/nrow(Inven_forecast)
HR.Index_TC.MNI.re.forecast <- TP.Index_TC.MNI.re.forecast/nrow(Inven_forecast)

# Results
Score <- data.frame(Case = c("FFWI","FPI","FWI","HDWI","MNI",
                             "Index_Simple",
                             "Index_VC.All","Index_VC.Clim","Index_VC.noFWI",
                             "Index_TC.FFWI","Index_TC.HDWI","Index_TC.MNI",
                             "Index_TC.FFWI.re","Index_TC.HDWI.re","Index_TC.MNI.re"),
                    TP.train = c(TP.FFWI.norm.train,TP.FPI.norm.train,TP.FWI.norm.train,TP.HDWI.norm.train,TP.MNI.norm.train,
                                 TP.Index_Simple.train,
                                 TP.Index_VC.All.train,TP.Index_VC.Clim.train,TP.Index_VC.noFWI.train,
                                 TP.Index_TC.FFWI.train,TP.Index_TC.HDWI.train,TP.Index_TC.MNI.train,
                                 TP.Index_TC.FFWI.re.train,TP.Index_TC.HDWI.re.train,TP.Index_TC.MNI.re.train),
                    HR.train = c(HR.FFWI.norm.train,HR.FPI.norm.train,HR.FWI.norm.train,HR.HDWI.norm.train,HR.MNI.norm.train,
                                 HR.Index_Simple.train,
                                 HR.Index_VC.All.train,HR.Index_VC.Clim.train,HR.Index_VC.noFWI.train,
                                 HR.Index_TC.FFWI.train,HR.Index_TC.HDWI.train,HR.Index_TC.MNI.train,
                                 HR.Index_TC.FFWI.re.train,HR.Index_TC.HDWI.re.train,HR.Index_TC.MNI.re.train),
                    TP.forecast = c(TP.FFWI.norm.forecast,TP.FPI.norm.forecast,TP.FWI.norm.forecast,TP.HDWI.norm.forecast,TP.MNI.norm.forecast,
                                    TP.Index_Simple.forecast,
                                    TP.Index_VC.All.forecast,TP.Index_VC.Clim.forecast,TP.Index_VC.noFWI.forecast,
                                    TP.Index_TC.FFWI.forecast,TP.Index_TC.HDWI.forecast,TP.Index_TC.MNI.forecast,
                                    TP.Index_TC.FFWI.re.forecast,TP.Index_TC.HDWI.re.forecast,TP.Index_TC.MNI.re.forecast),
                    HR.forecast = c(HR.FFWI.norm.forecast,HR.FPI.norm.forecast,HR.FWI.norm.forecast,HR.HDWI.norm.forecast,HR.MNI.norm.forecast,
                                    HR.Index_Simple.forecast,
                                    HR.Index_VC.All.forecast,HR.Index_VC.Clim.forecast,HR.Index_VC.noFWI.forecast,
                                    HR.Index_TC.FFWI.forecast,HR.Index_TC.HDWI.forecast,HR.Index_TC.MNI.forecast,
                                    HR.Index_TC.FFWI.re.forecast,HR.Index_TC.HDWI.re.forecast,HR.Index_TC.MNI.re.forecast)
)


setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Result")

write_xlsx(Score,paste0("Score_",prob,".xlsx"))







