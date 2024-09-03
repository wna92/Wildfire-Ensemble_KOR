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


####### Load index ##########

setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Index")

raw_FFWI_2014 <- nc_open("FFWI_2014.nc")
raw_FFWI_2015 <- nc_open("FFWI_2015.nc")
raw_FFWI_2016 <- nc_open("FFWI_2016.nc")
raw_FFWI_2017 <- nc_open("FFWI_2017.nc")
raw_FFWI_2018 <- nc_open("FFWI_2018.nc")
raw_FFWI_2019 <- nc_open("FFWI_2019.nc")
raw_FFWI_2020 <- nc_open("FFWI_2020.nc")
raw_FFWI_2021 <- nc_open("FFWI_2021.nc")
raw_FFWI_2022 <- nc_open("FFWI_2022.nc")
raw_FFWI_2023 <- nc_open("FFWI_2023.nc")
lon_FFWI <- ncvar_get(raw_FFWI_2014, "lon")
lat_FFWI <- ncvar_get(raw_FFWI_2014, "lat")
time_FFWI <- ncvar_get(raw_FFWI_2014, "time")
tunits_FFWI <- ncatt_get(raw_FFWI_2014,"time","units")

raw_FPI_2014 <- nc_open("FPI_2014.nc")
raw_FPI_2015 <- nc_open("FPI_2015.nc")
raw_FPI_2016 <- nc_open("FPI_2016.nc")
raw_FPI_2017 <- nc_open("FPI_2017.nc")
raw_FPI_2018 <- nc_open("FPI_2018.nc")
raw_FPI_2019 <- nc_open("FPI_2019.nc")
raw_FPI_2020 <- nc_open("FPI_2020.nc")
raw_FPI_2021 <- nc_open("FPI_2021.nc")
raw_FPI_2022 <- nc_open("FPI_2022.nc")
raw_FPI_2023 <- nc_open("FPI_2023.nc")
lon_FPI <- ncvar_get(raw_FPI_2014, "lon")
lat_FPI <- ncvar_get(raw_FPI_2014, "lat")

raw_HDWI_2014 <- nc_open("HDWI_2014.nc")
raw_HDWI_2015 <- nc_open("HDWI_2015.nc")
raw_HDWI_2016 <- nc_open("HDWI_2016.nc")
raw_HDWI_2017 <- nc_open("HDWI_2017.nc")
raw_HDWI_2018 <- nc_open("HDWI_2018.nc")
raw_HDWI_2019 <- nc_open("HDWI_2019.nc")
raw_HDWI_2020 <- nc_open("HDWI_2020.nc")
raw_HDWI_2021 <- nc_open("HDWI_2021.nc")
raw_HDWI_2022 <- nc_open("HDWI_2022.nc")
raw_HDWI_2023 <- nc_open("HDWI_2023.nc")
lon_HDWI <- ncvar_get(raw_HDWI_2014, "lon")
lat_HDWI <- ncvar_get(raw_HDWI_2014, "lat")

raw_MNI_2014 <- nc_open("MNI_2014.nc")
raw_MNI_2015 <- nc_open("MNI_2015.nc")
raw_MNI_2016 <- nc_open("MNI_2016.nc")
raw_MNI_2017 <- nc_open("MNI_2017.nc")
raw_MNI_2018 <- nc_open("MNI_2018.nc")
raw_MNI_2019 <- nc_open("MNI_2019.nc")
raw_MNI_2020 <- nc_open("MNI_2020.nc")
raw_MNI_2021 <- nc_open("MNI_2021.nc")
raw_MNI_2022 <- nc_open("MNI_2022.nc")
raw_MNI_2023 <- nc_open("MNI_2023.nc")
lon_MNI <- ncvar_get(raw_MNI_2014, "lon")
lat_MNI <- ncvar_get(raw_MNI_2014, "lat")

raw_FWI_2014 <- nc_open("FWI.GEOS-5.Daily_Kor_2014.nc")
raw_FWI_2015 <- nc_open("FWI.GEOS-5.Daily_Kor_2015.nc")
raw_FWI_2016 <- nc_open("FWI.GEOS-5.Daily_Kor_2016.nc")
raw_FWI_2017 <- nc_open("FWI.GEOS-5.Daily_Kor_2017.nc")
raw_FWI_2018 <- nc_open("FWI.GEOS-5.Daily_Kor_2018.nc")
raw_FWI_2019 <- nc_open("FWI.GEOS-5.Daily_Kor_2019.nc")
raw_FWI_2020 <- nc_open("FWI.GEOS-5.Daily_Kor_2020.nc")
raw_FWI_2021 <- nc_open("FWI.GEOS-5.Daily_Kor_2021.nc")
raw_FWI_2022 <- nc_open("FWI.GEOS-5.Daily_Kor_2022.nc")
raw_FWI_2023 <- nc_open("FWI.GEOS-5.Daily_Kor_2023.nc")
lon_FWI <- ncvar_get(raw_FWI_2014, "lon")
lat_FWI <- ncvar_get(raw_FWI_2014, "lat")
time_FWI <- ncvar_get(raw_FWI_2019, "time")
tunits_FWI <- ncatt_get(raw_FWI_2014,"time","units")

# For training & forecasting periods
FFWI.train <- abind(
  ncvar_get(raw_FFWI_2014, "FFWI"),
  ncvar_get(raw_FFWI_2015, "FFWI"),
  ncvar_get(raw_FFWI_2016, "FFWI"),
  ncvar_get(raw_FFWI_2017, "FFWI"),
  ncvar_get(raw_FFWI_2018, "FFWI"),
  ncvar_get(raw_FFWI_2019, "FFWI"),
  ncvar_get(raw_FFWI_2020, "FFWI")
  )

FFWI.forecast <- abind(
  ncvar_get(raw_FFWI_2021, "FFWI"),
  ncvar_get(raw_FFWI_2022, "FFWI"),
  ncvar_get(raw_FFWI_2023, "FFWI")
)

FPI.train <- abind(
  ncvar_get(raw_FPI_2014, "FPI"),
  ncvar_get(raw_FPI_2015, "FPI"),
  ncvar_get(raw_FPI_2016, "FPI"),
  ncvar_get(raw_FPI_2017, "FPI"),
  ncvar_get(raw_FPI_2018, "FPI"),
  ncvar_get(raw_FPI_2019, "FPI"),
  ncvar_get(raw_FPI_2020, "FPI")
)

FPI.forecast <- abind(
  ncvar_get(raw_FPI_2021, "FPI"),
  ncvar_get(raw_FPI_2022, "FPI"),
  ncvar_get(raw_FPI_2023, "FPI")
)

HDWI.train <- abind(
  ncvar_get(raw_HDWI_2014, "HDWI"),
  ncvar_get(raw_HDWI_2015, "HDWI"),
  ncvar_get(raw_HDWI_2016, "HDWI"),
  ncvar_get(raw_HDWI_2017, "HDWI"),
  ncvar_get(raw_HDWI_2018, "HDWI"),
  ncvar_get(raw_HDWI_2019, "HDWI"),
  ncvar_get(raw_HDWI_2020, "HDWI")
)

HDWI.forecast <- abind(
  ncvar_get(raw_HDWI_2021, "HDWI"),
  ncvar_get(raw_HDWI_2022, "HDWI"),
  ncvar_get(raw_HDWI_2023, "HDWI")
)

MNI.train <- abind(
  ncvar_get(raw_MNI_2014, "MNI"),
  ncvar_get(raw_MNI_2015, "MNI"),
  ncvar_get(raw_MNI_2016, "MNI"),
  ncvar_get(raw_MNI_2017, "MNI"),
  ncvar_get(raw_MNI_2018, "MNI"),
  ncvar_get(raw_MNI_2019, "MNI"),
  ncvar_get(raw_MNI_2020, "MNI")
)

MNI.forecast <- abind(
  ncvar_get(raw_MNI_2021, "MNI"),
  ncvar_get(raw_MNI_2022, "MNI"),
  ncvar_get(raw_MNI_2023, "MNI")
)


FWI.train <- abind(
  array(dim=c(length(lon_FWI),length(lat_FWI),120)),
  ncvar_get(raw_FWI_2014, "FWI"),
  ncvar_get(raw_FWI_2015, "FWI"),
  ncvar_get(raw_FWI_2016, "FWI"),
  ncvar_get(raw_FWI_2017, "FWI"),
  ncvar_get(raw_FWI_2018, "FWI"),
  ncvar_get(raw_FWI_2019, "FWI"),
  array(dim=c(length(lon_FWI),length(lat_FWI),4)),
  ncvar_get(raw_FWI_2020, "FWI")
)

FWI.forecast <- abind(
  ncvar_get(raw_FWI_2021, "FWI"),
  ncvar_get(raw_FWI_2022, "FWI"),
  ncvar_get(raw_FWI_2023, "FWI")
)

# Regrid FWI (& Bilinear interpolation)
FWI.train_re <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FWI.train[1,1,])))
FWI.forecast_re <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FWI.forecast[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    FWI.train_re[ii,jj,] <- FWI.train[order(abs(lon_FWI-lon_FFWI[ii]))[1],order(abs(lat_FWI-lat_FFWI[jj]))[1],]
    FWI.forecast_re[ii,jj,] <- FWI.forecast[order(abs(lon_FWI-lon_FFWI[ii]))[1],order(abs(lat_FWI-lat_FFWI[jj]))[1],]
  }
}


############ Fit distributions ("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma") #############

# FFWI
chisq <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
chisq.pvalue <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
cvm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ad <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ks <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ks.test <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
aic <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
bic <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (is.na(FFWI.train[ii,jj,1]) == FALSE){
      exp.f <- fitdist(FFWI.train[ii,jj,], "exp",method="mle")
      norm.f <- fitdist(FFWI.train[ii,jj,], "norm",method="mle")
      lnorm.f <- fitdist(FFWI.train[ii,jj,], "lnorm",method="mle")
      weib.f <- fitdist(FFWI.train[ii,jj,], "weibull",method="mle")
      llog.f <- fitdist(FFWI.train[ii,jj,], "llogis",method="mle")
      gam.f <- fitdist(FFWI.train[ii,jj,], "gamma",method="mle")
      
      chisq[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisq
      chisq.pvalue[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisqpvalue
      cvm[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$cvm
      ad[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ad
      ks[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ks
      ks.test[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$kstest
      aic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$aic
      bic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$bic
    }
  }
}

fit.FFWI <- list(chisq=chisq,chisq.pvalue=chisq.pvalue,cvm=cvm,ad=ad,ks=ks,ks.test=ks.test,aic=aic,bic=bic)

# Check which one is best(1-"Exponential",2-"Normal", 3-"Log-normal",4-"Weibull",5-"Log-Logis",6-"Gamma")
fit.FFWI$ks.test[,,3]

# FPI
chisq <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
chisq.pvalue <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
cvm <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
ad <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
ks <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
ks.test <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
aic <- array(dim=c(length(lon_FPI),length(lat_FPI),6))
bic <- array(dim=c(length(lon_FPI),length(lat_FPI),6))

for (ii in seq(length(lon_FPI))){
  for (jj in seq(length(lat_FPI))){
    if (is.na(FPI.train[ii,jj,1]) == FALSE){
      exp.f <- fitdist(FPI.train[ii,jj,], "exp",method="mle")
      norm.f <- fitdist(FPI.train[ii,jj,], "norm",method="mle")
      lnorm.f <- fitdist(FPI.train[ii,jj,], "lnorm",method="mle")
      weib.f <- fitdist(FPI.train[ii,jj,], "weibull",method="mle")
      llog.f <- fitdist(FPI.train[ii,jj,], "llogis",method="mle")
      gam.f <- fitdist(FPI.train[ii,jj,], "gamma",method="mle")
      
      chisq[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisq
      chisq.pvalue[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisqpvalue
      cvm[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$cvm
      ad[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ad
      ks[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ks
      ks.test[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$kstest
      aic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$aic
      bic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$bic
    }
  }
}

fit.FPI <- list(chisq=chisq,chisq.pvalue=chisq.pvalue,cvm=cvm,ad=ad,ks=ks,ks.test=ks.test,aic=aic,bic=bic)

# Check which one is best(1-"Exponential",2-"Normal", 3-"Log-normal",4-"Weibull",5-"Log-Logis",6-"Gamma")
fit.FPI$ks.test[,,6]

# FWI
chisq <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
chisq.pvalue <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
cvm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ad <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ks <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
ks.test <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
aic <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))
bic <- array(dim=c(length(lon_FFWI),length(lat_FFWI),6))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (is.na(FWI.train_re[ii,jj,1]) == FALSE){
      exp.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "exp",method="mle")
      norm.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "norm",method="mle")
      lnorm.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "lnorm",method="mle")
      weib.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "weibull",method="mle")
      llog.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "llogis",method="mle")
      gam.f <- fitdist(as.numeric(na.omit(FWI.train_re[ii,jj,])), "gamma",method="mle")
      
      chisq[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisq
      chisq.pvalue[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisqpvalue
      cvm[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$cvm
      ad[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ad
      ks[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ks
      ks.test[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$kstest
      aic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$aic
      bic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$bic
    }
  }
}

fit.FWI <- list(chisq=chisq,chisq.pvalue=chisq.pvalue,cvm=cvm,ad=ad,ks=ks,ks.test=ks.test,aic=aic,bic=bic)

# Check which one is best(1-"Exponential",2-"Normal", 3-"Log-normal",4-"Weibull",5-"Log-Logis",6-"Gamma")
fit.FWI$ks.test[,,6]

# HDWI
chisq <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
chisq.pvalue <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
cvm <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
ad <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
ks <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
ks.test <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
aic <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))
bic <- array(dim=c(length(lon_HDWI),length(lat_HDWI),6))

for (ii in seq(length(lon_HDWI))){
  for (jj in seq(length(lat_HDWI))){
    if (is.na(HDWI[ii,jj,1]) == FALSE){
      exp.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "exp",method="mle")
      norm.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "norm",method="mle")
      lnorm.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "lnorm",method="mle")
      weib.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "weibull",method="mle")
      llog.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "llogis",method="mle")
      gam.f <- fitdist(HDWI[ii,jj,][HDWI[ii,jj,] > 0], "gamma",method="mle")
      
      chisq[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisq
      chisq.pvalue[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisqpvalue
      cvm[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$cvm
      ad[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ad
      ks[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ks
      ks.test[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$kstest
      aic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$aic
      bic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$bic
    }
  }
}

fit.HDWI <- list(chisq=chisq,chisq.pvalue=chisq.pvalue,cvm=cvm,ad=ad,ks=ks,ks.test=ks.test,aic=aic,bic=bic)

# Check which one is best(1-"Exponential",2-"Normal", 3-"Log-normal",4-"Weibull",5-"Log-Logis",6-"Gamma")
fit.HDWI$ks.test[,,3]

# MNI
chisq <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
chisq.pvalue <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
cvm <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
ad <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
ks <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
ks.test <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
aic <- array(dim=c(length(lon_MNI),length(lat_MNI),6))
bic <- array(dim=c(length(lon_MNI),length(lat_MNI),6))

for (ii in seq(length(lon_MNI))){
  for (jj in seq(length(lat_MNI))){
    if (is.na(MNI[ii,jj,1]) == FALSE){
      exp.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "exp",method="mle"))
      norm.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "norm",method="mle"))
      lnorm.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "lnorm",method="mle"))
      weib.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "weibull",method="mle"))
      llog.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "llogis",method="mle", lower = c(0, 0)))
      gam.f <- try(fitdist(as.numeric(na.omit(MNI[ii,jj,][MNI[ii,jj,] > 0])), "gamma",method="mle", lower = c(0, 0)))
      
      chisq[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisq
      chisq.pvalue[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$chisqpvalue
      cvm[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$cvm
      ad[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ad
      ks[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$ks
      ks.test[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$kstest
      aic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$aic
      bic[ii,jj,] <- gofstat(list(exp.f,norm.f,lnorm.f,weib.f,llog.f,gam.f),fitnames = c("Exponential","Normal", "Log-normal","Weibull","Log-Logis","Gamma"))$bic
    }
  }
}

fit.MNI <- list(chisq=chisq,chisq.pvalue=chisq.pvalue,cvm=cvm,ad=ad,ks=ks,ks.test=ks.test,aic=aic,bic=bic)

# Check which one is best(1-"Exponential",2-"Normal", 3-"Log-normal",4-"Weibull",5-"Log-Logis",6-"Gamma")
fit.MNI$ks.test[,,6]


# For FPI
hist(FPI.train[5,10,], col='steelblue')
plot(FPI.train[10,10,])

result <- normalmixEM(FPI.train[10,10,])
plot(result$posterior[,1])


#################### Determined distributions ##########################
# FFWI: Log-normal
# FPI: Bimodal Gaussian
# FWI: Empirical (Any parametric distribution does not fit)
# HDWI: Log-normal
# MNI: Gamma


############### Normal Quantile Transform #######################
# FFWI
FFWI.train.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(FFWI.train[ii,jj,])) == FALSE){
      fit_data <- fitdist(FFWI.train[ii,jj,],"lnorm")
      cdf_data <- plnorm(FFWI.train[ii,jj,],meanlog=fit_data$estimate[1],sdlog = fit_data$estimate[2])
      FFWI.train.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

FFWI.forecast.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(FFWI.forecast[ii,jj,])) == FALSE){
      fit_data <- fitdist(FFWI.forecast[ii,jj,],"lnorm")
      cdf_data <- plnorm(FFWI.forecast[ii,jj,],meanlog=fit_data$estimate[1],sdlog = fit_data$estimate[2])
      FFWI.forecast.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

# FPI
FPI.train.norm <- array(dim=c(length(lon_FPI),length(lat_FPI),length(FPI.train[1,1,])))

for (ii in seq(length(lon_FPI))){
  for (jj in seq(length(lat_FPI))){
    if (all(is.na(FPI.train[ii,jj,])) == FALSE){
      fit_data <- normalmixEM(FPI.train[ii,jj,])
      cdf_data <- pmixnorm(FPI.train[ii,jj,],fit_data$mu,fit_data$sigma,fit_data$lambda)
      FPI.train.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

FPI.forecast.norm <- array(dim=c(length(lon_FPI),length(lat_FPI),length(FPI.forecast[1,1,])))

for (ii in seq(length(lon_FPI))){
  for (jj in seq(length(lat_FPI))){
    if (all(is.na(FPI.forecast[ii,jj,])) == FALSE){
      fit_data <- normalmixEM(FPI.forecast[ii,jj,])
      cdf_data <- pmixnorm(FPI.forecast[ii,jj,],fit_data$mu,fit_data$sigma,fit_data$lambda)
      FPI.forecast.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

# FWI
FWI.train.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FWI.train_re[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(FWI.train_re[ii,jj,])) == FALSE){
      fit_data <- ecdf(as.numeric(na.omit(FWI.train_re[ii,jj,])))
      cdf_data <- fit_data(FWI.train_re[ii,jj,])
      FWI.train.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

FWI.forecast.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FWI.forecast_re[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(FWI.forecast_re[ii,jj,])) == FALSE){
      fit_data <- ecdf(as.numeric(na.omit(FWI.forecast_re[ii,jj,])))
      cdf_data <- fit_data(FWI.forecast_re[ii,jj,])
      FWI.forecast.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

# HDWI
HDWI.train.norm <- array(dim=c(length(lon_HDWI),length(lat_HDWI),length(HDWI.train[1,1,])))

for (ii in seq(length(lon_HDWI))){
  for (jj in seq(length(lat_HDWI))){
    if (all(is.na(HDWI.train[ii,jj,])) == FALSE){
      fit_data <- fitdist(HDWI.train[ii,jj,][HDWI.train[ii,jj,] > 0],"lnorm")
      cdf_data <- plnorm(HDWI.train[ii,jj,],meanlog=fit_data$estimate[1],sdlog = fit_data$estimate[2])
      HDWI.train.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

HDWI.forecast.norm <- array(dim=c(length(lon_HDWI),length(lat_HDWI),length(HDWI.forecast[1,1,])))

for (ii in seq(length(lon_HDWI))){
  for (jj in seq(length(lat_HDWI))){
    if (all(is.na(HDWI.forecast[ii,jj,])) == FALSE){
      fit_data <- fitdist(HDWI.forecast[ii,jj,][HDWI.forecast[ii,jj,] > 0],"lnorm")
      cdf_data <- plnorm(HDWI.forecast[ii,jj,],meanlog=fit_data$estimate[1],sdlog = fit_data$estimate[2])
      HDWI.forecast.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

# MNI
MNI.train.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(MNI.train[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(MNI.train[ii,jj,])) == FALSE){
      fit_data <- fitdist(as.numeric(na.omit(MNI.train[ii,jj,][MNI.train[ii,jj,] > 0])), "gamma",lower = c(0, 0))
      cdf_data <- pgamma(MNI.train[ii,jj,],shape=fit_data$estimate[1],rate = fit_data$estimate[2])
      MNI.train.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

MNI.forecast.norm <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(MNI.forecast[1,1,])))

for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    if (all(is.na(MNI.forecast[ii,jj,])) == FALSE){
      fit_data <- fitdist(as.numeric(na.omit(MNI.forecast[ii,jj,][MNI.forecast[ii,jj,] > 0])), "gamma",lower = c(0, 0))
      cdf_data <- pgamma(MNI.forecast[ii,jj,],shape=fit_data$estimate[1],rate = fit_data$estimate[2])
      MNI.forecast.norm[ii,jj,] <- qnorm(cdf_data,mean=0,sd=1)
    }
  }
}

######################################################
############## Variance-Covariance method (with considering correlations) ##############

index_VC.All.train <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_VC.noFWI.train <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_VC.Clim.train <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))

index_VC.All.forecast <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_VC.noFWI.forecast <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_VC.Clim.forecast <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))

# All
for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    data <- data.frame(FFWI = FFWI.train.norm[ii,jj,],
                       FPI = FPI.train.norm[ii,jj,],
                       FWI = FWI.train.norm[ii,jj,],
                       HDWI = HDWI.train.norm[ii,jj,],
                       MNI = MNI.train.norm[ii,jj,]
    )
    
    data[sapply(data, is.infinite)] <- NA
    
    cov_grid <- cov(data,use = 'pairwise.complete.obs')
    
    weight <- (solve(cov_grid) %*% matrix(1,nrow=5,ncol=1))/as.numeric(matrix(1,nrow=1,ncol=5) %*% solve(cov_grid) %*% matrix(1,nrow=5,ncol=1))
    
    index_VC.All.train[ii,jj,] <- weight[1]*FFWI.train.norm[ii,jj,]+weight[2]*FPI.train.norm[ii,jj,]+weight[3]*FWI.train.norm[ii,jj,]+weight[4]*HDWI.train.norm[ii,jj,]+weight[5]*MNI.train.norm[ii,jj,]
    index_VC.All.forecast[ii,jj,] <- weight[1]*FFWI.forecast.norm[ii,jj,]+weight[2]*FPI.forecast.norm[ii,jj,]+weight[3]*FWI.forecast.norm[ii,jj,]+weight[4]*HDWI.forecast.norm[ii,jj,]+weight[5]*MNI.forecast.norm[ii,jj,]
    
  }
}

# No FWI
for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    data <- data.frame(FFWI = FFWI.train.norm[ii,jj,],
                       FPI = FPI.train.norm[ii,jj,],
                       HDWI = HDWI.train.norm[ii,jj,],
                       MNI = MNI.train.norm[ii,jj,]
    )
    
    data[sapply(data, is.infinite)] <- NA
    
    cov_grid <- cov(data,use = 'pairwise.complete.obs')
    
    weight <- (solve(cov_grid) %*% matrix(1,nrow=4,ncol=1))/as.numeric(matrix(1,nrow=1,ncol=4) %*% solve(cov_grid) %*% matrix(1,nrow=4,ncol=1))
    
    index_VC.noFWI.train[ii,jj,] <- weight[1]*FFWI.train.norm[ii,jj,]+weight[2]*FPI.train.norm[ii,jj,]+weight[3]*HDWI.train.norm[ii,jj,]+weight[4]*MNI.train.norm[ii,jj,]
    index_VC.noFWI.forecast[ii,jj,] <- weight[1]*FFWI.forecast.norm[ii,jj,]+weight[2]*FPI.forecast.norm[ii,jj,]+weight[3]*HDWI.forecast.norm[ii,jj,]+weight[4]*MNI.forecast.norm[ii,jj,]
  }
}

# Climate factors
for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    data <- data.frame(FFWI = FFWI.train.norm[ii,jj,],
                       HDWI = HDWI.train.norm[ii,jj,],
                       MNI = MNI.train.norm[ii,jj,]
    )
    
    data[sapply(data, is.infinite)] <- NA
    
    cov_grid <- cov(data,use = 'pairwise.complete.obs')
    
    weight <- (solve(cov_grid) %*% matrix(1,nrow=3,ncol=1))/as.numeric(matrix(1,nrow=1,ncol=3) %*% solve(cov_grid) %*% matrix(1,nrow=3,ncol=1))
    
    index_VC.Clim.train[ii,jj,] <- weight[1]*FFWI.train.norm[ii,jj,]+weight[2]*HDWI.train.norm[ii,jj,]+weight[3]*MNI.train.norm[ii,jj,]
    index_VC.Clim.forecast[ii,jj,] <- weight[1]*FFWI.forecast.norm[ii,jj,]+weight[2]*HDWI.forecast.norm[ii,jj,]+weight[3]*MNI.forecast.norm[ii,jj,]
  }
}


############## Triple collocation method (without considering correlations) ##############

index_TC.train.FFWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_TC.train.HDWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_TC.train.MNI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_TC.re.train.FFWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_TC.re.train.HDWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))
index_TC.re.train.MNI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.train[1,1,])))

index_TC.forecast.FFWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_TC.forecast.HDWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_TC.forecast.MNI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_TC.re.forecast.FFWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_TC.re.forecast.HDWI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))
index_TC.re.forecast.MNI <- array(dim=c(length(lon_FFWI),length(lat_FFWI),length(FFWI.forecast[1,1,])))


# For FFWI
for (ii in seq(length(lon_FFWI))){
  for (jj in seq(length(lat_FFWI))){
    
    # Without rescaling: Yin et al. (2017)
    data1 <- FWI.train.norm[ii,jj,]
    data2 <- FPI.train.norm[ii,jj,]
    data3 <- FFWI.train.norm[ii,jj,]
    
    data1[sapply(data1, is.infinite)] <- NA
    data2[sapply(data2, is.infinite)] <- NA
    data3[sapply(data3, is.infinite)] <- NA
    
    var_data1 <- mean((data1-data2)*(data1-data3),na.rm=TRUE)
    var_data2 <- mean((data2-data1)*(data2-data3),na.rm=TRUE)
    var_data3 <- mean((data3-data1)*(data3-data2),na.rm=TRUE)
    
    rmse_data1 <- 1/var_data1
    rmse_data2 <- 1/var_data2
    rmse_data3 <- 1/var_data3
    
    sum_weight <- rmse_data1+rmse_data2+rmse_data3
    weight1 <- var_data2*var_data3/sum_weight
    weight2 <- var_data1*var_data3/sum_weight
    weight3 <- var_data1*var_data2/sum_weight
    
    index_TC.train.FFWI[ii,jj,] <- weight1*FWI.train.norm[ii,jj,] + weight2*FPI.train.norm[ii,jj,] + weight3*FFWI.train.norm[ii,jj,]
    index_TC.forecast.FFWI[ii,jj,] <- weight1*FWI.forecast.norm[ii,jj,] + weight2*FPI.forecast.norm[ii,jj,] + weight3*FFWI.forecast.norm[ii,jj,]
    
    # With rescaling: Yin and Park (2021)
    S1 <- data2
    S2 <- data3
    S3 <- data1
    
    S1_star <- S1
    meanS1 <- mean(S1,na.rm=TRUE)
    meanS2 <- mean(S2,na.rm=TRUE)
    meanS3 <- mean(S3,na.rm=TRUE)
    
    beta2_star <- mean((S1-meanS1)*(S3-meanS3),na.rm=TRUE)/mean((S2-meanS2)*(S3-meanS3),na.rm=TRUE)
    beta3_star <- mean((S1-meanS1)*(S2-meanS2),na.rm=TRUE)/mean((S3-meanS3)*(S2-meanS2),na.rm=TRUE)
    
    S2_star <- beta2_star*(S2-meanS2)+meanS1
    S3_star <- beta3_star*(S3-meanS3)+meanS1
    
    var1 <- mean((S1_star-S2_star)*(S1_star-S3_star),na.rm=TRUE)
    var2 <- mean((S2_star-S1_star)*(S2_star-S3_star),na.rm=TRUE)
    var3 <- mean((S3_star-S1_star)*(S3_star-S2_star),na.rm=TRUE)
    
    sum_w <- var1*var2+var1*var3+var2*var3
    w1 <- var2*var3/sum_w
    w2 <- var1*var3/sum_w
    w3 <- var1*var2/sum_w
    
    index_TC.re.train.FFWI[ii,jj,] <- w1*FWI.train.norm[ii,jj,] + w2*FPI.train.norm[ii,jj,] + w3*FFWI.train.norm[ii,jj,]
    index_TC.re.forecast.FFWI[ii,jj,] <- w1*FPI.forecast.norm[ii,jj,] + w2*FFWI.forecast.norm[ii,jj,] + w3*FWI.forecast.norm[ii,jj,]
    
  }
}


# For HDWI
for (ii in seq(length(lon_HDWI))){
  for (jj in seq(length(lat_HDWI))){
    
    # Without rescaling: Yin et al. (2017)
    data1 <- FWI.train.norm[ii,jj,]
    data2 <- FPI.train.norm[ii,jj,]
    data3 <- HDWI.train.norm[ii,jj,]
    
    data1[sapply(data1, is.infinite)] <- NA
    data2[sapply(data2, is.infinite)] <- NA
    data3[sapply(data3, is.infinite)] <- NA
    
    var_data1 <- mean((data1-data2)*(data1-data3),na.rm=TRUE)
    var_data2 <- mean((data2-data1)*(data2-data3),na.rm=TRUE)
    var_data3 <- mean((data3-data1)*(data3-data2),na.rm=TRUE)
    
    rmse_data1 <- 1/var_data1
    rmse_data2 <- 1/var_data2
    rmse_data3 <- 1/var_data3
    
    sum_weight <- rmse_data1+rmse_data2+rmse_data3
    weight1 <- var_data2*var_data3/sum_weight
    weight2 <- var_data1*var_data3/sum_weight
    weight3 <- var_data1*var_data2/sum_weight
    
    index_TC.train.HDWI[ii,jj,] <- weight1*FWI.train.norm[ii,jj,] + weight2*FPI.train.norm[ii,jj,] + weight3*HDWI.train.norm[ii,jj,]
    index_TC.forecast.HDWI[ii,jj,] <- weight1*FWI.forecast.norm[ii,jj,] + weight2*FPI.forecast.norm[ii,jj,] + weight3*HDWI.forecast.norm[ii,jj,]
    
    # With rescaling: Yin and Park (2021)
    S1 <- data2
    S2 <- data3
    S3 <- data1
    
    S1_star <- S1
    meanS1 <- mean(S1,na.rm=TRUE)
    meanS2 <- mean(S2,na.rm=TRUE)
    meanS3 <- mean(S3,na.rm=TRUE)
    
    beta2_star <- mean((S1-meanS1)*(S3-meanS3),na.rm=TRUE)/mean((S2-meanS2)*(S3-meanS3),na.rm=TRUE)
    beta3_star <- mean((S1-meanS1)*(S2-meanS2),na.rm=TRUE)/mean((S3-meanS3)*(S2-meanS2),na.rm=TRUE)
    
    S2_star <- beta2_star*(S2-meanS2)+meanS1
    S3_star <- beta3_star*(S3-meanS3)+meanS1
    
    var1 <- mean((S1_star-S2_star)*(S1_star-S3_star),na.rm=TRUE)
    var2 <- mean((S2_star-S1_star)*(S2_star-S3_star),na.rm=TRUE)
    var3 <- mean((S3_star-S1_star)*(S3_star-S2_star),na.rm=TRUE)
    
    sum_w <- var1*var2+var1*var3+var2*var3
    w1 <- var2*var3/sum_w
    w2 <- var1*var3/sum_w
    w3 <- var1*var2/sum_w
    
    index_TC.re.train.HDWI[ii,jj,] <- w1*FWI.train.norm[ii,jj,] + w2*FPI.train.norm[ii,jj,] + w3*HDWI.train.norm[ii,jj,]
    index_TC.re.forecast.HDWI[ii,jj,] <- w1*FPI.forecast.norm[ii,jj,] + w2*HDWI.forecast.norm[ii,jj,] + w3*FWI.forecast.norm[ii,jj,]
    
  }
}


# For MNI
for (ii in seq(length(lon_MNI))){
  for (jj in seq(length(lat_MNI))){
    
    # Without rescaling: Yin et al. (2017)
    data1 <- FWI.train.norm[ii,jj,]
    data2 <- FPI.train.norm[ii,jj,]
    data3 <- MNI.train.norm[ii,jj,]
    
    data1[sapply(data1, is.infinite)] <- NA
    data2[sapply(data2, is.infinite)] <- NA
    data3[sapply(data3, is.infinite)] <- NA
    
    var_data1 <- mean((data1-data2)*(data1-data3),na.rm=TRUE)
    var_data2 <- mean((data2-data1)*(data2-data3),na.rm=TRUE)
    var_data3 <- mean((data3-data1)*(data3-data2),na.rm=TRUE)
    
    rmse_data1 <- 1/var_data1
    rmse_data2 <- 1/var_data2
    rmse_data3 <- 1/var_data3
    
    sum_weight <- rmse_data1+rmse_data2+rmse_data3
    weight1 <- var_data2*var_data3/sum_weight
    weight2 <- var_data1*var_data3/sum_weight
    weight3 <- var_data1*var_data2/sum_weight
    
    index_TC.train.MNI[ii,jj,] <- weight1*FWI.train.norm[ii,jj,] + weight2*FPI.train.norm[ii,jj,] + weight3*MNI.train.norm[ii,jj,]
    index_TC.forecast.MNI[ii,jj,] <- weight1*FWI.forecast.norm[ii,jj,] + weight2*FPI.forecast.norm[ii,jj,] + weight3*MNI.forecast.norm[ii,jj,]
    
    # With rescaling: Yin and Park (2021)
    S1 <- data2
    S2 <- data3
    S3 <- data1
    
    S1_star <- S1
    meanS1 <- mean(S1,na.rm=TRUE)
    meanS2 <- mean(S2,na.rm=TRUE)
    meanS3 <- mean(S3,na.rm=TRUE)
    
    beta2_star <- mean((S1-meanS1)*(S3-meanS3),na.rm=TRUE)/mean((S2-meanS2)*(S3-meanS3),na.rm=TRUE)
    beta3_star <- mean((S1-meanS1)*(S2-meanS2),na.rm=TRUE)/mean((S3-meanS3)*(S2-meanS2),na.rm=TRUE)
    
    S2_star <- beta2_star*(S2-meanS2)+meanS1
    S3_star <- beta3_star*(S3-meanS3)+meanS1
    
    var1 <- mean((S1_star-S2_star)*(S1_star-S3_star),na.rm=TRUE)
    var2 <- mean((S2_star-S1_star)*(S2_star-S3_star),na.rm=TRUE)
    var3 <- mean((S3_star-S1_star)*(S3_star-S2_star),na.rm=TRUE)
    
    sum_w <- var1*var2+var1*var3+var2*var3
    w1 <- var2*var3/sum_w
    w2 <- var1*var3/sum_w
    w3 <- var1*var2/sum_w
    
    index_TC.re.train.MNI[ii,jj,] <- w1*FWI.train.norm[ii,jj,] + w2*FPI.train.norm[ii,jj,] + w3*MNI.train.norm[ii,jj,]
    index_TC.re.forecast.MNI[ii,jj,] <- w1*FPI.forecast.norm[ii,jj,] + w2*MNI.forecast.norm[ii,jj,] + w3*FWI.forecast.norm[ii,jj,]
    
  }
}

 
############ NC output #####################
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Ensemble")

ncfname_train <- "Ensemble_training.nc"
ncfname_forecast <- "Ensemble_forecast.nc"

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon_FFWI))
latdim <- ncdim_def("lat","degrees_north",as.double(lat_FFWI))
timedim.train <- ncdim_def("time","day",as.double(seq(length(FFWI.train[1,1,]))))
timedim.forecast <- ncdim_def("time","day",as.double(seq(length(FFWI.forecast[1,1,]))))

# define variables
fillvalue <- NA

FFWI.train_def <- ncvar_def("FFWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
FFWI.norm.train_def <- ncvar_def("FFWI.norm","",list(londim,latdim,timedim.train),fillvalue,prec="double")
FPI.train_def <- ncvar_def("FPI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
FPI.norm.train_def <- ncvar_def("FPI.norm","",list(londim,latdim,timedim.train),fillvalue,prec="double")
FWI.train_def <- ncvar_def("FWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
FWI.norm.train_def <- ncvar_def("FWI.norm","",list(londim,latdim,timedim.train),fillvalue,prec="double")
HDWI.train_def <- ncvar_def("HDWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
HDWI.norm.train_def <- ncvar_def("HDWI.norm","",list(londim,latdim,timedim.train),fillvalue,prec="double")
MNI.train_def <- ncvar_def("MNI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
MNI.norm.train_def <- ncvar_def("MNI.norm","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_VC.All.train_def <- ncvar_def("Index_VC.All","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_VC.Clim.train_def <- ncvar_def("Index_VC.Clim","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_VC.noFWI.train_def <- ncvar_def("Index_VC.noFWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.FFWI.train_def <- ncvar_def("Index_TC.FFWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.HDWI.train_def <- ncvar_def("Index_TC.HDWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.MNI.train_def <- ncvar_def("Index_TC.MNI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.FFWI.re.train_def <- ncvar_def("Index_TC.rescale.FFWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.HDWI.re.train_def <- ncvar_def("Index_TC.rescale.HDWI","",list(londim,latdim,timedim.train),fillvalue,prec="double")
Index_TC.MNI.re.train_def <- ncvar_def("Index_TC.rescale.MNI","",list(londim,latdim,timedim.train),fillvalue,prec="double")

FFWI.forecast_def <- ncvar_def("FFWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
FFWI.norm.forecast_def <- ncvar_def("FFWI.norm","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
FPI.forecast_def <- ncvar_def("FPI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
FPI.norm.forecast_def <- ncvar_def("FPI.norm","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
FWI.forecast_def <- ncvar_def("FWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
FWI.norm.forecast_def <- ncvar_def("FWI.norm","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
HDWI.forecast_def <- ncvar_def("HDWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
HDWI.norm.forecast_def <- ncvar_def("HDWI.norm","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
MNI.forecast_def <- ncvar_def("MNI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
MNI.norm.forecast_def <- ncvar_def("MNI.norm","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_VC.All.forecast_def <- ncvar_def("Index_VC.All","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_VC.Clim.forecast_def <- ncvar_def("Index_VC.Clim","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_VC.noFWI.forecast_def <- ncvar_def("Index_VC.noFWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.FFWI.forecast_def <- ncvar_def("Index_TC.FFWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.HDWI.forecast_def <- ncvar_def("Index_TC.HDWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.MNI.forecast_def <- ncvar_def("Index_TC.MNI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.FFWI.re.forecast_def <- ncvar_def("Index_TC.rescale.FFWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.HDWI.re.forecast_def <- ncvar_def("Index_TC.rescale.HDWI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")
Index_TC.MNI.re.forecast_def <- ncvar_def("Index_TC.rescale.MNI","",list(londim,latdim,timedim.forecast),fillvalue,prec="double")

# create netCDF file and put arrays

ncout_train <- nc_create(ncfname_train,list(FFWI.train_def,FFWI.norm.train_def,
                                            FPI.train_def,FPI.norm.train_def,
                                            FWI.train_def,FWI.norm.train_def,
                                            HDWI.train_def,HDWI.norm.train_def,
                                            MNI.train_def,MNI.norm.train_def,
                                            Index_VC.All.train_def,
                                            Index_VC.Clim.train_def,
                                            Index_VC.noFWI.train_def,
                                            Index_TC.FFWI.train_def,
                                            Index_TC.HDWI.train_def,
                                            Index_TC.MNI.train_def,
                                            Index_TC.FFWI.re.train_def,
                                            Index_TC.HDWI.re.train_def,
                                            Index_TC.MNI.re.train_def),force_v4=FALSE)

ncout_forecast <- nc_create(ncfname_forecast,list(FFWI.forecast_def,FFWI.norm.forecast_def,
                                            FPI.forecast_def,FPI.norm.forecast_def,
                                            FWI.forecast_def,FWI.norm.forecast_def,
                                            HDWI.forecast_def,HDWI.norm.forecast_def,
                                            MNI.forecast_def,MNI.norm.forecast_def,
                                            Index_VC.All.forecast_def,
                                            Index_VC.Clim.forecast_def,
                                            Index_VC.noFWI.forecast_def,
                                            Index_TC.FFWI.forecast_def,
                                            Index_TC.HDWI.forecast_def,
                                            Index_TC.MNI.forecast_def,
                                            Index_TC.FFWI.re.forecast_def,
                                            Index_TC.HDWI.re.forecast_def,
                                            Index_TC.MNI.re.forecast_def),force_v4=FALSE)

ncvar_put(ncout_train,FFWI.train_def,FFWI.train)
ncvar_put(ncout_train,FFWI.norm.train_def,FFWI.train.norm)
ncvar_put(ncout_train,FPI.train_def,FPI.train)
ncvar_put(ncout_train,FPI.norm.train_def,FPI.train.norm)
ncvar_put(ncout_train,FWI.train_def,FWI.train_re)
ncvar_put(ncout_train,FWI.norm.train_def,FWI.train.norm)
ncvar_put(ncout_train,HDWI.train_def,HDWI.train)
ncvar_put(ncout_train,HDWI.norm.train_def,HDWI.train.norm)
ncvar_put(ncout_train,MNI.train_def,MNI.train)
ncvar_put(ncout_train,MNI.norm.train_def,MNI.train.norm)
ncvar_put(ncout_train,Index_VC.All.train_def,index_VC.All.train)
ncvar_put(ncout_train,Index_VC.Clim.train_def,index_VC.Clim.train)
ncvar_put(ncout_train,Index_VC.noFWI.train_def,index_VC.noFWI.train)
ncvar_put(ncout_train,Index_TC.FFWI.train_def,index_TC.train.FFWI)
ncvar_put(ncout_train,Index_TC.HDWI.train_def,index_TC.train.HDWI)
ncvar_put(ncout_train,Index_TC.MNI.train_def,index_TC.train.MNI)
ncvar_put(ncout_train,Index_TC.FFWI.re.train_def,index_TC.re.train.FFWI)
ncvar_put(ncout_train,Index_TC.HDWI.re.train_def,index_TC.re.train.HDWI)
ncvar_put(ncout_train,Index_TC.MNI.re.train_def,index_TC.re.train.MNI)

ncvar_put(ncout_forecast,FFWI.forecast_def,FFWI.forecast)
ncvar_put(ncout_forecast,FFWI.norm.forecast_def,FFWI.forecast.norm)
ncvar_put(ncout_forecast,FPI.forecast_def,FPI.forecast)
ncvar_put(ncout_forecast,FPI.norm.forecast_def,FPI.forecast.norm)
ncvar_put(ncout_forecast,FWI.forecast_def,FWI.forecast_re)
ncvar_put(ncout_forecast,FWI.norm.forecast_def,FWI.forecast.norm)
ncvar_put(ncout_forecast,HDWI.forecast_def,HDWI.forecast)
ncvar_put(ncout_forecast,HDWI.norm.forecast_def,HDWI.forecast.norm)
ncvar_put(ncout_forecast,MNI.forecast_def,MNI.forecast)
ncvar_put(ncout_forecast,MNI.norm.forecast_def,MNI.forecast.norm)
ncvar_put(ncout_forecast,Index_VC.All.forecast_def,index_VC.All.forecast)
ncvar_put(ncout_forecast,Index_VC.Clim.forecast_def,index_VC.Clim.forecast)
ncvar_put(ncout_forecast,Index_VC.noFWI.forecast_def,index_VC.noFWI.forecast)
ncvar_put(ncout_forecast,Index_TC.FFWI.forecast_def,index_TC.forecast.FFWI)
ncvar_put(ncout_forecast,Index_TC.HDWI.forecast_def,index_TC.forecast.HDWI)
ncvar_put(ncout_forecast,Index_TC.MNI.forecast_def,index_TC.forecast.MNI)
ncvar_put(ncout_forecast,Index_TC.FFWI.re.forecast_def,index_TC.re.forecast.FFWI)
ncvar_put(ncout_forecast,Index_TC.HDWI.re.forecast_def,index_TC.re.forecast.HDWI)
ncvar_put(ncout_forecast,Index_TC.MNI.re.forecast_def,index_TC.re.forecast.MNI)

nc_close(ncout_train)
nc_close(ncout_forecast)


