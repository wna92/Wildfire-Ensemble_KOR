###### Packages ########
list.of.packages <- c(
  "ncdf4", "viridis", "readxl","writexl"
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

#### Change index to others (ex. FFWI, HDWI, ...)
Index <- "FWI"

# Output file setting
Inven_out <- matrix(NA,nrow=1,ncol=20)
colnames(Inven_out) <-c("Year", "Month", "Day", "Time", "진화종료시간_년", "진화종료시간_월", "진화종료시간_일",
                        "진화종료시간_시간", "발생장소_관서", "발생장소_시도", "발생장소_시군구", "발생장소_읍면",
                        "발생장소_동리", "Latitude", "Longitude", "Area", "index", "date_order", "lat_order", "lon_order")
Inven_out <- Inven_out[-c(1), ]

######### Inventory for a given year #########
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis")
Inven <- read_excel("Wildfires_2014-2023_re.xlsx")

setwd ("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Data\\FWI")

raw_FWI <- nc_open("FWI.GEOS-5.Daily_Kor_2022.nc")

lon_FWI <- ncvar_get(raw_FWI, "lon")
lat_FWI <- ncvar_get(raw_FWI, "lat")
data_FWI <- ncvar_get(raw_FWI, "FWI")

############ All years #########

for (year in 2014:2023){

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

  # For FWI
  for (ii in seq(nrow(Inven_sub))){

    Inven_sub$lon_order[ii] <- order(abs(lon_FWI-Inven_sub$Longitude[ii]))[1]
    Inven_sub$lat_order[ii] <- order(abs(lat_FWI-Inven_sub$Latitude[ii]))[1]

  }


  ######### Extract indices #########
  setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis\\Index")

  # For FWI
  raw_index <- nc_open(paste0(Index,".GEOS-5.Daily_Kor_",year,".nc"))

  # lon <- ncvar_get(raw_index, "lon")
  # lat <- ncvar_get(raw_index, "lat")
  # time <- ncvar_get(raw_index, "time")

  index_read <- ncvar_get(raw_index, Index)

  for (ii in seq(nrow(Inven_sub))){
    if (Inven_sub$lon_order[ii] >= 1 && Inven_sub$lat_order[ii] >= 1){
      Inven_sub$index[ii] <- index_read[Inven_sub$lon_order[ii],Inven_sub$lat_order[ii],Inven_sub$date_order[ii]]
    }
  }

  Inven_out <- rbind(Inven_out,Inven_sub)

}

######### Output export #######
setwd("Z:\\Research\\(2023) Wildfire-Flood-Landslide\\Analysis")

write_xlsx(Inven_out,paste0(Index,"_result_2014-2023.xlsx"))


