#-------------------------------------
#Clean-up and append LISTER files.
#For the OSTRICH 2014 cruise, there are four summary files containing all data. Data is binned every 10-minutes.

#"WS1406-full-vdl.dat" has 28 May (10:59:51) - 30 May (16:24:58)
#"WS1406-full-2-vdl.dat" has 30 May (16:50:48) - 30 May (17:35:41)
#"WS1406-full-3-vdl.dat" has 30 May (19:43:58) - 31 May (00:10:04)
#"WS1406-full-4-vdl.dat" has 31 May (00:57:17) - 14 June (22:25:34)

#Gaps in summary files for 28 May - 31 May so will use individual day files "20140514800WS1406-dly-vdl.dat", "20140514900WS1406-dly-vdl.dat",
#"20140515000WS1406-dly-vdl.dat" and "20140515100WS1406-dly-vdl.dat"

#By: Kelly Robinson, Oregon State University
#Created: 17 February 2015
#Last modified: 17 Feburary 2015
#-------------------------------------

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/vids/Lister")

options(digits = 6) 

#STEP 1: Read in the file for 28 May 2014, change date format to YYYY-MM-DD, Remove redundant latitude and longitude fields

  d <- read.table("20140514800WS1406-dly-vdl.dat", skip = 2, sep = "\t", header = FALSE)
  
  #Assign colunm names because some headers (especially those related to the Tuner C7 sensor) are missing from original file.
  colnames(d) <- c("Date", "Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","GPS1_LatDecDeg",
                   "GPS1_LonDecDeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","GPS2_LatDecDeg","GPS2_LonDecDeg",
                   "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                   "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                   "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                   "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","POSMV_LatDecDeg","POSMV_LonDecDeg","POSMV_Heading_degreesTRUE",
                   "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                   "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                   "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                   "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC","x1","x2")
  
  df <- as.data.frame(d)
  
  #Drop extra fields "x1" and "x2" at the end
  df$x1 <- NULL
  df$x2 <- NULL

  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df$date <- as.Date(df$Date, "%d %B %Y")
  df$Date_Time <- NA
  df$Date_Time <- paste(df$date, df$Time, sep=' ')
  df$Date_Time <- as.POSIXct(df$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT")
  df$Date <- NULL
 
  #Convert GPS1 latitude and longitude to numeric values
  df$gps1latdecdeg <- gsub(" -  ", "", df$GPS1_LatDecDeg)
  df$gps1latdecdeg <- str_sub(df$gps1latdecdeg, 1, -2) #remove space at the end of the character string
  df$gps1latdecdeg <- as.numeric(df$gps1latdecdeg)
  y <- na.omit(df$GPS1_LatDir)
  ifelse(y == " N " , df$gps1latdecdeg, df$gps1latdecdeg*-1)

  df$gps1londecdeg <- gsub(" - ", "", df$GPS1_LonDecDeg)
  df$gps1londecdeg <- str_sub(df$gps1londecdeg, 1, -2) #remove space at the end of the character string
  df$gps1londecdeg <- as.numeric(df$gps1londecdeg)
  ifelse(df$GPS1_LonDir == " W ", df$gps1londecdeg*-1, df$gps1londecdeg)
  
  #Convert GPS2 latitude and longitude to numeric values
  y2 <- na.omit(df$GPS2_LatDir)  
  df$gps2latdecdeg <- gsub(" + ", "", df$GPS2_LatDecDeg, fixed = TRUE)
  df$gps2latdecdeg <- str_sub(df$gps2latdecdeg, 1, -2) #remove space at the end of the character string
  df$gps2latdecdeg <- as.numeric(df$gps2latdecdeg)
  df$gps2latdecdeg <- ifelse(y2 == " N ", df$gps2latdecdeg, df$gps2latdecdeg*-1)
  
  y3 <- na.omit(df$GPS2_LonDir)  
  df$gps2londecdeg <- gsub(" - ", "", df$GPS2_LonDecDeg)
  df$gps2londecdeg <- str_sub(df$gps2londecdeg, 1, -2) #remove space at the end of the character string
  df$gps2londecdeg <- as.numeric(df$gps2londecdeg)
  df$gps2londecdeg <- ifelse(y3 == " W ", df$gps2londecdeg*-1, df$gps2londecdeg)

  #Convert POSMV latitude and longitude to numeric values
  y4 <- na.omit(df$POSMV_LatDir)  
  df$posmvlatdecdeg <- gsub(" + ", "", df$POSMV_LatDecDeg, fixed = TRUE)
  df$posmvlatdecdeg <- str_sub(df$posmvlatdecdeg, 1, -2) #remove space at the end of the character string
  df$posmvlatdecdeg <- as.numeric(df$posmvlatdecdeg)
  df$posmvlatdecdeg <- ifelse(y4 == " N " , df$posmvlatdecdeg, df$posmvlatdecdeg*-1)
    
  y5 <- na.omit(df$POSMV_LonDir)
  df$posmvlondecdeg <- gsub(" - ", "", df$POSMV_LonDecDeg)
  df$posmvlondecdeg <- str_sub(df$posmvlondecdeg, 1, -2) #remove space at the end of the character string
  df$posmvlondecdeg <- as.numeric(df$posmvlondecdeg)
  df$posmvlondecdeg <- ifelse(y5 == " W ", df$posmvlondecdeg*-1, df$posmvlondecdeg)
  
  #Remove original (now redundant) fields
  df$GPS1_LatDecDeg <- NULL
  df$GPS1_LonDecDeg <- NULL
  df$GPS2_LatDecDeg <- NULL
  df$GPS2_LonDecDeg <- NULL
  df$POSMV_LatDecDeg <- NULL
  df$POSMV_LonDecDeg <- NULL

  #Re-order the columns
  df <- df[c("date", "Time", "Date_Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","gps1latdecdeg",
              "gps1londecdeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","gps2latdecdeg","gps2londecdeg",
                 "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                 "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                 "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                 "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","posmvlatdecdeg","posmvlondecdeg","POSMV_Heading_degreesTRUE",
                 "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                 "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                 "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                 "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")]

#STEP 2: Read in file for 29 May 2014, change date format to YYYY-MM-DD, Remove redundant latitude and longitude fields

  d2 <- read.table("20140514900WS1406-dly-vdl.dat", skip = 2, sep = "\t", header = FALSE)
  
  #Assign colunm names because some headers (especially those related to the Tuner C7 sensor) are missing from original file.
  colnames(d2) <- c("Date", "Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","GPS1_LatDecDeg",
                   "GPS1_LonDecDeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","GPS2_LatDecDeg","GPS2_LonDecDeg",
                   "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                   "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                   "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                   "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","POSMV_LatDecDeg","POSMV_LonDecDeg","POSMV_Heading_degreesTRUE",
                   "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                   "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                   "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                   "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC","x1","x2")
  
  df2 <- as.data.frame(d2)
  
  #Drop extra fields "x1" and "x2" at the end
  df2$x1 <- NULL
  df2$x2 <- NULL
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df2$date <- as.Date(df2$Date, format = "%d %B %Y")
  df2$Date_Time <- NA
  df2$Date_Time <- paste(df2$date, df2$Time, sep=' ')
  df2$Date_Time <- as.POSIXct(df2$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT")
  df2$Date <- NULL
  
  #Convert GPS1 latitude and longitude to numeric values
  y6 <- na.omit(df2$GPS1_LatDir)
  df2$gps1latdecdeg <- gsub(" -  ", "", df2$GPS1_LatDecDeg)
  df2$gps1latdecdeg <- str_sub(df2$gps1latdecdeg, 1, -2) #remove space at the end of the character string
  df2$gps1latdecdeg <- as.numeric(df2$gps1latdecdeg)
  df2$gps1latdecdeg <- ifelse(y6 == " N " , df2$gps1latdecdeg, df2$gps1latdecdeg*-1)
  
  y7 <- na.omit(df2$GPS1_LontDir)
  df2$gps1londecdeg <- gsub(" - ", "", df2$GPS1_LonDecDeg)
  df2$gps1londecdeg <- str_sub(df2$gps1londecdeg, 1, -2) #remove space at the end of the character string
  df2$gps1londecdeg <- as.numeric(df2$gps1londecdeg)
  df2$gps1londecdeg <- ifelse(y7 == " W ", df2$gps1londecdeg*-1, df2$gps1londecdeg)
  
  #Convert GPS2 latitude and longitude to numeric values
  y8 <- na.omit(df2$GPS2_LatDir)  
  df2$gps2latdecdeg <- gsub(" + ", "", df2$GPS2_LatDecDeg, fixed = TRUE)
  df2$gps2latdecdeg <- str_sub(df2$gps2latdecdeg, 1, -2) #remove space at the end of the character string
  df2$gps2latdecdeg <- as.numeric(df2$gps2latdecdeg)
  df2$gps2latdecdeg <- ifelse(y8 == " N ", df2$gps2latdecdeg, df2$gps2latdecdeg*-1)
  
  y9 <- na.omit(df2$GPS2_LonDir)  
  df2$gps2londecdeg <- gsub(" - ", "", df2$GPS2_LonDecDeg)
  df2$gps2londecdeg <- str_sub(df2$gps2londecdeg, 1, -2) #remove space at the end of the character string
  df2$gps2londecdeg <- as.numeric(df2$gps2londecdeg)
  df2$gps2londecdeg <- ifelse(y9 == " W ", df2$gps2londecdeg*-1, df2$gps2londecdeg)
  
  #Convert POSMV latitude and longitude to numeric values
  y10 <- na.omit(df2$POSMV_LatDir)  
  df2$posmvlatdecdeg <- gsub(" + ", "", df2$POSMV_LatDecDeg, fixed = TRUE)
  df2$posmvlatdecdeg <- str_sub(df2$posmvlatdecdeg, 1, -2) #remove space at the end of the character string
  df2$posmvlatdecdeg <- as.numeric(df2$posmvlatdecdeg)
  df2$posmvlatdecdeg <- ifelse(y10 == " N " , df2$posmvlatdecdeg, df2$posmvlatdecdeg*-1)
  
  y11 <- na.omit(df2$POSMV_LonDir)
  df2$posmvlondecdeg <- gsub(" - ", "", df2$POSMV_LonDecDeg)
  df2$posmvlondecdeg <- str_sub(df2$posmvlondecdeg, 1, -2) #remove space at the end of the character string
  df2$posmvlondecdeg <- as.numeric(df2$posmvlondecdeg)
  df2$posmvlondecdeg <- ifelse(y11 == " W ", df2$posmvlondecdeg*-1, df2$posmvlondecdeg)
  
  #Remove original (now redundant) fields
  df2$GPS1_LatDecDeg <- NULL
  df2$GPS1_LonDecDeg <- NULL
  df2$GPS2_LatDecDeg <- NULL
  df2$GPS2_LonDecDeg <- NULL
  df2$POSMV_LatDecDeg <- NULL
  df2$POSMV_LonDecDeg <- NULL
  
  #Re-order the columns
  df2 <- df2[c("date", "Time", "Date_Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","gps1latdecdeg",
             "gps1londecdeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","gps2latdecdeg","gps2londecdeg",
             "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
             "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
             "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
             "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","posmvlatdecdeg","posmvlondecdeg","POSMV_Heading_degreesTRUE",
             "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
             "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
             "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
             "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")]


#STEP 3: Read in file for 30 May 2014, change date format to YYYY-MM-DD, Remove redundant latitude and longitude fields
  
  options(digits = 6)

  d3 <- read.table("20140515000WS1406-dly-vdl_kr.csv", sep = ",", skip = 1, header = FALSE)
  
  #Assign colunm names because some headers (especially those related to the Tuner C7 sensor) are missing from original file.
  colnames(d3) <- c("Date", "Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","GPS1_LatDecDeg",
                    "GPS1_LonDecDeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","GPS2_LatDecDeg","GPS2_LonDecDeg",
                    "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                    "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                    "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                    "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","POSMV_LatDecDeg","POSMV_LonDecDeg","POSMV_Heading_degreesTRUE",
                    "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                    "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                    "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                    "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")
  
  df3 <- as.data.frame(d3)
  
  #Drop extra fields "x1" and "x2" at the end
  df3$x1 <- NULL
  df3$x2 <- NULL
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df3$date <- as.Date(df3$Date, format = "%m/%d/%Y")
  df3$Date_Time <- NA
  df3$Date_Time <- paste(df3$date, df3$Time, sep=' ')
  df3$Date_Time <- as.POSIXct(df3$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT" )
  df3$Date <- NULL
  
  #Convert GPS1 latitude and longitude to numeric values
  #df3$gps1latdecdeg <- gsub(" -  ", "", df3$GPS1_LatDecDeg)
  #df3$gps1latdecdeg <- str_sub(df3$gps1latdecdeg, 1, -2) #remove space at the end of the character string
  #df3$gps1latdecdeg <- as.numeric(df3$gps1latdecdeg)
  #y <- na.omit(df3$GPS1_LatDir)
  #ifelse(y == " N " , df3$gps1latdecdeg, df3$gps1latdecdeg*-1)
  
  #df3$gps1londecdeg <- gsub(" - ", "", df3$GPS1_LonDecDeg)
  #df3$gps1londecdeg <- str_sub(df3$gps1londecdeg, 1, -2) #remove space at the end of the character string
  #df3$gps1londecdeg <- as.numeric(df3$gps1londecdeg)
  #ifelse(df3$GPS1_LonDir == " W ", df3$gps1londecdeg*-1, df3$gps1londecdeg)
  
  #Convert GPS2 latitude and longitude to numeric values
  # df3$gps2latdecdeg <- gsub(" + ", "", df3$GPS2_LatDecDeg, fixed = TRUE)
  #df3$gps2latdecdeg <- str_sub(df3$gps2latdecdeg, 1, -2) #remove space at the end of the character string
  #df3$gps2latdecdeg <- as.numeric(df3$gps2latdecdeg)
  #df3$gps2latdecdeg <- ifelse(y2 == " N ", df3$gps2latdecdeg, df3$gps2latdecdeg*-1)
  
  #y3 <- na.omit(df3$GPS2_LonDir)  
  #df3$gps2londecdeg <- gsub(" - ", "", df3$GPS2_LonDecDeg)
  #df3$gps2londecdeg <- str_sub(df3$gps2londecdeg, 1, -2) #remove space at the end of the character string
  #df3$gps2londecdeg <- as.numeric(df3$gps2londecdeg)
  #df3$gps2londecdeg <- ifelse(y3 == " W ", df3$gps2londecdeg*-1, df3$gps2londecdeg)
  
  #Convert POSMV latitude and longitude to numeric values
  #y4 <- na.omit(df3$POSMV_LatDir)  
  #df3$posmvlatdecdeg <- gsub(" + ", "", df3$POSMV_LatDecDeg, fixed = TRUE)
  #df3$posmvlatdecdeg <- str_sub(df3$posmvlatdecdeg, 1, -2) #remove space at the end of the character string
  #df3$posmvlatdecdeg <- as.numeric(df3$posmvlatdecdeg)
  #df3$posmvlatdecdeg <- ifelse(y4 == " N " , df3$posmvlatdecdeg, df3$posmvlatdecdeg*-1)
  
  #y5 <- na.omit(df3$POSMV_LonDir)
  #df3$posmvlondecdeg <- gsub(" - ", "", df3$POSMV_LonDecDeg)
  #df3$posmvlondecdeg <- str_sub(df3$posmvlondecdeg, 1, -2) #remove space at the end of the character string
  #df3$posmvlondecdeg <- as.numeric(df3$posmvlondecdeg)
  #df3$posmvlondecdeg <- ifelse(y5 == " W ", df3$posmvlondecdeg*-1, df3$posmvlondecdeg)
  
  #Re-name fields to match other data frames' fields
  df3$gps1latdecdeg <- df3$GPS1_LatDecDeg
  df3$gps1londecdeg <- df3$GPS1_LonDecDeg
  df3$gps2latdecdeg <- df3$GPS2_LatDecDeg
  df3$gps2londecdeg <- df3$GPS2_LonDecDeg
  df3$posmvlatdecdeg <- df3$POSMV_LatDecDeg
  df3$posmvlondecdeg <- df3$POSMV_LonDecDeg
  
  #Re-order the columns
  df3 <- df3[c("date", "Time", "Date_Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","gps1latdecdeg",
               "gps1londecdeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","gps2latdecdeg","gps2londecdeg",
               "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
               "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
               "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
               "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","posmvlatdecdeg","posmvlondecdeg","POSMV_Heading_degreesTRUE",
               "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
               "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
               "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
               "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")]

  #STEP 4: Read in file for 30 May 2014, change date format to YYYY-MM-DD, Remove redundant latitude and longitude fields.
  #Note: Original .dat file contained hea
  
  d4 <- read.table("20140515100WS1406-dly-vdl_kr.csv", skip = 1, sep = ",", header = FALSE)

  options(digits = 6)
  
  #Assign colunm names because some headers (especially those related to the Tuner C7 sensor) are missing from original file.
  colnames(d4) <- c("Date", "Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","GPS1_LatDecDeg",
                    "GPS1_LonDecDeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","GPS2_LatDecDeg","GPS2_LonDecDeg",
                    "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                    "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                    "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                    "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","POSMV_LatDecDeg","POSMV_LonDecDeg","POSMV_Heading_degreesTRUE",
                    "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                    "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                    "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                    "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")
  
  df4 <- as.data.frame(d4)
  
  #Drop extra fields "x1" and "x2" at the end
  df4$x1 <- NULL
  df4$x2 <- NULL
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df4$date <- as.Date(df4$Date, format = "%m/%d/%Y")
  df4$Date_Time <- NA
  df4$Date_Time <- paste(df4$date, df4$Time, sep=' ')
  df4$Date_Time <- as.POSIXct(df4$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT" )
  df4$Date <- NULL
  
#   #Convert GPS1 latitude and longitude to numeric values
#   df4$gps1latdecdeg <- gsub(" -  ", "", df4$GPS1_LatDecDeg)
#   df4$gps1latdecdeg <- str_sub(df4$gps1latdecdeg, 1, -2) #remove space at the end of the character string
#   df4$gps1latdecdeg <- as.numeric(df4$gps1latdecdeg)
#   y <- na.omit(df4$GPS1_LatDir)
#   ifelse(y == " N " , df4$gps1latdecdeg, df4$gps1latdecdeg*-1)
#   
#   df4$gps1londecdeg <- gsub(" - ", "", df4$GPS1_LonDecDeg)
#   df4$gps1londecdeg <- str_sub(df4$gps1londecdeg, 1, -2) #remove space at the end of the character string
#   df4$gps1londecdeg <- as.numeric(df4$gps1londecdeg)
#   ifelse(df4$GPS1_LonDir == " W ", df4$gps1londecdeg*-1, df4$gps1londecdeg)
#   
#   #Convert GPS2 latitude and longitude to numeric values
#   y2 <- na.omit(df4$GPS2_LatDir)  
#   df4$gps2latdecdeg <- gsub(" + ", "", df4$GPS2_LatDecDeg, fixed = TRUE)
#   df4$gps2latdecdeg <- str_sub(df4$gps2latdecdeg, 1, -2) #remove space at the end of the character string
#   df4$gps2latdecdeg <- as.numeric(df4$gps2latdecdeg)
#   df4$gps2latdecdeg <- ifelse(y2 == " N ", df4$gps2latdecdeg, df4$gps2latdecdeg*-1)
#   
#   y3 <- na.omit(df4$GPS2_LonDir)  
#   df4$gps2londecdeg <- gsub(" - ", "", df4$GPS2_LonDecDeg)
#   df4$gps2londecdeg <- str_sub(df4$gps2londecdeg, 1, -2) #remove space at the end of the character string
#   df4$gps2londecdeg <- as.numeric(df4$gps2londecdeg)
#   df4$gps2londecdeg <- ifelse(y3 == " W ", df4$gps2londecdeg*-1, df4$gps2londecdeg)
#   
#   #Convert POSMV latitude and longitude to numeric values
#   y4 <- na.omit(df4$POSMV_LatDir)  
#   df4$posmvlatdecdeg <- gsub(" + ", "", df4$POSMV_LatDecDeg, fixed = TRUE)
#   df4$posmvlatdecdeg <- str_sub(df4$posmvlatdecdeg, 1, -2) #remove space at the end of the character string
#   df4$posmvlatdecdeg <- as.numeric(df4$posmvlatdecdeg)
#   df4$posmvlatdecdeg <- ifelse(y4 == " N " , df4$posmvlatdecdeg, df4$posmvlatdecdeg*-1)
#   
#   y5 <- na.omit(df4$POSMV_LonDir)
#   df4$posmvlondecdeg <- gsub(" - ", "", df4$POSMV_LonDecDeg)
#   df4$posmvlondecdeg <- str_sub(df4$posmvlondecdeg, 1, -2) #remove space at the end of the character string
#   df4$posmvlondecdeg <- as.numeric(df4$posmvlondecdeg)
#   df4$posmvlondecdeg <- ifelse(y5 == " W ", df4$posmvlondecdeg*-1, df4$posmvlondecdeg)
  
  #Re-name fields to match other data frames' fields
  df4$gps1latdecdeg <- df4$GPS1_LatDecDeg
  df4$gps1londecdeg <- df4$GPS1_LonDecDeg
  df4$gps2latdecdeg <- df4$GPS2_LatDecDeg
  df4$gps2londecdeg <- df4$GPS2_LonDecDeg
  df4$posmvlatdecdeg <- df4$POSMV_LatDecDeg
  df4$posmvlondecdeg <- df4$POSMV_LonDecDeg
    
  #Re-order the columns
  df4 <- df4[c("date", "Time", "Date_Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","gps1latdecdeg",
               "gps1londecdeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","gps2latdecdeg","gps2londecdeg",
               "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
               "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
               "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
               "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","posmvlatdecdeg","posmvlondecdeg","POSMV_Heading_degreesTRUE",
               "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
               "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
               "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
               "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")]

  #STEP 5: Read in file for 30 May - 14 June 2014, drop duplicate rows for 30-31 May, change date format to YYYY-MM-DD, Remove redundant latitude and longitude fields
  
  d5 <- read.table("WS1406-full-4-vdl.dat", skip = 8277, sep = "\t", header = FALSE, blank.lines.skip = TRUE)

  options("digits" = 6)
  
  #Assign colunm names because some headers (especially those related to the Tuner C7 sensor) are missing from original file.
  colnames(d5) <- c("Date", "Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","GPS1_LatDecDeg",
                    "GPS1_LonDecDeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","GPS2_LatDecDeg","GPS2_LonDecDeg",
                    "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
                    "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
                    "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
                    "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","POSMV_LatDecDeg","POSMV_LonDecDeg","POSMV_Heading_degreesTRUE",
                    "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
                    "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
                    "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
                    "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC","x1","x2")
  
  df5 <- as.data.frame(d5)
  
  #Drop extra fields "x1" and "x2" at the end
  df5$x1 <- NULL
  df5$x2 <- NULL
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df5$date <- as.Date(df5$Date, "%d %B %Y")
  df5$Date_Time <- NA
  df5$Date_Time <- paste(df5$date, df5$Time, sep=' ')
  df5$Date_Time <- as.POSIXct(df5$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "EST5EDT" )
  df5$Date <- NULL
  
  #Convert GPS1 latitude and longitude to numeric values
  y12 <- na.omit(df5$GPS2_LatDir) #pullling hemisphere from GPS2 as they should always be identical
  df5$gps1latdecdeg <- gsub(" -  ", "", df5$GPS1_LatDecDeg)
  df5$gps1latdecdeg <- str_sub(df5$gps1latdecdeg, 1, -2) #remove space at the end of the character string
  df5$gps1latdecdeg <- as.numeric(df5$gps1latdecdeg)
  df5$gps1latdecdeg <- ifelse(y12 == " N " , df5$gps1latdecdeg, df5$gps1latdecdeg*-1)
  
  y13 <- na.omit(df5$GPS2_LatDir)
  df5$gps1londecdeg <- gsub(" - ", "", df5$GPS2_LonDecDeg)
  df5$gps1londecdeg <- str_sub(df5$gps1londecdeg, 1, -2) #remove space at the end of the character string
  df5$gps1londecdeg <- as.numeric(df5$gps1londecdeg)
  df5$gps1londecdeg <- ifelse(y13 == " W ", df5$gps1londecdeg*-1, df5$gps1londecdeg)
  
  #Convert GPS2 latitude and longitude to numeric values
  y14 <- na.omit(df5$GPS2_LatDir)  
  df5$gps2latdecdeg <- gsub(" + ", "", df5$GPS2_LatDecDeg, fixed = TRUE)
  df5$gps2latdecdeg <- str_sub(df5$gps2latdecdeg, 1, -2) #remove space at the end of the character string
  df5$gps2latdecdeg <- as.numeric(df5$gps2latdecdeg)
  df5$gps2latdecdeg <- ifelse(y14 == " N ", df5$gps2latdecdeg, df5$gps2latdecdeg*-1)
  
  y15 <- na.omit(df5$GPS2_LonDir)  
  df5$gps2londecdeg <- gsub(" - ", "", df5$GPS2_LonDecDeg)
  df5$gps2londecdeg <- str_sub(df5$gps2londecdeg, 1, -2) #remove space at the end of the character string
  df5$gps2londecdeg <- as.numeric(df5$gps2londecdeg)
  df5$gps2londecdeg <- ifelse(y15 == " W ", df5$gps2londecdeg*-1, df5$gps2londecdeg)
  
  #Convert POSMV latitude and longitude to numeric values
  y16 <- na.omit(df5$POSMV_LatDir)  
  df5$posmvlatdecdeg <- gsub(" + ", "", df5$POSMV_LatDecDeg, fixed = TRUE)
  df5$posmvlatdecdeg <- str_sub(df5$posmvlatdecdeg, 1, -2) #remove space at the end of the character string
  df5$posmvlatdecdeg <- as.numeric(df5$posmvlatdecdeg)
  df5$posmvlatdecdeg <- ifelse(y16 == " N " , df5$posmvlatdecdeg, df5$posmvlatdecdeg*-1)
  
  y17 <- na.omit(df5$POSMV_LonDir)
  df5$posmvlondecdeg <- gsub(" - ", "", df5$POSMV_LonDecDeg)
  df5$posmvlondecdeg <- str_sub(df5$posmvlondecdeg, 1, -2) #remove space at the end of the character string
  df5$posmvlondecdeg <- as.numeric(df5$posmvlondecdeg)
  df5$posmvlondecdeg <- ifelse(y17 == " W ", df5$posmvlondecdeg*-1, df5$posmvlondecdeg)
  
  #Remove original (now redundant) fields
  df5$GPS1_LatDecDeg <- NULL
  df5$GPS1_LonDecDeg <- NULL
  df5$GPS2_LatDecDeg <- NULL
  df5$GPS2_LonDecDeg <- NULL
  df5$POSMV_LatDecDeg <- NULL
  df5$POSMV_LonDecDeg <- NULL
  
  #Re-order the columns
  df5 <- df5[c("date", "Time", "Date_Time", "GPS1_Lat", "GPS1_LatDir", "GPS1_Lon", "GPS1_LonDir", "GPS1_SOG_knts","GPS1_COG_DegTrue","gps1latdecdeg",
               "gps1londecdeg", "GPS2_Lat","GPS2_LatDir", "GPS2_Lon", "GPS2_LonDir","GPS2_SOG_knts","GPS2_COG_DegTrue","gps2latdecdeg","gps2londecdeg",
               "Gyro","Bottom_Speed_F/A_knts","P/S_speed_knts","Water_Speed_F/A_knts","P/S_speed_knts2","PIR_w/m^2","PSP_w/m^2","TUV_w/m^2",
               "Rain_Gauge_Cond_Code","Precip_mm/hr","Accum_Precip_mm","Fluorometer_volts","Fluor_Gain","DisOrgMatter_volts","DisOrgMatter_Gain",
               "SBTemp_degreesC","Pump_PVC_flow_LPM","Output_pressure_PSI","PreStrainer_pressure_PSI","PostStrainer_pressure_PSI","POSMV_Lat",
               "POSMV_LatDir","POSMV_Lon","POSMV_LonDir","SOG_knts","POSMV_CoG_degreesTrue","posmvlatdecdeg","posmvlondecdeg","POSMV_Heading_degreesTRUE",
               "Stbd_RM_Young_RelWindSpd_knts", "RelWindDir_deg","TrueWindSpeed_knts","TrueWindDir_deg","Port_RM_Young_MetAirTemp_degC","RelHumidity_%",
               "BarometricPressure_mb","MicroTSG_Temp_degC","MicroTSG_Conductivity_Seimens","MicroTSG_Salinity_psu","TurnerC7_Chl-a_Calib_fluor",
               "TurnerC7_CDOM_calib_fluor", "TurnerC7_turbidity_calib_fluor", "TurnerC7_crude_oil_calib_fluor","TurnerC7_phycoer_calib_fluor", 
               "TurnerC7_phycoc_calib_fluor","TurnerC7_depth_m","WaterTempC")]


#STEP 6: Append data frames
all.df <- rbind(df, df2, df3, df4, df5)

#STEP 7:  Order by date and write new text file
lister <-all.df[order(all.df$date, all.df$Time),]

suppressWarnings(dir.create("processed"))
write.table(lister, paste0("processed/Lister_20140528-20140614.txt"), row.names=FALSE, sep="\t")

options(digits = 6)

d <- read.table("Lister_20140528-20140614.txt", sep = "\t", header = TRUE,check.names = TRUE)
df <- as.data.frame(d)

df$GPS1_Lat <- substr(df$GPS1_Lat, 2, 9)
df$GPS1_Lon <- substr(df$GPS1_Lon, 5, 12)
df$date <- as.Date(df$date)
df$GPS2_Lat <- substr(df$GPS2_Lat, 2, 11)
df$GPS2_LatDir <-substr(df$GPS2_LatDir, 2, 2)
df$GPS2_LonDir <-substr(df$GPS2_LonDir, 2, 2)
df$GPS2_Lon <- substr(df$GPS2_Lon, 4, 13)
df$POSMV_Lat <- substr(df$POSMV_Lat, 1, 10)
df$POSMV_Lon <- substr(df$POSMV_Lon, 4, 13)
df$POSMV_LatDir <- substr(df$POSMV_LatDir, 2, 2)
df$POSMV_LonDir <- substr(df$POSMV_LonDir, 2, 2)

write.table(df, "Lister_20140528-20140614.csv", row.names=FALSE, sep=",")

#Remove fields with all zeros and NAs from table for geodatabase

df$GPS1_Lat <- NULL
df$GPS1_Lon <- NULL
df$GPS1_LatDir <- NULL
df$GPS1_LonDir <-NULL
df$GPS1_SOG_knts <- NULL
df$GPS1_COG_knts <- NULL
df$gps1latdecdeg <- NULL
df$gps1londecdeg <- NULL
df$GPS1_COG_DegTrue <- NULL
df$Bottom_Speed_F.A._knts <- NULL
df$P.S_speed_knts <- NULL
df$Water_Speed_F.A_knts <- NULL

