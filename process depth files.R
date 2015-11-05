
#PROCESS RAW DEPTH FILES FROM R/V WALTON SMITH & ADD LATITUDE AND LONGITUDE

#Step 1a: Round GPS time-stamp to the nearest second

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/GIS projects/OSTRICH14_PhysData/bridgeGPS/Daily/daily_GIS")
library(data.table)
library(lubridate)
library(plyr)

options("digits.secs" = 3)

fGPS <- list.files(full.names=T, recursive=FALSE)
for(i in 1:length(fGPS)) {
  
  GPSdata <- fread(fGPS[i], header=T)
  df2<- as.data.frame(GPSdata)
  df2<- df2[,c("Lat.DecDeg", "Lon.DecDeg", "Date_Time")]
  df2$DT <- NA
  df2$DT <- as.POSIXct(df2$Date_Time, format = "%Y-%m-%d %H:%M:%S" ) #Alternatively: df2$DT <- round_date(df2$DT, "second")
  df2$DT <- as.character(df2$DT)
  df2$Date_Time <- NULL
  x <- ddply(df2, c("DT"), summarise, mean(Lat.DecDeg), mean(Lon.DecDeg))
  colnames(x) <- c("Date_Time", "Lat.DecDeg", "Lon.DecDeg")
  x.na <- na.omit(x)
  
  ##write new text file
  suppressWarnings(dir.create("GPSByIntegerSec"))
  write.table(x.na, paste0("GPSByIntegerSec/LatLon_hms", i, ".txt"), row.names=FALSE, sep="\t")
}

#Step 1b: merge LatLon_hms files
setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/GIS projects/OSTRICH14_PhysData/bridgeGPS/Daily/daily_GIS/GPSByIntegerSec")

##Get a List of Files in working directory
GPS_files <- list.files()

##Merge the LatLon_hms files into a Single Dataframe
for (file in GPS_files){
  
  # if the merged dataset doesn't exist, create it
  #use 'skip' function to skip the first row and get the headers from the second row if need be
  if (!exists("dataset")){
    dataset <- do.call("rbind",lapply(GPS_files, FUN=function(files){read.table(files, header=TRUE, sep="\t")}))
  }
}
  
  GPSByIntegerSec <- as.data.frame(dataset)
  GPSByIntegerSec$Date_Time <- as.POSIXct(GPSByIntegerSec$Date_Time)
  

write.table(GPSByIntegerSec, file = "OSTRICH 2014_WS_GPS_YMDHMS.txt", sep = "\t", row.names = FALSE)

#Step 2a: Import and process R/V Walton Smith Depth "*DAT" files

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/vids/Depth")

##Loop through .DAT files in a directory and apply multiple functions
depth_files <- list.files(full.names=T, recursive=FALSE)
d <- depth_files[-grep("String", depth_files, fixed=T)]
for(i in 1:length(d)) {
  
  #read file in
  depth.data <- read.table(d[i], skip = 2)
  
  #assign column headers
  colnames(depth.data) <- c("Date","Time","Depth_ft","Depth_m","Depth_Fathom","Mnemonic","Checksum")
  
  #Remove "Depth (ft)", "Depth_Fathom", "CheckSum", and "Mnemonic" fields
  depth.data$Depth_ft <- NULL
  depth.data$Depth_Fathom <- NULL
  depth.data$Checksum <- NULL
  depth.data$Mnemonic <- NULL
  
  #convert "Date" in from a string to a date format
  depth.data$bDate <- as.Date(depth.data$Date, "%m/%d/%Y")
   
  ##If you need to replace the "-" with "/" in the dates
  #data$Date <- as.character(data$Date)
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  depth.data$Date_Time <- NA
  depth.data$Date_Time <- paste(depth.data$bDate, depth.data$Time, sep=' ')
  depth.data$Date_Time <- as.POSIXct(depth.data$Date_Time, format = "%Y-%m-%d %H:%M:%S" )
    
  #remove the "date" field so information is not duplicated in output
  depth.data$Date <- NULL
  depth.data$Time <- NULL
  depth.data$bDate <- NULL
  
  y <- ddply(depth.data, c("Date_Time"), summarise, mean(Depth_m))
  y.na <- na.omit(y)
  colnames(y.na) <- c("Date_Time", "Depth_m")
  
  #write new text file
  suppressWarnings(dir.create("depth processed"))
  write.table(y.na, paste0("depth processed/depth", i, ".txt"), row.names=FALSE, sep="\t")
}

#Step 2b: merge depth[i] files
setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/vids/Depth/depth processed/test")

#options("digits" = 2)

##Get a List of Files in working directory
depth_files <- list.files()

##Merge the LatLon_hms files into a Single Dataframe
for (file in depth_files){
  
  # if the merged dataset doesn't exist, create it
  #use 'skip' function to skip the first row and get the headers from the second row if need be
  if (!exists("depth.dataet")){
    depth.dataset301.408 <- do.call("rbind",lapply(depth_files, FUN=function(files){read.table(files, header=TRUE, sep="\t")}))
  }
  print(file)
}

#Took WAY too long to merge all depth files at one time. Better luck creating four small df and then merging them into one, large df.
df.depth1 <- as.data.frame(depth.dataset1.100)
df.depth2 <- as.data.frame(depth.dataset101.200)
df.depth3 <- as.data.frame(depth.dataset201.300)
df.depth4 <- as.data.frame(depth.dataset301.408)

df.depth <- rbind(df.depth1, df.depth2, df.depth3, df.depth4)
df.depth$Date_Time <- as.POSIXct(df.depth$Date_Time, format = "%Y-%m-%d %H:%M:%S")

write.table(x = df.depth, "depth_20140527_20140614.txt", row.names=FALSE, sep="\t")


setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/GIS projects/OSTRICH14_PhysData/bridgeGPS/Daily/daily_GIS/GPSBySec")

options(digits = 6)

library(data.table)
xy <- fread("OSTRICH 2014_WS_GPS_YMDHMS.txt", sep ="\t", header = TRUE, data.table = FALSE)


#Step 3: merge df.depth data frame and GPSByMin data frame
depth_xy <- join(df.depth, xy, type="left", match = "first")
  #Other examples of merging or joining two data frames
  #depth_xy <- merge(df.depth, GPSByIntegerSec, by = "Date_Time", all = TRUE)
  #depth_xy <- rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])

depth.na <- na.omit(depth_xy)

#re-order the columns
depth <- depth.na[c("Date_Time","Lat.DecDeg", "Lon.DecDeg", "Depth_m")]

#sort by date
depth[order(depth$Date_Time, decreasing = TRUE), ]

#write new text file
write.table(depth, file = "depthXY.txt", row.names=FALSE, sep="\t")
