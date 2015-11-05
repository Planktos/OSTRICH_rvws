#PROCESS XBT DEPTH FILES FROM R/V WALTON SMITH & ADD LATITUDE AND LONGITUDE

#Step 1: Add date, time, latitude, and longitude values to individual XBT casts so that data can be imported into and related in Access and Arc geodatabase

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/Data")
library(data.table)
library(stringr)

options("digits.secs" = 8)

d <- list.files(full.names=T, recursive=FALSE, pattern = ".EDF")
for(i in 1:length(d)) {
  
  #Read file in and create data frame with data values and the empty fields which will be filled-in
  x <- read.table(d[i], skip = 34, blank.lines.skip = TRUE)
  df <- as.data.frame(x)
  colnames(df) <- c("depth_m", "temperature_C", "sound velocity_m/s")
  df$date <- NA
  df$time <- NA
  df$latitude <- NA
  df$longitude <- NA
  
  #Extract launch date from header and paste into final data frame
  x2 <- readLines(d[i], skip = 2, n = 3)
  y <- grepl("//", x2)
  (x3 <- x2[!y])
  x3list <- strsplit(x3, split = ":  ")
  m <- matrix(unlist(x3list), nrow=length(x3list), byrow = TRUE)
  colnames(m) <- c("txt", "launch_date")
  df2 <- as.data.frame(m)
  df2$txt <- NULL
  df$date <- paste0(df2$launch_date)

  #Extract launch time from header and paste into final data frame
  x4 <- readLines(d[i], skip = 2, n = 4)
  y2 <- grepl("/", x4)
  (x5 <- x4[!y2])
  x5list <- strsplit(x5, split = ":  ")
  m2 <- matrix(unlist(x5list), nrow=length(x5list), byrow = TRUE)
  colnames(m2) <- c("time", "launch_time")
  df3 <- as.data.frame(m2)
  df3$time <- NULL
  df$time <- paste0(df3$launch_time)

  ##Extract latitude from header and paste into final data frame
  x6 <- read.table(d[i], skip = 5, nrow=1)
  colnames(x6) <- c("lattxt", "colon", "latdeg", "latmindir")
  df4 <- as.data.frame(x6)
  df4$latdeg <- as.numeric(df4$latdeg)
  df4$latmin <- NA
  df4$latmin <- str_sub(df4$latmindir, 1, -2) #removes the 'N'
  df4$latmin <- as.numeric(df4$latmin)
  df4$latdecdeg <- NA  
  df4$latdecdeg <- (df4$latdeg + (df4$latmin/60)) #NOTE: Does not take into account which hemisphere you are in. Default is Northern Hemisphere
  df4$lattxt <- NULL
  df4$colon <- NULL
  df4$latdeg <- NULL
  df4$latmindir <- NULL
  df$latitude<- paste0(df4$latdecdeg)

  #Extract longitude from header and paste into final data frame
  x7 <- read.table(d[i], skip = 6, nrow=1)
  colnames(x7) <- c("lontxt", "colon", "londeg", "lonmindir")
  df5 <- as.data.frame(x7)
  df5$londeg <- as.numeric(df5$londeg)
  df5$lonmin <- NA
  df5$lonmin <- str_sub(df5$lonmindir, 1, -2) #removes the 'W'
  df5$lonmin <- as.numeric(df5$lonmin)
  df5$londecdeg <- NA  
  df5$londecdeg <- ((df5$londeg + (df5$lonmin/60))*-1) #NOTE: Default is West of Prime Meridian
  df5$lontxt <- NULL
  df5$colon <- NULL
  df5$londeg <- NULL
  df5$lonmindir <- NULL
  df$longitude<- paste0(df5$londecdeg)

  df$latitude <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)
  
  round(df$latitude, digits = 7)
  round(df$longitude, digits = 7)
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df$bdate <- NA
  df$bdate <- as.Date(df$date, "%m/%d/%Y")
  df$Date_Time <- NA
  df$Date_Time <- paste(df$bdate, df$time, sep=' ')
  df$Date_Time <- as.POSIXct(df$Date_Time, format = "%Y-%m-%d %H:%M:%S" )
  df$bdate <- NULL
  
  
  #Re-order the column names
  df <- df[c("Date_Time", "date", "time", "latitude","longitude", "depth_m", "temperature_C", "sound velocity_m/s")]
  
#write new text file
suppressWarnings(dir.create("XBT processed")) #remove YMDHMS to create directory for high-resolution data
write.table(df, paste0("XBT processed/", substr(paste0(basename(d[i])),1,8),"_proc", ".txt"), row.names=FALSE, sep="\t")

}


#PROCESS XBT DEPTH FILES MISSING LATITUDE AND LONGITUDE FROM R/V WALTON SMITH

#Step 1: Add date and time to individual XBT casts so that data can be imported into and related in Access and Arc geodatabase

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/Data/proc_alt")
library(data.table)
library(stringr)

options("digits.secs" = 8)

d <- list.files(full.names=T, recursive=FALSE, pattern = ".EDF")
for(i in 1:length(d)) {
  
  #Read file in and create data frame with data values and the empty fields which will be filled-in
  x <- read.table(d[i], skip = 34, blank.lines.skip = TRUE)
  df <- as.data.frame(x)
  colnames(df) <- c("depth_m", "temperature_C", "sound velocity_m/s")
  df$date <- NA
  df$time <- NA
  df$latitude <- NA
  df$longitude <- NA
  
  #Extract launch date from header and paste into final data frame
  x2 <- readLines(d[i], skip = 2, n = 3)
  y <- grepl("//", x2)
  (x3 <- x2[!n])
  x3list <- strsplit(x3, split = ":  ")
  m <- matrix(unlist(x3list), nrow=length(x3list), byrow = TRUE)
  colnames(m) <- c("txt", "launch_date")
  df2 <- as.data.frame(m)
  df2$txt <- NULL
  df$date <- paste0(df2$launch_date)
  
  #Extract launch time from header and paste into final data frame
  x4 <- readLines(d[i], skip = 2, n = 4)
  y2 <- grepl("/", x4)
  (x5 <- x4[!y2])
  x5list <- strsplit(x5, split = ":  ")
  m2 <- matrix(unlist(x5list), nrow=length(x5list), byrow = TRUE)
  colnames(m2) <- c("time", "launch_time")
  df3 <- as.data.frame(m2)
  df3$time <- NULL
  df$time <- paste0(df3$launch_time)

  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  df$bdate <- NA
  df$bdate <- as.Date(df$date, "%m/%d/%Y")
  df$Date_Time <- NA
  df$Date_Time <- paste(df$bdate, df$time, sep=' ')
  df$Date_Time <- as.POSIXct(df$Date_Time, format = "%Y-%m-%d %H:%M:%S" )
  df$bdate <- NULL

  
  #Re-order the column names
  df <- df[c("Date_Time", "date", "time", "depth_m", "temperature_C", "sound velocity_m/s")]
  
  suppressWarnings(dir.create("processed")) #remove YMDHMS to create directory for high-resolution data
  write.table(df, paste0("processed/", substr(paste0(basename(d[i])),1,8),"_proc", ".txt"), row.names=FALSE, sep="\t")
}
 
  #Step 1b: Merge XBT files with ship GPS
  setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/GIS projects/txt files for gdb")
  
  options(digits = 8)
  
  xy <- fread("OSTRICH 2014_WS_GPS_YMDHMS.txt", sep ="\t", header = TRUE, data.table = FALSE)
  
  df_xy <- join(df, xy, by = "Date_Time", type="left", match = "first")
  #Other examples of merging or joining two data frames
  #depth_xy <- merge(df.depth, GPSByIntegerSec, by = "Date_Time", all = TRUE)
  #depth_xy <- rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])
  
  df.na <- na.omit(df_xy)
  
  #Re-name latitude and longitude columns to match other XBT processed files
  df.na$latitude <- df.na$Lat.DecDeg
  df.na$longitude <- df.na$Lon.DecDeg
  
  #re-order the columns
  XBT <- df.na[c("Date_Time", "date", "time", "latitude","longitude", "depth_m", "temperature_C", "sound velocity_m/s")]
  
  #sort by date
  XBT[order(XBT$Date_Time, decreasing = TRUE), ]
  
  #write new text file
  suppressWarnings(dir.create("XBT processed"))
  write.table(df, paste0("XBT processed/", substr(paste0(basename(d[i])),1,8),"_proc", ".txt"), row.names=FALSE, sep="\t")
  
}

#STEP 2: merge XBT processed files into a single file
setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/Data/XBT processed")

library(plyr)
options("digits" = 8)

##Get a List of Files in working directory
xbt_files <- list.files()

##Merge the LatLon_hms files into a Single Dataframe
for (file in xbt_files){
  
  # if the merged dataset doesn't exist, create it
  #use 'skip' function to skip the first row and get the headers from the second row if need be
  if (!exists("xbt.dataset")){
    xbt.dataset <- do.call("rbind",lapply(xbt_files, FUN=function(files){fread(files, header=TRUE, sep="\t")}))
  }
  print(file)
}

signif(xbt.dataset$latitude, digits = 8)
signif(xbt.dataset$longitude, digits = 8)

write.table(xbt.dataset, paste0("OSTRICH2014_XBT casts.txt"), row.names=FALSE, sep="\t")