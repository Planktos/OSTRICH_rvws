#PROCESS Turner C7 FILES FROM R/V WALTON SMITH & ADD LATITUDE AND LONGITUDE

#Step 1: Import and process R/V Walton Smith Turner C7 "*DAT" files

setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/vids/Turner C7")
library(data.table)
options(digits = 5)

##Loop through .DAT files in a directory and apply multiple functions
files <- list.files(full.names=T, recursive=FALSE)
d <- files[-grep("String", files, fixed=T)]
for(i in 1:length(d)) {
  
  #read file in
  chl.data <- read.table(d[i], skip = 2, blank.lines.skip = TRUE, sep = "\t")
  
  #assign column headers
  colnames(chl.data) <- c("comp_date","comp_time","instr_date","instru_time","Chl-a_raw_fluor","Chl-a_Calib_fluor","CDOM_raw_fluor",
  "CDOM_calib_fluor","turbidity_raw_fluor","turbidity_calib_fluor","crude_Oil_raw_fluor","crude_oil_Calib_fluor", "phycoer_raw_fluor","phycoer_calib_fluor",
  "phycocy_raw_fluor","phycocy_calib_fluor","depth","temp_C","space")
  
  #Remove "instru_date", "instru_time", "space" fields
  chl.data$instr_date <- NULL
  chl.data$instru_time <- NULL
  chl.data$space <- NULL
    
  #convert "comp_date" in from a string to a date format
  chl.data$bDate <- as.Date(chl.data$comp_date, "%m/%d/%Y")
  
  ##If you need to replace the "-" with "/" in the dates
  #data$Date <- as.character(data$Date)
  
  #create new field in the df "Date_Time" by joining "Date", "Time" , sep=' ')
  chl.data$Date_Time <- NA
  chl.data$Date_Time <- paste(chl.data$bDate, chl.data$comp_time, sep=' ')
   
  #remove the "date" and "time" fields so information is not duplicated in output
  chl.data$comp_date <- NULL
  chl.data$comp_time <- NULL
  chl.data$bDate <- NULL

  turnerC7 <- chl.data[c("Date_Time","depth","temp_C","Chl_a_raw_fluor","Chl_a_Calib_fluor","CDOM_raw_fluor","CDOM_calib_fluor",
                         "turbidity_raw_fluor","turbidity_calib_fluor","crude_Oil_raw_fluor","crude_oil_Calib_fluor", "phycoer_raw_fluor","phycoer_calib_fluor",
                         "phycocy_raw_fluor","phycocy_calib_fluor")]
  
  #write new text file
  suppressWarnings(dir.create("TurnerC7 processed")) #remove YMDHMS to create directory for high-resolution data
  write.table(turnerC7, paste0("TurnerC7 processed/", substr(paste0(basename(d[i])),1,11),"_TurnerC7", ".txt"), row.names=FALSE, sep="\t")
    
}

#STEP 2: merge TurnerC7 processed files into a single file
setwd("C:/Users/kelly.robinson/Dropbox/Cowen_Sponaugle/OSTRICH/PhysData/vids/Turner C7/TurnerC7 processed")

library(plyr)
options("digits" = 5)

##Get a List of Files in working directory
t_files <- list.files()

##Merge the LatLon_hms files into a Single Dataframe
for (file in t_files){
  
  # if the merged dataset doesn't exist, create it
  #use 'skip' function to skip the first row and get the headers from the second row if need be
  if (!exists("turnerC7.dataset")){
    turnerC7.dataset <- do.call("rbind",lapply(t_files, FUN=function(files){fread(files, header=TRUE, sep="\t")}))
  }
  print(file)
}

df.turnerC7 <- as.data.frame(turnerC7.dataset)
#format(x = df.turnerC7$crude_Oil_raw_fluo, digits = 5, format = "f")
#format(x = df.turnerC7$crude_Oil_calib_fluo, digits = 5, format = "f")

colnames(df.turnerC7) <- c("Date_Time","depth","temp_C","Chl_a_raw_fluor","Chl_a_Calib_fluor","CDOM_raw_fluor","CDOM_calib_fluor",
                         "turbidity_raw_fluor","turbidity_calib_fluor","crude_Oil_raw_fluor","crude_oil_Calib_fluor", "phycoer_raw_fluor","phycoer_calib_fluor",
                         "phycocy_raw_fluor","phycocy_calib_fluor")

df.turnerC7$AggDate_Time <- as.POSIXct(df.turnerC7$Date_Time, format = "%Y-%m-%d %H:%M:%S" )

#Second-averaged fluorescence data (YMDHMS)
y <- ddply(df.turnerC7, c("AggDate_Time"), summarise, mean(depth), mean(temp_C), mean(Chl_a_raw_fluor), mean(Chl_a_Calib_fluor), mean(CDOM_raw_fluor),
           mean(CDOM_calib_fluor), mean(turbidity_raw_fluor), mean(turbidity_calib_fluor), mean(crude_Oil_raw_fluor), mean(crude_oil_Calib_fluor),
           mean(phycoer_raw_fluor), mean(phycoer_calib_fluor), mean(phycocy_raw_fluor), mean(phycocy_calib_fluor))

y.na <- na.omit(y) #Remove rows with NAs

#Assign column names to summarized data
colnames(y.na) <- c("Date_Time","AvgDepth","AvgTemp_C","AvgChl-a_raw_fluor","AvgChl-a_Calib_fluor","AvgCDOM_raw_fluor","AvgCDOM_calib_fluor",
                    "Avgturbidity_raw_fluor","Avgturbidity_calib_fluor","Avgcrude_Oil_raw_fluor","Avgcrude_oil_Calib_fluor", "Avgphycoer_raw_fluor",
                    "Avgphycoer_calib_fluor","Avgphycocy_raw_fluor","Avgphycocy_calib_fluor")

#write new text file
suppressWarnings(dir.create("YMDHMS")) #remove YMDHMS to create directory for high-resolution data
write.table(y.na, "YMDHMS/turnerC7_20140527_20140615_YMDHMS.txt", row.names=FALSE, sep="\t")

