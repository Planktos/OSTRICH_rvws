#Process drifter spot points
setwd("C:/Users/Kelly/Dropbox/Cowen_Sponaugle_share/OSTRICH/PhysData/OST2015")

library(data.table)
library(stringr)
library(lubridate)
options("digits.secs" = 3)

d<-fread("drifter_20150219-20150620.txt",header=FALSE,stringsAsFactors = FALSE)
as.data.frame(d)
setnames(d, 1:9, c("unique_msg_id", "spot_esn_number", "spot_esn_name", "DateTtime", "sec_from_epoch","latitude","long", "msg_type","bat_state"))

t2 <- subset(d, d$spot_esn_name == "TRACE_002")
               
#format date time field for arcmap file
t2$DT<-NA
t2$DT<-str_sub(t2$DateTtime, 1, -2)
t2$arcDT <- NA
t2$arcDT <- gsub(pattern = "T", replacement = " ", x = t2$DT)
t2$arcDateTime <- as.POSIXlt(t2$arcDT, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

t2$date <- as.Date(t2$arcDateTime, format = "%Y-%m-%d")
t2$time <- strptime(t2$arcDT, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

t2sub <- t2[t2$date >= "2015-06-19" & t2$date <= "2015-06-21",]

#get daily tracks


#remove extra fields
t2sub$DT <- NULL
t2sub$arcDT <- NULL

write.table(t2sub, file = "drifter_20150619-20150621_arcgis.txt", row.names = FALSE, sep = "\t")
