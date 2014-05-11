#########################################################################################
# download the data file from the source
# unzip the downloaded zip file 
#########################################################################################
sourceURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(sourceURL, "usageData.zip")
unzip("usageData.zip")

#########################################################################################
# set appropriate column names and column data types
# read the file into a dataframe, filter out the data required 
#########################################################################################
colnames <- c("Date", "Time", "Global_active_power",
              "Global_reactive_power", "Voltage",
              "Global_intensity", "Sub_metering_1",
              "Sub_metering_2", "Sub_metering_3")

coltypes <- c("character", "character", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric",
                "numeric")

tempDF <- read.table("household_power_consumption.txt", header=TRUE,
                     sep=";", col.names = colnames, 
                     na.strings = "?", colClasses = coltypes)

feb1Dat <- subset(tempDF, tempDF[,1]== "1/2/2007")
feb2Dat <- subset(tempDF, tempDF[,1]== "2/2/2007")
febDat <- rbind(feb1Dat, feb2Dat)

#########################################################################################
# convert Date column to 'Date' object and Time column to 'POSIX' object
##########################################################################################
numRows <- nrow(febDat)
tempDt <- strsplit(febDat[,1], split = "/")
tempTm <- febDat[,2]
class(febDat[,1]) <- "Date"
formattedDtTm <- NULL

library(stringr)

for (i in 1:numRows)
{
  paddedDt <- str_pad(tempDt[[i]],2,side="left", pad="0")
  formattedDt <- paste(paddedDt[1], paddedDt[2], paddedDt[3], sep="/")
  formattedDtTm[i] <- paste(formattedDt, febDat[i,2],sep=" ")
  febDat[i,1] <- as.Date(formattedDt,format="%d/%m/%Y")
}

newDF <- cbind(febDat[,1],formattedDtTm,febDat[,3:9])
names(newDF)[1:2] <- c("Date","Time")

# plotDF is the data frame to be used for all the plots
plotDF <- within(newDF, Time <- as.POSIXct(Time, 
                                format="%d/%m/%Y %H:%M:%S"))

#########################################################################################
# Open the png graphic device, draw the histogram and close the device
##########################################################################################
png("Plot1.png", width=480,height=480)
hist(plotDF[,3], col="red", main ="Global Active Power",
     xlab="Global Active Power (kilowatts)")
dev.off()