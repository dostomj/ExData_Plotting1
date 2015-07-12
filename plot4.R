##  Read data from the source file and return the filtered data 
getTargetPowerData <- function() {
  
  ##Read all the data from the file with separator as ; and without factoring
  powerConsumption <- read.table("household_power_consumption.txt",header=TRUE,sep=";",quote="",  
                                   stringsAsFactors=FALSE,comment.char="")
  ##Cast the Date data type
  powerConsumption$Date <-as.Date(powerConsumption$Date,format="%d/%m/%Y")
  
  ##Filter the data based on the date
  targetData <- subset(powerConsumption, Date==as.Date('2007-02-01',format="%Y-%m-%d") | 
                                          Date==as.Date('2007-02-02',format="%Y-%m-%d") )
  ##Return the filtered data
  targetData
}

##Draw the Submetering graph for the 3 readings
plotEnergySubMetering <- function(powerData) {
  ## Initialize the graph
  plot(powerData$DateTime, powerData$Sub_metering_1, type="n", 
       xlab="", ylab = "Energy sub metering")
  
  ##Draw the points of Sub_metering_1
  points(powerData$DateTime, powerData$Sub_metering_1, type="l", col="Black")
  
  ##Draw the points of Sub_metering_2
  points(powerData$DateTime, powerData$Sub_metering_2, type="l", col="Red")
  
  ##Draw the points of Sub_metering_3
  points(powerData$DateTime, powerData$Sub_metering_3, type="l", col="Blue")
  
  ##Add the legend
  legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         col=c("Black", "Red","Blue"), lty="solid", bty="n")
}

## Create the Exploratory analysis and save as an image 
createExploratoryAnalysis <- function() {
  ##get the filtered data
  powerData <- getTargetPowerData()
  
  ##Create a new column for date time.
  powerData$DateTime <- as.POSIXct(paste(powerData$Date, powerData$Time), 
                                   format="%Y-%m-%d %H:%M:%S")
  
  ##Create a PNG device
  png(filename = "plot4.png", width = 480, height = 480)
  
  par(mfcol=c(2,2))
  
  ##Draw the line graph for Global Active Power  
  plot(powerData$DateTime, powerData$Global_active_power, type="l", 
       xlab="", ylab = "Global Active Power (kilowatts)")
  
  
  plotEnergySubMetering(powerData)
  
  
  ##Draw the line graph for Voltage   
  plot(powerData$DateTime, powerData$Voltage, type="l", 
       xlab="", ylab = "Voltage")
  
  
  ##Draw the line graph for Global Reactive Power  
  plot(powerData$DateTime, powerData$Global_reactive_power, type="l", 
       xlab="", ylab = "Voltage")
  
  
   ##close the device to create the graph
   dev.off()
  
}