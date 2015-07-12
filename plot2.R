##  Read data from the source file and return the filtered data 
getTargetPowerData <- function() {
  
  ##Read all the data from the file with separator as ; and without factoring
  powerConsumption <- read.table("household_power_consumption.txt",header=TRUE,sep=";",quote="",  
                                 stringsAsFactors=FALSE,comment.char="", na.strings="?")
  ##Cast the Date data type
  powerConsumption$Date <-as.Date(powerConsumption$Date,format="%d/%m/%Y")
  
  ##Filter the data based on the date
  targetData <- subset(powerConsumption, Date==as.Date('2007-02-01',format="%Y-%m-%d") | 
                         Date==as.Date('2007-02-02',format="%Y-%m-%d") )
  ##Return the filtered data
  targetData
}

##Draw the graph for Global Active Power and save as an image 
drawGlobalActivePowerPlot <- function() {
  ##get the filtered data
  powerData <- getTargetPowerData()
  
  ##Convert the Global_active_power as numeric
  powerData$Global_active_power <- as.numeric(powerData$Global_active_power)
  
  #powerData$Day <- format.Date(powerData$Date,format="%a")
  powerData$DateTime <- as.POSIXct(paste(powerData$Date, powerData$Time), format="%Y-%m-%d %H:%M:%S")
  
  ##Create the PNG device
  png(filename = "plot2.png", width = 480, height = 480)
  
  ##Draw the line graph for Global Active Power   and save as an image
  plot(powerData$DateTime, powerData$Global_active_power, type="l", 
       xlab="", ylab = "Global Active Power (kilowatts)")
  dev.off()
  
}