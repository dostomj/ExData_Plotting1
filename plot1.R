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

##Create the Histogram for Global Active Power
drawGlobalActivePowerHistogram <- function() {
  
  ##get the filtered data
  powerData <- getTargetPowerData()
  
  ##Convert the Global_active_power as numeric
  powerData$Global_active_power <-as.numeric(powerData$Global_active_power)
  
  ##Create the PNG device 
  png(filename = "plot1.png", width = 480, height = 480)
  
  ##Draw the histogram
  hist(powerData$Global_active_power, col="Red", xlab = "Global Active Power (kilowatts)", 
       main = "Global Active Power")
  
  ##close the device to create the graph
  dev.off()
  
}


