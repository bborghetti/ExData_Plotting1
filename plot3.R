plot3 <-function() {

# Datafile located at this URL.    
#  https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

myurl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

setInternet2(use = TRUE)  # recommended for downloading from https locations
# see source documentation at "http://rpubs.com/thoughtfulbloke/downloadtips"

#since this is a zipped file, i'm using the temp file to store it
# see source for details:  http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
temp<-tempfile()
download.file(myurl,temp)
unzip(temp)
data<-read.table("./household_power_consumption.txt",header = TRUE, sep = ";",na.strings = "?")
unlink(temp)

#while looking at the file, heres what I determined...
#this is a semicolon-delimted file.
#heading row is:  Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3

#filter down to only the rows (dates) we care about  <2007-02-01 and 2007-02-02>
#this allows the rest of the operations to occur faster
newdates<-strptime(data$Date, "%d/%m/%Y") #original format is dd/mm/yyyy (16/12/2006); hh:mm:ss (17:24:00)
goodRows <- ( newdates=="2007-02-01") | (newdates == "2007-02-02")
goodData<-data[goodRows,]


#convert the date/time info from the file into R date format
temptimes<-paste(goodData$Date, goodData$Time)
newtimes<-strptime(temptimes, "%d/%m/%Y %H:%M:%S" )
newdata <- data.frame(cbind(newtimes, goodData[,c(-1,-2)]))

#we are now going to plot time/day (x) vs global active power (y)

#open the png device
png(filename = "./plot3.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))

#make the plot frame
plot(newdata$newtimes, newdata$Sub_metering_1, type = "n",
     ylab = "Energy sub metering",
     xlab ="")
#add the first line for sub_metering_1 in black
lines(newdata$newtimes, newdata$Sub_metering_1,
     type = "l",
     col = "black")
#add the second line for sub_metering_2 in red
lines(newdata$newtimes, newdata$Sub_metering_2,
     type = "l",
     col = "red")
#add the third line for sub_metering_3 in blue
lines(newdata$newtimes, newdata$Sub_metering_3,
     type = "l",
     col = "blue")
legend("topright",
      legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
      lty=c(1,1,1),
      col = c("black", "red", "blue"))

dev.off()


}