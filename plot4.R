## Reads in the Housold Power Consumption data set and performs the required transformations to prepare the data
## This assumes the current working directory has the file in it and its named "household_power_consumption.txt"
prepData <- function(){
        ## Read in  a subset of the data
        hpc <- read.table("household_power_consumption.txt", sep=";", skip=grep("31/1/2007", readLines("household_power_consumption.txt")),nrows=4*(24*60))
        
        ## Read in the data headers and assign them to the data frame
        headerdf <- read.table("household_power_consumption.txt", sep=";", nrows=1)
        vheader <- as.vector(t(headerdf))
        names(hpc) <- vheader
        
        ## Get the subset of the data we want to use
        hpcs <- hpc[hpc$Date == '1/2/2007' | hpc$Date == '2/2/2007', ]
        
        ## Create a date/time object and replace the Date and Time columns of the data frame
        date <- hpcs$Date
        time <- hpcs$Time
        dt <- paste(date,time)
        datetime <- strptime(dt, "%d/%m/%Y %H:%M:%S")
        hpcd <- cbind(datetime, hpcs[ ,3:9])
        hpcd
}
## Takes in the data frame x and creates the appropriate plot
plot4 <- function(x){
        ## Creates plot 4
        par(mfrow = c(2,2))
        with(x, {
                ## Top left plot
                plot(Global_active_power ~ datetime, xlab = "", ylab = "Global Active Power", type = "l")
                ## Top right plot
                plot(Voltage ~ datetime, type = "l")
                ## Bottom left plot
                with(x, plot(Sub_metering_1 ~ datetime, xlab = "", ylab = "Energy sub metering", type = "l"))
                with(x, lines(Sub_metering_2 ~ datetime, col = "red"))
                with(x, lines(Sub_metering_3 ~ datetime, col = "blue"))
                legend("topright", lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
                ## Bottom right plot
                plot(Global_reactive_power ~ datetime, type = "l")
        })
}
## Takes in data frame x, calls the plot function, and saves the current plot as a png file with the name "plot4.png"
savePlot4 <- function(x){
        ## Makes a png file from the plot
        png(file = "plot4.png")
        plot4(x)
        dev.off()
}