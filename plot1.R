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
## Takes in the data set x and creates the appropriate plot
plot1 <- function(x){
        ## Creates plot 1
        hist(x$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
}
## Takes in data frame x, calls the plot function, and saves the current plot as a png file with the name "plot1.png"
savePlot1 <- function(x){
        ## Makes a png file from the plot
        png(file = "plot1.png")
        plot1(x)
        dev.off()
}