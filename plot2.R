## PROJECT 1: PLOT 2

## load necessary packages
library(data.table)
library(dplyr)
library(lubridate)

## load data
data <- fread(file = 'household_power_consumption.txt', header = TRUE, sep = ';')

## confirm data classes are wrong
str(data)

## filter data
power.com <- filter(data, Date %in% c('1/2/2007', '2/2/2007'))

## deal with missing data
## there are some NAs in the large data
sapply(data[,3:8], function(x) sum(x == '?'))
summary(data$Sub_metering_3)

## there are no NAs in the small data
sapply(power.com[,3:8], function(x) sum(x == '?'))
summary(power.com$Sub_metering_3)


## Deal with data types
power.com$Date <- dmy(power.com$Date)
power.com$Time <- format(strptime(power.com$Time, format = '%H:%M:%S'), '%H:%M:%S')
power.com[,3:8] <- sapply(power.com[, 3:8], as.numeric)

## Combine Date and Time into one DateTime variable
power.com <- mutate(power.com, 
                    DateTime = as.POSIXct(strptime(paste(power.com$Date, power.com$Time), 
                                                   '%Y-%m-%d %H:%M:%S')))

## confirm data classes are now correct
str(power.com)

## Plot 2: A line plot of global active power

png(file = 'plot2.png', width = 480, height = 480)

with(power.com, plot(DateTime, Global_active_power, type = 'l',
                     xlab = '', ylab = 'Global Active Power (kilowatts)'))

dev.off()

