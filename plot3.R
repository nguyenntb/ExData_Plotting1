## PROJECT 1 - PLOT 3

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

## Plot 3: A plot for 3 sub metering

png(file = 'plot3.png', width = 480, height = 480)

with(power.com, plot(DateTime, Sub_metering_1, type = 'l', 
     xlab = '', ylab = 'Energy sub metering'))
with(power.com, lines(DateTime, Sub_metering_2, col = 'red'))     
with(power.com, lines(DateTime, Sub_metering_3, col = 'blue'))
legend('topright',
       col = c('black', 'red', 'blue'), 
       lty = c(1, 1, 1),
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))

dev.off()
