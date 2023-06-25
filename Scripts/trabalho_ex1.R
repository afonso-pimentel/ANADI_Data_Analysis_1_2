# import the data
DADOS1 <- read.csv("./assets/dados1.csv")
# use setwd() to set the work directory on the console before running the readcsv command

# cleaner version without the description and unit rows
DADOS1_1<- DADOS1[-c(1, 2),]

#================================================================================================#
# a)
# add the first column with the correct time
DADOS1_1$Datetime <- as.POSIXct(as.numeric(DADOS1_1[,1]), tz="GMT", origin="1970-01-01")

# make datetime the first column
DADOS1_1 <- DADOS1_1[, c(ncol(DADOS1_1), 1:(ncol(DADOS1_1)-1))]

#================================================================================================#
# b)
# set the day to analyze to 4 of august of 2013
DAY_1 <- "2013-08-04"

# get the data only from that day
DADOS1_2 <- subset(DADOS1_1, format(Datetime, "%Y-%m-%d")==DAY_1)

# subset the engine temperature columns
DADOS1_2_1 <- DADOS1_2[,c("Datetime","ESP01.3","ESP02.3","ESP03.3")]

# Convert the temperature values to numeric
DADOS1_2_1$ESP01.3 <- as.numeric(DADOS1_2_1$ESP01.3)
DADOS1_2_1$ESP02.3 <- as.numeric(DADOS1_2_1$ESP02.3)
DADOS1_2_1$ESP03.3 <- as.numeric(DADOS1_2_1$ESP03.3)

# define line colors
plot_colors <- c("blue", "red", "forestgreen")

# define max and minimum for the range of values to show
max_y <- as.numeric(max(c(max(DADOS1_2_1$ESP01.3), max(DADOS1_2_1$ESP02.3), max(DADOS1_2_1$ESP03.3))))
max_y <- max_y + 5.0
min_y <- as.numeric(min(c(min(DADOS1_2_1$ESP01.3), min(DADOS1_2_1$ESP02.3), min(DADOS1_2_1$ESP03.3))))
min_y <- min_y - 5.0

# make the plot with the line for the ESP01.3 Data
plot(DADOS1_2_1$Datetime, DADOS1_2_1$ESP01.3, main = "Temperature comparison between engines", col=plot_colors[1], type="l", ylim = c(min_y, max_y), xlab="Date Time", ylab= "Engine Temperature (K)",cex.lab=1, lwd=3)

# add the ESP02.3 and ESP03.3 lines, red and green respectively
lines(DADOS1_2_1$Datetime, DADOS1_2_1$ESP02.3, type="l", lty=1, lwd=3,  col=plot_colors[2])
lines(DADOS1_2_1$Datetime, DADOS1_2_1$ESP03.3, type="l", lty=1, lwd=3,  col=plot_colors[3])

# add legend with line color and corresponding engine sensor
legend("topleft", names(DADOS1_2_1[-c(1)]), cex=0.8, col=plot_colors, lty = 1, lwd=2, bty="n")
title()
# this allows us to compare the average temperature on that day for each engine.
# the values seem to be near the 380 mark.

#================================================================================================#
# c)
# Create a list of temperature values for each sensor
temperature_list <- list(DADOS1_2_1$ESP01.3, DADOS1_2_1$ESP02.3, DADOS1_2_1$ESP03.3)

# Create the boxplot with boxplot()
boxplot(temperature_list, names = c("ESP01.3", "ESP02.3", "ESP03.3"),
        xlab = "Sensor", ylab = "Engine Temperature (K)",
        main = "Engine Temperature by Sensor", border = plot_colors)

# Engine number 1:
# Has small variance but relatively high mean engine temperature

# Engine number 2:
# Has a big variance, indicating instability, but relatively low mean engine temperature

# Engine number 3:
# Has the smallest variance of the three, indicating stability, but the highest mean engine temperature of the three

#================================================================================================#
# d)
# we have to evaluate the oil rate of each bomb in the day in question
# the columns of the oil rate are IC01.8 and IC02.8

#================================================================================================#
# i.
# Make a barplot with daily produced petrol barrels produced by pumps 1 and 2 in march 2014

MONTH_1 <- "2014-03"

# get the data only from that month
DADOS1_3 <- subset(DADOS1_1, format(Datetime, "%Y-%m")==MONTH_1)

# subset the oil rate columns
DADOS1_3_1 <- DADOS1_3[,c("Datetime","IC01.8","IC02.8")]

# Convert the temperature values to numeric
DADOS1_3_1$IC01.8 <- as.numeric(DADOS1_3_1$IC01.8)
DADOS1_3_1$IC02.8 <- as.numeric(DADOS1_3_1$IC02.8)

# Calculate the mean oil rates for each pump for each day
mean_oil_rates <- aggregate(cbind(IC01.8,IC02.8) ~ format(Datetime, "%Y-%m-%d"), data = DADOS1_3_1, mean)
days_seq <- 1:31

library(dplyr)

# Create new column called "Days" to have a more easy to read plot
mean_oil_rates <- mean_oil_rates %>% 
  mutate(Days = 1:n())

# Reorder the columns and replace the first one with the day count
mean_oil_rates <- mean_oil_rates %>% 
  select(Days, IC01.8, IC02.8)

# adjust the y axis for a better reading
max_oil_rate <- max(pmax(mean_oil_rates$IC01.8,mean_oil_rates$IC02.8))
barplot_limits <- c(0, max_oil_rate+100)

library(tidyr)

# Create a long format of the data frame
mean_oil_rates_long <- mean_oil_rates %>%
  pivot_longer(cols = c(IC01.8, IC02.8), names_to = "Pump", values_to = "Oil_Rate")

# Create the bar plot
barplot(height = mean_oil_rates_long$Oil_Rate, 
        beside = TRUE, 
        col = c("blue", "red"), 
        main = "Daily oil rate comparison between pumps", 
        xlab = "Days", ylab = "Oil Rate",
        names.arg = mean_oil_rates_long$Days, 
        legend.text = unique(mean_oil_rates_long$Pump), 
        args.legend = list(title = "Pump", x = 80, y = 300, cex = 0.8))

#================================================================================================#
# ii.
# Consider the months between 1-6-2013 and 31-5-2014
start_point <- "2013-06-01"
end_point <- "2014-05-31"

# subset only the IC columns from that time interval
# get the data only from that month
DADOS1_4 <- subset(DADOS1_1, format(Datetime, "%Y-%m-%d")>=start_point &  
                             format(Datetime, "%Y-%m-%d")<=end_point, 
                             select = c("Datetime", "IC01.8", "IC02.8"))

# Subset the data to include only rows for pump 1
DADOS1_4_1 <- subset(DADOS1_4, select = c("Datetime", "IC01.8"))
DADOS1_4_1$IC01.8 <- as.numeric(DADOS1_4_1$IC01.8)

# Convert the Datetime column to POSIXct format
DADOS1_4_1$Datetime <- as.POSIXct(DADOS1_4_1$Datetime, format = "%Y-%m-%d %H:%M:%S")

# Calculate the sum of oil rate for each month using aggregate()
daily_oil_rate_mean <- aggregate(IC01.8 ~ format(Datetime, "%Y-%m-%d"), data = DADOS1_4_1, mean)

# Change the name of the first column to "Days" and cast to POSIXct
names(daily_oil_rate_mean)[1] <- "Days"
daily_oil_rate_mean$Days <- as.POSIXct(daily_oil_rate_mean$Days, format = "%Y-%m-%d")

# Calculate the sum of oil rate for each month using aggregate()
monthly_oil_rate <- aggregate(IC01.8 ~ format(Days, "%Y-%m"), data = daily_oil_rate_mean, sum)

# Find the month with the largest sum using which.max() and calculate the max value
max_month <- monthly_oil_rate[which.max(monthly_oil_rate$IC01.8), 1]
max_value <- max(monthly_oil_rate$IC01.8)

# Print the result
cat("The month with the largest sum of oil rate for pump 1 is", max_month, "with ", max_value, " barrels produced. \n")
# The month with the largest sum of oil rate for pump 1 is 2013-08 with  16954.71  barrels produced. 
# The calculation was made taking in consideration the mean value per day and the sum of each day per month.
#================================================================================================#
# iii.
# first of all, we randomize a set of days to have as a start point
set.seed(300)
number_of_days <- as.numeric(difftime(end_point, start_point, units = "days"))
days_sample <- sample(number_of_days,10)

# convert to date
dates_sample <- as.Date(start_point) + days_sample - 1;
DADOS1_4_2 <- DADOS1_4
DADOS1_4_2$IC01.8 <- as.numeric(DADOS1_4_2$IC01.8)
DADOS1_4_2$IC02.8 <- as.numeric(DADOS1_4_2$IC02.8)


# Create column with date portion of the Datetime
DADOS1_4_2$date <- as.Date(DADOS1_4_2$Datetime)

# subset the data to include the rows with the sample dates
DADOS1_4_2 <- subset(DADOS1_4_2, date %in% dates_sample)

# Calculate the daily sum for each date
daily_oil_rate_mean <- aggregate(cbind(IC01.8, IC02.8) ~ date, data = DADOS1_4_2, mean)

# Create the boxplot
boxplot(daily_oil_rate_mean[,2:3], main = "Oil production per day", xlab="Pump", ylab = "Oil Production bbl/d")

# the plot presents two outliers in the IC01.8 Pump.The variance is far greater in the pump 1 than it is in the second one. 
# The IC02.8 pump is far more consistent.
#
#================================================================================================#
# iv
# make a hipothesis test of the sample to check if Pump 1 had a bigger daily average production than Pump 2
# H0: mean pump 1 = mean pump 2
# H1: mean pump 1 > mean pump 2
# This is a unilateral test to the left
# We can not consider the samples to be paired
# let's do a t.test:

res <- t.test(daily_oil_rate_mean$IC01.8, daily_oil_rate_mean$IC02.8, alternative="greater", paired=F)
res$p.value

# the result gave a really low p.value, so we can reject H0.
# So, we can probably say the pump 1 had a better mean than pump 2

#================================================================================================#
# v.
# now we have to see if the test checks out.

DADOS1_4_4 <- DADOS1_4
DADOS1_4_4$IC01.8 <- as.numeric(DADOS1_4_4$IC01.8)
DADOS1_4_4$IC02.8 <- as.numeric(DADOS1_4_4$IC02.8)

# Calculate the sum of oil rate for each month using aggregate()
full_daily_oil_rate_mean <- aggregate(cbind(IC01.8, IC02.8) ~ format(Datetime, "%Y-%m-%d"), data = DADOS1_4_4, mean)
mean_value_pump_1 <- mean(full_daily_oil_rate_mean$IC01.8)
mean_value_pump_2 <- mean(full_daily_oil_rate_mean$IC02.8)

# Check if both means are equal
if(mean_value_pump_1 == mean_value_pump_2){
    print("The mean is equal");
} else if (mean_value_pump_1 > mean_value_pump_2) {
    print("Pump 1 has a bigger mean oil production rate")
} else {
    print("Pump 2 has a bigger mean oil production rate")
}

# Pump 1 has a bigger mean oil production rate, so the test we have is accurate
#================================================================================================#

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L

# Clear plots
dev.off()  # But only if there IS a plot
