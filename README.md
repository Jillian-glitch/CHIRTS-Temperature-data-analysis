# CHIRTS-Temperature-data-analysis
setwd("D:/")
# Load packages
library(raster)
library(ncdf4)
library(dplyr)
library(magrittr)
library(tidyverse)

  
# Load the CRU TS data into R 
pre <- brick("Average.nc") # Temp

# Import site GPS 
samples <- read.csv(file = "./allgps.csv", header = T)
samples<- samples %>% select("lon","lat")


# Extracting temperature 
pre.sites <- data.frame(extract(pre, samples, ncol=2)) # Temp



# Change column names
years <- 1900:2022
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(pre.sites) <- paste(rep(years, each=12), rep(month, times=122), sep="_")



## Reshape and generating time variables by cleaning the date/time variable
clean.rsh<- pre.sites %>% 
  tidyr::gather(Time, Temp,`1900_Jan`:`2022_Dec`) %>% 
  mutate(Month = stringr::str_extract(Time, pattern = "\\w{3}$"),
         Year = as.numeric(stringr::str_extract(Time, pattern = "\\d{4}")))

# Generating year month
sum.df <- clean.rsh %>% 
  group_by(Year, Month) %>% 
  summarise(`Mean Temp` = mean(Temp, na.rm= T)) %>% 
  arrange(Year,Month)



sum.df1 <- sum.df$Month[sum.df$Month=="Jan"] <- "1"
sum.df2 <- sum.df$Month[sum.df$Month=="Feb"] <- "2"
sum.df3 <- sum.df$Month[sum.df$Month=="Mar"] <- "3"
sum.df4 <- sum.df$Month[sum.df$Month=="Apr"] <- "4"
sum.df5 <- sum.df$Month[sum.df$Month=="May"] <- "5"
sum.df6 <- sum.df$Month[sum.df$Month=="Jun"] <- "6"
sum.df7 <- sum.df$Month[sum.df$Month=="Jul"] <- "7"
sum.df8 <- sum.df$Month[sum.df$Month=="Aug"] <- "8"
sum.df9 <- sum.df$Month[sum.df$Month=="Sep"] <- "9"
sum.df10 <- sum.df$Month[sum.df$Month=="Oct"] <- "10"
sum.df11 <- sum.df$Month[sum.df$Month=="Nov"] <- "11"
sum.df12 <- sum.df$Month[sum.df$Month=="Dec"] <- "12"


sapply(sum.df, class)
sum.df$Month <- as.numeric(as.character(sum.df$Month))
sapply(sum.df, class)

order1 <- order(sum.df$Year,sum.df$Month)
finaldata <- sum.df[order1,]



write.csv(finaldata, file="averagetemp.csv")
