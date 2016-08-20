################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#  Using the base plotting system, make a plot showing the total PM2.5 emission from 
#  all sources for each of the years 1999, 2002, 2005, and 2008.

################################################################################

## Load library

library(reshape2)

## Source data and files

fileURL   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
filename  <- "NEI_data.zip"
fileNEI   <- "summarySCC_PM25.rds"

allFiles    <- dir()

## Download data from source and unzip 

if(!(fileNEI %in% allFiles))
{
  if(!file.exists(filename))
  {
    download.file(url = fileURL, destfile = filename)
  }
  unzip(filename)
}

## Read file

if(!exists("NEI"))
{
  NEI <- readRDS(fileNEI)
}
  
## Calculate sum by years

plot1DT <- subset(NEI, select = c(year, Emissions))
plot1DT <- melt(plot1DT, id = "year")
plot1DT <- dcast(plot1DT, year ~ variable, sum)

## Generate plot

png(file = "plot1.png", width = 680, height = 480)
with(plot1DT, plot(year, Emissions,
                   type = "l",
                   main = "Total PM2.5 emission from all sources for years 1999 - 2008",
                   xlab = "Year",
                   ylab = "PM2.5 Emissions"))

dev.off()
cat("plot1.png is generated in", getwd())