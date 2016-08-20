################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#  (fips == "24510") from 1999 to 2008? 
#  Use the base plotting system to make a plot answering this question.

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

## Calculate sum of Baltimore City (fips = 24510) emission by years

plot2DT <- subset(NEI, fips == "24510", select = c(year, Emissions))
plot2DT <- melt(plot2DT, id = "year")
plot2DT <- dcast(plot2DT, year ~ variable, sum)

## Generate plot

png(file = "plot2.png", width = 680, height = 480)
with(plot2DT, plot(year, Emissions,
                   type = "l",
                   main = "Total Baltimore City PM2.5 emission for years 1999 - 2008",
                   xlab = "Year",
                   ylab = "PM2.5 Emissions"))
dev.off()
cat("plot2.png is generated in", getwd())