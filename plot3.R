################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#  variable, which of these four sources have seen decreases in emissions 
#  from 1999 to 2008 for Baltimore City?
#  Which have seen increases in emissions from 1999 to 2008? 
#  Use the ggplot2 plotting system to make a plot answer this question.

################################################################################

## Load libraries

library(reshape2)
library(ggplot2)

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

## Calculate sum of Baltimore City (fips = 24510) emission types by years

plot3DT <- subset(NEI, fips == "24510", select = c(year, type, Emissions))
plot3DT[,2] <- as.factor(plot3DT[,2])
plot3DT <- melt(plot3DT, id = c("year", "type"))
plot3DT <- dcast(plot3DT, year + type ~ variable, sum)

## Generate plot

png(file = "plot3.png", width = 680, height = 480)

g <- ggplot(plot3DT, aes(year, Emissions))
g + geom_line(aes(color = type), size = 1) +
  labs(title = "Total Baltimore City PM2.5 emission by type for years 1999 - 2008") +
  labs(x = "Year", y = "PM2.5 Emissions")

dev.off()
cat("plot3.png is generated in", getwd())