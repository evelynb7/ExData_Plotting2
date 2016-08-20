################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  How have emissions from motor vehicle sources changed from 1999 to 2008 
#  in Baltimore City?

################################################################################

## Load libraries

library(reshape2)
library(ggplot2)

## Source data and files

fileURL   <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
filename  <- "NEI_data.zip"
fileNEI   <- "summarySCC_PM25.rds"
fileSCC   <- "Source_Classification_Code.rds"

neededFiles <- c(fileNEI, fileSCC)
allFiles    <- dir()

## Download data from source and unzip

if(!all(neededFiles %in% allFiles))
{
  if(!file.exists(filename))
  {
    download.file(url = fileURL, destfile = filename)
  }
  unzip(filename)
}

## Read files

if(!exists("NEI"))
{
  NEI <- readRDS(fileNEI)
}
if(!exists("SCC"))
{
  SCC <- readRDS(fileSCC)
}

## Factorize column 2 - "SCC" in NEI

NEI[,2] <- factor(NEI[,2], levels = SCC[,1], labels = SCC[,1])

## Merge NEI and SCC data on SCC - this could take a while the first time

if(!exists("masterDT"))
{
  masterDT <- merge(x = NEI, y = SCC, by = "SCC", all.x = TRUE)
}

## Subset rows for Baltimore City (fips = 24510), assuming motor vehicles source is type = ON-ROAD

plot5DT <- subset(masterDT, fips == "24510" & type == "ON-ROAD", select = c(year, Emissions))

## Calculate motor vehicle emissions

plot5DT <- melt(plot5DT, id = "year")
plot5DT <- dcast(plot5DT, year ~ variable, sum)
plot5DT[,1] <- as.factor(plot5DT[,1])

## Generate plot

png(file = "plot5.png", width = 680, height = 480)

g <- ggplot(plot5DT, aes(year, Emissions))
g + geom_bar(fill = "#DD8888", width = .5, stat = "identity") +
  labs(title = "Baltimore  City motor vehicle emission for years 1999 - 2008") +
  labs(x = "Year", y = "PM2.5 Emissions")

dev.off()
cat("plot5.png is generated in", getwd())