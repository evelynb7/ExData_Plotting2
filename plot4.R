################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  Across the United States, how have emissions from 
#  coal combustion-related sources changed from 1999 to 2008?

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

## Find index of rows with "Coal" occuring in column 8 - "Short.Name"

if(!exists("coalIndex"))
{
  coalIndex <- grep(".*Coal.*", masterDT[,8])
}

## Subset rows with "Coal" occurance

plot4DT <- masterDT[coalIndex, ]
plot4DT <- subset(plot4DT, select = c(year, type, Emissions))
plot4DT[,2] <- as.factor(plot4DT[,2])

## Calculate coal emissions

plot4DT <- melt(plot4DT, id = c("year", "type"))
plot4DT <- dcast(plot4DT, year + type ~ variable, sum)

## Generate plot

png(file = "plot4.png", width = 680, height = 480)

g <- ggplot(plot4DT, aes(year, Emissions))
g + geom_line(aes(colour = type), size = 1) + 
  labs(title = "Coal combustion-related emissions by sources for years 1999 - 2008") +
  labs(x = "Year", y = "PM2.5 Emissions") +
  stat_summary(fun.y = sum, geom = "line", aes(linetype = "TOTAL"), colour = "grey80", size = 1) +
  guides(linetype = guide_legend(title = NULL, order = 1))
  
dev.off()
cat("plot4.png is generated in", getwd())