################################################################################

## Exploratory Data Analysis Assignment 2
## Evelyn B.
## 21 July 2016

## Assignment Requirements:
#  Compare emissions from motor vehicle sources in Baltimore City with emissions 
#  from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#  Which city has seen greater changes over time in motor vehicle emissions?

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

bcity <- subset(masterDT, fips == "24510" & type == "ON-ROAD", select = c(year, fips, Emissions))

## Subset rows for Los Angeles Country (fips = 06037), assuming motor vehicles source is type = ON-ROAD

lacounty <- subset(masterDT, fips == "06037" & type == "ON-ROAD", select = c(year, fips, Emissions))

## Merge Baltimore City and Los Angeles County data

plot6DT <- rbind(bcity, lacounty)
plot6DT[,1] <- as.factor(plot6DT[,1])
plot6DT[,2] <- factor(plot6DT[,2], levels = c("24510", "06037"), labels = c("Baltimore City", "Los Angeles County"))

## Calculate motor vehicle emissions

plot6DT <- melt(plot6DT, id = c("year", "fips"))
plot6DT <- dcast(plot6DT, year + fips ~ variable, sum)

## Generate plot

png(file = "plot6.png", width = 680, height = 480)

g <- ggplot(plot6DT, aes(year, Emissions, fill = fips, label = round(Emissions, 2)))
g + geom_bar(stat = "identity") +
  facet_grid(. ~ fips) +
  guides(fill = FALSE) +
  labs(title = "Baltimore City & Los Angeles County motor vehicle emissions for years 1999 - 2008") +
  labs(x = "Year", y = "PM2.5 Emissions") +
  geom_text(vjust = -0.5)

dev.off()
cat("plot6.png is generated in", getwd())