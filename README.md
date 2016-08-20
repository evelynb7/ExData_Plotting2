# Introduction

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the [EPA National Emissions Inventory web site](http://www.epa.gov/ttn/chief/eiinformation.html).

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.

# Review Criteria

For each question

1. Does the plot appear to address the question being asked?
2. Does the plot appear to address the question being asked?

# Data

The data for this assignment are available from the course web site as a single zip file:

* [Data for Peer Assessment](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip) [29Mb]

The zip file contains two files:

PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. 

* fips: A five-digit number (represented as a string) indicating the U.S. county
* SCC: The name of the source as indicated by a digit string (see source code classification table)
* Pollutant: A string indicating the pollutant
* Emissions: Amount of PM2.5 emitted, in tons
* type: The type of source (point, non-point, on-road, or non-road)
* year: The year of emissions recorded

Source Classification Code Table (**Source_Classification_Code.rds**): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

You can read each of the two files using the **readRDS()** function in R. 

# Assignment

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

# Questions (with answers inline)

You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the **base** plotting system, make a plot showing the *total* PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.</br>
**The total PM2.5 emissions from all sources in the United States exhibits a general decreasing trend from 1999 to 2008.**

2. Have total emissions from PM2.5 decreased in the **Baltimore City**, Maryland (**fips == "24510"**) from 1999 to 2008? Use the **base** plotting system to make a plot answering this question.</br>
**There is a general decrease in total PM2.5 emissions in Baltimore City, MD, with a spike in 2005 although this does not exceed the maximum observed in 1999.**

3. Of the four types of sources indicated by the **type** (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for **Baltimore City**? Which have seen increases in emissions from 1999–2008? Use the **ggplot2** plotting system to make a plot answer this question.</br>
**All types except "Point" exhibit a general decrease in emissions from 1999 to 2008 for Baltimore City, somewhat plateauing between 2002 and 2005.
Type "Point" is the only source showing an increase from 1999 to 2005.
However this trend decreases quite drastically in 2008 almost down to the lowest emissions recorded in 1999.**

4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?</br>
**Coal combustion=related sources are Point and Non-Point.
The general trend shows a decrease for both types between 1999 and 2008. 
Upon closer inspection, the plateau observed for both sources between 2002 and 2005 exhibit a very slight increase for the Point source.
This is also reflected in the sum total emissions for both sources.**

5. How have emissions from motor vehicle sources changed from 1999–2008 in **Baltimore City**?</br>
**This is based on the assumption that "motor vehicle" source is of type "On Road".
Motor vehicle emissions in Baltimore City have generally decreased from 1999 to 2008, the most significant being from 1999 to 2002.**

6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in **Los Angeles County**, California (**fips == "06037"**). Which city has seen greater changes over time in motor vehicle emissions?</br>
**This is based on the assumption that "motor vehicle" source is of type "On Road".
Baltimore City and Los Angeles County have seen changes in opposite directions; Baltimore City having decreased and Los Angeles County increased between 1999 and 2008.
However, Baltimore City has seen a greater magnitude of change between the two locations for that time period.**

# Making and Submitting Plots

For each plot you should

* Construct the plot and save it to a PNG file.
* Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You must also include the code that creates the PNG file. Only include the code for a single plot (i.e. plot1.R should only include code for producing plot1.png)