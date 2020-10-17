library(countrycode)
library(tidyverse)

rm(list=ls())

# read case data
case_data <- read.csv('../data/for_code/owid-covid-data.csv')



# read the file with the cities included in the analysis
cities_data <- read.csv("../data/for_code/UN_Cities_names.csv")


# define a vector with city names
cities <- as.vector(cities_data[,1])

# define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")




# make sure that I exclude Hong Kong as we assume that it is reported with China
case_data_subset <- subset(case_data, iso_code %in% unique(country)[-grep("HKG", unique(country))])

# include dates only up to 
case_data_subset <- subset(case_data_subset, date <= as.Date("2020-03-31") )

# convert the date to the correct format
case_data_subset$date <- as.Date(case_data_subset$date)

# keep only the columns needed
case_data_subset <- case_data_subset[,c("iso_code", "location", "date", "new_cases", "new_deaths")]


# read in mobility data ---------------------------------------------------

# read mobility data
mob_data <- read.csv("../data/for_code/mobility_and_stringency_uncities.csv")

# attach the country code
mob_data$iso_code <- countrycode(mob_data[,2], "country.name", "iso3c")

# modify the date column
mob_data$Date <- as.Date(mob_data$Date, "%d/%m/%Y")

# set the maximum date to end of March
mob_data <- subset(mob_data, Date <= as.Date("2020-03-31") )

# extract the month
mob_data$month <- strftime(mob_data$Date, "%m")


# combine cases and mobility data
combined_data <- left_join(mob_data, case_data_subset, by = c("iso_code" = "iso_code", "Date" = "date"))




