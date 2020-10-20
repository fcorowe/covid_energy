library(countrycode)
library(tidyverse)
library(timetk)

rm(list=ls())

#######
# 1. Read data

# COVID data
case_data <- read.csv('../data/for_code/owid-covid-data.csv')

# list of cities
cities_data <- read.csv("../data/for_code/UN_Cities_names.csv")

# mobility data
mob_data <- read.csv("../data/for_code/mobility_and_stringency_uncities.csv")

#######
# 2. Data wrangling

## 2.1 COVID data
# define a vector of city names
cities <- as.vector(cities_data[,1])

# define a vector of country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

# exclude Hong Kong - assume reported with China
case_data_subset <- subset(case_data, iso_code %in% unique(country)[-grep("HKG", unique(country))])

# restrict period of analysis to end of March
case_data_subset <- subset(case_data_subset, date <= as.Date("2020-04-30") )

# format dates
case_data_subset$date <- as.Date(case_data_subset$date)



## 2.2. Mobility data

# attach the country code
mob_data$iso_code <- countrycode(mob_data[,2], "country.name", "iso3c")

# modify the date column
mob_data$Date <- as.Date(mob_data$Date, "%d/%m/%Y")

# set the maximum date to end of March
mob_data <- subset(mob_data, Date <= as.Date("2020-04-30") )

# extract the month
mob_data$month <- strftime(mob_data$Date, "%m")

# select variable set
mob_data <- mob_data %>%
  select(-c(4)) 

## 2.3 Create a single data frame
cdf <- left_join(case_data_subset, mob_data,by = c("iso_code" = "iso_code", "date" = "Date"))

## 2.4 Restrict sample to 14/02 when mobility data start
cdf <- cdf %>% filter(date > "2020-02-14")

## 2.5 Creating additional variables
# growth rate
cdf <- cdf %>% group_by(City) %>% 
  filter(total_cases > 0) %>% 
  mutate(t_cases_lag = lag(total_cases, n=1, default = NA))

cdf <- cdf %>% mutate(gr_cases = ( (total_cases - t_cases_lag) / t_cases_lag) *100 )

# doubling time: time taken for a population to double in size
cdf <- cdf %>% mutate(dng_time = log(2) / log( 1 + gr_cases) ) 
cdf$dng_time[is.infinite(cdf$dng_time)] <- 0

# troubleshooting
#cdf[1:15, c(3, 4, 5, 6, 49, 46)]
#cdf[85:100, c(3, 4, 5, 6, 49, 46)]
rm(case_data,
   case_data_subset,
   cities_data,
   mob_data)

#######
# 3. Exploratory data analysis


## 3.1. N of observations by city
table(cdf$City)

## 3.2 Autocorrelation in mobility
cdf %>% dplyr::filter(City %in% c("Bangkok", "Tokyo", "New York")) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "7 days",
                       .interactive = FALSE
  )
 #   lag.max = NULL, 
#    type = "correlation", 
 #   plot = TRUE)

#######
# 4. Multilevel modelling




grouped_acf_values <- sample_data %>%
  tidyr::nest(-group) %>%
  dplyr::mutate(acf_results = purrr::map(data, ~ acf(.x$value, plot = F)),
                acf_values = purrr::map(acf_results, ~ drop(.x$acf))) %>%
  tidyr::unnest(acf_values) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(lag = seq(0, n() - 1))