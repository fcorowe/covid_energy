library(countrycode)
library(tidyverse)
library(timetk)
library(ggpubr)
library(lme4)

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
cities_data <- cities_data %>% arrange(first.case.rank)
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

## 2.4 Use a factor to sort data by cities
cdf <- cdf[order(cdf$City),]
cdf$City <- ordered(cdf$City, levels = cities)

## 2.5 Restrict sample to 14/02 when mobility data start
cdf <- cdf %>% filter(date > "2020-02-14")

## 2.6 Creating additional variables
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

## 2.7 Filter out Chinese Cities
cdf <-cdf %>% dplyr::filter( location !=  "China")

## 2.8 remove dfs
rm(case_data,
   case_data_subset,
   cities_data,
   mob_data)

#######
# 3. Exploratory data analysis


## 3.1. N of observations by city
table(cdf$City)

## 3.2 Autocorrelation in mobility
p1 <- cdf %>% dplyr::filter(as.integer(City) %in% c(1:12)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p2 <- cdf %>% dplyr::filter(as.integer(City) %in% c(12:20)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p3 <- cdf %>% dplyr::filter(as.integer(City) %in% c(21:29)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p4 <- cdf %>% dplyr::filter(as.integer(City) %in% c(30:38)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p5 <- cdf %>% dplyr::filter(as.integer(City) %in% c(39:47)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p6 <- cdf %>% dplyr::filter(as.integer(City) %in% c(48:50)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Workplaces, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )
png("../outputs/modelling/pacf1.png",units="in", width=10, height=10, res=300)
p1
dev.off()

png("../outputs/modelling/pacf2.png",units="in", width=10, height=10, res=300)
p2
dev.off()

png("../outputs/modelling/pacf3.png",units="in", width=10, height=10, res=300)
p3
dev.off()

png("../outputs/modelling/pacf4.png",units="in", width=10, height=10, res=300)
p4
dev.off()

png("../outputs/modelling/pacf5.png",units="in", width=10, height=10, res=300)
p5
dev.off()

png("../outputs/modelling/pacf5.png",units="in", width=10, height=10, res=300)
p6
dev.off()

rm(p1,p2,p3,p4,p5,p6)


## 3.4 Correlation
corr <- cdf %>%
  group_by(City) %>% 
  dplyr::summarise(r = cor(Workplaces, 
                    Residential, 
                    new_cases_per_million, 
                    new_deaths_per_million, 
                    stringency_index))


vars_keep <- names(mtcars)[c(1, 3, 4)]
some <- mtcars %>% split(.$cyl) %>% map(select, vars_keep) %>% map(cor)

df <- some %>% reshape2::melt() %>% rename(cyl = L1)
ggplot(df, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + facet_wrap(~cyl, 
                                                                             nrow = 1)


#######
# 4. Multilevel modelling

## 4.1 Model with varying intercept
# specify a model equation
eq1 <- Workplaces ~ 1 + (1 | City) + (1 | date)
model1 <- lmer(eq1, data = cdf)

# estimates
summary(model1)



## 4.2 Model with varying intercept
# specify a model equation
eq2 <- Workplaces ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + new_deaths_per_million + stringency_index
model2 <- lmer(eq1, data = cdf)

# estimates
summary(model2)

