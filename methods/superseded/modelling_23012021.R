library(countrycode)
library(tidyverse)
library(timetk)
library(ggpubr)
library(lme4)
library(merTools)
library(ggcorrplot)
library(corrplot)
library(viridis)
library(sf)
library(ggthemes)
library(zoo)
library(showtext)
library(equatiomatic)

rm(list=ls())

# Google’s COVID-19 Community Mobility Reports show how visits and length of stay in 
# various place categories have changed compared to a baseline period before the pandemic.

# we focus on the residential category, which is defined as the time users spent at home, 
# using the home addresses provided to or estimated by Google Maps. 
# Our focus on a variable related to time use and duration of events is consistent 
# with the ep/Users/Franciscorowe 1/Dropbox/Francisco/Research/in_progress/covid19_energy/github/covid_energy/methodsidemiological literature on time use and the spread of close-contact infectious diseases.

#######
# 1. Read data

# Set wdir
setwd("/Users/Franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/github/covid_energy/methods")

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

# COVID cases at t1-7
cdf <- cdf %>% group_by(City) %>% 
  mutate(
    casespm_t1lag = lag(new_cases_per_million, n=1, default = NA),
    casespm_t2lag = lag(new_cases_per_million, n=2, default = NA),
    casespm_t3lag = lag(new_cases_per_million, n=3, default = NA),
    casespm_t4lag = lag(new_cases_per_million, n=4, default = NA),
    casespm_t5lag = lag(new_cases_per_million, n=5, default = NA),
    casespm_t6lag = lag(new_cases_per_million, n=6, default = NA),
    casespm_t7lag = lag(new_cases_per_million, n=7, default = NA)
    )

# Cases growth rate
cdf <- cdf %>% group_by(City) %>% 
  filter(total_cases > 0) %>% 
  mutate(cases_t1lag = lag(total_cases, n=1, default = NA))
cdf <- cdf %>% mutate(gr_cases = ( (total_cases - cases_t1lag) / cases_t1lag) *100 )

# Cases doubling time: time taken for a population to double in size
cdf <- cdf %>% mutate(dng_time = log(2) / log( 1 + gr_cases) ) 
cdf$dng_time[is.infinite(cdf$dng_time)] <- 0

# troubleshooting
#cdf[1:15, c(3, 4, 5, 6, 49, 46)]
#cdf[85:100, c(3, 4, 5, 6, 49, 46)]

# COVID deaths at t1-7
cdf <- cdf %>% group_by(City) %>% 
  mutate(deathspm_t1lag = lag(new_deaths_per_million, n=1, default = NA),
         deathspm_t2lag = lag(new_deaths_per_million, n=2, default = NA),
         deathspm_t3lag = lag(new_deaths_per_million, n=3, default = NA),
         deathspm_t4lag = lag(new_deaths_per_million, n=4, default = NA),
         deathspm_t5lag = lag(new_deaths_per_million, n=5, default = NA),
         deathspm_t6lag = lag(new_deaths_per_million, n=6, default = NA),
         deathspm_t7lag = lag(new_deaths_per_million, n=7, default = NA)
         )

# Stringency measures at t1-7
cdf <- cdf %>% group_by(City) %>% 
  mutate(
    stringency_t1lag = lag(stringency_index, n=1, default = NA),
    stringency_t2lag = lag(stringency_index, n=2, default = NA),
    stringency_t3lag = lag(stringency_index, n=3, default = NA),
    stringency_t4lag = lag(stringency_index, n=4, default = NA),
    stringency_t5lag = lag(stringency_index, n=5, default = NA),
    stringency_t6lag = lag(stringency_index, n=6, default = NA),
    stringency_t7lag = lag(stringency_index, n=7, default = NA)
         )

## 2.7 Filter out Chinese Cities
cdf <- cdf %>% dplyr::filter( location !=  "China")

## 2.8 remove dfs
rm(case_data,
   case_data_subset,
   cities_data,
   mob_data)

## 2.9 Impute Stay-at-Home data
cdf$stayhome <- na.locf(cdf$Residential)

## 2.10 rename cities
cdf$City <- recode_factor(cdf$City, `Osaka [Kyoto]` = "Osaka", `Quezon City [Manila]` = "Manila", `Delhi [New Delhi]` = "Delhi", `S\xe3o Paulo` = "Sao Paulo")
cities <- recode(cities, `Osaka [Kyoto]` = "Osaka", `Quezon City [Manila]` = "Manila", `Delhi [New Delhi]` = "Delhi", `S\xe3o Paulo` = "Sao Paulo")
cdf$City <- ordered(cdf$City, levels = cities)

## 2.11 Set font for graphics
# load font
font_add_google("Roboto Condensed", "robotocondensed")
# automatically use showtext to render text
showtext_auto()



#######
# 3. Exploratory data analysis


## 3.1. N of observations by city
table(cdf$City)

## 3.2 Autocorrelation in mobility
p1 <- cdf %>% dplyr::filter(as.integer(City) %in% c(1:12)) %>% 
  group_by(City) %>% 
  arrange(City) %>% 
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p2 <- cdf %>% dplyr::filter(as.integer(City) %in% c(12:20)) %>% 
  group_by(City) %>%
  arrange(City) %>%
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p3 <- cdf %>% dplyr::filter(as.integer(City) %in% c(21:29)) %>% 
  group_by(City) %>%
  arrange(City) %>%
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p4 <- cdf %>% dplyr::filter(as.integer(City) %in% c(30:38)) %>% 
  group_by(City) %>%
  arrange(City) %>%
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p5 <- cdf %>% dplyr::filter(as.integer(City) %in% c(39:47)) %>% 
  group_by(City) %>%
  arrange(City) %>%
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p6 <- cdf %>% dplyr::filter(as.integer(City) %in% c(48:50)) %>% 
  group_by(City) %>%
  arrange(City) %>%
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )
png("../outputs/modelling/autocorrelation/pacf1.png",units="in", width=10, height=10, res=300)
p1
dev.off()

png("../outputs/modelling/autocorrelation/pacf2.png",units="in", width=10, height=10, res=300)
p2
dev.off()

png("../outputs/modelling/autocorrelation/pacf3.png",units="in", width=10, height=10, res=300)
p3
dev.off()

png("../outputs/modelling/autocorrelation/pacf4.png",units="in", width=10, height=10, res=300)
p4
dev.off()

png("../outputs/modelling/autocorrelation/pacf5.png",units="in", width=10, height=10, res=300)
p5
dev.off()

png("../outputs/modelling/autocorrelation/pacf6.png",units="in", width=10, height=10, res=300)
p6
dev.off()

rm(p1,p2,p3,p4,p5,p6)


## 3.1 Scatteplot Mobility vs COVID cases, deaths and stringency

  # Stay-at-home vs COVID cases t
p1_mc0 <- ggplot(cdf, aes(x = new_cases_per_million, y = Residential)) +
  geom_point(colour = "darkblue", alpha = 0.2, aes(size = new_cases)) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.5, color="darkblue") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() +
#  theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none") +
  labs(x= "Daily New Confirmed COVID-19 Cases Number Per Million",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_mc0.png",units="in", width=10, height=10, res=300)
p1_mc0
dev.off()

  # Stay-at-home vs COVID cases t7
p1_mc7 <- ggplot(cdf, aes(x = casespm_t7lag, y = Residential)) +
  geom_point(colour = "darkblue", alpha = 0.2, aes(size = new_cases)) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.5, color="darkblue", linetype = "dashed") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() +
#  theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none") +
  labs(x= "Daily New Confirmed COVID-19 Cases Number Per Million t=7",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_mc7.png",units="in", width=10, height=10, res=300)
p1_mc7
dev.off()

  # Stay-at-home vs Deaths
p1_md <- ggplot(cdf, aes(x = new_deaths/1000 , y = Residential)) +
  geom_point(colour = "darkred", alpha = 0.2, aes(size = new_cases)) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.5, color="darkred") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() +
#  theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none") +
  labs(x= "New COVID-19 Death Numbers (1,000)",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_md.png",units="in", width=10, height=10, res=300)
p1_md 
dev.off()


  # Stay-at-home vs Stringency
p1_ms <- ggplot(cdf, aes(x = stringency_index, y = Residential)) +
  geom_point(colour = "darkorange3", alpha = 0.2, aes(size = new_cases)) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.9, color="darkorange3") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() +
#  theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none") +
  labs(x= "Stringency Index",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_ms.png",units="in", width=10, height=10, res=300)
p1_ms 
dev.off()


## 3.2 Line plot Mobility vs COVID cases, deaths and stringency

# Stay-at-home vs COVID cases t & t7
wgt <- 5

p1_mc07 <- ggplot(cdf) + 
  geom_smooth(aes(x = date, y = Residential), method = "loess", se = FALSE, size=1.5, span = 0.3, color="#287D8EFF") +
  geom_smooth(aes(x = date, y = new_cases_per_million / wgt), method = "loess", se = FALSE, size=1.5, span = 0.3, color="darkblue") +
  geom_smooth(aes(x = date, y = casespm_t7lag / wgt), method = "loess", se = FALSE, size=1, span = 0.3, color="grey", linetype = "dashed") +
  facet_wrap(~ City, nrow = 7) + 
  scale_y_continuous(sec.axis = sec_axis(trans=~.*wgt, 
                                         name="Daily New Confirmed COVID-19 Cases Number Per Million")) +
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/lineplot/p1_mc07.png",units="in", width=10, height=10, res=300)
p1_mc07
dev.off()

# Stay-at-home vs Deaths t & t7
wgt1 <- 1000
p1_md07 <- ggplot(cdf) +
  geom_smooth(aes(x = date, y = Residential), method = "loess", se = FALSE, size=1.5, span = 0.3, color="#287D8EFF") +
  geom_smooth(aes(x = date, y = new_deaths/1000), method = "loess", se = FALSE, size=1.5, span = 0.3, color="darkred") +
  geom_smooth(aes(x = date, y = lag(new_deaths/1000, n=7, default = NA)), method = "loess", se = FALSE, size=1, span = 0.3, color="darkred", linetype = "dashed") +
  facet_wrap(~ City, nrow = 7) + 
  scale_y_continuous(sec.axis = sec_axis(trans=~.*.125, 
                                         name="New COVID-19 Death Numbers (1,000)")) +
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/lineplot/p1_md07.png",units="in", width=10, height=10, res=300)
p1_md07
dev.off()

# Stay-at-home vs Stringency t & t7
wgt2 <- 2.5
p1_ms07 <- ggplot(cdf) +
  geom_smooth(aes(x = date, y = Residential), method = "loess", se = FALSE, size=1.5, span = 0.3, color="#287D8EFF") +
  geom_smooth(aes(x = date, y = stringency_index / wgt2), method = "loess", se = FALSE, size=1.5, span = 0.3, color="darkorange3") +
  geom_smooth(aes(x = date, y = stringency_t7lag / wgt2), method = "loess", se = FALSE, size=1, span = 0.3, color="grey", linetype = "dashed") +
  facet_wrap(~ City, nrow = 7) + 
  scale_y_continuous(sec.axis = sec_axis(trans=~.*wgt2, 
                                         name="Stringency Index")) +
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/lineplot/p1_ms07.png",units="in", width=10, height=10, res=300)
p1_ms07
dev.off()


# To do list
#  * add spline line mobility variable
#  * run models:
#      * 

## 3.3 Correlation
    ### Full sample
pc <- cor( cdf[ , c("Residential", 
                    "new_cases_per_million",
                    "casespm_t1lag",
                    "casespm_t2lag",
                    "casespm_t3lag",
                    "casespm_t4lag",
                    "casespm_t5lag",
                    "casespm_t6lag",
                    "casespm_t7lag",
                    "gr_cases",
                    "new_deaths_per_million",
                    "stringency_index",
                    "stringency_t1lag",
                    "stringency_t2lag",
                    "stringency_t3lag",
                    "stringency_t4lag",
                    "stringency_t5lag",
                    "stringency_t6lag",
                    "stringency_t7lag",
                    "Workplaces", 
                    "population_density",
                    "gdp_per_capita",
                    "aged_65_older",
                    "cardiovasc_death_rate",
                    "life_expectancy") ], 
          use = "complete.obs",
          method="pearson" )

# Change labels
colnames(pc) <- c("Stay-at-home", "New cases t", "New cases t-1", 
                 "New cases t-2", "New cases t-3", 
                 "New cases t-4", "New cases t-5", "New cases t-6", 
                  "New cases t-7", 
                  "Cases growth rate", "Deaths", "Stringency t", "Stringency t-1",
                  "Stringency t-2", "Stringency t-3", "Stringency t-4", "Stringency t-5", "Stringency t-6", "Stringency t-7",
                   "Workplace", "Pop density", "GDP", "Pop 65+", "Cardiovascular death", "Life expectancy")
rownames(pc) <- c("Stay-at-home", "New cases t", "New cases t-1", 
                  "New cases t-2", "New cases t-3", "New cases t-4", "New cases t-5", "New cases t-6", 
                  "New cases t-7", 
                  "Cases growth rate", "Cases doubling time", "Deaths", "Stringency t", "Stringency t-1",
                  "Stringency t-2", "Stringency t-3", "Stringency t-4", "Stringency t-5", "Stringency t-6", "Stringency t-7", 
                  "Workplace", "Pop density", "GDP", "Pop 65+", "Cardiovascular death", "Life expectancy")
# significance test
sig <- corrplot::cor.mtest(pc, conf.level = .95)

# create a correlogram
corrplot::corrplot(pc, type="lower",
                   method = "circle", 
                   order = "original", 
                   tl.cex = 0.7,
                   p.mat = sig$p, sig.level = .05, 
                   col = viridis::viridis(100, option = "plasma"),
                   diag = FALSE)

#######
# 4. Multilevel modelling

# data frame for regression
reg_df <- cdf %>% dplyr::select(c(
                  "City",
                  "date",
                   "Residential", 
                   "new_cases_per_million",
                   "casespm_t1lag",
                   "casespm_t2lag",
                   "casespm_t3lag",
                   "casespm_t4lag",
                   "casespm_t5lag",
                   "casespm_t6lag",
                   "casespm_t7lag",
                   "gr_cases",
                   "new_deaths_per_million",
                   "stringency_index",
                   "stringency_t1lag",
                   "stringency_t2lag",
                   "stringency_t3lag",
                   "stringency_t4lag",
                   "stringency_t5lag",
                   "stringency_t6lag",
                   "stringency_t7lag",
                   "Workplaces", 
                   "population_density",
                   "gdp_per_capita",
                   "aged_65_older",
                   "cardiovasc_death_rate",
                   "life_expectancy"))

glimpse(reg_df)

## 4.1 Model with varying intercept: city and time - crossed random effects
# specify a model equation
eq1 <- Residential ~ 1 + (1 | City) + (1 | date)
m1 <- lmer(eq1, data = reg_df)

# estimates
summary(m1)

# prediction 
reg_df$m1_stayhome <- predict(m1)

# confidence intervals for predictions
  #With simple linear regression, standard errors and confidence intervals for fitted (and predicted) values are easily computed. In R, we can use the se.fit argument in predict.lm, which returns the standard error for the fitted values, and interval = "confidence" to return confidence intervals. With linear mixed-effects models, however, it is not so easy. Neither predict.lme (from nlme) nor predict.merMod (from lme4) provide these methods, as confidence intervals on mixed-effects model predictions are harder to produce.
  #The solution is to use the parametric bootstrap, which is conveniently implemented in bootMer to be applied to models fit with the lme4 package (lmer, not glmer). Here I describe a simple wrapper around bootMer, providing an alternative for predict.merMod that calculates standard errors (and confidence intervals) for predictions.
  #see: https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
reg_df$m1_ci <- predictInterval(m1)

reg_df %>% dplyr::select( c("Residential", "m1_stayhome", "m1_ci") ) %>% 
  head()

head(reg_df$m1_ci$lwr, 5)

p1_m1 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m1_ci$fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m1_ci$lwr, ymax = m1_ci$upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p1_m1.png",units="in", width=10, height=10, res=300)
p1_m1
dev.off()


## 4.2 Model with varying intercept
# specify a model equation
eq2 <- Residential ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + new_deaths_per_million + stringency_index
m2 <- lmer(eq2, data = cdf)

# estimates
summary(m2)

# prediction 
reg_df$m2_stayhome <- predict(m2)

# confidence intervals for predictions
reg_df$m2_ci <- predictInterval(m2)

reg_df %>% dplyr::select( c("Residential", "m2_stayhome", "m2_ci") ) %>% 
  head()

p2_m2 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m2_ci$fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m2_ci$lwr, ymax = m2_ci$upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p2_m2.png",units="in", width=10, height=10, res=300)
p2_m2
dev.off()

## 4.2 Model with varying intercept
# specify a model equation
eq3 <- Residential ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + new_deaths_per_million + stringency_index + Workplaces
m3 <- lmer(eq3, data = cdf)

# estimates
summary(m3)

# prediction 
reg_df$m3_stayhome <- predict(m3)

# confidence intervals for predictions
reg_df$m3_ci <- predictInterval(m3)

reg_df %>% dplyr::select( c("Residential", "m3_stayhome", "m3_ci") ) %>% 
  head()

p3_m3 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m3_ci$fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m3_ci$lwr, ymax = m3_ci$upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p3_m3.png",units="in", width=10, height=10, res=300)
p3_m3
dev.off()

eq4 <- Residential ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + casespm_t1lag + casespm_t2lag + casespm_t3lag + new_deaths_per_million + stringency_index + Workplaces
m4 <- lmer(eq4, data = cdf)
summary(m4)

eq5 <- Residential ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + casespm_t1lag + casespm_t2lag + casespm_t3lag + casespm_t4lag + casespm_t5lag +
  new_deaths_per_million + stringency_index + stringency_t1lag + stringency_t3lag + stringency_t4lag + stringency_t5lag + Workplaces
m5 <- lmer(eq5, data = cdf)
summary(m5)

eq6 <- Residential ~ 1 + (1 | City) + (1 | date) + new_cases_per_million + casespm_t1lag + casespm_t2lag + casespm_t3lag + casespm_t4lag + casespm_t5lag + casespm_t6lag + casespm_t7lag +
  new_deaths_per_million + 
  stringency_index + stringency_t1lag + stringency_t2lag + stringency_t3lag + stringency_t4lag + stringency_t5lag + stringency_t6lag + stringency_t7lag +
  Workplaces
m6 <- lmer(eq6, data = cdf)
summary(m6)

eq7 <- Residential ~ 1 + (1 + stringency_t3lag | City) + (1 | date) + new_cases_per_million + casespm_t1lag + casespm_t2lag + casespm_t3lag +
  new_deaths_per_million + 
  stringency_index + stringency_t1lag + stringency_t3lag +
  Workplaces
m7 <- lmer(eq7, data = cdf)
summary(m7)



equatiomatic::extract_eq(m1)

library(ciTools)

## Not run: 
dat <- lme4::sleepstudy
# Fit a linear mixed model (random intercept model)
fit <- lme4::lmer(Reaction ~ Days + (1|Subject), data = lme4::sleepstudy)
# Get the fitted values for each observation in dat, and
# append CIs for those fitted values to dat
add_ci(dat, fit, alpha = 0.5)
# Try the parametric bootstrap method, and make prediction at the population level
add_ci(dat, fit, alpha = 0.5, type = "boot", includeRanef = FALSE, nSims = 100)


use this last line of code 

To do list:
  - add pop density
  - compare models using AIC 
  - estimate a model showing variations in the relationship stay at home and stringency across cities - that would be a random slope for stringency across cities

# read the FUAs polygons
fua <- st_read("../data/for_code/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)



# this is population density by sqkm
fua$pop_dens <- fua$FUA_p_2015 /  fua$FUA_area
