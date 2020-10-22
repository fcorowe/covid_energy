library(countrycode)
library(tidyverse)
library(timetk)
library(ggpubr)
library(lme4)
library(ggcorrplot)
library(corrplot)
library(viridis)
library(sf)
library(ggthemes)

rm(list=ls())

# Google’s COVID-19 Community Mobility Reports show how visits and length of stay in 
# various place categories have changed compared to a baseline period before the pandemic.

# we focus on the residential category, which is defined as the time users spent at home, 
# using the home addresses provided to or estimated by Google Maps. 
# Our focus on a variable related to time use and duration of events is consistent 
# with the epidemiological literature on time use and the spread of close-contact infectious diseases.

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
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p2 <- cdf %>% dplyr::filter(as.integer(City) %in% c(12:20)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p3 <- cdf %>% dplyr::filter(as.integer(City) %in% c(21:29)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p4 <- cdf %>% dplyr::filter(as.integer(City) %in% c(30:38)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p5 <- cdf %>% dplyr::filter(as.integer(City) %in% c(39:47)) %>% 
  group_by(City) %>% 
  plot_acf_diagnostics(date, Residential, 
                       .lags = "14 days",
                       .line_size = 2,
                       .show_white_noise_bars = TRUE,
                       .interactive = FALSE
  )

p6 <- cdf %>% dplyr::filter(as.integer(City) %in% c(48:50)) %>% 
  group_by(City) %>% 
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


# To do list
#  * create a correlogram for each city
#  * create scatter plots bwt mobility vs COVID cases, deaths and stringency at lags t1, t7 and t14
#  * add spline line mobility variable
#  * run models:
#      * 

## Scatteplot Mobility vs COVID cases, deaths and stringency
p1_mc0 <- ggplot(cdf, aes(x = new_cases_per_million, y = Residential)) +
  geom_point(colour = "darkblue", alpha = 0.1) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.3, color="darkblue") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Daily New Confirmed COVID-19 Cases Number Per Million \n (Weekly Moving Average)",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_mc0.png",units="in", width=10, height=10, res=300)
p1_mc0
dev.off()

p1_mc0 <- ggplot(cdf, aes(x = new_cases_per_million, y = Residential)) +
  geom_point(colour = "darkblue", alpha = 0.1) + 
  geom_smooth(method = "loess", se = FALSE, size=2, span = 0.3, color="darkblue") +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Daily New Confirmed COVID-19 Cases Number Per Million \n (Weekly Moving Average)",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/scatterplot/p1_mc0.png",units="in", width=10, height=10, res=300)
p1_mc0
dev.off()

plotList <- list()
plotList2 <- list()

# we run this code for two groups of  25 cities

for (j in levels(cdf$City)[1:25]) {
  
  plotList[[1]] <- ggplot(cdf, aes(x = date, y = Residential, colour= "#74787b")) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.3) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(text = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.1,0.1,-0.5,0.1), "cm")) + # set the bottom margin to negative
    scale_color_manual(values = cpal) +
    guides(color=guide_legend(""))
  
  plotList[[2]] <- ggplot(data4, aes(x = Date, y = Residential, colour= "#000000")) +
    geom_smooth(method = "loess", se = FALSE, size=1, span = 0.3) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(text = element_text(size = 20),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(-0.5,0.1,0.1,0.1), "cm")) + # set the upper margin to negative
    scale_color_manual(values = cpal) +
    guides(color=guide_legend(""))
  
  
  
  
  
  plotList2[[j]] <- annotate_figure(ggarrange(plotlist = plotList, ncol=1, nrow=2), 
                                    top = text_grob(j, size = 10))
  
}


png("mobility_plot_v1.png",units="in", width=10, height=10, res=300)
ggarrange(plotlist = plotList2, ncol=5, nrow=5)
dev.off() #


## 3.4 Correlation
    ### Full sample
pc <- cor( cdf[ , c("Residential", 
                    "Workplaces", 
                    "new_cases_per_million",
                    "cases_t1lag",
                    "gr_cases",
                    "dng_time",
                    "new_deaths_per_million",
                    "stringency_index",
                    "population_density",
                    "gdp_per_capita",
                    "aged_65_older",
                    "cardiovasc_death_rate",
                    "life_expectancy") ], 
          use = "complete.obs",
           method="pearson" )

# Change labels
colnames(pc) <- c("Stay-at-home", "Workplace", "New cases", "New cases t-1", "Cases growth rate", "Cases doubling time", "Deaths", "Stringency", "Pop density", "GDP", "Pop 65+", "Cardiovascular death", "Life expectancy")
rownames(pc) <- c("Stay-at-home", "Workplace", "New cases", "New cases t-1", "Cases growth rate", "Cases doubling time", "Deaths", "Stringency", "Pop density", "GDP", "Pop 65+", "Cardiovascular death", "Life expectancy")

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


   ### By city
corr <- cdf %>%
  group_by(City) %>% 
  dplyr::summarise(r = cor(Workplaces, 
                    Residential, 
                    new_cases_per_million, 
                    new_deaths_per_million, 
                    stringency_index))


vars_keep <- names(cdf)[c("Workplaces", "Residential", "new_cases_per_million")]
some <- cdf %>% split(.$City) %>% 
  map(select, vars_keep) %>%
  map(cor)

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






use this last line of code 


# read the FUAs polygons
fua <- st_read("../data/for_code/FUA_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)



# this is population density by sqkm
fua$pop_dens <- fua$FUA_p_2015 /  fua$FUA_area
