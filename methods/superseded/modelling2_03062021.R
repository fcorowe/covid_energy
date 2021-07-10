# data wrangling
library(tidyverse)
# data utilities
library(countrycode)
library(timetk)
# estimating mixed effects models
library(lme4)
library(merTools)
library(glmmTMB)
library(nlme)
# correlograms
library(ggcorrplot)
library(corrplot)
# data visualisation
library(viridis)
library(ggthemes)
library(ggpubr)
library(zoo)
library(showtext)
# display regression equation
library(equatiomatic)
# standardise input variables
  #library(arm)
  # reporting regression results
library(broom.mixed)
library(gtsummary)
library(sjPlot)
#library(parameters)
#library(jtools)
#library(AICcmodavg)
#library(mixedup)

rm(list=ls())

# Google’s COVID-19 Community Mobility Reports show how visits and length of stay in 
# various place categories have changed compared to a baseline period before the pandemic.

# we focus on the residential category, which is defined as the time users spent at home, 
# using the home addresses provided to or estimated by Google Maps. 
# Our focus on a variable related to time use and duration of events is consistent 
# with the ep/Users/Franciscorowe 1/Dropbox/Francisco/Research/in_progress/covid19_energy/github/covid_energy/methodsidemiological literature on time use and the spread of close-contact infectious diseases.

reg_df <- read_csv("../data/modelling_df.csv") %>% as.data.frame()
attr(reg_df, 'spec') <- NULL

# Standardising
reg_df$z_stringency_index <- (reg_df$stringency_index - mean(reg_df$stringency_index) ) / sd(reg_df$stringency_index)
reg_df$z_cases <- (reg_df$new_cases_per_million - mean(reg_df$new_cases_per_million) ) / sd(reg_df$new_cases_per_million)

# note: standardised coefficients by centering y and x's on to their mean and using 1sd are
# interpreted as follows: if the standardised independent variable (x) increases by one standard deviation
# unit, on average, the standardized dependent variable (y) increases by beta_x1 standard deviation units

# 2 standard deviations were used to standardise independent variables. So coefficients should be
# interpreted in measures of 2 standard deviations. A coefficient of 2.25 indicates that a change of 2
# standard deviations in the independent variable results in a 2.25 increase in
# the dependent variable of its standard deviations

#######
# 4. Multilevel modelling
## 4.1 Main model: including varying intercept & time slope using natural splines + concurrent and lagged stringency & cases terms + autoregressive term
### specify a model equation
eq1 <- Residential ~ 1 +  z_stringency_index + z_cases + #fixed
   lag(z_stringency_index, 1) + # main lagged effects
  splines::ns(time, 3) + # splines
  ( splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m1 <- glmmTMB(eq1, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m1)


## 4.2 Model testing city variation in stringency
  # *Outcome*: the results provide evidence of an immediate impact of the stringency measures on increasing the share of population staying at home
### specify a model equation
eq2 <- Residential ~ 1 + z_stringency_index + lag(z_stringency_index, 1) + #fixed
  z_cases  + # main lagged effects
  splines::ns(time, 3) + # splines
  (z_stringency_index | City) #+ # random
#  ar1( factor(time) + 0 | City) # autoregressive term

m2 <- glmmTMB(eq2, 
              #dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m2)


## 4.3 Model testing city variation in cases
  # *Outcome*: the results show no significant relationship between new cases and mobility
### specify a model equation
eq3 <- Residential ~ 1 + z_stringency_index + lag(z_stringency_index, 1) + # main fixed effects
  z_cases + lag(z_cases, 1) +  # main fixed lagged effects
  splines::ns(time, 3) + # splines
  (z_cases + lag(z_cases, 1) | City) #+ # random
  #ar1( factor(time) + 0 | City) # autoregressive term

m3 <- glmmTMB(eq3, 
              #dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m3)

## regression plot for fixed effects from full model
plot_model(m1,
           terms = c("z_stringency_index", "z_cases", "lag(z_stringency_index, 1)")
           )

## regression table
tab_model(m1, m2, m3, 
          collapse.ci = TRUE,
          p.style = "stars",
          show.aic = TRUE,
          #terms = c(), # show selected coefficients
          #rm.terms = c("time [1st degree]"), #remove selected coefficients
          pred.labels = c("Intercept", "Stringency t", "New Cases t", "Stringency t-1",
                          "Spline 1st degree", "Spline 2nd degree", 
                          "Spline 3rd degree", "New cases t-1"),
          dv.labels = c("Autoregressive", "Stringency random effects", "New cases random effects")
          )

