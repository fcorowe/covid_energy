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
# reporting regression results
library(sjPlot)
#library(broom.mixed)
#library(gtsummary)
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

reg_df <- read_csv("../data/modelling_df.csv")

#######
# 4. Multilevel modelling

## 4.1 Model with varying intercept: linear 
# specify a model equation
eq1 <- Residential ~ 1 + time + (1 | City)
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

p1_m1 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m1_ci$fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m1_ci$lwr, ymax = m1_ci$upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

## 4.2 Model with varying intercept & time slope
# specify a model equation
eq2 <- Residential ~ 1 + time + (1 + time | City)
m2 <- glmmTMB(eq2, data = reg_df)

# estimates
summary(m2)


# prediction
reg_df <- predict(m2, reg_df, se.fit=TRUE) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

# confidence intervals for predictions
reg_df <- reg_df %>% rename(m2_fit = fit,
                            m2_se_fit = se.fit)
reg_df$m2_ci_lwr <- reg_df$m2_fit - 2.58 * reg_df$m2_se_fit
reg_df$m2_ci_upr <- reg_df$m2_fit + 2.58 * reg_df$m2_se_fit

reg_df %>% dplyr::select( c("Residential", "m2_fit", "m2_ci_lwr", "m2_ci_upr") ) %>% 
  head()

p2_m2 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m2_fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m2_ci_lwr, ymax = m2_ci_upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")


## 4.3 Model with varying intercept & time slope using natural splines
# specify a model equation
eq3 <- Residential ~ 1 + splines::ns(time, 4) + (1 + time | City)
m3 <- glmmTMB(eq3, data = reg_df)

# estimates
summary(m3)
tbl_m3 <- tbl_regression(m3) %>% 
  add_significance_stars()

# prediction
reg_df <- predict(m3, reg_df, se.fit=TRUE) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

# confidence intervals for predictions
reg_df <- reg_df %>% dplyr::rename(m3_fit = fit,
                            m3_se_fit = se.fit)
reg_df$m3_ci_lwr <- reg_df$m3_fit - 2.58 * reg_df$m3_se_fit
reg_df$m3_ci_upr <- reg_df$m3_fit + 2.58 * reg_df$m3_se_fit

reg_df %>% dplyr::select( c("Residential", "m3_fit", "m3_ci_lwr", "m3_ci_upr") ) %>% 
  head()

p3_m3 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m3_fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m3_ci_lwr, ymax = m3_ci_upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")


## 4.4 Model with varying intercept & time slope using natural splines + stringency & cases
# specify a model equation
eq4 <- Residential ~ 1 + splines::ns(time, 4) + z_stringency_index + z_cases + #fixed
  (1 + splines::ns(time, 4) | City) # random
m4 <- glmmTMB(eq4, data = reg_df)

# estimates
summary(m4)

# prediction
reg_df <- predict(m4, reg_df, se.fit=TRUE) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

# confidence intervals for predictions
reg_df <- reg_df %>% dplyr::rename(m4_fit = fit,
                            m4_se_fit = se.fit)
reg_df$m4_ci_lwr <- reg_df$m4_fit - 2.58 * reg_df$m4_se_fit
reg_df$m4_ci_upr <- reg_df$m4_fit + 2.58 * reg_df$m4_se_fit

reg_df %>% dplyr::select( c("Residential", "m4_fit", "m4_ci_lwr", "m4_ci_upr") ) %>% 
  head()

p4_m4 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_line(aes(x = date, y = m4_fit), size=1.5, color="darkblue") +
  geom_ribbon(aes(ymin = m4_ci_lwr, ymax = m4_ci_upr), linetype = 5, alpha=0.4) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p4_m4.png",units="in", width=10, height=10, res=300)
p4_m4
dev.off()

## 4.5 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term
### specify a model equation
eq5 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m5 <- glmmTMB(eq5, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df)

### estimates
summary(m5)

### prediction
reg_df <- predict(m5, reg_df, se.fit=TRUE) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

### confidence intervals for predictions
reg_df <- reg_df %>% dplyr::rename(m5_fit = fit,
                            m5_se_fit = se.fit)
reg_df$m5_ci_lwr <- reg_df$m5_fit - 1.96 * reg_df$m5_se_fit
reg_df$m5_ci_upr <- reg_df$m5_fit + 1.96 * reg_df$m5_se_fit

reg_df %>% dplyr::select( c("Residential", "m5_fit", "m5_ci_lwr", "m5_ci_upr") ) %>% 
  head()

p5_m5 <-  ggplot(reg_df, aes(x = date, y = Residential)) + 
  geom_line(aes(x = date, y = m5_fit), size=1.5, color="darkblue") +
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_ribbon(aes(ymin = m5_ci_lwr, ymax = m5_ci_upr), linetype = 2, alpha=0.2) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p5_m5.png",units="in", width=10, height=10, res=300)
p5_m5
dev.off()

## 4.6 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=1
time_lag <- 1
### specify a model equation
eq6 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index + z_cases + # fixed - main effects
  lag(z_stringency_index, time_lag) +  lag(z_cases, time_lag) + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m6 <- glmmTMB(eq6, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m6)
tbl_m6 <- tbl_regression(m6) %>% 
  add_significance_stars()

### prediction
reg_df <- predict(m6, reg_df, se.fit=TRUE, na.action = na.omit) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

### 

### confidence intervals for predictions
reg_df <- reg_df %>% rename(m6_fit = fit,
                            m6_se_fit = se.fit)
reg_df$m6_ci_lwr <- reg_df$m6_fit - 2.58 * reg_df$m6_se_fit
reg_df$m6_ci_upr <- reg_df$m6_fit + 2.58 * reg_df$m6_se_fit

reg_df %>% dplyr::select( c("Residential", "m6_fit", "m6_ci_lwr", "m6_ci_upr") ) %>% 
  head()

p6_m6 <-  ggplot(data= subset(reg_df, time!=time_lag & time!=max(reg_df$time)), aes(x = date, y = Residential)) + 
  geom_line(aes(x = date, y = m6_fit), size=1.5, color="darkblue") +
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_ribbon(aes(ymin = m6_ci_lwr, ymax = m6_ci_upr), linetype = 6, alpha=0.6) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p6_m6.png",units="in", width=10, height=10, res=300)
p6_m6
dev.off()

## 4.7 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 2
### specify a model equation
eq7 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index + lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) #+ # random
  #ar1( factor(time) + 0 | City) # autoregressive term

m7 <- glmmTMB(eq7, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

# m7b <- nlme::lme(Residential ~ splines::ns(time, 3) + z_stringency_index + lag(z_stringency_index, 2), #fixed
#         random = ~ splines::ns(time, 3) | City,
#         correlation = corAR1( form = ~ time | City),
#         data = reg_df)
  

### estimates
summary(m7)
tbl_m7 <- tbl_regression(m7) %>% 
  add_significance_stars()

### prediction
reg_df <- predict(m7, reg_df, se.fit=TRUE, na.action = na.omit) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

### confidence intervals for predictions
reg_df <- reg_df %>% rename(m7_fit = fit,
                            m7_se_fit = se.fit)
reg_df$m7_ci_lwr <- reg_df$m7_fit - 2.58 * reg_df$m7_se_fit
reg_df$m7_ci_upr <- reg_df$m7_fit + 2.58 * reg_df$m7_se_fit

reg_df %>% dplyr::select( c("Residential", "m7_fit", "m7_ci_lwr", "m7_ci_upr") ) %>% 
  head()

p7_m7 <-  ggplot(data= subset(reg_df, time!=time_lag & time!=max(reg_df$time)), aes(x = date, y = Residential)) + 
  geom_line(aes(x = date, y = m7_fit), size=1.5, color="darkblue") +
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_ribbon(aes(ymin = m7_ci_lwr  , ymax = m7_ci_upr), linetype = 6, alpha=0.6) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p7_m7.png",units="in", width=10, height=10, res=300)
p7_m7
dev.off()

## 4.8 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 3
### specify a model equation
eq8 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
(1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m8 <- glmmTMB(eq8, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m8)
tbl_m8 <- tbl_regression(m8) %>% 
  add_significance_stars()

### prediction
reg_df <- predict(m8, reg_df, se.fit=TRUE, na.action = na.omit) %>% 
  sapply(., function(x){as.numeric(x[1:nrow(reg_df)])}) %>%
  as.data.frame(.) %>% 
  bind_cols(reg_df, .)

### confidence intervals for predictions
reg_df <- reg_df %>% rename(m8_fit = fit,
                            m8_se_fit = se.fit)
reg_df$m8_ci_lwr <- reg_df$m8_fit - 2.58 * reg_df$m8_se_fit
reg_df$m8_ci_upr <- reg_df$m8_fit + 2.58 * reg_df$m8_se_fit

reg_df %>% dplyr::select( c("Residential", "m8_fit", "m8_ci_lwr", "m8_ci_upr") ) %>% 
  head()

p8_m8 <-  ggplot(data= subset(reg_df, time!=time_lag & time!=max(reg_df$time)), aes(x = date, y = Residential)) + 
  geom_line(aes(x = date, y = m8_fit), size=1.5, color="darkblue") +
  geom_point(aes(x = date, y = Residential), size=0.5, color="grey50") +
  geom_ribbon(aes(ymin = m8_ci_lwr  , ymax = m8_ci_upr), linetype = 6, alpha=0.6) +
  facet_wrap(~ City, nrow = 7) + 
  theme_tufte() + 
  theme(legend.position = "none") +
  labs(x= "Date",
       y = "Stay-at-Home Rate (%)")

png("../outputs/modelling/prediction/p8_m8.png",units="in", width=10, height=10, res=300)
p8_m8
dev.off()


## 4.9 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 4
### specify a model equation
eq9 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m9 <- glmmTMB(eq9, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m9)
tbl_m9 <- tbl_regression(m9) %>% 
  add_significance_stars()

source(system.file("other_methods", "extract.R", package="glmmTMB"))
texreg(m9, caption="Try", label=" ")

## 4.10 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 4
### specify a model equation
eq10 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m10 <- glmmTMB(eq10, 
              dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m10)
tbl_m10 <- tbl_regression(m10) %>% 
  add_significance_stars()

## 4.11 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 5
### specify a model equation
eq11 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m11 <- glmmTMB(eq11, 
               dispformula = ~ 0, 
               REML = TRUE,
               data = reg_df,
               na.action = na.omit)

### estimates
summary(m11)
tbl_m11 <- tbl_regression(m11) %>% 
  add_significance_stars()

## 4.12 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 6
### specify a model equation
eq12 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m12 <- glmmTMB(eq12, 
               dispformula = ~ 0, 
               REML = TRUE,
               data = reg_df,
               na.action = na.omit)

### estimates
summary(m12)
tbl_m12 <- tbl_regression(m12) %>% 
  add_significance_stars()

## 4.13 Model with varying intercept & time slope using natural splines + stringency & cases + autoregressive term + lags t=2
time_lag <- 7
### specify a model equation
eq13 <- Residential ~ 1 + splines::ns(time, 3) + z_stringency_index +  lag(z_stringency_index, time_lag) + z_cases + #fixed
  (1 + splines::ns(time, 3) | City) + # random
  ar1( factor(time) + 0 | City) # autoregressive term

m13 <- glmmTMB(eq13, 
               dispformula = ~ 0, 
               REML = TRUE,
               data = reg_df,
               na.action = na.omit)

### estimates
summary(m13)
tbl_m13 <- tbl_regression(m13) %>% 
  add_significance_stars()


### Summary tables
theme_gtsummary_compact()
#> Setting theme `Compact`
tbl_merge(
  list(tbl_m2, tbl_m3),
  tab_spanner = c("**Model 2**", "**Model 3**")
) 






We need evidence of:
- a *positive* relationship between changes in the percentage of stay home, and changes in stringency measures.
  > We have this from the scatter plots which show a positive relationship across the board
  > Need evidence from regression models too!
- a *negative* relationship between changes in stringency measures and lagged new COVID cases
  > Need to produce a scatter plot on this
  > Need to capture this in a separate regression. Maybe make the coefficient of new COVID cases a function of stringency
- no relationship between changes in the percentage of stay home and new COVID cases
  > We have this from the scatter plots which largely show this relationship
  > Need evidence from regression models too!





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
