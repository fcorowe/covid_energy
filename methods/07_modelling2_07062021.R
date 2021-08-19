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
library(patchwork)
library(ggrepel)
# display regression equation
library(equatiomatic)
# standardise input variables
  #library(arm)
  # reporting regression results
library(broom.mixed)
library(gtsummary)
library(sjPlot)

# setting font style
  # load font
font_add_google("Roboto Condensed", "robotocondensed")
  # automatically use showtext to render text
showtext_auto()

# setting ggplot theme
set_theme(
  base = theme_tufte(),
#  theme.font = "robotocondensed",
  title.size = 1.6,
  axis.title.size = 1.2,
  axis.textsize.x = 1.1,
  axis.textsize.y = 1.1,
  axis.linecolor.x = "grey50",
  axis.line.size = 0.2,
  #geom.alpha = 0.5
)

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
reg_df$z_cases <- (reg_df$new_cases_smoothed_per_million - mean(reg_df$new_cases_smoothed_per_million, na.rm=TRUE) ) / sd(reg_df$new_cases_smoothed_per_million, na.rm=TRUE)

# note: standardised coefficients by centering y and x's on to their mean and using 1sd are
# interpreted as follows: if the standardised independent variable (x) increases by one standard deviation
# unit, on average, the standardized dependent variable (y) increases by beta_x1 standard deviation units

#######
# 4. Multilevel modelling
## 4.1 Main model: including varying intercept & time slope using natural splines + concurrent and lagged stringency & cases terms + autoregressive term
### specify a model equation
eq1 <- Residential ~ 1 +  z_stringency_index + z_cases + #fixed
   lag(z_stringency_index, 1) + # main lagged effects
  splines::ns(time, 3) + # splines
 ( splines::ns(time, 2) | City) + # random
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
  (z_cases | City) #+ # random
  #ar1( factor(time) + 0 | City) # autoregressive term

m3 <- glmmTMB(eq3, 
              #dispformula = ~ 0, 
              REML = TRUE,
              data = reg_df,
              na.action = na.omit)

### estimates
summary(m3)

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
          dv.labels = c("Autoregressive", "Stringency random effects", "New COVID-19 cases random effects"),
          file = "../outputs/modelling/estimates/reg_table.txt"
)

## regression plots

 ### main fixed effects
# p1_m1me <- plot_model(m1,
#            sort.est = TRUE,
#            terms = c("z_stringency_index", "z_cases", "lag(z_stringency_index, 1)"),
#            show.values = TRUE,
#            axis.labels = c("New Cases t", "Stringency t-1", "Stringency t"),
#            title = "Main fixed effects",
#            colors = "#0072B2",
#            dot.size = 4,
#            line.size = 0.8,
#            width = .05
# )

p1_m1me <- plot_model(m1,terms = c("z_stringency_index", "z_cases", "lag(z_stringency_index, 1)")) %>% 
  .$data %>% ggplot(aes(x=estimate, y=term, color=group)) +
  geom_point(size = 3.5,  colour = "black") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width=.05, color= "black") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_manual(values=c("#88ccee")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10.5)
  ) +
  scale_y_discrete(labels=c("z_stringency_index" = "Stringency t", 
                            "z_cases" = "New cases t",
                            "lag(z_stringency_index, 1)" = "Stringency t-1")) +
  geom_vline(xintercept=0, 
             linetype="solid", 
             color = "grey20",
             size=1,
             alpha =.2) +
  labs(title = "c",
       subtitle = " Main fixed effects")

  ### stringency random effects
m2_re <- plot_model(m2,
           type ="re",
           terms = "z_stringency_index",
           sort.est = "z_stringency_index"
) %>% .$data

p2_m2re <- m2_re %>% filter(facet =="z_stringency_index") %>% 
  ggplot(aes(x=estimate, y=term, color=group)) +
  geom_point(size = 3.5,  colour = "black") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width=.8, color= "black") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_manual(values=c("#ddcc77", "#88ccee")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10.5)
        ) +
  geom_vline(xintercept=0, 
             linetype="solid", 
             color = "grey20",
             size=1,
             alpha =.2) +
  labs(title = "d",
       subtitle = "Stringency")


  ### cases random effects
m3_re <- plot_model(m3,
                    type ="re",
                    terms = "z_cases",
                    sort.est = "z_cases"
) %>% .$data

p3_m3re <- m3_re %>% filter(facet =="z_cases") %>% 
  ggplot(aes(x=estimate, y=term, color=group)) +
  geom_point(size = 3.5,  colour = "black") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width=.8, color= "black") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_manual(values=c("#ddcc77", "#88ccee")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10.5)
  ) +
  geom_vline(xintercept=0, 
             linetype="solid", 
             color = "grey20",
             size=1,
             alpha =.2) +
  labs(title = "e",
       subtitle = "COVID-19 cases")

p1_m1me + p2_m2re + p3_m3re

png("../outputs/modelling/estimates/glmmtmb_plots.png", units="in", width=15, height=8, res=300)
p1_m1me + p2_m2re + p3_m3re
dev.off()


# Creating a classification of cities
  ## Creating the dfs for random effects
  m2_re_subset <- m2_re %>% filter(facet == "z_stringency_index")

  m3_re_subset <- m3_re %>% dplyr::select(estimate, facet, term) %>% 
    filter(facet == "z_cases") %>% 
    rename(
      cases = estimate,
      facet_cases = facet,
      term_cases = term 
    )
  
   ## Sorting data by city
    ## list of cities
  cities_data <- read.csv("../data/for_code/UN_Cities_names.csv")

    ### define a vector of city names
  cities_data <- cities_data %>% arrange(first.case.rank)
  cities <- as.vector(cities_data[,5]) 
  cities[cities=="S\x8bo Paulo"] <- "São Paulo"

    ### Use a factor to sort data by cities
  m2_re_subset <- m2_re_subset[order(m2_re_subset$term),]
  m2_re_subset$term <- ordered(m2_re_subset$term, levels = cities)

  m3_re_subset <- m3_re_subset[order(m3_re_subset$term_cases),]
  m3_re_subset$term_cases <- ordered(m3_re_subset$term_cases, levels = cities)

re_df <- left_join(x = m2_re_subset, y = m3_re_subset,  by = c("term" = "term_cases"))

re_df <- cbind(m2_re_subset, m3_re_subset)

class_p <- ggplot(re_df, 
       aes(x = estimate, y = cases)) + 
  geom_point(size = 4, color="#F0E442") +
  geom_text_repel(aes(label=term)) +
  geom_vline(xintercept=0, 
             linetype="solid", 
             color = "grey20",
             size=1,
             alpha =.2) +
  geom_hline(yintercept=0, 
             linetype="solid", 
             color = "grey20",
             size=1,
             alpha =.2) +
  labs(title="f",
       x="Stringency random effects", y = "COVID-19 cases random effects") +
  theme(axis.title=element_text(size=18))

png("../outputs/modelling/estimates/reclassification_plot.png", units="in", width=15, height=8, res=300)
class_p 
dev.off()

