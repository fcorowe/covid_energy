library(countrycode)
library(dplyr)

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

# normalise cases and deaths to be between 0 and 100
case_data_subset <- case_data_subset %>% group_by(iso_code) %>% mutate(new_cases_nor = 100*(new_cases-min(new_cases))/(max(new_cases)-min(new_cases)),
                                                   new_deaths_nor = 100*(new_deaths-min(new_deaths))/(max(new_deaths)-min(new_deaths)))


# read in mobility data ---------------------------------------------------

# read mobility data
mob_data <- read.csv("../data/for_code/mobility_and_stringency_uncities.csv")

#attach the country code
mob_data$iso_code <- countrycode(mob_data[,2], "country.name", "iso3c")

# modify the date column
mob_data$Date <- as.Date(mob_data$Date, "%d/%m/%Y")

# set the maximum date to end of March
mob_data <- subset(mob_data, Date <= as.Date("2020-03-31") )

# extract the month
mob_data$month <- strftime(mob_data$Date, "%m")





# combine cases and mobility data
combined_data <- left_join(mob_data, case_data_subset, by = c("iso_code" = "iso_code", "Date" = "date"))



# I convert the dataset from wide to ling format as it will help in the visualisation
library(tidyr)
mob_data_long <- gather(combined_data, variable, measure, c("Stringency", "Workplaces", "Residential",
                                                            "new_cases_nor", "new_deaths_nor"), factor_key=TRUE)

mob_data_long$variable <- as.character(mob_data_long$variable)

# rename new cases and deaths
mob_data_long$variable[mob_data_long$variable == "new_cases_nor"] <- "New cases"
mob_data_long$variable[mob_data_long$variable == "new_deaths_nor"] <- "New deaths"

# specify the order we want to be plotted
mob_data_long$variable <- factor(mob_data_long$variable, 
                                 levels = c("Stringency", "Residential", "Workplaces",
                                            "New cases", "New deaths"))

# I use this as a factor to have the correct order  in the facets

mob_data_long$City <- factor(mob_data_long$City, levels = cities)


# specify colours
cpal =c("Stringency" = "#e41a1c",
        "Residential" = "#377eb8",
        "Workplaces" = "#4daf4a", 
        "New cases" = "#984ea3", 
        "New deaths" = "#ff7f00")

library(ggplot2)
library(ggpubr)

# subset in two datasets
# one for the mobility data and one for stringency and cases/deaths
# they will be used to create two different plots
data3 <- subset(mob_data_long, variable %in% c("Workplaces", "Residential"))
data4 <- subset(mob_data_long, variable %in% c("Stringency", "New cases", "New deaths"))



# plot a figure with two plots for each city
plotList <- list()
plotList2 <- list()


# we run this code for two groups of  25 cities

for (j in levels(mob_data_long$City)[1:25]) {
    
    plotList[[1]] <- ggplot(data3, aes(x = Date, y = measure, colour= variable)) +
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
    
    plotList[[2]] <- ggplot(data4, aes(x = Date, y = measure, colour= variable)) +
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




# Figure V2 ---------------------------------------------------------------

# Using this code we produce facets that show each variable on the x axis and each city on the y axis
# we do this for groups of 10 cities

# we subset the frist 10 cities
data_subset <- subset(mob_data_long, City %in% unique(mob_data_long$City) [1:10])



ggsave(file = 'mobility_plot_v2.png', units="in", width=10, height=15, dpi=300, 
       ggplot(data_subset, aes(x = Date, y = measure, colour = variable)) +
         geom_smooth(method = 'loess', se = FALSE, size=1, span = 0.3) +
         xlab("Date, 2020") +
         ylab("Mobility/Stringency measures and COVID-19 cases/deaths") +
         theme_minimal() +
         theme(text = element_text(size = 20),
               axis.text.x = element_text(angle = 90),
               #axis.text.y = element_blank(),
               legend.position = "bottom",
               legend.direction = "horizontal",
               strip.text.y = element_text(size = 10)) +
         scale_color_manual(values = cpal) +
         guides(color=guide_legend("")) +
         facet_grid(City ~ variable)
)



