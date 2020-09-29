library(countrycode)
library(tidyverse)
library(viridis)

# covid data
case_data <- read.csv('/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/methods/covid/owid-covid-data.csv')

# list of cities
cities_data <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised.csv")

# countries by first confirmed case
ctry_rank <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/dates_first_case/date_ctry_fcase.csv")

cities <- as.vector(cities_data[,1])

country <- countrycode(cities_data[,2], "country.name", "iso3c")

# make sure that I exclude Hong Kong as we assume that it is reported with China
case_data_subset <- subset(case_data, iso_code %in% unique(country)[-grep("HKG", unique(country))])

# include dates only up to 
case_data_subset <- subset(case_data_subset, date <= as.Date("2020-04-30") )

# convert the date to the correct format
case_data_subset$date <- as.Date(case_data_subset$date)

# replace zero cases with one so to calculate log
# note that the log of one is zero
case_data_subset$new_cases[ case_data_subset$new_cases <= 0] <- 1
case_data_subset$new_cases_per_million[ case_data_subset$new_cases_per_million <= 0] <- 1
case_data_subset$new_deaths[ case_data_subset$new_deaths <= 0] <- 1
case_data_subset$new_deaths_per_million[ case_data_subset$new_deaths_per_million  <= 0] <- 1

# adding ranking of countries by first confirmed case
case_data_subset <- left_join(case_data_subset, ctry_rank, by = c("location" = "ctry_nm"), keep = TRUE)
case_data_subset <- case_data_subset %>% arrange(rank)

# remove duplicated elements
ctry_nm_factor <- case_data_subset$ctry_nm[!duplicated(case_data_subset$ctry_nm)]

# create factor to sort plots by country in order of first confirmed case
case_data_subset$ctry_nm <- factor(case_data_subset$ctry_nm,
                                   levels = ctry_nm_factor,
                                   labels = ctry_nm_factor)

ggsave(file = 'new_smoothed.png', units = "in", width = 10, height = 10, dpi = 300, 
       
       ggplot( case_data_subset) +
         geom_smooth( aes( x = date, y = log( new_cases)), method = 'loess', se = FALSE, color = "#440154FF", size = 2, span = 0.2, show.legend = TRUE) +
         geom_smooth( aes( x = date, y = log( new_deaths)), method = 'loess', se = FALSE, color = "#287D8EFF", size = 2, span = 0.2, show.legend = TRUE) +
         geom_smooth( aes( x = date, y = (stringency_index / 10)), method = 'loess', se = FALSE, color = "#FDE725FF", size = 2, span = 0.2, show.legend = TRUE) +
        #geom_rect( data = NULL, aes( xmin = "0", xmax = "1", ymin = -Inf, ymax = Inf), fill="lightblue") +
         xlab( "Date 2020") +
         ylab( "Number of cases (purple) / deaths (blue) \n (Log)") +
         #scale_color_manual(values = c("#023FA5", "#8E063B")) +
         scale_y_continuous(limits = c(0, 10),
                            sec.axis = sec_axis(trans=~.*10, 
                                                name="Stringency Index")) +
         theme_classic() +
         theme(text = element_text(size = 15),
               axis.text.x = element_text(angle = 90),
               strip.background = element_rect(colour = "white", fill = "white"),
               legend.title = element_text( size = 14, color = "black"), 
               legend.position = "bottom",
               legend.box = "horizontal") +
         facet_wrap(ctry_nm ~ ., nrow = 7) #, scales = "free_y"
       
       )


ggsave(file = 'newpmillion_smoothed.png', units = "in", width = 10, height = 10, dpi = 300, 
       
       ggplot(case_data_subset) +
         geom_smooth(aes(x = date, y = log(new_cases_per_million) ), method = 'loess', se = FALSE, color = "#023FA5", size = 2, span = 0.2, show.legend = TRUE) +
         geom_smooth(aes(x = date, y = log(new_deaths_per_million)), method = 'loess', se = FALSE, color = "#8E063B", size = 2, span = 0.2, show.legend = TRUE) +
               geom_rect(data = NULL,
                         fill = "red", 
                         xmin = decimal_date(as.Date(c("2020-01-01"))),
                         xmax = decimal_date(as.Date(c("2020-01-31"))),
                         ymin = -Inf,
                         ymax = Inf
               ) +
         xlab("Date 2020") +
         ylab("Number of cases (red) / deaths (blue) per million \n (Log)") +
         #scale_color_manual(values = c("#023FA5", "#8E063B")) +
         scale_y_continuous(limits = c(0, 5)) +
         theme_classic() +
         theme(text = element_text(size = 15),
               axis.text.x = element_text(angle = 90),
               strip.background = element_rect(colour = "white", fill = "white"),
               legend.title = element_text( size = 14, color = "black"), 
               legend.position = "bottom",
               legend.box = "horizontal") +
         facet_wrap(location ~ ., nrow = 7) #, scales = "free_y"
       
)


##############################
# Create plots shading the plot background identifying changes in lockdown measures

        # Create new data frame
        shade <- case_data_subset %>% dplyr::select(ctry_nm, date, stringency_index)
        # Create an index to identify changes in lockdown measures
        inx <- c(FALSE, diff(shade$stringency_index) != 0)
        shade <- shade[inx, ]
        
        # Remove Nas
        shade <- shade %>% dplyr::filter(!is.na(stringency_index))
        
        # add end date
        shade <- shade %>% mutate(
                end = lead(date)
        )
        shade$end[is.na(shade$end)] <- as.Date("2020-04-30", origin="1970-01-01")
        # rename date to start date
        shade <- shade %>% rename(
                "start" = "date"
        )
        shade$y1 = -Inf
        shade$y2 = Inf

        ggsave(file = 'shaded_plot.png', units = "in", width = 10, height = 10, dpi = 300,         
        
                pt <- ggplot( case_data_subset) +
                geom_smooth( aes( x = date, y = log( new_cases)), method = 'loess', se = FALSE, color = "#440154FF", size = 2, span = 0.2, show.legend = TRUE) +
                geom_smooth( aes( x = date, y = log( new_deaths)), method = 'loess', se = FALSE, color = "#287D8EFF", size = 2, span = 0.2, show.legend = TRUE) +
                       geom_rect(data = shade, ymin = 0, ymax = Inf, 
                                 aes(NULL, NULL, xmin = start, xmax = end, fill = stringency_index),
                                 alpha = .2) +
                xlab( "Date 2020") +
                ylab( "Number of cases (purple) / deaths (blue) \n (Log)") +
                scale_y_continuous(limits = c(0, 10)) +
                theme_classic() +
                theme(text = element_text(size = 15),
                      axis.text.x = element_text(angle = 90),
                      strip.background = element_rect(colour = "white", fill = "white"),
                      legend.title = element_text( size = 14, color = "black"), 
                      legend.position = "bottom",
                      legend.box = "horizontal") +
                facet_wrap(ctry_nm ~ ., nrow = 7) #, scales = "free_y"        
              
        )
        
