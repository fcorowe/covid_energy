# 1. Dependencies
library(tidyverse)
library(raster)
library(sf)
library(countrycode)
library(dplyr)
library(patchwork)
library(viridis)
library(viridisLite)

# 2. Read data
## 2.1 raster data
dec_data <- raster("../data/ntl/dec_data.tif")
jan_data <- raster("../data/ntl/jan_data.tif")
feb_data <- raster("../data/ntl/feb_data.tif")
mar_data <- raster("../data/ntl/mar_data.tif")
apr_data <- raster("../data/ntl/apr_data.tif")
may_data <- raster("../data/ntl/may_data.tif")
jun_data <- raster("../data/ntl/june_data.tif")

## 2.2 Gridded population data
pop1 <- raster('../data/worldpop/pop_dens_worldpop-0000000000-0000000000 11.18.10.tif')
pop2 <- raster('../data/worldpop/pop2_3.tif')

## 2.3 OECD Functional Urban Areas (FUAs) polygons
fua <- st_read("../data/fua_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"

  ### Ensure consistency in CRS: WGS84
fua <- st_transform(fua, WGS84)

## 2.4 Selected cities for analysis
cities_data <- read.csv("../data/un_cities_names.csv", encoding = "iso-8859-1")

# 3. Data wrangling
## 3.1 define a vector with city names
cities <- as.vector(cities_data[,5])

## 3.2 define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

## 3.3. rename cities to be consistent with the cities df
fua$eFUA_name[fua$eFUA_name=="Quezon City [Manila]"] <- "Manila"
fua$eFUA_name[fua$eFUA_name=="Osaka [Kyoto]"] <- "Osaka"
fua$eFUA_name[fua$eFUA_name=="Delhi [New Delhi]"] <- "Delhi"
fua$eFUA_name[fua$eFUA_name=="S\x8bo Paulo"] <- "São Paulo"
cities[cities=29] <- "São Paulo"

data_total <- data.frame() # new empty data frame

# 4. Building indicators

for(i in 1:length(cities)){
  city_subset <- subset(fua, eFUA_name==cities[i] & Cntry_ISO ==country[i])
  
  Month1_cropped <- crop(dec_data, extent(city_subset))
  Month1_cropped <- mask(Month1_cropped, city_subset)
  
  Month2_cropped <- crop(jan_data, extent(city_subset))
  Month2_cropped <- mask(Month2_cropped, city_subset)
  
  Month3_cropped <- crop(feb_data, extent(city_subset))
  Month3_cropped <- mask(Month3_cropped, city_subset)
  
  Month4_cropped <- crop(mar_data, extent(city_subset))
  Month4_cropped <- mask(Month4_cropped, city_subset)
  
  Month5_cropped <- crop(apr_data, extent(city_subset))
  Month5_cropped <- mask(Month5_cropped, city_subset)
  
  Month6_cropped <- crop(may_data, extent(city_subset))
  Month6_cropped <- mask(Month6_cropped, city_subset)

  Month7_cropped <- crop(jun_data, extent(city_subset))
  Month7_cropped <- mask(Month7_cropped, city_subset)
  
  if (!is.null(raster::intersect(Month1_cropped@extent, pop1@extent))) {
    pop_cropped <- crop(pop1, extent(Month1_cropped))
  } else if (!is.null(raster::intersect(Month1_cropped@extent, pop2@extent))) {
    pop_cropped <- crop(pop2, extent(Month1_cropped))
  }
  
  
  pop_df <- as.data.frame(pop_cropped, xy = TRUE)
  colnames(pop_df)[3] <- "pop"
  
  # calculate median population density
  median_pop <- median(pop_df$pop, na.rm = TRUE)
  
  #create stack for all months
  multiple_months <- stack(Month1_cropped, 
                           Month2_cropped, 
                           Month3_cropped, 
                           Month4_cropped, 
                           Month5_cropped, 
                           Month6_cropped, 
                           Month7_cropped)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  colnames(r_df)[6] <- "March"
  colnames(r_df)[7] <- "April"
  colnames(r_df)[8] <- "May"
  colnames(r_df)[9] <- "June"
  
  # r_df$JanDec <- r_df$January / r_df$December
  # r_df$FebDec <- r_df$February / r_df$December
  # 
  # city_subset$med_JanDec <- median(r_df$JanDec, na.rm = TRUE)
  # city_subset$med_FebDec <- median(r_df$FebDec, na.rm = TRUE)
  
  
  r_df$JanDec <- r_df$January - r_df$December
  r_df$FebDec <- r_df$February - r_df$December
  r_df$MarDec <- r_df$March - r_df$December
  r_df$AprDec <- r_df$April - r_df$December
  r_df$MayDec <- r_df$May - r_df$December
  r_df$JunDec <- r_df$June - r_df$December

  r_df$class_JanDec <- ifelse(sign(r_df$JanDec) == -1, "negative",
                              ifelse(sign(r_df$JanDec) == 0, "neutral", "positive"))


  r_df$class_FebDec <- ifelse(sign(r_df$FebDec) == -1, "negative",
                              ifelse(sign(r_df$FebDec) == 0, "neutral", "positive"))
  
  r_df$class_MarDec <- ifelse(sign(r_df$MarDec) == -1, "negative",
                              ifelse(sign(r_df$MarDec) == 0, "neutral", "positive"))
  
  r_df$class_AprDec <- ifelse(sign(r_df$AprDec) == -1, "negative",
                              ifelse(sign(r_df$AprDec) == 0, "neutral", "positive"))
  
  r_df$class_MayDec <- ifelse(sign(r_df$MayDec) == -1, "negative",
                              ifelse(sign(r_df$MayDec) == 0, "neutral", "positive"))
  
  r_df$class_JunDec <- ifelse(sign(r_df$JunDec) == -1, "negative",
                              ifelse(sign(r_df$JunDec) == 0, "neutral", "positive"))



  stats_JanDec <- r_df %>% group_by(class_JanDec) %>% 
    summarise(count=n(),
              medianJanDec = median(JanDec, na.rm=TRUE))
  
  
  stats_FebDec <- r_df %>% group_by(class_FebDec) %>% 
    summarise(count=n(),
              medianFebDec = median(FebDec, na.rm=TRUE))
  
  stats_MarDec <- r_df %>% group_by(class_MarDec) %>% 
    summarise(count=n(),
              medianMarDec = median(MarDec, na.rm=TRUE))
  
  stats_AprDec <- r_df %>% group_by(class_AprDec) %>% 
    summarise(count=n(),
              medianAprDec = median(AprDec, na.rm=TRUE))
 
  stats_MayDec <- r_df %>% group_by(class_MayDec) %>% 
    summarise(count=n(),
              medianMayDec = median(MayDec, na.rm=TRUE))
  
  stats_JunDec <- r_df %>% group_by(class_JunDec) %>% 
    summarise(count=n(),
              medianJunDec = median(JunDec, na.rm=TRUE))

  # attach the numbers in new columns
  city_subset$med_pop <- median_pop
  
  city_subset$JanDec_negative <- as.numeric(stats_JanDec[1,2])
  city_subset$JanDec_neutral <- as.numeric(stats_JanDec[2,2])
  city_subset$JanDec_positive <- as.numeric(stats_JanDec[3,2])
  city_subset$JanDec_negative_per <- city_subset$JanDec_negative / (city_subset$JanDec_negative + city_subset$JanDec_neutral + city_subset$JanDec_positive)
  city_subset$JanDec_neutral_per <- city_subset$JanDec_neutral / (city_subset$JanDec_negative + city_subset$JanDec_neutral + city_subset$JanDec_positive)
  city_subset$JanDec_positive_per <- city_subset$JanDec_positive / (city_subset$JanDec_negative + city_subset$JanDec_neutral + city_subset$JanDec_positive)
  city_subset$JanDec_negative_median <- as.numeric(stats_JanDec[1,3])
  city_subset$JanDec_neutral_median <- as.numeric(stats_JanDec[2,3])
  city_subset$JanDec_positive_median <- as.numeric(stats_JanDec[3,3])
  
  city_subset$FebDec_negative <- as.numeric(stats_FebDec[1,2])
  city_subset$FebDec_neutral <- as.numeric(stats_FebDec[2,2])
  city_subset$FebDec_positive <- as.numeric(stats_FebDec[3,2])
  city_subset$FebDec_negative_per <- city_subset$FebDec_negative / (city_subset$FebDec_negative + city_subset$FebDec_neutral + city_subset$FebDec_positive)
  city_subset$FebDec_neutral_per <- city_subset$FebDec_neutral / (city_subset$FebDec_negative + city_subset$FebDec_neutral + city_subset$FebDec_positive)
  city_subset$FebDec_positive_per <- city_subset$FebDec_positive / (city_subset$FebDec_negative + city_subset$FebDec_neutral + city_subset$FebDec_positive)
  city_subset$FebDec_negative_median <- as.numeric(stats_FebDec[1,3])
  city_subset$FebDec_neutral_median <- as.numeric(stats_FebDec[2,3])
  city_subset$FebDec_positive_median <- as.numeric(stats_FebDec[3,3])
  
  city_subset$MarDec_negative <- as.numeric(stats_MarDec[1,2])
  city_subset$MarDec_neutral <- as.numeric(stats_MarDec[2,2])
  city_subset$MarDec_positive <- as.numeric(stats_MarDec[3,2])
  city_subset$MarDec_negative_per <- city_subset$MarDec_negative / (city_subset$MarDec_negative + city_subset$MarDec_neutral + city_subset$MarDec_positive)
  city_subset$MarDec_neutral_per <- city_subset$MarDec_neutral / (city_subset$MarDec_negative + city_subset$MarDec_neutral + city_subset$MarDec_positive)
  city_subset$MarDec_positive_per <- city_subset$MarDec_positive / (city_subset$MarDec_negative + city_subset$MarDec_neutral + city_subset$MarDec_positive)
  city_subset$MarDec_negative_median <- as.numeric(stats_MarDec[1,3])
  city_subset$MarDec_neutral_median <- as.numeric(stats_MarDec[2,3])
  city_subset$MarDec_positive_median <- as.numeric(stats_MarDec[3,3])
  
  city_subset$AprDec_negative <- as.numeric(stats_AprDec[1,2])
  city_subset$AprDec_neutral <- as.numeric(stats_AprDec[2,2])
  city_subset$AprDec_positive <- as.numeric(stats_AprDec[3,2])
  city_subset$AprDec_negative_per <- city_subset$AprDec_negative / (city_subset$AprDec_negative + city_subset$AprDec_neutral + city_subset$AprDec_positive)
  city_subset$AprDec_neutral_per <- city_subset$AprDec_neutral / (city_subset$AprDec_negative + city_subset$AprDec_neutral + city_subset$AprDec_positive)
  city_subset$AprDec_positive_per <- city_subset$AprDec_positive / (city_subset$AprDec_negative + city_subset$AprDec_neutral + city_subset$AprDec_positive)
  city_subset$AprDec_negative_median <- as.numeric(stats_AprDec[1,3])
  city_subset$AprDec_neutral_median <- as.numeric(stats_AprDec[2,3])
  city_subset$AprDec_positive_median <- as.numeric(stats_AprDec[3,3])

  city_subset$MayDec_negative <- as.numeric(stats_MayDec[1,2])
  city_subset$MayDec_neutral <- as.numeric(stats_MayDec[2,2])
  city_subset$MayDec_positive <- as.numeric(stats_MayDec[3,2])
  city_subset$MayDec_negative_per <- city_subset$MayDec_negative / (city_subset$MayDec_negative + city_subset$MayDec_neutral + city_subset$MayDec_positive)
  city_subset$MayDec_neutral_per <- city_subset$MayDec_neutral / (city_subset$MayDec_negative + city_subset$MayDec_neutral + city_subset$MayDec_positive)
  city_subset$MayDec_positive_per <- city_subset$MayDec_positive / (city_subset$MayDec_negative + city_subset$MayDec_neutral + city_subset$MayDec_positive)
  city_subset$MayDec_negative_median <- as.numeric(stats_MayDec[1,3])
  city_subset$MayDec_neutral_median <- as.numeric(stats_MayDec[2,3])
  city_subset$MayDec_positive_median <- as.numeric(stats_MayDec[3,3])
  
  city_subset$JunDec_negative <- as.numeric(stats_JunDec[1,2])
  city_subset$JunDec_neutral <- as.numeric(stats_JunDec[2,2])
  city_subset$JunDec_positive <- as.numeric(stats_JunDec[3,2])
  city_subset$JunDec_negative_per <- city_subset$JunDec_negative / (city_subset$JunDec_negative + city_subset$JunDec_neutral + city_subset$JunDec_positive)
  city_subset$JunDec_neutral_per <- city_subset$JunDec_neutral / (city_subset$JunDec_negative + city_subset$JunDec_neutral + city_subset$JunDec_positive)
  city_subset$JunDec_positive_per <- city_subset$JunDec_positive / (city_subset$JunDec_negative + city_subset$JunDec_neutral + city_subset$JunDec_positive)
  city_subset$JunDec_negative_median <- as.numeric(stats_JunDec[1,3])
  city_subset$JunDec_neutral_median <- as.numeric(stats_JunDec[2,3])
  city_subset$JunDec_positive_median <- as.numeric(stats_JunDec[3,3])
  
  # add vector to a dataframe
  df <- data.frame(city_subset)
  data_total <- rbind(data_total,df)
  
}

data_total <- data_total %>% mutate(
  mean_neg = rowMeans(cbind(JanDec_negative, FebDec_negative, MarDec_negative, AprDec_negative, MayDec_negative, JunDec_negative), na.rm=TRUE),
  mean_neu = rowMeans(cbind(JanDec_neutral, FebDec_neutral, MarDec_neutral, AprDec_neutral, MayDec_neutral, JunDec_neutral), na.rm=TRUE),
  mean_pos = rowMeans(cbind(JanDec_positive, FebDec_positive, MarDec_positive, AprDec_positive, MayDec_positive, JunDec_positive), na.rm=TRUE),
  
  mean_per_neg = rowMeans(cbind(JanDec_negative_per, FebDec_negative_per, MarDec_negative_per, AprDec_negative_per, MayDec_negative_per, JunDec_negative_per), na.rm=TRUE),
  mean_per_neu = rowMeans(cbind(JanDec_neutral_per, FebDec_neutral_per, MarDec_neutral_per, AprDec_neutral_per, MayDec_neutral_per, JunDec_neutral_per), na.rm=TRUE),
  mean_per_pos = rowMeans(cbind(JanDec_positive_per, FebDec_positive_per, MarDec_positive_per, AprDec_positive_per, MayDec_positive_per, JunDec_positive_per), na.rm=TRUE),
  
  mean_median_neg = rowMeans(cbind(JanDec_negative_median, FebDec_negative_median, MarDec_negative_median, AprDec_negative_median, MayDec_negative_median, JunDec_negative_median), na.rm=TRUE),
  mean_median_neu = rowMeans(cbind(JanDec_neutral_median, FebDec_neutral_median, MarDec_neutral_median, AprDec_neutral_median, MayDec_neutral_median, JunDec_neutral_median), na.rm=TRUE),
  mean_median_pos = rowMeans(cbind(JanDec_positive_median, FebDec_positive_median, MarDec_positive_median, AprDec_positive_median, MayDec_positive_median, JunDec_positive_median), na.rm=TRUE)
)


data_total %>% dplyr::select(c(
  "eFUA_name",
  "Cntry_name",
  "FUA_p_2015",
  "mean_neg",
  "mean_neu",
  "mean_pos",
  "mean_per_neg",
  "mean_per_neu",
  "mean_per_pos",
  "mean_median_neg",
  "mean_median_neu",
  "mean_median_pos"
)) %>%   write_csv("../outputs/summary/table1.csv")


# 5. Creating heat maps

## 5.1 Mean change
mean_change_df <- data_total %>% dplyr::select(c("eFUA_name", "mean_neg", "mean_neu", "mean_pos")) %>% 
  pivot_longer(cols = starts_with("mean_"), names_to = "category", values_to ="mean_change")

hm_count <- ggplot(data = mean_change_df, 
       mapping = aes(x= category, y= eFUA_name, fill= mean_change)) +
  geom_tile() +
  scale_fill_viridis(name=" ", option ="viridis", begin = 0, end = 1, direction = 1) +
  theme_tufte() + 
  scale_x_discrete(labels=c("mean_neg" = "Negative", "mean_neu" = "No change",
                            "mean_pos" = "Positive")) +
  scale_y_discrete(limits=rev) +
  labs(title= "Change in Night-time light intensity", subtitle = "a", x="Mean Number of Pixels", y="City") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=15)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size=14)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.title=element_text(size=16, face="plain")) +
  theme(axis.title.y = element_text(size = 22)) +
  theme(legend.key.width = unit(1.5, "cm"), 
        legend.key.height = unit(.8, "cm"),
        legend.text=element_text(size=12)) 


## 5.2 Mean percentage change
mean_perchange_df <- data_total %>% dplyr::select(c("eFUA_name", "mean_per_neg", "mean_per_neu", "mean_per_pos")) %>% 
  pivot_longer(cols = starts_with("mean_per"), names_to = "category", values_to ="mean_per_change")

hm_per <- ggplot(data = mean_perchange_df, 
                 mapping = aes(x= category, y= eFUA_name, fill= mean_per_change)) +
  geom_tile() +
  scale_fill_viridis(name=" ", option ="viridis", begin = 0, end = 1, direction = 1) +
  theme_tufte() + 
  scale_x_discrete(labels=c("mean_per_neg" = "Negative", "mean_per_neu" = "No change",
                            "mean_per_pos" = "Positive")) +
  scale_y_discrete(limits=rev) +
  labs(title= paste(" "), subtitle = "b", x="Mean Percentage of Pixels", y="City") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=15)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size=13)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.title=element_text(size=16, face="plain")) +
  theme(legend.key.width = unit(1.5, "cm"), 
        legend.key.height = unit(.8, "cm"),
        legend.text=element_text(size=12)) 

## 5.2 Median change
median_df <- data_total %>% dplyr::select(c("eFUA_name", "mean_median_neg", "mean_median_pos")) %>% 
  pivot_longer(cols = starts_with("mean_median"), names_to = "category", values_to ="median_change")

hm_median <- ggplot(data = median_df, 
                 mapping = aes(x= category, y= eFUA_name, fill= median_change)) +
  geom_tile() +
  scale_fill_viridis(name=" ", option ="viridis", begin = 0, end = 1, direction = 1) +
  theme_tufte() + 
  scale_x_discrete(labels=c("mean_median_neg" = "Negative", 
                            "mean_median_pos" = "Positive")) +
  scale_y_discrete(limits=rev) +
  labs(title= paste(" "), subtitle = "c", x="Median Percentage Change", y="City") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=15)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size=13)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.title=element_text(size=16, face="plain")) +
  theme(legend.key.width = unit(1.5, "cm"), 
        legend.key.height = unit(.8, "cm"),
        legend.text=element_text(size=12)) 


png("../outputs/summary/heatmaps_ntlchange.png",units="in", width=12, height=15, res=300)
  hm_count + hm_per + hm_median
dev.off()

# read in mobility data ---------------------------------------------------

# read mobility data
# mob_data <- read.csv("../data/for_code/mobility_and_stringency_uncities.csv")
# 
# #attach the country code
# mob_data$iso_code <- countrycode(mob_data[,2], "country.name", "iso3c")
# 
# # modify the date column
# mob_data$Date <- as.Date(mob_data$Date, "%d/%m/%Y")
# 
# # set the maximum date to end of March
# mob_data <- subset(mob_data, Date <= as.Date("2020-04-30") )
# 
# # extract the month
# mob_data$month <- strftime(mob_data$Date, "%m")
# 
# 
# # # rename months of we want
# # mob_data$month[mob_data$month == "01"] <- "January"
# # mob_data$month[mob_data$month == "02"] <- "Febrary"
# # mob_data$month[mob_data$month == "03"] <- "March"
# 
# 
# summary_mean <- mob_data %>% 
#   group_by(City, month) %>%
#   summarise_at(c("Stringency", "Workplaces", "Residential"), mean, na.rm = TRUE)
# 
# # in this part of code we convert the summary table from long to wide format 
# # and then combine it with all other data to export it
# library(reshape2)
# Stringency_wide_mean <- dcast(summary_mean, City ~ month, value.var=c("Stringency"))
# colnames(Stringency_wide_mean) <- c("City", "Stringency_Jan", "Stringency_Feb", "Stringency_Mar", "Stringency_Apr")
# 
# Workplaces_wide_mean <- dcast(summary_mean, City ~ month, value.var=c("Workplaces"))
# colnames(Workplaces_wide_mean) <- c("City", "Workplaces_Jan", "Workplaces_Feb", "Workplaces_Mar", "Workplaces_Apr")
# 
# Residential_wide_mean <- dcast(summary_mean, City ~ month, value.var=c("Residential"))
# colnames(Residential_wide_mean) <- c("City", "Residential_Jan", "Residential_Feb", "Residential_Mar", "Residential_Apr")
# 
# join1_mean <- left_join(Stringency_wide_mean, Workplaces_wide_mean, by = "City")
# join2_mean <- left_join(join1_mean, Residential_wide_mean, by = "City")
# 
# # this is the dataset with the mean value for Stringency and mobility data 
# mean_data <- left_join(join2_mean, data_total, by = c("City" = "eFUA_name"))
# st_write(mean_data, "mean_data.csv")
# 
# 
# summary_median <- mob_data %>% 
#   group_by(City, month) %>%
#   summarise_at(c("Stringency", "Workplaces", "Residential"), median, na.rm = TRUE)
# 
# Stringency_wide_median <- dcast(summary_median, City ~ month, value.var=c("Stringency"))
# colnames(Stringency_wide_median) <- c("City", "Stringency_Jan", "Stringency_Feb", "Stringency_Mar", "Stringency_Apr")
# 
# Workplaces_wide_median <- dcast(summary_median, City ~ month, value.var=c("Workplaces"))
# colnames(Workplaces_wide_median) <- c("City", "Workplaces_Jan", "Workplaces_Feb", "Workplaces_Mar", "Workplaces_Apr")
# 
# Residential_wide_median <- dcast(summary_median, City ~ month, value.var=c("Residential"))
# colnames(Residential_wide_median) <- c("City", "Residential_Jan", "Residential_Feb", "Residential_Mar", "Residential_Apr")
# 
# join1_median <- left_join(Stringency_wide_median, Workplaces_wide_median, by = "City")
# join2_median <- left_join(join1_median, Residential_wide_median, by = "City")
# 
# # this is the dataset with the median value for Stringency and mobility data 
# median_data <- left_join(join2_median, data_total, by = c("City" = "eFUA_name"))
# st_write(median_data, "median_data.csv")
# 
# 
# 
# summary_IQR <- mob_data %>% 
#   group_by(City, month) %>%
#   summarize(Stringency = IQR(Stringency, na.rm = TRUE),
#             Workplaces = IQR(Workplaces, na.rm = TRUE),
#             Residential = IQR(Residential, na.rm = TRUE))
# 
# 
# Stringency_wide_IQR <- dcast(summary_IQR, City ~ month, value.var=c("Stringency"))
# colnames(Stringency_wide_IQR) <- c("City", "Stringency_Jan", "Stringency_Feb", "Stringency_Mar", "Stringency_Apr")
# 
# Workplaces_wide_IQR <- dcast(summary_IQR, City ~ month, value.var=c("Workplaces"))
# colnames(Workplaces_wide_IQR) <- c("City", "Workplaces_Jan", "Workplaces_Feb", "Workplaces_Mar")
# 
# Residential_wide_IQR <- dcast(summary_IQR, City ~ month, value.var=c("Residential"))
# colnames(Residential_wide_IQR) <- c("City", "Residential_Jan", "Residential_Feb", "Residential_Mar", "Residential_Apr")
# 
# join1_IQR <- left_join(Stringency_wide_IQR, Workplaces_wide_IQR, by = "City")
# join2_IQR <- left_join(join1_IQR, Residential_wide_IQR, by = "City")
# 
# 
# # this is the dataset with the IQR value for Stringency and mobility data 
# IQR_data <- left_join(join2_IQR, data_total, by = c("City" = "eFUA_name"))
# st_write(IQR_data, "IQR_data.csv")
# 
