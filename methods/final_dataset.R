library(raster)
library(sf)
library(countrycode)
library(dplyr)

# read the raster data
dec_data <- raster("../data/for_code/NTL/dec_data.tif")
jan_data <- raster("../data/for_code/NTL/jan_data.tif")
feb_data <- raster("../data/for_code/NTL/feb_data.tif")
mar_data <- raster("../data/for_code/NTL/mar_data.tif")

# read in the population grids
pop1 <- raster('../data/for_code/worldpop/pop_dens_worldpop-0000000000-0000000000 11.18.10.tif')
pop2 <- raster('../data/for_code/worldpop/pop2_3.tif')

# read the FUAs polygons
fua <- st_read("../data/for_code/FUA_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)







# read the file with the cities included in the analysis
cities_data <- read.csv("../data/for_code/UN_Cities_names.csv")


# define a vector with city names
cities <- as.vector(cities_data[,1])

# define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")




data_total <- data.frame() # new empty data frame

for(i in 1:length(cities)){
  city_subset <- subset(fua, eFUA_name==cities[i] & Cntry_ISO ==country[i])
  
  Month1_cropped <- crop(dec_data, extent(city_subset))
  Month2_cropped <- crop(jan_data, extent(city_subset))
  Month3_cropped <- crop(feb_data, extent(city_subset))
  Month4_cropped <- crop(mar_data, extent(city_subset))
  
  
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
  multiple_months <- stack(Month1_cropped, Month2_cropped, Month3_cropped, Month4_cropped)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  colnames(r_df)[6] <- "March"
  
  # r_df$JanDec <- r_df$January / r_df$December
  # r_df$FebDec <- r_df$February / r_df$December
  # 
  # city_subset$med_JanDec <- median(r_df$JanDec, na.rm = TRUE)
  # city_subset$med_FebDec <- median(r_df$FebDec, na.rm = TRUE)
  
  
  r_df$JanDec <- r_df$January - r_df$December
  r_df$FebDec <- r_df$February - r_df$December
  r_df$MarDec <- r_df$March - r_df$December

  r_df$class_JanDec <- ifelse(sign(r_df$JanDec) == -1, "negative",
                              ifelse(sign(r_df$JanDec) == 0, "neutral", "positive"))


  r_df$class_FebDec <- ifelse(sign(r_df$FebDec) == -1, "negative",
                              ifelse(sign(r_df$FebDec) == 0, "neutral", "positive"))
  
  r_df$class_MarDec <- ifelse(sign(r_df$MarDec) == -1, "negative",
                              ifelse(sign(r_df$MarDec) == 0, "neutral", "positive"))





  stats_JanDec <- r_df %>% group_by(class_JanDec) %>% 
    summarise(count=n(),
              medianJanDec = median(JanDec, na.rm=TRUE))
  
  
  stats_FebDec <- r_df %>% group_by(class_FebDec) %>% 
    summarise(count=n(),
              medianFebJan = median(FebDec, na.rm=TRUE))
  
  stats_MarDec <- r_df %>% group_by(class_MarDec) %>% 
    summarise(count=n(),
              medianMarJan = median(MarDec, na.rm=TRUE))

  

  # attach the numbers in new columns
  city_subset$med_pop <- median_pop
  city_subset$JanDec_negative <- as.numeric(stats_JanDec[1,2])
  city_subset$JanDec_neutral <- as.numeric(stats_JanDec[2,2])
  city_subset$JanDec_postitive <- as.numeric(stats_JanDec[3,2])
  city_subset$JanDec_negative_median <- as.numeric(stats_JanDec[1,3])
  city_subset$JanDec_neutral_median <- as.numeric(stats_JanDec[2,3])
  city_subset$JanDec_postitive_median <- as.numeric(stats_JanDec[3,3])
  city_subset$FebDec_negative <- as.numeric(stats_FebDec[1,2])
  city_subset$FebDec_neutral <- as.numeric(stats_FebDec[2,2])
  city_subset$FebDec_postitive <- as.numeric(stats_FebDec[3,2])
  city_subset$FebDec_negative_median <- as.numeric(stats_FebDec[1,3])
  city_subset$FebDec_neutral_median <- as.numeric(stats_FebDec[2,3])
  city_subset$FebDec_postitive_median <- as.numeric(stats_FebDec[3,3])
  city_subset$MarDec_negative <- as.numeric(stats_MarDec[1,2])
  city_subset$MarDec_neutral <- as.numeric(stats_MarDec[2,2])
  city_subset$MarDec_postitive <- as.numeric(stats_MarDec[3,2])
  city_subset$MarDec_negative_median <- as.numeric(stats_MarDec[1,3])
  city_subset$MarDec_neutral_median <- as.numeric(stats_MarDec[2,3])
  city_subset$MarDec_postitive_median <- as.numeric(stats_MarDec[3,3])
  
  # add vector to a dataframe
  df <- data.frame(city_subset)
  data_total <- rbind(data_total,df)
  
}



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


# # rename months of we want
# mob_data$month[mob_data$month == "01"] <- "January"
# mob_data$month[mob_data$month == "02"] <- "Febrary"
# mob_data$month[mob_data$month == "03"] <- "March"


summary_mean <- mob_data %>% 
  group_by(City, month) %>%
  summarise_at(c("Stringency", "Workplaces", "Residential"), mean, na.rm = TRUE)


summary_median <- mob_data %>% 
  group_by(City, month) %>%
  summarise_at(c("Stringency", "Workplaces", "Residential"), median, na.rm = TRUE)


summary_IQR <- mob_data %>% 
  group_by(City, month) %>%
  summarize(Stringency = IQR(Stringency, na.rm = TRUE),
            Workplaces = IQR(Workplaces, na.rm = TRUE),
            Residential = IQR(Residential, na.rm = TRUE))



# this is the dataset with the mean value for Stringency and mobility data 
mean_data <- left_join(summary_mean, data_total, by = c("City" = "eFUA_name"))
st_write(mean_data, "mean_data.csv")

# this is the dataset with the median value for Stringency and mobility data 
median_data <- left_join(summary_median, data_total, by = c("City" = "eFUA_name"))
st_write(median_data, "median_data.csv")

# this is the dataset with the IQR value for Stringency and mobility data 
IQR_data <- left_join(summary_IQR, data_total, by = c("City" = "eFUA_name"))
st_write(IQR_data, "IQR_data.csv")


