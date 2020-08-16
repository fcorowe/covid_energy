library(raster)
library(sf)
library(countrycode)
library(gridExtra)
library(grid)

# read the tif images
dec_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/dec_data.tif")
jan_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/jan_data.tif")
feb_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/feb_data.tif")


pop1 <- raster('/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/worldpop/pop_dens_worldpop_1.tif')
pop2 <- raster('/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/worldpop/pop_dens_worldpop_2.tif')

# read the FUAs polygons
fua <- st_read("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/fua/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)

# read the files with the cities included in the analysis
cities_data <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted_nm.csv")

cities <- as.vector(cities_data[,1])

country <- countrycode(cities_data[,2], "country.name", "iso3c")

myplots <- list()  # new empty list

for(i in 1:length(cities)){
  #city_subset <- subset(fua, eFUA_name==cities[i])
  city_subset <- subset(fua, eFUA_name==cities[i] & Cntry_ISO ==country[i])
  
  Month1_cropped <- crop(dec_data, extent(city_subset))
  Month2_cropped <- crop(jan_data, extent(city_subset))
  Month3_cropped <- crop(feb_data, extent(city_subset))
  
  
  month_diff <- Month2_cropped - Month1_cropped
  #month_diff <- Month3_cropped - Month1_cropped
  
  if (!is.null( intersect( Month1_cropped@extent, pop1@extent))) {
    pop_cropped <- crop( pop1, extent( month_diff))
  } else if (!is.null( intersect( Month1_cropped@extent, pop2@extent))) {
    pop_cropped <- crop( pop2, extent( month_diff))
  }
  
  # filter grids with more than 10 people
  pop_cropped[pop_cropped<=10]<-NA
  
  pop_df <- as.data.frame( pop_cropped, xy = TRUE)
  colnames(pop_df)[3] <- "pop"
  
  month_df <- as.data.frame( month_diff, xy = TRUE)
  colnames(month_df)[3] <- "month_diff"
  
  combined <- cbind(pop_df, month_df)
  
  # select the columns needed
  combined <- combined[,c(1:3, 6)]
  
  p <- ggplot(combined, aes(x= log(pop), y= month_diff)) +
    geom_point(colour = "grey90", size = .25) +
    geom_smooth(se = TRUE, level = .95, size = 2) +
    xlab("") +
    ylab("") +
    theme_classic() +
    labs(title = cities[i]) +
    scale_x_continuous(limits = c(1,12)) +
    scale_y_continuous(limits = c(-150, 150)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))

  myplots[[i]] <- p  # add each plot into plot list
  
}


png("population_combined_jandec.png", units = "in", width=10, height=15, res=300)
grid.arrange(arrangeGrob(grobs = myplots, ncol = 5), 
             bottom = textGrob("Population (log)"), 
             left= textGrob("Change in nigthtime light intensity \n (January-December)", rot = 90, vjust = 1))
dev.off()

#png("population_combined_febdec.png", units = "in", width=10, height=15, res=300)
#grid.arrange(arrangeGrob(grobs = myplots, ncol = 5), 
#             bottom = textGrob("Population (log)"), 
#             left= textGrob("Change in nigthtime light intensity \n (February-December)", rot = 90, vjust = 1))
#dev.off()
