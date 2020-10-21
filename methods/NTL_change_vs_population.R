library(raster)
library(sf)
library(countrycode)
library(ggplot2)

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




myplots <- list()  # new empty list

for(i in 1:length(cities)){
  #city_subset <- subset(fua, eFUA_name==cities[i])
  city_subset <- subset(fua, eFUA_name==cities[i] & Cntry_ISO ==country[i])
  
  Month1_cropped <- crop(dec_data, extent(city_subset))
  Month2_cropped <- crop(jan_data, extent(city_subset))
  Month3_cropped <- crop(feb_data, extent(city_subset))
  Month4_cropped <- crop(mar_data, extent(city_subset))
  
  
  
  month_diff <- Month4_cropped - Month1_cropped 
  
  if (!is.null(raster::intersect(Month1_cropped@extent, pop1@extent))) {
    pop_cropped <- crop(pop1, extent(Month1_cropped))
  } else if (!is.null(raster::intersect(Month1_cropped@extent, pop2@extent))) {
    pop_cropped <- crop(pop2, extent(Month1_cropped))
  }
  
  # filter grids with more than 10 people
  pop_cropped[pop_cropped<=10]<-NA
  
  
  
  pop_df <- as.data.frame(pop_cropped, xy = TRUE)
  colnames(pop_df)[3] <- "pop"
  
  
  month_df <- as.data.frame(month_diff, xy = TRUE)
  colnames(month_df)[3] <- "month_diff"
  
  combined <- cbind(pop_df, month_df)
  
  # select the columns needed
  combined <- combined[,c(1:3,6)]
  
  
  p <- ggplot(combined, aes(x= log(pop), y= month_diff)) +
    geom_point(colour = "grey90", size = .25) +
    geom_smooth(se = TRUE, level = .95, size = 2) +
    xlab("") +
    ylab("") +
    theme_classic() +
    labs(title = cities[i]) +
    scale_x_continuous(limits = c(1,12)) +
    scale_y_continuous(limits = c(-120,120)) +
    theme(axis.text.x=element_text(angle=45, hjust=1))

  myplots[[i]] <- p  # add each plot into plot list
  
}



library(gridExtra)
library(grid)
png("NTL_change_vs_population.png",units="in", width=10, height=15, res=300)
grid.arrange(arrangeGrob(grobs = myplots, ncol = 5), 
             bottom = textGrob("Population (log)"), 
             left= textGrob("Change in nigthtime light intensity \n (March-December)", rot = 90, vjust = 1))
dev.off()

