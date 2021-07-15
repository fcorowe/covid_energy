library(raster)
library(countrycode)
library(osmdata)
library(tmaptools)
library(sf)
library(tidyr)
library(ggplot2)
library(viridis)

# read the raster data
dec_data <- raster("../data/for_code/NTL/dec_data.tif")
jan_data <- raster("../data/for_code/NTL/jan_data.tif")
feb_data <- raster("../data/for_code/NTL/feb_data.tif")
mar_data <- raster("../data/for_code/NTL/mar_data.tif")
apr_data <- raster("../data/for_code/NTL/apr_data.tif")



# read the FUAs polygons
fua <- st_read("../data/for_code/FUA_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)




# function that creates the density plots with the NTL change by month
create_density_plot <- function(month1raster, month2raster, month3raster, month4raster, month5raster, city_name, country_code) {

  city_subset <- subset(fua, eFUA_name==city_name & Cntry_ISO ==country_code)
  
  month1_crop <- crop(month1raster, extent(city_subset))
  month1_crop <- mask(month1_crop, city_subset)
  
  month2_crop <- crop(month2raster, extent(city_subset))
  month2_crop <- mask(month2_crop, city_subset)
  
  month3_crop <- crop(month3raster, extent(city_subset))
  month3_crop <- mask(month3_crop, city_subset)
  
  month4_crop <- crop(month4raster, extent(city_subset))
  month4_crop <- mask(month4_crop, city_subset)
  
  month5_crop <- crop(month5raster, extent(city_subset))
  month5_crop <- mask(month5_crop, city_subset)
  
  #create stack for all months
  multiple_months <- stack(month1_crop, month2_crop, month3_crop, month4_crop, month5_crop)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  colnames(r_df)[6] <- "March"
  colnames(r_df)[7] <- "April"

  
  r_df$JanDec <- r_df$January - r_df$December
  r_df$FebDec <- r_df$February - r_df$December
  r_df$MarDec <- r_df$March - r_df$December
  r_df$AprDec <- r_df$April - r_df$December
  
  # convert from wide to long format
  
  r_df_long <- gather(r_df, month, measurement, 8:11, factor_key=TRUE)
  

  p1 <- ggplot(data = r_df_long, mapping = aes(x = measurement, color = month)) +
    geom_density( size = 2) +
    scale_color_viridis( discrete = TRUE, option = "viridis") +
    theme_classic() +
    xlab("") +
    ylab("") +
    scale_y_continuous(limits = c(0,0.6)) +
    scale_x_continuous(limits = c(-100,100)) +
    ggtitle(city_name)
    
  

  
  return(p1)
  
  
}



# read the file with the cities included in the analysis
cities_data <- read.csv("../data/for_code/UN_Cities_names.csv")


# define a vector with city names
cities <- as.vector(cities_data[,1])

# define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")



# create an empty list to store the plots
plotList <- list()

for(i in 1:length(cities)){
  plotList[[i]] <- create_density_plot(month1raster = dec_data, month2raster = jan_data,
                               month3raster = feb_data, month4raster = mar_data, month5raster = apr_data,
                               city_name = cities[i], country_code = country[i])
}
  


library(ggpubr)
combined_plots <- ggarrange(plotlist = plotList, ncol=5, nrow=10, common.legend = TRUE, legend="right")


png("density_plots_NTL_change.png",units="in", width=15, height=15, res=300)
annotate_figure(combined_plots,
                bottom = text_grob("Night-time light intensity (nanoWatts/cm2/sr)", size = 10),
                left = text_grob("Density", size = 10, rot = 90)
                )
dev.off() # Close the file

