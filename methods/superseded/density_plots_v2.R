library(raster)
library(countrycode)
library(osmdata)
library(tmaptools)
library(sf)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggpubr)

# read the tif images
dec_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/dec_data.tif")
jan_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/jan_data.tif")
feb_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/feb_data.tif")


# read the FUAs polygons
fua <- st_read("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/fua/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg")
WGS84 = "+init=epsg:4326"
# make sure that all layers have consistent CRS- in this case is WGS84
fua <- st_transform(fua, WGS84)


create_density_plot <- function(month1raster, month2raster, month3raster, city_name, country_code) {

  city_subset <- subset(fua, eFUA_name==city_name & Cntry_ISO ==country_code)
  
  month1_crop <- crop(month1raster, extent(city_subset))
  month2_crop <- crop(month2raster, extent(city_subset))
  month3_crop <- crop(month3raster, extent(city_subset))
  
  #create stack for all months
  multiple_months <- stack(month1_crop, month2_crop, month3_crop)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  

  
  r_df$JanDec <- r_df$January - r_df$December
  r_df$FebDec <- r_df$February - r_df$December
  
  # convert from wide to long format
  
  r_df_long <- gather(r_df, month, measurement, 6:7, factor_key=TRUE)
  

  p1 <- ggplot(data = r_df_long, mapping = aes(x = measurement, color = month)) +
    geom_density( size = 2) +
    scale_color_viridis( discrete = TRUE, option = "viridis") +
    theme_classic() +
    labs(color = 'Difference',
         ) +
    #xlab("") +
    #ylab("") +
    scale_y_continuous(limits = c(0,0.6)) +
    scale_x_continuous(limits = c(-50,50)) +
    ggtitle(city_name)
    
  

  
  return(p1)
  
  
}



# read the files with the cities included in the analysis
cities_data <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted_nm.csv")

cities <- as.vector(cities_data[,1])

country <- countrycode(cities_data[,2], "country.name", "iso3c")

plotList <- list()

for(i in 1:length(cities)){
  plotList[[i]] <- create_density_plot(month1raster = dec_data, month2raster = jan_data,
                               month3raster = feb_data, city_name = cities[i], country_code = country[i])
}

combined_plots <- ggarrange(plotlist = plotList, 
                            ncol=5, nrow=6, 
                            common.legend = TRUE, 
                            legend="bottom")


png("density_plots2.png",units="in", width=15, height=10, res=300)
annotate_figure(combined_plots,
                bottom = text_grob("Night-time light intensity (nanoWatts/cm2/sr)", size = 15),
                left = text_grob("Density", size = 15, rot = 90)
                ) 
dev.off() # Close the file