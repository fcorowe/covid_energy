# 1. Dependencies
library(raster)
library(countrycode)
library(osmdata)
library(tmaptools)
library(sf)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggthemes)

# 2. Read data
## Clean workspace
rm(list=ls())

## Night-time satellite data
dec_data <- raster("../data/ntl/dec_data.tif")
jan_data <- raster("../data/ntl/jan_data.tif")
feb_data <- raster("../data/ntl/feb_data.tif")
mar_data <- raster("../data/ntl/mar_data.tif")
apr_data <- raster("../data/ntl/apr_data.tif")
may_data <- raster("../data/ntl/may_data.tif")
jun_data <- raster("../data/ntl/june_data.tif")


## OECD Functional Urban Areas (FUAs) polygons
fua <- st_read("../data/fua_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"

## Ensure consistency in CRS: WGS84
fua <- st_transform(fua, WGS84)

## Selected cities for analysis
cities_data <- read.csv("../data/un_cities_names.csv", encoding = "iso-8859-1")

## Define a vector with city names
cities <- as.vector(cities_data[,5])

## Rename cities to be consistent with the cities df
fua$eFUA_name[fua$eFUA_name=="Quezon City [Manila]"] <- "Manila"
fua$eFUA_name[fua$eFUA_name=="Osaka [Kyoto]"] <- "Osaka"
fua$eFUA_name[fua$eFUA_name=="Delhi [New Delhi]"] <- "Delhi"
fua$eFUA_name[fua$eFUA_name=="S\x8bo Paulo"] <- "São Paulo"
cities[cities=29] <- "São Paulo"

## Define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

# create an empty list to store the plots
plotList <- list()

# function that creates the density plots with the NTL change by month
create_density_plot <- function(month1raster, month2raster, month3raster, month4raster, month5raster, month6raster, month7raster, city_name, country_code) {
  
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
  
  month6_crop <- crop(month6raster, extent(city_subset))
  month6_crop <- mask(month6_crop, city_subset)
  
  month7_crop <- crop(month7raster, extent(city_subset))
  month7_crop <- mask(month7_crop, city_subset)
  
  #create stack for all months
  multiple_months <- stack(month1_crop, month2_crop, month3_crop, month4_crop, month5_crop, month6_crop, month7_crop)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  colnames(r_df)[6] <- "March"
  colnames(r_df)[7] <- "April"
  colnames(r_df)[8] <- "May"
  colnames(r_df)[9] <- "June"

  # 3.3 Exclude noises caused by aurora and dim light noises caused by temporal lights from fires and boats
  r_df$December[r_df$December < 1.5] <- 0
  r_df$January[r_df$January < 1.5] <- 0
  r_df$February[r_df$February < 1.5] <- 0
  r_df$March[r_df$March < 1.5] <- 0
  r_df$April[r_df$April < 1.5] <- 0
  r_df$May[r_df$May < 1.5] <- 0
  r_df$June[r_df$June < 1.5] <- 0
  
  # 3.4 Difference in light intensity - base month: Dec 2019
  r_df$JanDec_dif <- r_df$January - r_df$December
  r_df$FebDec_dif <- r_df$February - r_df$December
  r_df$MarDec_dif <- r_df$March - r_df$December
  r_df$AprDec_dif <- r_df$April - r_df$December
  r_df$MayDec_dif <- r_df$May - r_df$December
  r_df$JunDec_dif <- r_df$June - r_df$December
  
  # 3.5 nas to 0
  r_df[is.na(r_df)] <- 0
  
  # 3.6 convert from wide to long format
  r_df_long <- gather(r_df, month, measurement, 10:15, factor_key=TRUE)
  
  # 3.7 capping min / max at 30
  r_df_long$measurement[r_df_long$measurement > 30] <- 30
  r_df_long$measurement[r_df_long$measurement < -30] <- -30
  

  p1 <- ggplot(data = r_df_long, mapping = aes(x = measurement, color = month)) +
    stat_ecdf(geom = "step",
              size = 1) +
    theme_tufte() + 
    xlab("") +
    ylab("") +
    scale_color_viridis( discrete = TRUE, option = "viridis",
                         name = "Difference",
                         breaks = c("JanDec_dif", "FebDec_dif", "MarDec_dif", "AprDec_dif", "MayDec_dif", "JunDec_dif"), 
                         labels = c("Jan20 - Dec19", "Feb20 - Dec19", "Mar20 - Dec19", "Apr20 - Dec19", "May20 - Dec19", "Jun20 - Dec19"),
                         ) +
    theme(text = element_text(size = 13),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14)
          ) +
    #scale_y_continuous(limits = c(0,0.6)) +
    #scale_x_continuous(limits = c(-100,100)) +
    ggtitle(city_name)
    
  return(p1)
}


for(i in 1:length(cities)){
  plotList[[i]] <- create_density_plot(month1raster = dec_data, 
                                       month2raster = jan_data,
                                       month3raster = feb_data, 
                                       month4raster = mar_data, 
                                       month5raster = apr_data,
                                       month6raster = may_data,
                                       month7raster = jun_data,
                                       city_name = cities[i], country_code = country[i])
}
  


library(ggpubr)
combined_plots <- ggarrange(plotlist = plotList, ncol=5, nrow=10, common.legend = TRUE, legend="right")


png("../outputs/dist_analysis/density_plots_ntlchange.png", units="in", width=15, height=15, res=300)
annotate_figure(combined_plots,
                bottom = text_grob("Night-time light intensity (nanoWatts/cm2/sr)", size = 16),
                left = text_grob("Density", size = 16, rot = 90)
                )
dev.off() # Close the file

