library(raster)
library(sf)
library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)

# read the raster data
dec_data <- raster("../data/for_code/NTL/dec_data.tif")
jan_data <- raster("../data/for_code/NTL/jan_data.tif")
feb_data <- raster("../data/for_code/NTL/feb_data.tif")
mar_data <- raster("../data/for_code/NTL/mar_data.tif")
apr_data <- raster("../data/for_code/NTL/apr_data.tif")

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


cities <- as.vector(cities_data[,1])


# create an empty list to store the plots
plotList <- list()

#data_total <- data.frame() # new empty data frame

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
  
  
  
  #create stack for all months
  multiple_months <- stack(Month1_cropped, Month2_cropped, Month3_cropped, Month4_cropped, Month5_cropped)
  
  # convert to data frame
  r_df <- as.data.frame(multiple_months, xy = TRUE)
  colnames(r_df)[3] <- "December"
  colnames(r_df)[4] <- "January"
  colnames(r_df)[5] <- "February"
  colnames(r_df)[6] <- "March"
  colnames(r_df)[7] <- "April"
  
  
  
  r_df$JanDec_dif <- r_df$January - r_df$December
  r_df$FebDec_dif <- r_df$February - r_df$December
  r_df$MarDec_dif <- r_df$March - r_df$December
  r_df$AprDec_dif <- r_df$April - r_df$December
  
  r_df[is.na(r_df)] <- 0
  
  
  
  r_df <- r_df %>%
    mutate(JanDec = case_when(JanDec_dif <= -1 & abs(JanDec_dif) >= sd(JanDec_dif) ~ "strong_negative",
                              JanDec_dif <= -1 & abs(JanDec_dif) < sd(JanDec_dif) ~ "negative",
                              JanDec_dif >= 1 & abs(JanDec_dif) >= sd(JanDec_dif) ~ "strong_positive",
                              JanDec_dif >= 1 & abs(JanDec_dif) < sd(JanDec_dif) ~ "positive",
                                    TRUE  ~ "neutral"),
           FebDec = case_when(FebDec_dif <= -1 & abs(FebDec_dif) >= sd(FebDec_dif) ~ "strong_negative",
                              FebDec_dif <= -1 & abs(FebDec_dif) < sd(FebDec_dif) ~ "negative",
                              FebDec_dif >= 1 & abs(FebDec_dif) >= sd(FebDec_dif) ~ "strong_positive",
                              FebDec_dif >= 1 & abs(FebDec_dif) < sd(FebDec_dif) ~ "positive",
                                    TRUE  ~ "neutral"),
           MarDec = case_when(MarDec_dif <= -1 & abs(MarDec_dif) >= sd(MarDec_dif) ~ "strong_negative",
                              MarDec_dif <= -1 & abs(MarDec_dif) < sd(MarDec_dif) ~ "negative",
                              MarDec_dif >= 1 & abs(MarDec_dif) >= sd(MarDec_dif) ~ "strong_positive",
                              MarDec_dif >= 1 & abs(MarDec_dif) < sd(MarDec_dif) ~ "positive",
                                    TRUE  ~ "neutral"),
           AprDec = case_when(AprDec_dif <= -1 & abs(AprDec_dif) >= sd(AprDec_dif) ~ "strong_negative",
                              AprDec_dif <= -1 & abs(AprDec_dif) < sd(AprDec_dif) ~ "negative",
                              AprDec_dif >= 1 & abs(AprDec_dif) >= sd(AprDec_dif) ~ "strong_positive",
                              AprDec_dif >= 1 & abs(AprDec_dif) < sd(AprDec_dif) ~ "positive",
                                    TRUE  ~ "neutral")
           
           
           )
  
  
  write.csv(r_df, "r_df.csv")
  
  r_df <- r_df[,c("x", "y", "JanDec", "FebDec", "MarDec", "AprDec")]
  
  
  data_long <- gather(r_df, time, change, JanDec:AprDec, factor_key=TRUE)
  data_long$change <- factor(data_long$change, levels = c("strong_negative", "negative", "neutral", "positive", "strong_positive"))   
  
  
  colors <- c("#e6550d", "#fdae6b", "grey", "#9ecae1", "#3182bd")
  # data_long$city <- cities[i]
  # 
  # 
  # # add vector to a dataframe
  # df <- data.frame(data_long)
  # data_total <- rbind(data_total,df)
  
  plotList[[i]] <-ggplot(data=data_long) +
    geom_tile(aes(x=x,y=y,fill=change)) +
    scale_fill_manual(values=colors) +
    #geom_sf(data = city_subset, fill = NA, color = 'gray34') +
    facet_grid(~time) +
    #scale_fill_brewer("Legend_title", type = "seq", palette = "Greys") +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          strip.text = element_blank()) +
    xlab("") + ylab(cities[i])
 
}

# library(ggpubr)
# combined_plots <- ggarrange(plotlist = plotList[1:5],  nrow=5, common.legend = TRUE, legend="bottom")

library(cowplot)
# extract a legend that is laid out horizontally
legend <- get_legend(
  plotList[[1]] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)




final_plots <- plot_grid(plotlist=plotList[46:50], ncol=1, align="hv")



# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))




png("combined_plots10.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

