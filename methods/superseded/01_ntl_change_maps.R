# 1. Dependencies
library(raster)
library(sf)
library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggmap)

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

  ## Gridded population data
pop1 <- raster('../data/worldpop/pop_dens_worldpop-0000000000-0000000000 11.18.10.tif')
pop2 <- raster('../data/worldpop/pop2_3.tif')

  ## OECD Functional Urban Areas (FUAs) polygons
fua <- st_read("../data/fua_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"

  ## Ensure consistency in CRS: WGS84
fua <- st_transform(fua, WGS84)

  ## Selected cities for analysis
cities_data <- read.csv("../data/un_cities_names.csv")

  ## Define a vector with city names
cities <- as.vector(cities_data[,1])

  ## Define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

  ## Create an empty list to store plots
plotList <- list()

  #data_total <- data.frame() # new empty data frame

# 3. Measuring difference in monthly night-time light intensity

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
  
  # Exclude noises caused by aurora and dim light noises caused by temporal lights from fires and boats
  r_df[r_df < 1.5] <- 0
  
  r_df$JanDec_dif <- r_df$January - r_df$December
  r_df$FebDec_dif <- r_df$February - r_df$December
  r_df$MarDec_dif <- r_df$March - r_df$December
  r_df$AprDec_dif <- r_df$April - r_df$December
  r_df$MayDec_dif <- r_df$May - r_df$December
  r_df$JunDec_dif <- r_df$June - r_df$December
  
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
                                    TRUE  ~ "neutral"),
           MayDec = case_when(MayDec_dif <= -1 & abs(MayDec_dif) >= sd(MayDec_dif) ~ "strong_negative",
                              MayDec_dif <= -1 & abs(MayDec_dif) < sd(MayDec_dif) ~ "negative",
                              MayDec_dif >= 1 & abs(MayDec_dif) >= sd(MayDec_dif) ~ "strong_positive",
                              MayDec_dif >= 1 & abs(MayDec_dif) < sd(MayDec_dif) ~ "positive",
                              TRUE  ~ "neutral"),
           JunDec = case_when(JunDec_dif <= -1 & abs(JunDec_dif) >= sd(JunDec_dif) ~ "strong_negative",
                              JunDec_dif <= -1 & abs(JunDec_dif) < sd(JunDec_dif) ~ "negative",
                              JunDec_dif >= 1 & abs(JunDec_dif) >= sd(JunDec_dif) ~ "strong_positive",
                              JunDec_dif >= 1 & abs(JunDec_dif) < sd(JunDec_dif) ~ "positive",
                              TRUE  ~ "neutral")    
           
           )
  
  
  write.csv(r_df, "../outputs/ntl_analysis/tab1_change.csv")
  
  r_df <- r_df[,c("x", "y", "JanDec", "FebDec", "MarDec", "AprDec", "MayDec", "JunDec")]
  
  
  data_long <- gather(r_df, time, change, JanDec:AprDec, factor_key=TRUE)
  data_long$change <- factor(data_long$change, levels = c("strong_negative", "negative", "neutral", "positive", "strong_positive"))   
  
  
  colors <- c("#e6550d", "#fdae6b", "grey", "#9ecae1", "#3182bd")
  # data_long$city <- cities[i]
  # 
  # 
  # # add vector to a dataframe
  # df <- data.frame(data_long)
  # data_total <- rbind(data_total,df)
  
  plotList[[i]] <-ggplot(data = data_long) +
    geom_tile( aes(x=x,y=y,fill=change)) +
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




png("../outputs/ntl_analysis/combined_plots10.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

