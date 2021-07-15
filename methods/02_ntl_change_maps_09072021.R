# 1. Dependencies
library(raster)
library(sf)
library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggmap)
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

  ## Gridded population data
pop1 <- raster('../data/worldpop/pop_dens_worldpop-0000000000-0000000000 11.18.10.tif')
pop2 <- raster('../data/worldpop/pop2_3.tif')

  ## OECD Functional Urban Areas (FUAs) polygons
fua <- st_read("../data/fua_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"

  ## Ensure consistency in CRS: WGS84
fua <- st_transform(fua, WGS84)

  ## Selected cities for analysis
cities_data <- read.csv("../data/un_cities_names.csv", encoding = "iso-8859-1")

  ## Define a vector with city names
cities <- as.vector(cities_data[,1])
cities[cities=29] <- "São Paulo"

  ## Define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

  ## Create an empty list to store plots
plotList <- list()

  #data_total <- data.frame() # new empty data frame

# 3. Measuring and visualising difference in monthly night-time light intensity

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
  
  # 3.1 create stack for all months
  multiple_months <- stack(Month1_cropped, 
                           Month2_cropped, 
                           Month3_cropped, 
                           Month4_cropped, 
                           Month5_cropped,
                           Month6_cropped,
                           Month7_cropped)
  
  # 3.2 convert to data frame
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
  
  # 3.6 Classifying pixels according to extent of change
  r_df <- r_df %>%
    mutate(JanDec = case_when(abs(JanDec_dif) > 40 & JanDec_dif < 0 ~ "strong_negative",
                              abs(JanDec_dif) <= 40 & abs(JanDec_dif) > 20 & JanDec_dif < 0  ~ "moderate_negative",
                              abs(JanDec_dif) <= 20 & abs(JanDec_dif) > 1.5 & JanDec_dif < 0 ~ "slight_negative",
                              JanDec_dif >= -1.5 & JanDec_dif <= 1.5 ~ "no_change",
                              abs(JanDec_dif) <= 20 & abs(JanDec_dif) > 1.5 & JanDec_dif > 0 ~ "slight_positive",
                              abs(JanDec_dif) <= 40 & abs(JanDec_dif) > 20 & JanDec_dif > 0  ~ "moderate_positive",
                              abs(JanDec_dif) > 40 & JanDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
           FebDec = case_when(abs(FebDec_dif) > 40 & FebDec_dif < 0 ~ "strong_negative",
                              abs(FebDec_dif) <= 40 & abs(FebDec_dif) > 20 & FebDec_dif < 0  ~ "moderate_negative",
                              abs(FebDec_dif) <= 20 & abs(FebDec_dif) > 1.5 & FebDec_dif < 0 ~ "slight_negative",
                              FebDec_dif >= -1.5 & FebDec_dif <= 1.5 ~ "no_change",
                              abs(FebDec_dif) <= 20 & abs(FebDec_dif) > 1.5 & FebDec_dif > 0 ~ "slight_positive",
                              abs(FebDec_dif) <= 40 & abs(FebDec_dif) > 20 & FebDec_dif > 0  ~ "moderate_positive",
                              abs(FebDec_dif) > 40 & FebDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
           MarDec = case_when(abs(MarDec_dif) > 40 & MarDec_dif < 0 ~ "strong_negative",
                              abs(MarDec_dif) <= 40 & abs(MarDec_dif) > 20 & MarDec_dif < 0  ~ "moderate_negative",
                              abs(MarDec_dif) <= 20 & abs(MarDec_dif) > 1.5 & MarDec_dif < 0 ~ "slight_negative",
                              MarDec_dif >= -1.5 & MarDec_dif <= 1.5 ~ "no_change",
                              abs(MarDec_dif) <= 20 & abs(MarDec_dif) > 1.5 & MarDec_dif > 0 ~ "slight_positive",
                              abs(MarDec_dif) <= 40 & abs(MarDec_dif) > 20 & MarDec_dif > 0  ~ "moderate_positive",
                              abs(MarDec_dif) > 40 & MarDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
           AprDec = case_when(abs(AprDec_dif) > 40 & AprDec_dif < 0 ~ "strong_negative",
                              abs(AprDec_dif) <= 40 & abs(AprDec_dif) > 20 & AprDec_dif < 0  ~ "moderate_negative",
                              abs(AprDec_dif) <= 20 & abs(AprDec_dif) > 1.5 & AprDec_dif < 0 ~ "slight_negative",
                              AprDec_dif >= -1.5 & AprDec_dif <= 1.5 ~ "no_change",
                              abs(AprDec_dif) <= 20 & abs(AprDec_dif) > 1.5 & AprDec_dif > 0 ~ "slight_positive",
                              abs(AprDec_dif) <= 40 & abs(AprDec_dif) > 20 & AprDec_dif > 0  ~ "moderate_positive",
                              abs(AprDec_dif) > 40 & AprDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
           MayDec = case_when(abs(MayDec_dif) > 40 & MayDec_dif < 0 ~ "strong_negative",
                              abs(MayDec_dif) <= 40 & abs(MayDec_dif) > 20 & MayDec_dif < 0  ~ "moderate_negative",
                              abs(MayDec_dif) <= 20 & abs(MayDec_dif) > 1.5 & MayDec_dif < 0 ~ "slight_negative",
                              MayDec_dif >= -1.5 & MayDec_dif <= 1.5 ~ "no_change",
                              abs(MayDec_dif) <= 20 & abs(MayDec_dif) > 1.5 & MayDec_dif > 0 ~ "slight_positive",
                              abs(MayDec_dif) <= 40 & abs(MayDec_dif) > 20 & MayDec_dif > 0  ~ "moderate_positive",
                              abs(MayDec_dif) > 40 & MayDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
           JunDec = case_when(abs(JunDec_dif) > 40 & JunDec_dif < 0 ~ "strong_negative",
                              abs(JunDec_dif) <= 40 & abs(JunDec_dif) > 20 & JunDec_dif < 0  ~ "moderate_negative",
                              abs(JunDec_dif) <= 20 & abs(JunDec_dif) > 1.5 & JunDec_dif < 0 ~ "slight_negative",
                              JunDec_dif >= -1.5 & JunDec_dif <= 1.5 ~ "no_change",
                              abs(JunDec_dif) <= 20 & abs(JunDec_dif) > 1.5 & JunDec_dif > 0 ~ "slight_positive",
                              abs(JunDec_dif) <= 40 & abs(JunDec_dif) > 20 & JunDec_dif > 0  ~ "moderate_positive",
                              abs(JunDec_dif) > 40 & JunDec_dif > 0 ~ "strong_positive",
                              TRUE  ~ "no_change"),
    )
  
  # 3.7 Saving summary table
  write.csv(r_df, "../outputs/ntl_analysis/tab1_change.csv")
  
  # 3.8 Creating a dataframe
  r_df <- r_df[,c("x", "y", "JanDec_dif", "FebDec_dif", "MarDec_dif", "AprDec_dif", "MayDec_dif", "JunDec_dif")]
  
    # Converting to long format - discrete categories
    # data_long <- gather(r_df, time, change, JanDec:AprDec, factor_key=TRUE)
    # data_long$change <- factor(data_long$change, levels = c("strong_negative", 
    #                                                       "moderate_negative", 
    #                                                       "slight_negative", 
    #                                                       "no_change", 
    #                                                       "slight_positive",
    #                                                       "moderate_positive",
    #                                                       "strong_positive"))   
    # Converting to long format - continuous categories
    data_long <- gather(r_df, time, intensity, JanDec_dif:JunDec_dif, factor_key=TRUE)
  
    data_long$intensity[data_long$intensity > 30] <- 30
    data_long$intensity[data_long$intensity < -30] <- -30
    
  # colors <- c("#e6550d", "#fdae6b", "grey", "#9ecae1", "#3182bd")
  # data_long$city <- cities[i]
  # 
  # 
  # # add vector to a dataframe
  # df <- data.frame(data_long)
  # data_total <- rbind(data_total,df)
  
  # Get background map
  map <- get_stamenmap(bbox = bbox(Month1_cropped), 
                         maptype = "toner-background", 
                         #color = "bw", 
                         force = TRUE)
    
  plotList[[i]] <- ggmap(map, darken = 0.9) +
    geom_raster(data = data_long, aes(x = x, y = y, fill = intensity)) +
    scale_fill_gradientn(colours = c("#ff0000", "#cc0000", "#9b0000", "#c1c1c100", "#009991", "#00ccc2", "#80fff9"),
                         breaks = c(-30, 30),
                         labels = c("Less", "More")) +
    facet_grid(~time) +
    coord_sf() +
    theme_tufte() +
    theme(text = element_text(size = 18),
          panel.grid.major = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          #legend.title = element_blank(),
          legend.position = "none",
          strip.text = element_blank(),
          plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm")) +
    xlab("") + ylab(cities[i])
  
}

# library(ggpubr)
# combined_plots <- ggarrange(plotlist = plotList[1:5],  nrow=5, common.legend = TRUE, legend="bottom")

# 4. Extract a horizontal legend
legend <- get_legend(
  plotList[[1]] + 
    guides(fill = guide_colourbar(barwidth = 15, 
                                  barheight = .8,
                                  title.vjust = 1,
                                  title = "Light intensity")) +
                                  #title = expression(atop(Light~intensity,italic((nW~cm^{2}~sr^{-1})))))) +
                                  #title.position = "top")) + 
    theme(legend.position = "bottom",
          legend.direction="horizontal",
          legend.box = "horizontal",
          legend.text = element_text(size=18))
)

# Set 1
final_plots <- plot_grid(plotlist = plotList[1:5],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set1.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 2
final_plots <- plot_grid(plotlist = plotList[6:10],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set2.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 3
final_plots <- plot_grid(plotlist = plotList[11:15],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set3.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 4
final_plots <- plot_grid(plotlist = plotList[16:20],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set4.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 5
final_plots <- plot_grid(plotlist = plotList[21:25],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set5.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 6
final_plots <- plot_grid(plotlist = plotList[26:30],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set6.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 7
final_plots <- plot_grid(plotlist = plotList[31:35],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set7.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 8
final_plots <- plot_grid(plotlist = plotList[36:40],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set8.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 9
final_plots <- plot_grid(plotlist = plotList[41:45],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set9.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 

# Set 10
final_plots <- plot_grid(plotlist = plotList[46:50],
                         ncol = 1,
                         align = "hv")

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
final_plots_legend <- plot_grid(final_plots, legend, ncol = 1, rel_heights = c(1, .05))

png("../outputs/ntl_analysis/ntl_plots_set10.png",units="in", width=10, height=10, res=300)
final_plots_legend
dev.off() 
