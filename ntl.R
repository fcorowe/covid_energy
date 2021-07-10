city_subset <- subset(fua, eFUA_name==cities[24] & Cntry_ISO ==country[24])

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

r_df[is.na(r_df)] <- 0


# 1.5 excluded noises caused by aurora and remove dim light noises caused by temporal lights from fires and boats
r_df[r_df < 1.5] <- 0

r_df$JanDec_dif <- r_df$January - r_df$December
r_df$FebDec_dif <- r_df$February - r_df$December
r_df$MarDec_dif <- r_df$March - r_df$December
r_df$AprDec_dif <- r_df$April - r_df$December

# mean
df_mean <- mean(as.matrix(r_df[,8:11]))
# standard deviation
df_sd <- sd(as.matrix(r_df[,8:11]))

r_df %>% select(JanDec_dif) %>% 
  ggplot(.) +
  geom_histogram(bins = 100, aes(x = JanDec_dif, y = ..density..)) +
  geom_density(alpha=0.5, colour="#FF6666", aes(x = JanDec_dif))

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
  )




#discrete
r_df <- r_df[,c("x", "y", "JanDec", "FebDec", "MarDec", "AprDec")]
data_long <- gather(r_df, time, change, JanDec:AprDec, factor_key=TRUE)
data_long$change <- factor(data_long$change, levels = c("strong_negative",
                                                        "moderate_negative",
                                                        "slight_negative",
                                                        "no_change",
                                                        "slight_positive",
                                                        "moderate_positive",
                                                        "strong_positive"))


# continuous
r_df <- r_df[,c("x", "y", "JanDec_dif", "FebDec_dif", "MarDec_dif", "AprDec_dif")]
data_long_c <- gather(r_df, time, intensity, JanDec_dif:AprDec_dif, factor_key=TRUE)

data_long_c %>%  #filter(as.numeric(time)==4) %>% 
  ggplot(aes(x = intensity, fill = time)) +
  geom_histogram(bins = 200, aes(y = ..density..))# +
 # geom_density(alpha=0.5, colour="#FF6666", aes(x = intensity))

data_long_c$intensity[data_long_c$intensity > 30] <- 30
data_long_c$intensity[data_long_c$intensity < -30] <- -30


#colors <- c("#460B6AFF", "#87216BFF", "#000004FF", "#FCA007FF", "#FCFFA4FF")
#colors = c("#ff0000", "#cc0000", "#9b0000", "#c1c1c100", "#009991", "#00ccc2", "#80fff9")
# data_long$city <- cities[i]
# 
# 
# # add vector to a dataframe
# df <- data.frame(data_long)
# data_total <- rbind(data_total,df)

map <- get_stamenmap(bbox = bbox(Month1_cropped), 
                     maptype = "toner-background", 
                     #color = "bw", 
                     force = TRUE)

p1d <- ggmap(map, darken = 0.9) +
  geom_tile(data=data_long, aes(x=x,y=y, fill=change, alpha = 1)) + # lower values >  more transparent colors.
  scale_fill_manual(values=colors) +
  #geom_sf(data = city_subset, fill = NA, color = 'gray34') +
  facet_grid(~time) +
  #scale_fill_brewer("Legend_title", type = "seq", palette = "Greys") +
  coord_equal() +
  theme_tufte() +
  theme(panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

png("../outputs/ntl_analysis/tests/p1d_tonerbackground30.png",units="in", width=10, height=10, res=300)
  p1d
dev.off()

p1c <- ggmap(map, darken = 0.9) +
  geom_raster(data = data_long_c, aes(x=x, y=y, fill=intensity)) +
  scale_fill_gradientn(colours = c("#ff0000", "#cc0000", "#9b0000", "#c1c1c100", "#009991", "#00ccc2", "#80fff9"),
                       breaks = c(-30, 30),
                       labels = c("Less", "More")) +
  facet_grid(~time) +
  coord_sf() +
  theme_tufte() +
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.direction="horizontal",
        legend.box = "horizontal",
        strip.text = element_blank()) +
  xlab("") + ylab(cities[24]) +
  guides(fill = guide_colourbar(barwidth = 15, 
                                barheight = 1,
                                title = expression(Light~intensity~(nW~cm^{2}~sr^{-1})),
                                title.position = "top"))
  
png("../outputs/ntl_analysis/tests/p1c_tonerbackground30.png",units="in", width=10, height=10, res=300)
p1c
dev.off() 


# library(ggpubr)
# combined_plots <- ggarrange(plotlist = plotList[1:5],  nrow=5, common.legend = TRUE, legend="bottom")

# extract a legend that is laid out horizontally
legend <- get_legend(
  p1c + 
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


########################
# This works but uses the Google API
devtools::install_github("dr-harper/ggmapstyles")
library(ggmapstyles)
# Google API: AIzaSyALqKmNpssGtlFHrITDmkq1ZbWZkFml6dY
#register_google(key = "AIzaSyALqKmNpssGtlFHrITDmkq1ZbWZkFml6dY")
                  
map <- get_snazzymap(center = 'Shanghai, China',
                     mapRef ="https://snazzymaps.com/style/99143/dark-map")
                     #mapRef = "https://snazzymaps.com/style/108536/google-map-dark")
                     #mapRef = "https://snazzymaps.com/style/72742/dark")
                     #mapRef = "https://snazzymaps.com/style/20694/dark-map")
                     #mapRef = "https://snazzymaps.com/style/88079/dark-map")
                     #mapRef = "https://snazzymaps.com/style/61119/dark")
                     #mapRef = "https://snazzymaps.com/style/8010/minimal-dark-theme")
                     #mapRef = "https://snazzymaps.com/style/267363/map-dark-gray-black")
                     #mapRef = "https://snazzymaps.com/style/1261/dark")
                     #mapRef = "https://snazzymaps.com/style/6296/darkdetail")
                     #mapRef ="https://snazzymaps.com/style/53441/dark-map")
                     #mapRef ="https://snazzymaps.com/style/114407/dark-map")
                     #
                     #mapRef ="https://snazzymaps.com/style/108536/google-map-dark")
                     #mapRef = "https://snazzymaps.com/style/27616/noli-plan")
                     #mapRef = "https://snazzymaps.com/style/73/a-dark-world")


ggmap(map) +
  geom_tile(data=data_long, aes(x=x, y=y, fill=change, alpha = .8)) +
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
        strip.text = element_blank())



ggmap(map) +
  geom_tile(data=data_long_c, aes(x = x, y = y, fill = intensity, alpha = .4)) +
  scale_fill_gradient2(low = "#0D0887FF", mid = "#000004FF", high = "#F0F921FF", midpoint = 0) +
  #scale_fill_viridis(option ="inferno", begin = 0, end = 1, direction = 1) +
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
        strip.text = element_blank())

ggmap(map, darken = .8) +
  geom_tile(data=data_long_c, aes(x = x, y = y, fill = intensity)) +
  scale_fill_gradientn(colours = c("#ff0000", "#d20000", "#9b0000", "#c1c1c100", "#00998b", "#24cebe", "#66fff8")) +
  #scale_fill_gradient2(low = "#0D0887FF", mid = "#000004FF", high = "#F0F921FF", midpoint = 0) +
  #scale_fill_gradient2(low = "#e6550d", mid = "grey", high = "#3182bd", midpoint = 0) +
  #scale_fill_viridis(option ="inferno", begin = 0, end = 1, direction = 1) +
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
        strip.text = element_blank())

#geom_rect and geom_tile do the same thing, but are parameterised differently. geom_rect uses the locations of the four corners (xmin, xmax, ymin and ymax). geom_tile uses the center of the tile and its size (x, y, width, height). geom_raster is a high performance special case for when all the tiles are the same size.

########################
# Does not work
paris <- get_map(location = "paris")
str(paris)
qmap(baylor, zoom = 14, maptype = 53428, api_key = api_key, source = "cloudmade")




library(basemaps)
data(ext)

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "mapbox", map_type = "dark",
             map_token = "pk.eyJ1IjoiZmNvcm93ZSIsImEiOiJja2JyMWtxb2kxdWM1MnlwbzBlaWRpbjBmIn0.0jgdVvBdic6P3l3E-T6OcA")

# load and return basemap map as many different classes:
basemap_plot(ext)
basemap_plot(Month1_cropped)

basemap_magick(ext, map_service = "carto", map_type = "dark")
basemap_magick(Month1_cropped, map_service = "carto", map_type = "dark")
basemap_magick(Month1_cropped, map_service = "carto", map_type = "dark_no_labels")
basemap_magick(Month1_cropped, map_service = "carto", map_type = "dark_only_labels")

base <- ggplot() + 
  basemap_gglayer(Month1_cropped) + 
  coord_sf() +
  scale_fill_identity() 

ext <- st_bbox(Month1_cropped)

basemap_ggplot(ext) +
  geom_tile(data=data_long, aes(x=x,y=y,fill=change, alpha = 0.6)) +
  scale_fill_manual(values=colors) +
  facet_grid(~time) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())

base +
  geom_tile(data=data_long, aes(x=x,y=y,fill=change, alpha = 0.6)) +
  scale_fill_manual(values=colors) +
  facet_grid(~time) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_blank())


1) using osm highways
2) using ggmap, download toner and then change colour
3) using ggmapstyles - Google dark or one of the dark options

#I try this and I don't know how this works. I don't understand the conflict caused by fill
#3) using dark mapbox using other functions