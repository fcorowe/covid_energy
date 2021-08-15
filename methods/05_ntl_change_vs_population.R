# 1 Dependencies
library(raster)
library(sf)
library(countrycode)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)

rm(list=ls())

# 2. Read data
## 2.1 raster data
dec_data <- raster("../data/ntl/dec_data.tif")
jan_data <- raster("../data/ntl/jan_data.tif")
feb_data <- raster("../data/ntl/feb_data.tif")
mar_data <- raster("../data/ntl/mar_data.tif")
apr_data <- raster("../data/ntl/apr_data.tif")
may_data <- raster("../data/ntl/may_data.tif")
jun_data <- raster("../data/ntl/june_data.tif")

## 2.2 Gridded population data
pop1 <- raster('../data/worldpop/pop_dens_worldpop-0000000000-0000000000 11.18.10.tif')
pop2 <- raster('../data/worldpop/pop2_3.tif')

## 2.3 OECD Functional Urban Areas (FUAs) polygons
fua <- st_read("../data/fua_layer/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_MERGED.gpkg")
WGS84 = "+init=epsg:4326"

### Ensure consistency in CRS: WGS84
fua <- st_transform(fua, WGS84)

## 2.4 Selected cities for analysis
cities_data <- read.csv("../data/un_cities_names.csv", encoding = "iso-8859-1")

# 3. Data wrangling
## 3.1 define a vector with city names
cities <- as.vector(cities_data[,5])

## 3.2 define a vector with country names
country <- countrycode(cities_data[,2], "country.name", "iso3c")

## 3.3. rename cities to be consistent with the cities df
fua$eFUA_name[fua$eFUA_name=="Quezon City [Manila]"] <- "Manila"
fua$eFUA_name[fua$eFUA_name=="Osaka [Kyoto]"] <- "Osaka"
fua$eFUA_name[fua$eFUA_name=="Delhi [New Delhi]"] <- "Delhi"
fua$eFUA_name[fua$eFUA_name=="S\x8bo Paulo"] <- "São Paulo"
cities[cities=29] <- "São Paulo"

myplots <- list()  # new empty list

for(i in 1:length(cities)){
  #city_subset <- subset(fua, eFUA_name==cities[i])
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
  
  # month_diff1 <- (Month2_cropped - Month1_cropped)
  # month_diff2 <- (Month3_cropped - Month1_cropped)
  # month_diff3 <- (Month4_cropped - Month1_cropped)
  # month_diff4 <- (Month5_cropped - Month1_cropped)
  # month_diff5 <- (Month6_cropped - Month1_cropped)
  # month_diff6 <- (Month7_cropped - Month1_cropped)
  
  #create stack for all months
  multiple_months <- stack(Month1_cropped, Month2_cropped, Month3_cropped, Month4_cropped, Month5_cropped, Month6_cropped, Month7_cropped)

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

  # 3.6 Average difference across months by city
  r_df$month_diff <- rowMeans( cbind(r_df$JanDec_dif, r_df$FebDec_dif, r_df$MarDec_dif, r_df$AprDec_dif, r_df$MayDec_dif, r_df$JunDec_dif), na.rm=TRUE )
  
  if (!is.null(raster::intersect(Month1_cropped@extent, pop1@extent))) {
    pop_cropped <- crop(pop1, extent(Month1_cropped))
  } else if (!is.null(raster::intersect(Month1_cropped@extent, pop2@extent))) {
    pop_cropped <- crop(pop2, extent(Month1_cropped))
  }
  
  # filter grids with more than 10 people
  pop_cropped[pop_cropped<=10]<-NA
  
  
  pop_df <- as.data.frame(pop_cropped, xy = TRUE)
  colnames(pop_df)[3] <- "pop"
  
  
  month_df <- r_df %>%  dplyr::select(x, y, month_diff)
  #colnames(month_df)[3] <- "month_diff"
  
  combined <- cbind(pop_df, month_df)
  
  # select the columns needed
  combined <- combined[,c(1:3,6)]
  
  
  p <- ggplot(combined, aes(x= log(pop), y= month_diff)) +
    geom_point(colour = "grey90", size = .25) +
    geom_smooth(se = TRUE, level = .95, size = 2) +
    xlab("") +
    ylab("") +
    theme_tufte() +
    labs(title = cities[i]) +
    scale_x_continuous(limits = c(1,12)) +
    scale_y_continuous(limits = c(-80, 80)) +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme(text = element_text(size = 13),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13)) +
    theme(axis.text.y = element_text(size=12)) +
    theme(axis.text.x = element_text(size=12)) +
    theme(axis.title=element_text(size=12))

  myplots[[i]] <- p  # add each plot into plot list
  
}

png("../outputs/pop_density/ntl_n_popdensity.png",units="in", width=10, height=15, res=300)
grid.arrange(arrangeGrob(grobs = myplots, ncol = 5), 
             bottom = textGrob("Population (log)", gp=gpar(fontsize=15,font=8)), 
             left= textGrob("Mean change in nigth-time light intensity (Dec19 - Jun20)", gp=gpar(fontsize=15,font=8), rot = 90, vjust = 1))
dev.off()

