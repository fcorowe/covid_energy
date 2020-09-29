library(raster)
library(viridis)
library(tmaptools)
library(sf)
library(countrycode)

# read the raster data
dec_data <- raster("../data/for_code/NTL/dec_data.tif")
jan_data <- raster("../data/for_code/NTL/jan_data.tif")
feb_data <- raster("../data/for_code/NTL/feb_data.tif")
mar_data <- raster("../data/for_code/NTL/mar_data.tif")

# stack all the months
multiple_years <- stack(dec_data, jan_data, feb_data, mar_data)


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

# we split the cities by groups of five to plot them
cities <- cities[46:50]
country <- country[46:50]

png("testing_mapBASE.png",units="in", width=10, height=10, res=300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(5,4), bg='black', bty='n')

for(i in 1:length(cities)){


  for (k in 1:nlayers(multiple_years)) {
    
    city_subset <- subset(fua, eFUA_name==cities[i] & Cntry_ISO ==country[i])
    
    
    

    
    bbox_extent <- st_bbox(city_subset)
    
    crop_extent <- extent(bbox_extent[1],bbox_extent[3],bbox_extent[2],bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    

    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 0)

    

    

    
  }

  

}


dev.off()

