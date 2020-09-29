library(raster)
library(viridis)
library(tmaptools)
library(sf)


dec_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/dec_data.tif")
jan_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/jan_data.tif")
feb_data <- raster("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/nt_imagery/feb_data.tif")

multiple_years <- stack(dec_data, jan_data, feb_data)

# define a list with city names
# read the files with the cities included in the analysis
# cities_data <- read.csv("cities_data_revised.csv")
# cities <- as.vector(cities_data[,1])
#cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities.csv")
cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- head(cities, 4) %>% dplyr::select(City)

png("1_4.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0), mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){


  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    

    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])

    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    

    

    
  }

  

}


dev.off() # Close the file


##########################################
#### Mapping Cities 5-8


cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[5:8, ] %>% dplyr::select(City)


png("5-8.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file


##########################################
#### Mapping Cities 9-12


cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[9:12, ] %>% dplyr::select(City)


png("9-12.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file


##########################################
#### Mapping Cities 13-16

cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[13:16, ] %>% dplyr::select(City)


png("13-16.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file


##########################################
#### Mapping Cities 17-20

cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[17:20, ] %>% dplyr::select(City)


png("17-20.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file


##########################################
#### Mapping Cities 21-24

cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[21:24, ] %>% dplyr::select(City)


png("21-24.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file


##########################################
#### Mapping Cities 25-28

cities <- read.csv("/Users/franciscorowe/Dropbox/Francisco/Research/in_progress/covid19_energy/data/city/cities_data_revised_sorted.csv")
cities <- cities[25:28, ] %>% dplyr::select(City)


png("25-28.png", units = "in", width = 10, height = 10, res = 300)
#Set layout
par(mai=c(0,0,0,0),mfrow = c(nrow(cities), 3), bg='black', bty='n')

for(i in 1:nrow(cities)){
  
  
  for (k in 1:nlayers(multiple_years)) {
    
    
    WGS84 = "+init=epsg:4326"
    
    
    bb <- geocode_OSM(cities[i,])
    
    yPlus <- bb$coords[2]+0.5
    xPlus <- bb$coords[1]+0.5
    yMinus <- bb$coords[2]-0.5
    xMinus <- bb$coords[1]-0.5
    
    b <- st_polygon(list(rbind(c(xMinus,yPlus),
                               c(xPlus, yPlus),
                               c(xPlus,yMinus),
                               c(xMinus,yMinus),
                               c(xMinus,yPlus))))
    
    bbox_extent <- st_bbox(b)
    
    crop_extent <- extent(bbox_extent[1], 
                          bbox_extent[3], 
                          bbox_extent[2], 
                          bbox_extent[4])
    
    r <- crop(multiple_years[[k]], crop_extent)
    
    
    
    
    # set.seed(123) #set seed for reproducibility
    # sampled <- as.vector(r)
    # clusters <- 15 ##15 clusters
    # clust <- kmeans(sampled,clusters)$cluster
    # combined <- as.data.frame(cbind(sampled,clust))
    # brk <- sort(aggregate(combined[,1], list(combined[,2]), max)[,2])
    
    # plot(r, breaks=brk, col=colorRampPalette(c("#001a4d","#0066FF", "yellow"))(clusters),
    #      legend=F,yaxt='n',xaxt='n',frame = F, asp=1.5)
    plot(r,
         col = magma(255, direction = 1),
         legend = F,
         yaxt ='n',
         xaxt ='n',
         frame = F,
         asp = 1)
    # text((bbox_extent[1]+(bbox_extent[3]- bbox_extent[1])/2), (bbox_extent[4]-(bbox_extent[4]-bbox_extent[2])/30), cities[i],
    #      col="white", cex=1.25)
    
    
    
    
    
  }
  
  
  
}


dev.off() # Close the file
