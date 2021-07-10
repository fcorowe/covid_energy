library(raster)
# read the tif images
may1_20 <- raster("May_2020-0000000000-0000000000.tif")
may2_20 <- raster("May_2020-0000000000-0000065536.tif")

june1_20 <- raster("June_2020-0000000000-0000000000.tif")
june2_20 <- raster("June_2020-0000000000-0000065536.tif")


start.time <- Sys.time()
may_data <- raster::merge(may1_20, may2_20)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
june_data <- raster::merge(june1_20, june2_20)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


writeRaster(may_data,'may_data.tif')

writeRaster(june_data,'june_data.tif')






dec_data <- raster("dec_data.tif")

