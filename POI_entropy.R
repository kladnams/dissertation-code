# Script to calculate entropy for POI data as an urban design measure

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)

#import the POI from the shapefile
poiData_CI_2017import <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CI_2017")
