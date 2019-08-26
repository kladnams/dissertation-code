# Script to import and clean the POI data and extract the CIs using lookup file
# Part 3 - Cleaning the POI data general London set for Entropy measure

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)

#import the POI data for London (Elsa boundaries)
POI_london <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/POI/full_shapefile","os_poi_london")

#remove some columns that we don't need 
POI_london_1 <- POI_london[,c(1:5,8,15:19,27,28,29,30)] 

#only interested in 2010 and 2017 (to match VOA data)##############################

## 2010 ----------------------------------------------------

#get rid of everything first listed after that
poiData_2010 <- subset(POI_london_1, POI_london_1$date_first < "2011-01-01" & POI_london_1$date_last_ > "2009-12-31")

# subset out certain POI classes for entropy measure (e.g. relating to transport items)
# Transport won't contribute to measuring diversity, remove public infrastructure too
poiData_2010_1 <- poiData_2010[-grep("^10",poiData_2010$class_code),]

# write out to csv in case it crashes so its easier to import!
st_write(poiData_2010_1 , "poiData_2010_forEntropy.shp")


#ADD THE LSOA CODES TO THE POI DATA 

#read in LSOA file
LSOAboundaries <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/Boundaries/London_shape","LSOA_London")

#set projections
BNG = "+init=epsg:27700"
LSOAboundaries <-st_transform(LSOAboundaries,BNG)
poiData_2010_1 <-st_transform(poiData_2010_1,BNG)
st_crs(poiData_2010_1)

#Next step: spatial join to add LSOA code to points

#plotto check (takes too long)
tmap_mode("view")
tm_shape(LSOAboundaries) +
  tm_borders("black", lwd = .5) 

#join codes to POI data (note that not all POIs will have codes
#this is because of the different boundaries)

poiData_2010_LSOA <-st_join(poiData_2010_1, LSOAboundaries, join =st_within,
          left = TRUE)

#remove infrastructure categories too (shouldve done earlier!)
poiData_2010_LSOA <- poiData_2010_LSOA[-grep("^0633",poiData_2010_LSOA$class_code),]
poiData_2010_LSOA <- poiData_2010_LSOA[-grep("^0634",poiData_2010_LSOA$class_code),]

# write out file to shp and csv for entropy computation (overwrite previous):
st_write(poiData_2010_LSOA, "poiData_2010_forEntropy.shp",delete_layer=TRUE)

#-------------------------------------------------------------



## 2017 ----------------------------------------------------

#get rid of everything first listed after that
poiData_2017 <- subset(POI_london_1, POI_london_1$date_first < "2018-01-01" & POI_london_1$date_last_ > "2016-12-31")

# subset out certain POI classes for entropy measure (e.g. relating to transport items)

# Transport won't contribute to measuring diversity, remove public infrastructure too
poiData_2017_1 <- poiData_2017[-grep("^10",poiData_2017$class_code),]
#remove infrastructure categories too 
poiData_2017_1 <- poiData_2017_1[-grep("^0633",poiData_2017_1$class_code),]
poiData_2017_1 <- poiData_2017_1[-grep("^0634",poiData_2017_1$class_code),]

# write out to csv in case it crashes so its easier to import!
st_write(poiData_2017_1 , "poiData_2017_forEntropy.shp")


#ADD THE LSOA CODES TO THE POI DATA 

#read in LSOA file
#LSOAboundaries <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/Boundaries/London_shape","LSOA_London")

#set projections
poiData_2017_1 <-st_transform(poiData_2017_1,BNG)
st_crs(poiData_2017_1)

#Next step: spatial join to add LSOA code to points

#join codes to POI data (note that not all POIs will have codes
#this is because of the different boundaries)

poiData_2017_LSOA <-st_join(poiData_2017_1, LSOAboundaries, join =st_within,
                            left = TRUE)

# write out file to shp and csv for entropy computation (overwrite previous):
st_write(poiData_2017_LSOA, "poiData_2017_forEntropy.shp",delete_layer=TRUE)

#-------------------------------------------------------------

