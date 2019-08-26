# Script for entropy calculation

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)

#import entropy package 
#install.packages("SpatEntropy")
library(SpatEntropy)


#---------
#import LSOA shapefile 
LSOAboundaries <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/Boundaries/London_shape","LSOA_London")
#set projections
BNG = "+init=epsg:27700"
LSOAboundaries <-st_transform(LSOAboundaries,BNG)
#---------

# 2010 -----------------------

#import the subsetted year data with LSOA codes 
POIinput_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_2010_forEntropy")


# 2010 - all categories entropy (not used - see commercial)-----------------------

#delete NA LSOA codes - so only selecting ELSA boundary LSOAs in GL
POIinput_2010 <- POIinput_2010[!is.na(POIinput_2010$LSOA11CD),]


# test data - ignore -----------------------------
subsetPOI <- POIinput_2010[1:1000,]
length(unique(subsetPOI$LSOA11CD))
LSOAcounts <-aggregate(data.frame(count = subsetPOI$LSOA11CD), list(value = subsetPOI$LSOA11CD), length)
#-------------------------------------------------


#compute number of businesses per LSOA
bizCountLSOA <-aggregate(data.frame(count = POIinput_2010$LSOA11CD), list(value = POIinput_2010$LSOA11CD), length)

#compute entropy value by LSOA 
entropyResults <- POIinput_2010 %>%
  group_by(LSOA11CD)  %>%
  count(class_code) %>%
  group_by(class_code)  %>%
  count(LSOA11CD) %>%
  ungroup(LSOA11CD,class_code)  %>%
  left_join(.,bizCountLSOA, by = c("LSOA11CD" = "value"))  %>%
  mutate(prob = n/count, 
                         logprob = log(1/prob), #to account for the negative sign
                         PilnPi = prob*logprob) %>%
  rename(countClassCodeLSOA =n,countTotalBusinessesLSOA =count )

# Now create summary table by LSOA
# this will sum the PilnPi values by class code within the LSOA
LSOAentropyResults <- entropyResults %>%
  group_by(LSOA11CD) %>%
  summarise(entropy = sum(PilnPi)) %>%
  left_join(.,bizCountLSOA, by = c("LSOA11CD" = "value")) %>%
  rename(countTotalBusinessesLSOA =count ) 

# max entropy value
LSOAentropyResults$maxEntropy <- log(LSOAentropyResults$countTotalBusinessesLSOA)


#drop geometry and add to LSOA shapefile
LSOAentropyResults <- as.data.frame(LSOAentropyResults)
LSOAentropyResults <- subset(LSOAentropyResults, select =-c(geometry))

#join to shp
LSOAentropyResults_1 <- left_join(LSOAboundaries,LSOAentropyResults, by ="LSOA11CD")

#map to have a look
tmap_mode("view")
tm_shape(LSOAentropyResults_1) +
  tm_polygons(col = "entropy", alpha=0.5)




# 2010 - commercial categories only from POI data  -----------------------

# select only commercial categories 
POIinput_2010_commercial <- POIinput_2010[grep("^02",POIinput_2010$class_code),]

#delete NA LSOA codes - so only selecting ELSA boundary LSOAs in GL
POIinput_2010_commercial <- POIinput_2010_commercial[!is.na(POIinput_2010_commercial$LSOA11CD),]

#how many commercial class codes are there? 
length(unique(POIinput_2010_commercial$class_code))

#compute number of businesses per LSOA
bizCountLSOA_commercial <-aggregate(data.frame(count = POIinput_2010_commercial$LSOA11CD), list(value = POIinput_2010_commercial$LSOA11CD), length)

#compute entropy value by LSOA 
entropyResults_commercial <- POIinput_2010_commercial %>%
  group_by(LSOA11CD)  %>%
  count(class_code) %>%
  group_by(class_code)  %>%
  count(LSOA11CD) %>%
  ungroup(LSOA11CD,class_code)  %>%
  left_join(.,bizCountLSOA_commercial, by = c("LSOA11CD" = "value"))  %>%
  mutate(prob = n/count, 
         logprob = log(1/prob), #to account for the negative sign
         PilnPi = prob*logprob) %>%
  rename(countClassCodeLSOA =n,countTotalBusinessesLSOA =count )

# Now create summary table by LSOA
# this will sum the PilnPi values by class code within the LSOA
LSOAentropyResults_commercial <- entropyResults_commercial %>%
  group_by(LSOA11CD) %>%
  summarise(entropy = sum(PilnPi)) %>%
  left_join(.,bizCountLSOA_commercial, by = c("LSOA11CD" = "value")) %>%
  rename(countTotalBusinessesLSOA =count ) 

# max entropy value
LSOAentropyResults_commercial$maxEntropy <- log(LSOAentropyResults_commercial$countTotalBusinessesLSOA)


#drop geometry and add to LSOA shapefile
LSOAentropyResults_commercial <- as.data.frame(LSOAentropyResults_commercial)
LSOAentropyResults_commercial <- subset(LSOAentropyResults_commercial, select =-c(geometry))

#join to shp
LSOAentropyResults_commercial1 <- left_join(LSOAboundaries,LSOAentropyResults_commercial, by ="LSOA11CD")

#map to have a look
tmap_mode("view")
tm_shape(LSOAentropyResults_commercial1) +
  tm_polygons(col = "entropy", alpha=0.5)

# write out to shp and csv file 
st_write(LSOAentropyResults_commercial1, "POIentropy_2010_commercial.shp")
write.csv(LSOAentropyResults_commercial1, file = "POIentropy_2010_commercial.csv")


# end of 2010 commercial computation  ----------------------


# 2017 #######

#import the subsetted year data with LSOA codes 
POIinput_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_2017_forEntropy")

#delete NA LSOA codes - so only selecting ELSA boundary LSOAs in GL
POIinput_2017 <- POIinput_2017[!is.na(POIinput_2017$LSOA11CD),]


# 2017 - commercial categories only from POI data  -----------------------

# select only commercial categories 
POIinput_2017_commercial <- POIinput_2017[grep("^02",POIinput_2017$class_code),]

#how many commercial class codes are there? 
length(unique(POIinput_2017_commercial$class_code))

#compute number of businesses per LSOA
bizCountLSOA_commercial_2017 <-aggregate(data.frame(count = POIinput_2017_commercial$LSOA11CD), list(value = POIinput_2017_commercial$LSOA11CD), length)

#compute entropy value by LSOA 
entropyResults_commercial_2017 <- POIinput_2017_commercial %>%
  group_by(LSOA11CD)  %>%
  count(class_code) %>%
  group_by(class_code)  %>%
  count(LSOA11CD) %>%
  ungroup(LSOA11CD,class_code)  %>%
  left_join(.,bizCountLSOA_commercial_2017, by = c("LSOA11CD" = "value"))  %>%
  mutate(prob = n/count, 
         logprob = log(1/prob), #to account for the negative sign
         PilnPi = prob*logprob) %>%
  rename(countClassCodeLSOA =n,countTotalBusinessesLSOA =count )

# Now create summary table by LSOA
# this will sum the PilnPi values by class code within the LSOA
LSOAentropyResults_commercial_2017 <- entropyResults_commercial_2017 %>%
  group_by(LSOA11CD) %>%
  summarise(entropy = sum(PilnPi)) %>%
  left_join(.,bizCountLSOA_commercial_2017, by = c("LSOA11CD" = "value")) %>%
  rename(countTotalBusinessesLSOA =count ) 

# max entropy value
LSOAentropyResults_commercial_2017$maxEntropy <- log(LSOAentropyResults_commercial_2017$countTotalBusinessesLSOA)


#drop geometry and add to LSOA shapefile
LSOAentropyResults_commercial_2017 <- as.data.frame(LSOAentropyResults_commercial_2017)
LSOAentropyResults_commercial_2017 <- subset(LSOAentropyResults_commercial_2017, select =-c(geometry))

#join to shp
LSOAentropyResults_commercial1_2017 <- left_join(LSOAboundaries,LSOAentropyResults_commercial_2017, by ="LSOA11CD")

#map to have a look
#tmap_mode("view")
#tm_shape(LSOAentropyResults_commercial1_2017) +
  #tm_polygons(col = "entropy", alpha=0.5)

# write out to shp and csv file 
st_write(LSOAentropyResults_commercial1_2017, "POIentropy_2017_commercial.shp")
write.csv(LSOAentropyResults_commercial1_2017, file = "POIentropy_2017_commercial.csv")












########################
#checks on results 
summary(entropyResults)
length(unique(entropyResults$LSOA11CD))
subsetTest <- entropyResults[entropyResults$LSOA11CD =="E01033737",]


#NOT RELEVANT - FOR REFERENCE ########################

# Now create a list of POI classes 
POIclassTypes <- as.data.frame(unique(POIinput_2010$class_code))
colnames(POIclassTypes) <- "POIclass"
length(unique(POIclassTypes$POIclass))



# CRAP THATS WRONG BELOW PLS IGNORE ###############################

#################################
#take only the poi classification and name 
POIinput_2010_WestminsterSub <- as.data.frame(POIinput_2010_Westminster[,c(2,3)]) 
POIinput_2010_WestminsterSub <-POIinput_2010_WestminsterSub[,c(1,2)]
#POIinput_2010_WestminsterSub <- as.matrix(POIinput_2010_WestminsterSub)


# 1. Create array of classification types
POIclassTypes <- as.data.frame(unique(POIinput_2010$class_code))
colnames(POIclassTypes) <- "POIclass"

# 2. Create a corresponding array of counts for LSOA
POIcountsWestminster <-aggregate(data.frame(count = POIinput_2010_WestminsterSub$class_code), list(value = POIinput_2010_WestminsterSub$class_code), length)

# 3. Match the counts to the possible POI types
POIcountsWestminster2 <- left_join(POIclassTypes,POIcountsWestminster, by = c("POIclass"="value") )

# 4. Calculate the probabilities 
POIcountsWestminster2$probability <- POIcountsWestminster2$count/sum(POIcountsWestminster2$count)

# 5. Add the logs (natural log is just log in R, 1/prob is same as negative)
POIcountsWestminster2$problog <- log(1/POIcountsWestminster2$probability)

#6. multiply and sum it all together

#first replace NA values with 0 for sumation
POIcountsWestminster2[is.na(POIcountsWestminster2)] <- 0

entropyLSOA <- sum(POIcountsWestminster2$probability)*POIcountsWestminster2$problog

maxEntropy <- log(571)
  
  
POIcountsWestminster2$PlnP <- POIcountsWestminster2$probability*POIcountsWestminster2$problog

sum(POIcountsWestminster2$PlnP)

#shannonX(POIinput_2010_WestminsterSub)
