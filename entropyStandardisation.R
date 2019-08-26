# Part II to the regression table file  
#Script to standardise entropy and aggregate the building period value categories 

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)
require(tidyr)
require(reshape2)
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)

#-------------------------------------------------------------------------------------
# 2010
#-------------------------------------------------------------------------------------    


  # import the data table 
    regressionTable <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2010")
    
    #summary
    summaryRegression_2010 <- as.data.frame(summary(regressionTable))
    
    #set projection
    BNG = "+init=epsg:27700"
    regressionTable_2010 <-st_transform(regressionTable,BNG)
    
#need to standardise the entropy measure 
    #record the original value
    regressionTable_2010$entropyOrig <- regressionTable_2010$entropy
    
    #calculate the maximum entropy
    maxEntropy_2010 <- log(182)
    
    #now standardise entropy by dividing all calculated values by this
    regressionTable_2010$entropy <- regressionTable_2010$entropyOrig/maxEntropy_2010
    
    #plot to make sure it looks sensible?
    qtm(regressionTable_2010, fill = "entropy")
    
    #look at histogram 
    ggplot(regressionTable_2010, aes(x=entropy)) + geom_histogram()
    #more or less normally distributed 
    
# need to colapse the building period types into 3-4 categories - yr64 onwards not relevant
    regressionTable_2010_1 <- regressionTable_2010
    x <- levels(regressionTable_2010_1$bldng_p)
    levels(regressionTable_2010_1$bldng_p) <- ifelse(x %in%  c("p1900_18","p1919_29"), "p1900_29", x)
    x <- levels(regressionTable_2010_1$bldng_p)
    levels(regressionTable_2010_1$bldng_p) <- ifelse(x %in%  c("p1930_39","p1945_54"), "p1929_54", x)
    x <- levels(regressionTable_2010_1$bldng_p)
    levels(regressionTable_2010_1$bldng_p) <- ifelse(x %in%  c("p1955_64","p1965_72","p1973_82","p1983_92","p1993_99"), "p1955_99", x)
    levels(regressionTable_2010_1$bldng_p)
    
# export new table for the GWR regressions, only use 2 categories (up to pre1965)
    st_write(regressionTable_2010_1, "LSOA_regressionTable_2010_2.shp",delete_layer=TRUE)
    
#-------------------------------------------------------------------------------------
    #2017
#-------------------------------------------------------------------------------------    
 
    
    # import the data table 
    regressionTable_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2017")
    
    #summary
    summaryRegression_2017 <- as.data.frame(summary(regressionTable_2017))
    
    #set projection
    BNG = "+init=epsg:27700"
    regressionTable_2017 <-st_transform(regressionTable_2017,BNG)
    
#need to standardise the entropy measure 
    #record the original value
    regressionTable_2017$entropyOrig <- regressionTable_2017$entropy
    
    #calculate the maximum entropy- count of how many commercial POI classification codes
    maxEntropy_2017 <- log(174)
    
    #now standardise entropy by dividing all calculated values by this
    regressionTable_2017$entropy <- regressionTable_2017$entropyOrig/maxEntropy_2017
    
    #plot to make sure it looks sensible?
    qtm(regressionTable_2017, fill = "entropy")
    
    #look at histogram 
    ggplot(regressionTable_2017, aes(x=entropy)) + geom_histogram()
    #more or less normally distributed 
    
# need to colapse the building period types into 3-4 categories - yr64 onwards not relevant
    regressionTable_2017_1 <- regressionTable_2017
    x <- levels(regressionTable_2017_1$bldng_p)
    levels(regressionTable_2017_1$bldng_p) <- ifelse(x %in%  c("p1900_18","p1919_29"), "p1900_29", x)
    x <- levels(regressionTable_2017_1$bldng_p)
    levels(regressionTable_2017_1$bldng_p) <- ifelse(x %in%  c("p1930_39","p1945_54"), "p1929_54", x)
    x <- levels(regressionTable_2017_1$bldng_p)
    levels(regressionTable_2017_1$bldng_p) <- ifelse(x %in%  c("p1955_64","p1965_72","p1973_82","p1983_92","p1993_99"), "p1955_99", x)
    levels(regressionTable_2017_1$bldng_p)
    
    # export new table for the GWR regressions, only use 2 categories (up to pre1965)
    st_write(regressionTable_2017_1, "LSOA_regressionTable_2017_2.shp",delete_layer=TRUE)
    
    