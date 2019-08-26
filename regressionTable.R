# Script to compose the regression table 

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
#install.packages("PerformanceAnalytics")
#install.packages("ggcorrplot")
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)

# 1. Geographical unit/shape to add attributes to:

      # import the LSOA boundaries
      LSOAboundaries <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/Boundaries/London_shape","LSOA_London")
      
      #set projections
      BNG = "+init=epsg:27700"
      LSOAboundaries <-st_transform(LSOAboundaries,BNG)
      st_crs(LSOAboundaries)
      
      #choose only the useful columns for final table
      LSOAboundaries <- LSOAboundaries[,c(1:4,12,13)]
      
      #add area to each LSOA
      LSOAboundaries$LSOAarea <- st_area(LSOAboundaries)
      LSOAboundaries$LSOAarea <- as.numeric(LSOAboundaries$LSOAarea)
      #convert to hectares
      LSOAboundaries$LSOAarea <- LSOAboundaries$LSOAarea/10000
      
      
# -----------------------------------------------------------------      

# PART I : THE 2010 DATA TABLE
      
# 2. POI - CI data - no. of CIs per Ha, count CIs (all)
    
      # import the cleaned/selected point data - a shp in this case 
      POI_CIlondon_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CI_2010")
      
      #set projection
      POI_CIlondon_2010 <-st_transform(POI_CIlondon_2010,BNG)
      
      # add LSOA code with spatial join
      POI_CIlondon_2010_1 <-st_join(POI_CIlondon_2010, LSOAboundaries, join =st_within,
                             left = TRUE)
      #select out unnecessary columns
      POI_CIlondon_2010_2 <- POI_CIlondon_2010_1[,c(1:5,13:15,23,24:25,30)] 
      
      #remove CI data outside of the Greater London boundary
      #only use data in both Elsa and GLA boundary
      POI_CIlondon_2010_2 <- POI_CIlondon_2010_2[!is.na(POI_CIlondon_2010_2$LSOA11CD),]
      
      #perform counts of CI groups by LSOA, then by industry
      POI_CIlondon_2010_LSOAsum <- POI_CIlondon_2010_2 %>%
        group_by(LSOA11CD) %>%
        count(indst) %>%
        group_by(indst) %>%
        ungroup(LSOA11CD,indst) 
      
      #get areas
      LSOAareaTable <- POI_CIlondon_2010_2 %>%
        group_by(LSOA11CD) %>%
        summarise(LSOAarea1 = mean(LSOAarea))
      LSOAareaTable <- as.data.frame(LSOAareaTable)
      LSOAareaTable <- LSOAareaTable[,-c(3)]
      
      #attach areas to counts table
      POI_CI_LSOAcounts<- left_join(POI_CIlondon_2010_LSOAsum, LSOAareaTable,by = "LSOA11CD" )
      
      #calculate density of CIs by industry
      POI_CI_LSOAcounts$CIPerHa <- POI_CI_LSOAcounts$n/POI_CI_LSOAcounts$LSOAarea1
      
      #remove point geometry
      POI_CI_LSOAcounts_1 <- as.data.frame(POI_CI_LSOAcounts)
      POI_CI_LSOAcounts_1 <- POI_CI_LSOAcounts_1[,-c(5)]
      
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(POI_CI_LSOAcounts_1, file = "POI_CIclassCountsDensityLSOA_2010_byIndustry.csv")
      #-----------------------------------------------------------
      
      POI_CI_LSOAcounts_2 <- POI_CI_LSOAcounts_1[,-c(4,5)]
      
      # Now need to reformat (pivot) so each industry is a column
      #and add a total count & density
      POI_CI_LSOA_pivot <- reshape(POI_CI_LSOAcounts_2,v.names = "n",idvar = "LSOA11CD",
                                   timevar = "indst", direction = "wide")
      # replace NA's
      POI_CI_LSOA_pivot[is.na(POI_CI_LSOA_pivot)] <- 0
      
      #add total as a column
      POI_CI_LSOA_pivot<- POI_CI_LSOA_pivot %>%
        mutate(CItotal = select(., 2:10) %>% rowSums(na.rm = TRUE))
      
      
      # add to the LSOA boundary file
      LSOAboundaries_CI <- left_join(LSOAboundaries, POI_CI_LSOA_pivot,by = "LSOA11CD" )
      
      #add the area and get total density of CI count 
      LSOAboundaries_CI$CItotalDensity <- LSOAboundaries_CI$CItotal/LSOAboundaries_CI$LSOAarea
      
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(LSOAboundaries_CI, file = "POI_CIclassCountsLSOA_2010.csv")
      #-----------------------------------------------------------
      
      #choose only the useful columns for final table
      LSOAboundaries_CI_1  <- LSOAboundaries_CI[,c(1,17,19)]
      
      #remove geometry and convert to dataframe
      LSOAboundaries_CI_1 <- as.data.frame(LSOAboundaries_CI_1)
      LSOAboundaries_CI_1 <- LSOAboundaries_CI_1[,-c(4)]
 
     
      #so now add CI total data to the overall LSOA regression table
      LSOAboundariesTotal <-LSOAboundaries
    
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, LSOAboundaries_CI_1,by = "LSOA11CD" )

      
# 2. VOA data  - median rent per m2, median floorspace m2, floorspace variation
      
      # import the cleaned/selected point data - a shp in this case 
      VOA_london_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","VOA_LSOA_floorspace_rent_2010")
      
      #remove geometry
      VOA_london_2010_1 <- as.data.frame(VOA_london_2010)
      VOA_london_2010_1 <- VOA_london_2010_1[,-c(24)]
      
      #get only the columns we want for the overall table
      VOA_london_2010_1 <- VOA_london_2010_1[,c(1,18,21,23)]
      
      # add LSOA code with join
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, VOA_london_2010_1,by = c("LSOA11CD"="LSOA11C" ) )

      # save the total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal, "LSOA_regressionTable.shp",delete_layer=TRUE)
      
      
# 3. LDD data - no. applications per Ha, median floorspace area (m2) of developments
      
      # import the cleaned/selected point data - a shp in this case 
      LDD_london_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LDD_LSOA_2010")
      
      #remove geometry
      LDD_london_2010_1 <- as.data.frame(LDD_london_2010)
      LDD_london_2010_1 <- LDD_london_2010_1[,-c(23)]
      
      #get only the columns we want for the overall table
      LDD_london_2010_1 <- LDD_london_2010_1[,c(1,18,20,22)]
      
      # add LSOA code with  join
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, LDD_london_2010_1,by = c("LSOA11CD"="LSOA11C" ) )
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal, "LSOA_regressionTable.shp",delete_layer=TRUE)

# 4. POI - entropy measure for diversity of land use 
      
      # import the cleaned/selected LSOA data - a shp in this case 
      POI_entLondon_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","POIentropy_2010_commercial")
      
      #remove geometry
      POI_entLondon_2010_1 <- as.data.frame(POI_entLondon_2010)
      POI_entLondon_2010_1 <- POI_entLondon_2010_1[,-c(19)]
      
      #get only the columns we want for the overall table and reorder 
      POI_entLondon_2010_1 <- POI_entLondon_2010_1[,c(1,16:18)]
      POI_entLondon_2010_2 <- POI_entLondon_2010_1[,c(1,3,2,4)]
      
      # add LSOA code with join
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, POI_entLondon_2010_2,by = c("LSOA11CD"="LSOA11C" ) )
      
      
# 5. Betweenness centrality - normalised average value of nodes by LSOA 
      
      # import the cleaned/selected LSOA data - a csv in this case 
      betCent_London_2010 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis/LSOAroadsData_betValues.csv", header=TRUE, sep=",")
      
      #remove dup x column
      betCent_London_2010 <- betCent_London_2010[,c(2,3)]

      # add LSOA code with left join
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, betCent_London_2010,by = c("LSOA11CD"="LSOA" ) )
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal, "LSOA_regressionTable.shp",delete_layer=TRUE)
      
# ------------------------------------------------------------------
      # Now for the measures not pre-processed/calculated 
# ------------------------------------------------------------------
      
   # 1. IMD - deprivation of population measure 
      
      # import the csv LSOA data  
      IMD_London_2010 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/Demographics/IMD_LSOA_2010.csv", header=TRUE, sep=",")
      
      # add LSOA code with left join
      LSOAboundariesTotal <- left_join(LSOAboundariesTotal, IMD_London_2010,by = "LSOA11CD") 
      #remove extra column added
      LSOAboundariesTotal_1 <-LSOAboundariesTotal[, -c(20)]
      LSOAboundariesTotal <-LSOAboundariesTotal_1 
      
      
  # 2. Building age - median building age by LSOA (for dwellings)
      
      # import the csv LSOA data - note it is 2015 data not 2010 
      builtPeriod_London_2015 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/VOA/dwelling-period-built-2015-lsoa-msoa.csv", header=TRUE, sep=",")
      
      # a bit messy so needs some cleaning 
      
      #select only LSOAs in London
      builtPeriod_London_2015_1 <- builtPeriod_London_2015[builtPeriod_London_2015$GEOGRAPHY == "LSOA",]
      builtPeriod_London_2015_1 <- builtPeriod_London_2015[grep("^E01",builtPeriod_London_2015$ECODE),]
      
      #convert counts to numeric
      builtPeriod_London_2015_2 <- data.frame(lapply(builtPeriod_London_2015_1, gsub, pattern = "-", replacement = 0, fixed = TRUE))
      builtPeriod_London_2015_2 <- data.frame(lapply(builtPeriod_London_2015_2, gsub, pattern = ",", replacement ="", fixed = TRUE))
      
      builtPeriod_London_2015_2$BP_PRE_1900<-as.numeric(as.character(builtPeriod_London_2015_2$BP_PRE_1900))
      builtPeriod_London_2015_2$BP_1900_1918<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1900_1918))
      builtPeriod_London_2015_2$BP_1919_1929<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1919_1929))
      builtPeriod_London_2015_2$BP_1930_1939<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1930_1939))
      builtPeriod_London_2015_2$BP_1945_1954<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1945_1954))
      builtPeriod_London_2015_2$BP_1955_1964<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1955_1964))
      builtPeriod_London_2015_2$BP_1965_1972<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1965_1972))
      builtPeriod_London_2015_2$BP_1973_1982<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1973_1982))
      builtPeriod_London_2015_2$BP_1983_1992<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1983_1992))
      builtPeriod_London_2015_2$BP_1993_1999<-as.numeric(as.character(builtPeriod_London_2015_2$BP_1993_1999))
      builtPeriod_London_2015_2$BP_2000_2009<-as.numeric(as.character(builtPeriod_London_2015_2$BP_2000_2009))
      builtPeriod_London_2015_2$BP_2010_2015<-as.numeric(as.character(builtPeriod_London_2015_2$BP_2010_2015))
      builtPeriod_London_2015_2$ALL_PROPERTIES<-as.numeric(as.character(builtPeriod_London_2015_2$ALL_PROPERTIES))
      
      # group by LSOA and get counts for all tax bands - only doing up to 2010 
      builtPeriod_London_2015_3 <-builtPeriod_London_2015_2 %>%
        group_by(ECODE) %>%
        summarise(pre1900 = sum(BP_PRE_1900),
                  p1900_18 = sum(BP_1900_1918),
                  p1919_29 = sum(BP_1919_1929),
                  p1930_39 = sum(BP_1930_1939),
                  p1945_54 = sum(BP_1945_1954),
                  p1955_64 = sum(BP_1955_1964),
                  p1965_72 = sum(BP_1965_1972),
                  p1973_82 = sum(BP_1973_1982),
                  p1983_92 = sum(BP_1983_1992),
                  p1993_99 = sum(BP_1993_1999),
                  p2000_09 = sum(BP_2000_2009)
                  )
      
      builtPeriod_London_2015_melt <- melt(builtPeriod_London_2015_3, id.vars = 1)
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(builtPeriod_London_2015_melt, file = "builtPeriod_London_2015_melt.csv")
      #-----------------------------------------------------------
      
      builtPeriod_London_2015_byLSOA <-builtPeriod_London_2015_melt %>%
        group_by(ECODE) %>%
        top_n(1,value)
 
      #select only 1 value per LSOA 
      builtPeriod_London_2015_byLSOA_1 <- builtPeriod_London_2015_byLSOA[!duplicated(builtPeriod_London_2015_byLSOA[,c('ECODE')]),]

      #attach the dominant category of dwelling period to LSOA file 
      LSOAboundariesTotal_1 <- LSOAboundariesTotal
      LSOAboundariesTotal_1 <- left_join(LSOAboundariesTotal, builtPeriod_London_2015_byLSOA_1,by = c("LSOA11CD"="ECODE" ),match = "first" )

      #remove extra count added, just want the category
      LSOAboundariesTotal_1 <- LSOAboundariesTotal_1[,-c(22)]

      LSOAboundariesTotal <- LSOAboundariesTotal_1
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal, "LSOA_regressionTable.shp",delete_layer=TRUE)
      
     
  # 3. PTAL values - 0-6 (9 values) for connectivity - note it is not temporal
      
      # import the csv LSOA data - note it is 2015 data not 2010 
      PTAL_London_2015 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/PTAL/LSOA2011 AvPTAI2015.csv", header=TRUE, sep=",")
      
      #include both the Access Index and PTAL score (one is a number, the other a category)
      PTAL_London_2015_1 <- PTAL_London_2015[,c(1:3)]
      
      # join to the overall LSOA table
      LSOAboundariesTotal_1 <- left_join(LSOAboundariesTotal,  PTAL_London_2015_1,by = c("LSOA11CD"="LSOA2011"))
      LSOAboundariesTotal <-LSOAboundariesTotal_1
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal, "LSOA_regressionTable.shp",delete_layer=TRUE)

      #rename columns so its easier to understand
      summary(LSOAboundariesTotal)
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'CItotal'] <- 'CIcount'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'CItotalDensity'] <- 'CIdensity'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'flrsp_M'] <- 'med_bizFlrspace'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'mdRntP2'] <- 'med_bizRentpm2'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'flrsp_C'] <- 'coef_bizFlrspace'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'dvFlr_M'] <- 'med_devFlrspace'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'cTBLSOA'] <- 'POI_bizCount'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'IMD.2010.adjusted'] <- 'IMD_2010'
      names(LSOAboundariesTotal)[names(LSOAboundariesTotal) == 'variable'] <- 'bldng_period'
      
      #save as 2010 table
      LSOAboundariesTotal_2010 <- LSOAboundariesTotal
      
      # add density of business addresses
      LSOAboundariesTotal_2010$bizDensity <- LSOAboundariesTotal_2010$POI_bizCount/LSOAboundariesTotal_2010$LSOAarea
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2010, "LSOA_regressionTable_2010.shp",delete_layer=TRUE)
      
      #drop geometry and also save as a csv file 
      LSOAboundariesTotal_2010_noGeom <- as.data.frame(LSOAboundariesTotal_2010)
      LSOAboundariesTotal_2010_noGeom <- subset(LSOAboundariesTotal_2010_noGeom, select =-c(geometry))
      write.csv(LSOAboundariesTotal_2010_noGeom, file = "LSOA_regressionTable_2010_noGeom.csv")
      
      
      
      #select only inner London boroughs 
      #inner selected using ONS/Eurostat definition
      innerBoroughs <- c("Camden","Hackney","Hammersmith and Fulham",
                         "Islington","Kensington and Chelsea","Lambeth","Lewisham",
                         "Southwark",
                         "Tower Hamlets",
                         "Wandsworth",
                         "Westminster",
                         "City of London",
                         "Haringey",
                         "Newham"
      )
      #select inner boroughs only 
      LSOAboundariesTotal_2010_inner <- LSOAboundariesTotal_2010[grep(paste(innerBoroughs,collapse="|"), 
                                                                      LSOAboundariesTotal_2010$LSOA11NM.x),]
      
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2010_inner, "LSOA_regressionTable_2010_inner.shp",delete_layer=TRUE)
      
# END OF 2010 FILE # --------------------------------------------------------------------------------------------
      
      
      
      
# PART II : THE 2017 DATA TABLE
      
      # 2. 2017 POI - CI data - no. of CIs per Ha, count CIs (all)
      
      # import the cleaned/selected point data - a shp in this case 
      POI_CIlondon_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CI_2017")
      
      #set projection
      POI_CIlondon_2017 <-st_transform(POI_CIlondon_2017,BNG)
      
      # add LSOA code with spatial join
      POI_CIlondon_2017_1 <-st_join(POI_CIlondon_2017, LSOAboundaries, join =st_within,
                                    left = TRUE)
      #select out unnecessary columns
      POI_CIlondon_2017_2 <- POI_CIlondon_2017_1[,c(1:5,13:15,23,24:25,30)] 
      
      #remove CI data outside of the Greater London boundary
      #only use data in both Elsa and GLA boundary
      POI_CIlondon_2017_2 <- POI_CIlondon_2017_2[!is.na(POI_CIlondon_2017_2$LSOA11CD),]
      
      #perform counts of CI groups by LSOA, then by industry
      POI_CIlondon_2017_LSOAsum <- POI_CIlondon_2017_2 %>%
        group_by(LSOA11CD) %>%
        count(indst) %>%
        group_by(indst) %>%
        ungroup(LSOA11CD,indst) 
      
      #get areas
      LSOAareaTable <- POI_CIlondon_2017_2 %>%
        group_by(LSOA11CD) %>%
        summarise(LSOAarea1 = mean(LSOAarea))
      LSOAareaTable <- as.data.frame(LSOAareaTable)
      LSOAareaTable <- LSOAareaTable[,-c(3)]
      
      #attach areas to counts table
      POI_CI_LSOAcounts<- left_join(POI_CIlondon_2017_LSOAsum, LSOAareaTable,by = "LSOA11CD" )
      
      #calculate density of CIs by industry
      POI_CI_LSOAcounts$CIPerHa <- POI_CI_LSOAcounts$n/POI_CI_LSOAcounts$LSOAarea1
      
      #remove point geometry
      POI_CI_LSOAcounts_1 <- as.data.frame(POI_CI_LSOAcounts)
      POI_CI_LSOAcounts_1 <- POI_CI_LSOAcounts_1[,-c(5)]
      
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(POI_CI_LSOAcounts_1, file = "POI_CIclassCountsDensityLSOA_2017_byIndustry.csv")
      #-----------------------------------------------------------
      
      POI_CI_LSOAcounts_2 <- POI_CI_LSOAcounts_1[,-c(4,5)]
      
      # Now need to reformat (pivot) so each industry is a column
      #and add a total count & density
      POI_CI_LSOA_pivot <- reshape(POI_CI_LSOAcounts_2,v.names = "n",idvar = "LSOA11CD",
                                   timevar = "indst", direction = "wide")
      # replace NA's
      POI_CI_LSOA_pivot[is.na(POI_CI_LSOA_pivot)] <- 0
      
      #add total as a column
      POI_CI_LSOA_pivot<- POI_CI_LSOA_pivot %>%
        mutate(CItotal = select(., 2:10) %>% rowSums(na.rm = TRUE))
      
      
      # add to the LSOA boundary file
      LSOAboundaries_CI_2017 <- left_join(LSOAboundaries, POI_CI_LSOA_pivot,by = "LSOA11CD" )
      
      #add the area and get total density of CI count 
      LSOAboundaries_CI_2017$CItotalDensity <- LSOAboundaries_CI_2017$CItotal/LSOAboundaries_CI$LSOAarea
      
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(LSOAboundaries_CI_2017, file = "POI_CIclassCountsLSOA_2017.csv")
      #-----------------------------------------------------------
      
      #choose only the useful columns for final table
      LSOAboundaries_CI_1_2017  <- LSOAboundaries_CI_2017[,c(1,17,19)]
      
      #remove geometry and convert to dataframe
      LSOAboundaries_CI_1_2017 <- as.data.frame(LSOAboundaries_CI_1_2017)
      LSOAboundaries_CI_1_2017 <- LSOAboundaries_CI_1_2017[,-c(4)]
      
      
      #so now add CI total data to the overall LSOA regression table
      LSOAboundariesTotal_2017 <-LSOAboundaries
      
      LSOAboundariesTotal_2017  <- left_join(LSOAboundariesTotal_2017 , LSOAboundaries_CI_1_2017 ,by = "LSOA11CD" )

      
      
      # 2. 2017 VOA data  - median rent per m2, median floorspace m2, floorspace variation
      
      # import the cleaned/selected point data - a shp in this case 
      VOA_london_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","VOA_LSOA_floorspace_rent_2017")
      
      #remove geometry
      VOA_london_2017_1 <- as.data.frame(VOA_london_2017)
      VOA_london_2017_1 <- VOA_london_2017_1[,-c(24)]
      
      #get only the columns we want for the overall table
      VOA_london_2017_1 <- VOA_london_2017_1[,c(1,18,21,23)]
      
      # add LSOA code with join
      LSOAboundariesTotal_2017 <- left_join(LSOAboundariesTotal_2017, VOA_london_2017_1,by = c("LSOA11CD"="LSOA11C" ) )
      
      # save the total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)

      
      # 3. 2017 LDD data - no. applications per Ha, median floorspace area (m2) of developments
      
      # import the cleaned/selected point data - a shp in this case 
      LDD_london_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LDD_LSOA_2017")
      
      #remove geometry
      LDD_london_2017_1 <- as.data.frame(LDD_london_2017)
      LDD_london_2017_1 <- LDD_london_2017_1[,-c(23)]
      
      #get only the columns we want for the overall table
      LDD_london_2017_1 <- LDD_london_2017_1[,c(1,18,20,22)]
      
      # add LSOA code with  join
      LSOAboundariesTotal_2017 <- left_join(LSOAboundariesTotal_2017, LDD_london_2017_1,by = c("LSOA11CD"="LSOA11C" ) )
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)
      
      
      
      # 4. 2017 POI - entropy measure for diversity of land use 
      
      # import the cleaned/selected LSOA data - a shp in this case 
      POI_entLondon_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","POIentropy_2017_commercial")
      
      #remove geometry
      POI_entLondon_2017_1 <- as.data.frame(POI_entLondon_2017)
      POI_entLondon_2017_1 <- POI_entLondon_2017_1[,-c(19)]
      
      #get only the columns we want for the overall table and reorder 
      POI_entLondon_2017_1 <- POI_entLondon_2017_1[,c(1,16:18)]
      POI_entLondon_2017_2 <- POI_entLondon_2017_1[,c(1,3,2,4)]
      
      # add LSOA code with join
      LSOAboundariesTotal_2017 <- left_join(LSOAboundariesTotal_2017, POI_entLondon_2017_2,by = c("LSOA11CD"="LSOA11C" ) )
      
      
      
      # 5. Betweenness centrality - normalised average value of nodes by LSOA 
      
      # import the cleaned/selected LSOA data - a csv in this case 
      betCent_London_2017 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis/LSOAroadsData_betValues.csv", header=TRUE, sep=",")
      
      #remove dup x column
      betCent_London_2017 <- betCent_London_2017[,c(2,3)]
      
      # add LSOA code with left join
      LSOAboundariesTotal_2017 <- left_join(LSOAboundariesTotal_2017, betCent_London_2017,by = c("LSOA11CD"="LSOA" ) )
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)
      
            
      
 # ------------------------------------------------------------------
      # Now for the measures not pre-processed/calculated 
# ------------------------------------------------------------------
      
      # 1. 2015 IMD - deprivation of population measure (no 2017 available)
      
      # import the csv LSOA data  
      IMD_London_2015 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/Demographics/IMD_LSOA_2015.csv", header=TRUE, sep=",")
      
      # add LSOA code with left join
      LSOAboundariesTotal_2017 <- left_join(LSOAboundariesTotal_2017, IMD_London_2015,by = "LSOA11CD") 
      #remove extra column added
      LSOAboundariesTotal_1_2017 <-LSOAboundariesTotal_2017[, -c(20,22)]
      LSOAboundariesTotal_2017 <-LSOAboundariesTotal_1_2017 
      
      
      # 2. Building age - median building age by LSOA (for dwellings)
      
      # use same data import as in the 2010 code but include 2010+ category 
      
      # group by LSOA and get counts for all tax bands - only doing up to 2010 
      builtPeriod_London_2015_incl <-builtPeriod_London_2015_2 %>%
        group_by(ECODE) %>%
        summarise(pre1900 = sum(BP_PRE_1900),
                  p1900_18 = sum(BP_1900_1918),
                  p1919_29 = sum(BP_1919_1929),
                  p1930_39 = sum(BP_1930_1939),
                  p1945_54 = sum(BP_1945_1954),
                  p1955_64 = sum(BP_1955_1964),
                  p1965_72 = sum(BP_1965_1972),
                  p1973_82 = sum(BP_1973_1982),
                  p1983_92 = sum(BP_1983_1992),
                  p1993_99 = sum(BP_1993_1999),
                  p2000_09 = sum(BP_2000_2009),
                  p2010_15 = sum(BP_2010_2015)
        )
      
      builtPeriod_London_2015_melt_incl <- melt(builtPeriod_London_2015_incl, id.vars = 1)
      #-----------------------------------------------------------
      #write out this as a csv for reference
      write.csv(builtPeriod_London_2015_melt_incl, file = "builtPeriod_London_2015included_melt.csv")
      #-----------------------------------------------------------
      
      builtPeriod_London_2015_byLSOA_incl <-builtPeriod_London_2015_melt_incl %>%
        group_by(ECODE) %>%
        top_n(1,value)
      
      #select only 1 value per LSOA 
      builtPeriod_London_2015_byLSOA_1_incl <- builtPeriod_London_2015_byLSOA_incl[!duplicated(builtPeriod_London_2015_byLSOA_incl[,c('ECODE')]),]
      
      #attach the dominant category of dwelling period to LSOA file 
      LSOAboundariesTotal_1_2017 <- LSOAboundariesTotal_2017
      LSOAboundariesTotal_1_2017 <- left_join(LSOAboundariesTotal_2017, builtPeriod_London_2015_byLSOA_1_incl,by = c("LSOA11CD"="ECODE" ),match = "first" )
      
      #remove extra count added, just want the category
      LSOAboundariesTotal_1_2017 <- LSOAboundariesTotal_1_2017[,-c(22)]
      
      LSOAboundariesTotal_2017 <- LSOAboundariesTotal_1_2017
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)
      
      
      
      # 3. 2017 PTAL values - 0-6 (9 values) for connectivity - note it is not temporal
      
      # import the csv LSOA data - note it is 2015 data not 2017
      #PTAL_London_2015 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/PTAL/LSOA2011 AvPTAI2015.csv", header=TRUE, sep=",")
      
      #include both the Access Index and PTAL score (one is a number, the other a category)
      #PTAL_London_2015_1 <- PTAL_London_2015[,c(1:3)]
      
      # join to the overall LSOA table
      LSOAboundariesTotal_1_2017 <- left_join(LSOAboundariesTotal_2017,  PTAL_London_2015_1,by = c("LSOA11CD"="LSOA2011"))
      LSOAboundariesTotal_2017 <-LSOAboundariesTotal_1_2017
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)
      
      #rename columns so its easier to understand
      summary(LSOAboundariesTotal_2017)
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'CItotal'] <- 'CIcount'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'CItotalDensity'] <- 'CIdensity'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'flrsp_M'] <- 'med_bizFlrspace'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'mdRntP2'] <- 'med_bizRentpm2'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'flrsp_C'] <- 'coef_bizFlrspace'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'dvFlr_M'] <- 'med_devFlrspace'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'cTBLSOA'] <- 'POI_bizCount'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'IMD.2010.adjusted'] <- 'IMD_2010'
      names(LSOAboundariesTotal_2017)[names(LSOAboundariesTotal_2017) == 'variable'] <- 'bldng_period'
      
      # add density of business addresses
      LSOAboundariesTotal_2017$bizDensity <- LSOAboundariesTotal_2017$POI_bizCount/LSOAboundariesTotal_2017$LSOAarea
      
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017, "LSOA_regressionTable_2017.shp",delete_layer=TRUE)
      
      #drop geometry and also save as a csv file 
      LSOAboundariesTotal_2017_noGeom <- as.data.frame(LSOAboundariesTotal_2017)
      LSOAboundariesTotal_2017_noGeom <- subset(LSOAboundariesTotal_2017_noGeom, select =-c(geometry))
      write.csv(LSOAboundariesTotal_2017_noGeom, file = "LSOA_regressionTable_2017_noGeom.csv")
      
      
      #select only inner London boroughs 
      #inner selected using ONS/Eurostat definition
      innerBoroughs <- c("Camden","Hackney","Hammersmith and Fulham",
                         "Islington","Kensington and Chelsea","Lambeth","Lewisham",
                         "Southwark",
                         "Tower Hamlets",
                         "Wandsworth",
                         "Westminster",
                         "City of London",
                         "Haringey",
                         "Newham"
                         )
      LSOAboundariesTotal_2017_inner <- LSOAboundariesTotal_2017[grep(paste(innerBoroughs,collapse="|"), 
                                                                  LSOAboundariesTotal_2017$LSOA11NM.x),]
      
      
      # save the new total LSOA table to a shapefile ----------------
      st_write(LSOAboundariesTotal_2017_inner, "LSOA_regressionTable_2017_inner.shp",delete_layer=TRUE)
      
      # END OF 2017 TABLE ----------------------------------------------------
      
      
# PART III: VISUALISING THE TABLE FOR BOTH YEARS
      
      # import the data table 
      regressionTable_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2010_2")
      regressionTable_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2017_2")
      LSOAboundariesTotal_2010 <-regressionTable_2010 
      LSOAboundariesTotal_2017 <-regressionTable_2017 
      
      summary(LSOAboundariesTotal_2010)
      summary(LSOAboundariesTotal_2017)
      #-----------------------------------------------
      
      # 1. 2010 correlation plot
      
      #take only numerical columns from the table
      LSOAboundariesTotal_2010_corr <-LSOAboundariesTotal_2010
      # and set the index to LSOA codes 
      rownames(LSOAboundariesTotal_2010_corr) <- LSOAboundariesTotal_2010_corr$LSOA11CD
      LSOAcor_2010 <- LSOAboundariesTotal_2010_corr[, c(5,9,10:12,14:15,17,19,20,22,25)]
      # set NA's to 0
      LSOAcor_2010[is.na( LSOAcor_2010)] <- 0
      #remove geometry and make a dataframe
      LSOAcor_2010 <- as.data.frame(LSOAcor_2010)
      LSOAcor_2010 <- subset(LSOAcor_2010, select =-c(geometry))
      head(LSOAcor_2010)
      #take out variables not included in the regression
      LSOAcor_2010 <- subset(LSOAcor_2010, select =-c(POPDEN,md_bzFl,md_dvFl,nmDvPrH,entrpyO))
      
      # plot the table
      chart.Correlation(LSOAcor_2010, histogram=TRUE, pch=19, method="spearman")
      #try spearman because the data is not normal...
      
      
      #colourful plot
      corrplot.mixed(cor(LSOAcor_2010), order="hclust", tl.col="black")
      ggcorrplot(cor(LSOAcor_2010), p.mat = cor_pmat(LSOAcor_2010), hc.order=TRUE, type='full')
      ??corrplot.mixed
      
      #-----------------------------------------------
    
      
      # 1. 2017 correlation plot
      
      #take only numerical columns from the table
      LSOAboundariesTotal_2017_corr <-LSOAboundariesTotal_2017
      # and set the index to LSOA codes 
      rownames(LSOAboundariesTotal_2017_corr) <- LSOAboundariesTotal_2017_corr$LSOA11CD
      LSOAcor_2017 <- LSOAboundariesTotal_2017_corr[, c(5,9,10:12,14:15,17,19,20,22,25)]
      # set NA's to 0
      LSOAcor_2017[is.na( LSOAcor_2017)] <- 0
      #remove geometry and make a dataframe
      LSOAcor_2017 <- as.data.frame(LSOAcor_2017)
      LSOAcor_2017 <- subset(LSOAcor_2017, select =-c(geometry))
      
      # plot the table
      chart.Correlation(LSOAcor_2017, histogram=TRUE, pch=19, method="spearman")
      #try spearman because the data is not normal...
      
      #colourful plot
      corrplot.mixed(cor(LSOAcor_2017), order="hclust", tl.col="black")
      ggcorrplot(cor(LSOAcor_2017), p.mat = cor_pmat(LSOAcor_2017), hc.order=TRUE, type='full')
      
      
      
  #---