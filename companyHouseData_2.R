# Script to extract company age data from Company House records

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
install.packages("eeptools")
library(eeptools)

# -----------------------------------------------------------------
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

# 2. Import the Company House data 

    CH_london <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/Company House/companyHouseLondon_2.csv", header=TRUE, sep=",")
    
    #convert to sf object
    CH_london_1 <- st_as_sf(CH_london, coords = c("oseast1m","osnrth1m"), crs = BNG)
    
    # check the CRS
    st_crs(CH_london_1)
    st_crs(LSOAboundaries)
    
    # spatial join with LSOA boundaries - left join to point data
    CH_london_2 <-st_join(CH_london_1, LSOAboundaries, join =st_within,
                                   left = TRUE)
    summary(CH_london_2)
    
# 3. Import the SIC code file to extract CI only and classify

      #import the list of CI SIC codes 
      CI_SICcodes <- read.csv(file="DCMS_SICcodes.csv", header=TRUE, sep=",")
      CI_SICcodes$SIC <- as.character(CI_SICcodes$SIC)
      # use this to subset only CI companies
      
          # left join the SIC code and industry grouping 
          CH_london_CI <- CH_london_2[grep(paste(CI_SICcodes$SIC,collapse="|"),CH_london_2$SICCode.SicText_1),] 
          #this time make sure its at the beginning of the string 
          CH_london_CI_1 <- CH_london_CI[grep(paste(CI_SICcodes$SIC,collapse="|^"),CH_london_CI$SICCode.SicText_1),] 
          
          #-----------------------------------------------------------
          #write out this as a csv for reference
          write.csv(CH_london_CI_1, file = "CH_london_CI.csv")
          #-----------------------------------------------------------
          
          # change the date type to allow filtering
          CH_london_CI_1$IncorporationDate <-as.Date(CH_london_CI_1$IncorporationDate, format = "%d/%m/%Y")
          
          # remove NAs
          CH_london_CI_2 <- CH_london_CI_1[!is.na(CH_london_CI_1$LSOA11CD),]
          
          #Add the industry group based on SIC code 
          CH_london_CI_1 <- CH_london_CI[grep(paste(CI_SICcodes$SIC,collapse="|^"),CH_london_CI$SICCode.SicText_1),] 
          
          CH_london_CI_2$group <- ifelse(grepl("^7021|^7311|^7312",  CH_london_CI_2$SICCode.SicText_1), 'Advertising and marketing', 'NO')
          CH_london_CI_2$group <- ifelse(grepl("^7111",  CH_london_CI_2$SICCode.SicText_1), 'Architecture', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^3212",  CH_london_CI_2$SICCode.SicText_1), 'Crafts', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^741",  CH_london_CI_2$SICCode.SicText_1), 'Design: product, graphic and fashion design', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^5911|^5912|^5913|^5914|^601|^602|^742",  CH_london_CI_2$SICCode.SicText_1), 'Film, TV, video, radio and photography', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^5821|^5829|^6201|^6202", CH_london_CI_2$SICCode.SicText_1), 'IT, software and computer services', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^5811|^5812|^5813|^5814|^5819|^743", CH_london_CI_2$SICCode.SicText_1), 'Publishing', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^9101|^9102", CH_london_CI_2$SICCode.SicText_1), 'Museums, galleries and libraries', CH_london_CI_2$group)
          CH_london_CI_2$group <- ifelse(grepl("^592|^8552|^9001|^9002|^9003|^9004", CH_london_CI_2$SICCode.SicText_1), 'Music, performing and visual arts', CH_london_CI_2$group)
          

# 4. filter out by incorporation date
      
      # 2010 year
      
      # filter out any incorp date after 2010
      CH_london_CI_2010 <- CH_london_CI_2[(CH_london_CI_2$IncorporationDate<"2011-01-01"), ]
      summary(CH_london_CI_2010)

      # calculate age of company from incorp date
      date = as.Date("2010-12-31")
      CH_london_CI_2010$companyAge <- age_calc(CH_london_CI_2010$IncorporationDate,enddate = date ,units = "years")
      
      # write out as shapefile
      st_write( CH_london_CI_2010, "CH_london_CI_2010_companyAge2.shp",delete_layer=TRUE)
      
#--------------------------------------------------------------------------------      
     
      
#------------------------------------------------------------------------------- 
# 2017 year
#------------------------------------------------------------------------------- 
      
      # filter out any incorp date after 2017
      CH_london_CI_2017 <- CH_london_CI_2[(CH_london_CI_2$IncorporationDate<"2018-01-01"), ]
      summary(CH_london_CI_2017)
      
      # calculate age of company from incorp date
      date = as.Date("2017-12-31")
      CH_london_CI_2017$companyAge <- age_calc(CH_london_CI_2017$IncorporationDate,enddate = date ,units = "years")
      
      # write out as shapefile
      st_write( CH_london_CI_2017, "CH_london_CI_2017_companyAge2.shp",delete_layer=TRUE)
      
#--------------------------------------------------------------------------------      
      
      # now, subset by industry
      
    #! Advertisting & Marketing
      
      CH_london_CI_2017_AM <- CH_london_CI_2017[CH_london_CI_2017$group=="Advertising and marketing",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_AM %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
     
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
        #tm_dots(col = "compCategory")
  
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 

      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","AMcat")
      LSOA_2017_CHcat <- left_join(LSOAboundaries,CIageCat_LSOA_df,by = c("LSOA11CD"))
      
      #tm_shape(LSOA_2017_CHcat) +
       # tm_fill("AMcat", 
             #   style="jenks",
               # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
    #---------------------------------------------------------------

   # 2. Architecture
      
      CH_london_CI_2017_A <- CH_london_CI_2017[CH_london_CI_2017$group=="Architecture",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_A %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Acat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 3. Crafts
      
      CH_london_CI_2017_C <- CH_london_CI_2017[CH_london_CI_2017$group=="Crafts",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_C %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Ccat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 4. Film, TV, video, radio and photography
      
      CH_london_CI_2017_FTVP <- CH_london_CI_2017[CH_london_CI_2017$group=="Film, TV, video, radio and photography",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_FTVP %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","FTVRPcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 5. Publishing 
      
      CH_london_CI_2017_P <- CH_london_CI_2017[CH_london_CI_2017$group=="Publishing",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_P %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Pcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 6. Music, performing and visual arts 
      
      CH_london_CI_2017_MPVA <- CH_london_CI_2017[CH_london_CI_2017$group=="Music, performing and visual arts",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_MPVA %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","MPVAcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 7. Design: product, graphic and fashion design
      
      CH_london_CI_2017_DPGF <- CH_london_CI_2017[CH_london_CI_2017$group=="Design: product, graphic and fashion design",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_DPGF %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","DPGFcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 8. IT, software and computer services
      
      CH_london_CI_2017_ISC <- CH_london_CI_2017[CH_london_CI_2017$group=="IT, software and computer services",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_ISC %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","ISCcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
      
      
      #---------------------------------------------------------------
      
      # 9. Museums, galleries and libraries
      
      CH_london_CI_2017_MGL <- CH_london_CI_2017[CH_london_CI_2017$group=="Museums, galleries and libraries",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2017_MGL %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","MGLcat")
      LSOA_2017_CHcat <- left_join(LSOA_2017_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2017_CHcat, "LSOA_2017_CHcat.shp",delete_layer=TRUE)
   
# -----------------------------------------------------------------------------------          # -----------------------------------------------------------------------------------         
 # -----------------------------------------------------------------------------------         
   #2010 YEAR
 # -----------------------------------------------------------------------------------         
# -----------------------------------------------------------------------------------         
      #1. Advertisting & Marketing
      
      CH_london_CI_2010_AM <- CH_london_CI_2010[CH_london_CI_2010$group=="Advertising and marketing",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_AM %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","AMcat")
      LSOA_2010_CHcat <- left_join(LSOAboundaries,CIageCat_LSOA_df,by = c("LSOA11CD"))
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 2. Architecture
      
      CH_london_CI_2010_A <- CH_london_CI_2010[CH_london_CI_2010$group=="Architecture",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_A %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Acat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 3. Crafts
      
      CH_london_CI_2010_C <- CH_london_CI_2010[CH_london_CI_2010$group=="Crafts",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_C %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Ccat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 4. Film, TV, video, radio and photography
      
      CH_london_CI_2010_FTVP <- CH_london_CI_2010[CH_london_CI_2010$group=="Film, TV, video, radio and photography",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_FTVP %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","FTVRPcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 5. Publishing 
      
      CH_london_CI_2010_P <- CH_london_CI_2017[CH_london_CI_2010$group=="Publishing",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_P %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","Pcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 6. Music, performing and visual arts 
      
      CH_london_CI_2010_MPVA <- CH_london_CI_2010[CH_london_CI_2010$group=="Music, performing and visual arts",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_MPVA %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","MPVAcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 7. Design: product, graphic and fashion design
      
      CH_london_CI_2010_DPGF <- CH_london_CI_2010[CH_london_CI_2010$group=="Design: product, graphic and fashion design",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_DPGF %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","DPGFcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      #---------------------------------------------------------------
      
      # 8. IT, software and computer services
      
      CH_london_CI_2010_ISC <- CH_london_CI_2017[CH_london_CI_2010$group=="IT, software and computer services",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_ISC %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","ISCcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
      
      #---------------------------------------------------------------
      
      # 9. Museums, galleries and libraries
      
      CH_london_CI_2010_MGL <- CH_london_CI_2010[CH_london_CI_2010$group=="Museums, galleries and libraries",]
      
      #get the percentages   
      CIageCat <- CH_london_CI_2010_MGL %>%
        group_by(LSOA11CD) %>%
        mutate(LSOAcount=length(LSOA11CD),
               percStartUps = length(which(companyAge<4))/LSOAcount,
               percEstablished = length(which(companyAge>9))/LSOAcount,
               percSemiEst = length(which(companyAge>4 &companyAge<9 ))/LSOAcount)
      
      #round the percentages
      CIageCat$percStartUps<-round( CIageCat$percStartUps, digits = 1)
      CIageCat$percEstablished<-round( CIageCat$percEstablished, digits = 1)
      CIageCat$percSemiEst<-round( CIageCat$percSemiEst, digits = 1)
      
      length(CIageCat[CIageCat$percEstablished>=0.5,])
      #categorise
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>=0.5, "Majority Start-ups", "no")
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.5, "Majority established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percSemiEst>=0.5, "Majority semi-established",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percEstablished>=0.4 & CIageCat$percEstablished<0.5, "Established community continuing to attract new businesses",  CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$percStartUps>0.3 &  CIageCat$percSemiEst>0.3, "Start-ups & semi-established", CIageCat$compCategory)
      CIageCat$compCategory <- ifelse( CIageCat$compCategory =="no", "Even Spread",  CIageCat$compCategory)
      
      #count of each category
      compCat <- aggregate(data.frame(count =  CIageCat$compCategory), list(value =  CIageCat$compCategory), length)
      compCat
      #check even spread is even
      evenSpread <- CIageCat[CIageCat$compCategory=="Even Spread",]
      
      #see what it looks like
      #tmap_mode("view")
      #tm_shape( CIageCat) +
      #tm_dots(col = "compCategory")
      
      # now aggregate by LSOA to get the choropleth 
      CIageCat_LSOA <- CIageCat[!duplicated(CIageCat$LSOA11CD),]
      CIageCat_LSOA <- CIageCat_LSOA[,c("LSOA11CD","compCategory")] 
      
      #! customise ---------------------------
      
      #left join to LSOA shapefile and export 
      CIageCat_LSOA_df <- as.data.frame(CIageCat_LSOA)
      CIageCat_LSOA_df <- subset(CIageCat_LSOA_df, select =-c(geometry))
      
      #! customise ---------------------------
      colnames(CIageCat_LSOA_df) <- c("LSOA11CD","MGLcat")
      LSOA_2010_CHcat <- left_join(LSOA_2010_CHcat,CIageCat_LSOA_df,by = c("LSOA11CD"))
      #! customise ---------------------------
      
      #tm_shape(LSOA_2017_CHcat) +
      # tm_fill("AMcat", 
      #   style="jenks",
      # palette="YlOrBr")
      
      # save the new CH age LSOA table to a shapefile
      st_write(LSOA_2010_CHcat, "LSOA_2010_CHcat.shp",delete_layer=TRUE)
      
