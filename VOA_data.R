# Script to import, clean and process the VOA data
# Used for rent and floorspaces data by LSOA

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)
require(reshape2)

# import the LSOA boundaries
LSOAboundaries <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/Boundaries/London_shape","LSOA_London")

#set projections
BNG = "+init=epsg:27700"
LSOAboundaries <-st_transform(LSOAboundaries,BNG)
st_crs(LSOAboundaries)

# 2010 VOA data ---------------------------------------

#import the VOA data
VOA_london_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/VOA/VOA_Gareth","voa_2010")
st_crs(VOA_london_2010) = 27700
VOA_london_2010_1 <-st_transform(VOA_london_2010,BNG)
st_crs(VOA_london_2010_1)

#left join the LSOA codes 
VOA_london_2010_LSOA <-st_join(VOA_london_2010_1, LSOAboundaries, join =st_within,
                            left = TRUE)

#create subset for testing and remove na LSOAs 
#VOA_london_2010_LSOA_subset <- VOA_london_2010_LSOA[1:5000,]

#get rid of na LSOA values 
VOA_london_2010_LSOA_1<- VOA_london_2010_LSOA[!is.na(VOA_london_2010_LSOA$LSOA11CD),]

# cleaning - removing types of use that aren't relevant -------------------------

#select relevant columns
VOA_london_2010_LSOA_1 <- VOA_london_2010_LSOA_1[,c(16,18,24,34,37,38,39,40)]

# loads of low rents because of the type of things included
# so, get rid of low value categories:

#look at what needs to be omitted                                    
VOA_description_2010 <- VOA_london_2010_LSOA_1 %>%
  group_by(primary_de) %>%
  summarise(count = n(),
            aveSize = mean(total_area))

#get rid of things with a floorspace of <300
VOA_london_2010_LSOA_2 <- subset(VOA_london_2010_LSOA_1,VOA_london_2010_LSOA_1$total_area >300)

# then get rid of things that don't want to measure, eg car parking, advertising
toMatchDesc <- c("Car","car","Advertising Right","Right", "Bus",
                 "0advertising",
                 "Bays","Motorcycle Spaces And Premises", "Phone Booths", "Advertising Column","Parking","Shower")

# use the above list to get rid of irrelevant categories 

VOA_london_2010_LSOA_3 <- VOA_london_2010_LSOA_2[grep(paste(toMatchDesc,collapse="|"), 
                                                      VOA_london_2010_LSOA_2$primary_de,invert = TRUE),]

#-------------------------

#histogram to look at distribution of an LSOA to see whether mean or median is appropriate
VOA_london_2010_LSOAsubset <- VOA_london_2010_LSOA_3[VOA_london_2010_LSOA_3$LSOA11CD=="E01032739",]
hist_2010_1LSOA <- ggplot(VOA_london_2010_LSOAsubset, aes(x=total_area)) + geom_histogram(binwidth=10)+
            geom_vline(aes(xintercept=mean(total_area)),
           color="blue", linetype="dashed", size=0.5)+ 
            geom_vline(aes(xintercept=median(total_area)),
             color="red", linetype="dashed", size=0.5)+ggtitle("2010 VOA subLSOA")  
hist_2010_1LSOA 
#v skewed floorspace distribution
#-------------------------

#group the values and summarise by LSOA
VOA_london_2010_LSOAsum<- VOA_london_2010_LSOA_3 %>%
  mutate(rentPerm2 = adopted_ra/total_area) %>%
  group_by(LSOA11CD) %>%
  summarise(numProperties = n(),
            floorspace_Ave = mean(total_area),
            floorspace_Med = median(total_area),
            floorspace_sd = sd(total_area),
            aveRentPerm2 = mean(rentPerm2),
            medRentPerm2 = median(rentPerm2),
            RentPerm2_sd = sd(rentPerm2)
            ) %>%
  mutate(floorspace_varCoeff = (floorspace_sd/floorspace_Ave)*100)
  
#see how many have less than 10 valuations
VOA_london_2010_LSOAsum[VOA_london_2010_LSOAsum$numProperties<10,]
#1369 have less than 10 fields in an LSOA 

# join by LSOA code back to sf object 

#drop geometry and add to LSOA shapefile
VOA_london_2010_LSOAsum <- as.data.frame(VOA_london_2010_LSOAsum)
VOA_london_2010_LSOAsum <- subset(VOA_london_2010_LSOAsum, select =-c(geometry))

#join to shp
VOA_london_2010_byLSOA <- left_join(LSOAboundaries,VOA_london_2010_LSOAsum, by ="LSOA11CD")

#-----------------------------------
#hmm there's a lot of LSOAs with only 1 value which is troublesome
#create a comparison dataset with only LSOAs with 5+ businesses for comparison
VOA_london_2010_byLSOA_5plus <- VOA_london_2010_byLSOA[VOA_london_2010_byLSOA$numProperties >4,] 
#-----------------------------------

#map to have a look
tmap_mode("view")
tm_shape(VOA_london_2010_byLSOA_5plus) +
tm_fill(col = "medRentPerm2",style = "jenks")

# write out to shp and csv file 
st_write(VOA_london_2010_byLSOA, "VOA_LSOA_floorspace_rent_2010.shp",delete_layer=TRUE)
write.csv(VOA_london_2010_byLSOA, file = "VOA_LSOA_floorspace_rent_2010.csv")

# ---------------------------------------

# histograms of the 2010 VOA summary measures ##############

# facetting
# step 1  - reshape the data for facetting 
LondonMelt_2010 <- VOA_london_2010_byLSOA[,c(1,16:23)] 
LondonMelt_2010 <- LondonMelt_2010[!is.na(LondonMelt_2010$numProperties),] 
LondonMelt_2010 <- as.data.frame(LondonMelt_2010)
LondonMelt_2010 <- subset(LondonMelt_2010, select =-c(geometry))

LondonMelt2_2010 <- melt(LondonMelt_2010, id.vars = 1)
LondonMelt2_2010[is.na(LondonMelt2_2010)] <- 0

attach(LondonMelt2_2010)

# faceting histogram plot########################
hist_2010 <- ggplot(LondonMelt2_2010, aes(x=value)) + geom_histogram()+ ggtitle("2010 VOA data") # + geom_density(colour="red", size=1, adjust=1)
facetHist_2010 <-hist_2010 + facet_wrap(~ variable, scales="free")
facetHist_2010

#save the plot
ggsave("VOA_2010_histoPlot.png", plot =facetHist_2010)



# 2017 VOA data ---------------------------------------

#import the VOA data
VOA_london_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/VOA/VOA_Gareth","voa_2017")
st_crs(VOA_london_2017) = 27700
VOA_london_2017_1 <-st_transform(VOA_london_2017,BNG)
st_crs(VOA_london_2017_1)

#left join the LSOA codes 
VOA_london_2017_LSOA <-st_join(VOA_london_2017_1, LSOAboundaries, join =st_within,
                               left = TRUE)

#get rid of na LSOA values 
VOA_london_2017_LSOA_1<- VOA_london_2017_LSOA[!is.na(VOA_london_2017_LSOA$LSOA11CD),]

# cleaning - removing types of use that aren't relevant -------------------------

# because looks weird when grouped look closer at rents per sqm
#VOA_london_2017_LSOA_1$rentpersqm<- VOA_london_2017_LSOA_1$adopted_ra/VOA_london_2017_LSOA_1$total_area
VOA_london_2017_LSOA_1 <- VOA_london_2017_LSOA_1[,c(16,18,24,34,37,38,39,40)]

# loads of low rents because of the type of things included
# so, get rid of low value categories:

#look at what needs to be omitted                                    
VOA_description <- VOA_london_2017_LSOA_1 %>%
  group_by(primary_de) %>%
  summarise(count = n(),
            aveSize = mean(total_area))


#get rid of things with a floorspace of <300
VOA_london_2017_LSOA_2 <- subset(VOA_london_2017_LSOA_1,VOA_london_2017_LSOA_1$total_area >300)

# then get rid of things that don't want to measure, eg car parking, advertising
toMatchDesc <- c("Car","car","Advertising Right","Right", "Bus",
                 "0advertising",
               "Bays","Motorcycle Spaces And Premises", "Phone Booths", "Advertising Column","Parking","Shower")

# find relevent SIC codes in POI lookup in each column

VOA_london_2017_LSOA_3 <- VOA_london_2017_LSOA_2[grep(paste(toMatchDesc,collapse="|"), 
                                                       VOA_london_2017_LSOA_2$primary_de,invert = TRUE),]

#-------------------------


#group the values and summarise by LSOA
VOA_london_2017_LSOAsum<- VOA_london_2017_LSOA_3 %>%
  mutate(rentPerm2 = adopted_ra/total_area) %>%
  group_by(LSOA11CD) %>%
  summarise(numProperties = n(),
            floorspace_Ave = mean(total_area),
            floorspace_Med = median(total_area),
            floorspace_sd = sd(total_area),
            aveRentPerm2 = mean(rentPerm2),
            medRentPerm2 = median(rentPerm2),
            RentPerm2_sd = sd(rentPerm2)
  ) %>%
  mutate(floorspace_varCoeff = (floorspace_sd/floorspace_Ave)*100)

#see how many have less than 10 valuations
VOA_london_2017_LSOAsum[VOA_london_2017_LSOAsum$numProperties<10,]
# have less than 10 fields in an LSOA 

# join by LSOA code back to sf object 

#drop geometry and add to LSOA shapefile
VOA_london_2017_LSOAsum <- as.data.frame(VOA_london_2017_LSOAsum)
VOA_london_2017_LSOAsum <- subset(VOA_london_2017_LSOAsum, select =-c(geometry))

#join to shp
VOA_london_2017_byLSOA <- left_join(LSOAboundaries,VOA_london_2017_LSOAsum, by ="LSOA11CD")

#-----------------------------------
#hmm there's a lot of LSOAs with only 1 value which is troublesome
#create a comparison dataset with only LSOAs with 5+ businesses for comparison
VOA_london_2017_byLSOA_5plus <- VOA_london_2017_byLSOA[VOA_london_2017_byLSOA$numProperties >4,] 
#-----------------------------------


#map to have a look
tmap_mode("view")
tm_shape(VOA_london_2017_byLSOA_5plus) +
  tm_fill(col = "medRentPerm2",style = "jenks")

# write out to shp and csv file 
st_write(VOA_london_2017_byLSOA, "VOA_LSOA_floorspace_rent_2017.shp",delete_layer=TRUE)
write.csv(VOA_london_2017_byLSOA, file = "VOA_LSOA_floorspace_rent_2017.csv")

# Issue - loads of LSOAs have just 1 valuation -not valid and affecting the results?

# histograms of the VOA summary measures ##############

# facetting
# step 1  - reshape the data for facetting 
LondonMelt <- VOA_london_2017_byLSOA_5plus[,c(1,16:23)] 
LondonMelt <- LondonMelt[!is.na(LondonMelt$numProperties),] 
LondonMelt <- as.data.frame(LondonMelt)
LondonMelt <- subset(LondonMelt, select =-c(geometry))

LondonMelt2 <- melt(LondonMelt, id.vars = 1)
LondonMelt2[is.na(LondonMelt2)] <- 0

attach(LondonMelt2)

# faceting histogram plot########################
hist <- ggplot(LondonMelt2, aes(x=value)) + geom_histogram()+ ggtitle("2017 VOA data") # + geom_density(colour="red", size=1, adjust=1)
facetHist <-hist + facet_wrap(~ variable, scales="free")
facetHist

#save the plot
ggsave("VOA_2017_histoPlot.png", plot =facetHist)

########################

#END - JUST SOME TESTS BELOW
########################
hist2 <- ggplot(VOA_london_2017_byLSOA, aes(x=numProperties)) + geom_histogram(aes(y = ..density..)) # + geom_density(colour="red", size=1, adjust=1)
hist2
max(VOA_london_2017_byLSOA$numProperties, na.rm=T)
min(VOA_london_2017_byLSOA$numProperties, na.rm=T)

max(VOA_london_2017_byLSOA$floorspace_Ave, na.rm=T)
min(VOA_london_2017_byLSOA$numProperties, na.rm=T)

