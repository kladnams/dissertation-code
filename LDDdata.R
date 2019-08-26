# Script to import, clean and process the LDD data
# Used to put in regression model for development levels 

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

#import the LDD data
LDD_london <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/LDD planning permission/LDD_permissions.csv", header=TRUE, sep=",")

#get rid of superfluous columns
LDD_london <- LDD_london[,c(1:4,6,14:21,28,37,38)]

#fix coord mistake
LDD_london$Easting[LDD_london$Easting== 100021] <- 530332
LDD_london$Northing[LDD_london$Northing== 528202] <- 169682
boxplot(LDD_london$Northing)

#get rid of missing xy points 
LDD_london <- LDD_london[!is.na(LDD_london$Northing),]
LDD_london <- LDD_london[!is.na(LDD_london$Easting),]

# convert data to sf 
LDD_london <- st_as_sf(LDD_london, coords = c("Easting","Northing"), crs = BNG)

# get rid of planning permission that aren't actually development
LDD_london_1 <- LDD_london[LDD_london$Permission.Type %in% c("Full", "Outline",
                                                                   "Prior Approval (Class G - formerly F)",
                                                                   "Prior Approval (Class M - formerly IA)",
                                                                   "Prior Approval (Class O - formerly J)",
                                                                   "Prior Approval (Class Q - formerly MB)",
                                                                   "Prior Approval (Class T - formerly K)",
                                                                   "Prior Approval (Class P)"
                                                                   ),]

# now calculate the columns 

# total floorspace area proposed in m2 - assumes floorspaces in m2
# data is messy - 0's where obviously a number so need a conditional update
LDD_london_2 <- LDD_london_1 

#correct data type in proposed floorspace
#get rid of commas 
LDD_london_2$Proposed.Total.Floorspace <- gsub(",", "",LDD_london_2$Proposed.Total.Floorspace, fixed = TRUE)

#create the floorspace column by using a combination of columns 
LDD_london_2$Proposed.Total.Floorspace <- as.numeric(as.character(LDD_london_2$Proposed.Total.Floorspace))
LDD_london_2$devFloorspace <-ifelse(LDD_london_2$Proposed.Total.Floorspace>0,LDD_london_2$Proposed.Total.Floorspace , 0)
LDD_london_2$devFloorspace <-ifelse(LDD_london_2$devFloorspace ==0,LDD_london_2$Existing.Total.Floorspace,LDD_london_2$Proposed.Total.Floorspace)
LDD_london_2$devFloorspace <-ifelse(LDD_london_2$devFloorspace ==0,LDD_london_2$Total.Site.Area..Proposed..Hectares..ha.*10000,LDD_london_2$devFloorspace)

#change data type to dates for filtering by year
LDD_london_2$Permission.Date <-as.Date(LDD_london_2$Permission.Date, format = "%d/%m/%Y")
LDD_london_2$Date.work.commenced.on.site..Started.Date. <-as.Date(LDD_london_2$Date.work.commenced.on.site..Started.Date., format = "%d/%m/%Y")
LDD_london_2$Date.work.commenced.on.site..Started.Date. <-as.Date(LDD_london_2$Date.work.commenced.on.site..Started.Date., format = "%d/%m/%Y")
LDD_london_2$Date.construction.completed..Completed.Date. <-as.Date(LDD_london_2$Date.construction.completed..Completed.Date., format = "%d/%m/%Y")

#add the areas of each LSOA to calculate density of the planning apps
LSOAboundaries$LSOAarea <- st_area(LSOAboundaries)
LSOAboundaries$LSOAarea <- as.numeric(LSOAboundaries$LSOAarea)

#match to LSOA code
LDD_london_2 <-st_join(LDD_london_2, LSOAboundaries, join =st_within,
                       left = TRUE)

#convert to Ha
LDD_london_2$LSOAarea <- LDD_london_2$LSOAarea/10000

# 2010 LDD data ---------------------------------------

#subset out the 2010 data
LDD_london_2010 <- subset(LDD_london_2, LDD_london_2$Permission.Date < "2011-01-01" & LDD_london_2$Permission.Date > "2009-12-31")
  

#group the values and summarise by LSOA
LDD_london_2010_LSOAsum<- LDD_london_2010 %>%
  group_by(LSOA11CD) %>%
  summarise(numDevs = n(),
            devFloorspace_Ave = mean(devFloorspace),
            devFloorspace_Med = median(devFloorspace),
            devFloorspace_sd = sd(devFloorspace),
            numDevPerHa = (numDevs/median(LSOAarea))
  ) 

# join by LSOA code back to sf object ------------------

#drop geometry and add to LSOA shapefile
LDD_london_2010_LSOAsum <- as.data.frame(LDD_london_2010_LSOAsum)
LDD_london_2010_LSOAsum <- subset(LDD_london_2010_LSOAsum, select =-c(geometry))

#join to shp
LDD_london_2010_byLSOA <- left_join(LSOAboundaries,LDD_london_2010_LSOAsum, by ="LSOA11CD")

#map to have a look
tmap_mode("plot")
tm_shape(LDD_london_2010_byLSOA) +
  tm_fill(col = "numDevPerHa",style = "jenks")


#histogram to look at data

# facetting
# step 1  - reshape the data for facetting 
LondonMelt_2010 <- LDD_london_2010_byLSOA[,c(1,18:20,22)] 
LondonMelt_2010 <- LondonMelt_2010[!is.na(LondonMelt_2010$numDevs),] 
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
ggsave("LDD_2010_histoPlot.png", plot =facetHist_2010)

# write out to shp and csv file 
st_write(LDD_london_2010_byLSOA, "LDD_LSOA_2010.shp",delete_layer=TRUE)
write.csv(LDD_london_2010_byLSOA, file = "LDD_LSOA_2010.csv")

# end of 2010 data ------------------------------------


# 2017 LDD data ---------------------------------------


#subset out the 2017 data
LDD_london_2017 <- subset(LDD_london_2, LDD_london_2$Permission.Date < "2018-01-01" & LDD_london_2$Permission.Date > "2016-12-31")


#group the values and summarise by LSOA
LDD_london_2017_LSOAsum<- LDD_london_2017 %>%
  group_by(LSOA11CD) %>%
  summarise(numDevs = n(),
            devFloorspace_Ave = mean(devFloorspace),
            devFloorspace_Med = median(devFloorspace),
            devFloorspace_sd = sd(devFloorspace),
            numDevPerHa = (numDevs/median(LSOAarea))
  ) 

# join by LSOA code back to sf object ------------------

#drop geometry and add to LSOA shapefile
LDD_london_2017_LSOAsum <- as.data.frame(LDD_london_2017_LSOAsum)
LDD_london_2017_LSOAsum <- subset(LDD_london_2017_LSOAsum, select =-c(geometry))

#join to shp
LDD_london_2017_byLSOA <- left_join(LSOAboundaries,LDD_london_2017_LSOAsum, by ="LSOA11CD")

#map to have a look
tmap_mode("plot")
tm_shape(LDD_london_2017_byLSOA) +
  tm_fill(col = "numDevPerHa",style = "jenks")


#histogram to look at data

# facetting
# step 1  - reshape the data for facetting 
LondonMelt_2017 <- LDD_london_2017_byLSOA[,c(1,18:20,22)] 
LondonMelt_2017 <- LondonMelt_2017[!is.na(LondonMelt_2017$numDevs),] 
LondonMelt_2017 <- as.data.frame(LondonMelt_2017)
LondonMelt_2017 <- subset(LondonMelt_2017, select =-c(geometry))

LondonMelt2_2017 <- melt(LondonMelt_2017, id.vars = 1)
LondonMelt2_2017[is.na(LondonMelt2_2017)] <- 0

attach(LondonMelt2_2017)

# faceting histogram plot########################
hist_2017 <- ggplot(LondonMelt2_2017, aes(x=value)) + geom_histogram()+ ggtitle("2017 LDD data") # + geom_density(colour="red", size=1, adjust=1)
facetHist_2017 <-hist_2017 + facet_wrap(~ variable, scales="free")
facetHist_2017

#save the plot
ggsave("LDD_2017_histoPlot.png", plot =facetHist_2017)

# write out to shp and csv file 
st_write(LDD_london_2017_byLSOA, "LDD_LSOA_2017.shp",delete_layer=TRUE)
write.csv(LDD_london_2017_byLSOA, file = "LDD_LSOA_2017.csv")
