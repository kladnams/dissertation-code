# Script to import and clean the POI data and extract the CIs using lookup file
# Part 2 - CI subset and cleaning -subsetting the POI data by CIs and by year

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

#check how it looks
summary(POI_london)

#read in the SIC classes file to match
POI_CI_classes_final_3 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis/POI_CI_classes.csv", header=TRUE, sep=",")

#the leading 0's have been omitted during the import! need to reinstate 
zeroClasses1 <- paste0("0", POI_CI_classes_final_3$PointX.Classification.Code)
POI_CI_classes_final_3$PointX.Classification.Code <-zeroClasses1

#select only creative industry categories
poiData_CI <- inner_join(POI_london,POI_CI_classes_final_3, by = c("class_code" = "PointX.Classification.Code") )

#write out to shapefile 
st_write(poiData_CI, "poiData_CIonly.shp")

#read in the data (avoid lengthy calculation)
poiData_CI <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CIonly")

#summary statistics of CI's in London by grouping

#counts of large groupings
summary(poiData_CI$industry)
summary(poiData_CI$Description)

#check for na values 
sum(is.na(poiData_CI$industry))
sum(is.na(poiData_CI$class_code))

#counts of large groupings
summary(poiData_CI$industry)

#remove some columns that we don't need 
poiData_CI_1 <- poiData_CI[,c(1:5,8,15:19,27,28,29,33:42)] 

summary(poiData_CI_1$dt_fr)
summary(poiData_CI_1$dt_l_)

# count the dates 
aggregate(data.frame(count = poiData_CI_1$dt_l_), list(value = poiData_CI_1$dt_l_), length)
aggregate(data.frame(count = poiData_CI_1$dt_fr), list(value = poiData_CI_1$dt_fr), length)
sum(is.na(poiData_CI_1$dt_l_))
#no NAs for last listed, so will just be when it was last updated 
#42 different dates last listed - quarterly updates 



#only interested in 2010 and 2017 (to match VOA data)##############################

#2010- get rid of everything first listed after that
poiData_CI_2010 <- subset(poiData_CI_1, poiData_CI_1$dt_fr < "2011-01-01" & poiData_CI_1$dt_l_ > "2009-12-31")
#now get rid of dups where companies change address

#select the right records 
poiData_CI_2010noDupsList <- aggregate(poiData_CI_2010$dt_fr, by = list(poiData_CI_2010$name), max)

#join back the rest of the data
poiData_CI_2010noDups <- inner_join(poiData_CI_2010,poiData_CI_2010noDupsList, by = c("name"= "Group.1", "dt_fr"="x") )

#check there are no duplicates 
compCountsCheck2010 <- aggregate(data.frame(count = poiData_CI_2010noDups$name), list(value = poiData_CI_2010noDups$name), length)
sum(compCountsCheck2010$count)

#counts of large groupings
summary(poiData_CI_2010noDups$indst)

#write to shapefile 
st_write(poiData_CI_2010noDups, "poiData_CI_2010.shp")


#2017- get rid of everything first listed after that
poiData_CI_2017 <- subset(poiData_CI_1, poiData_CI_1$dt_fr < "2018-01-01" & poiData_CI_1$dt_l_ > "2016-12-31")
#now get rid of dups where companies change address

#check the duplicates by name
compCounts <- aggregate(data.frame(count = poiData_CI_2017$name), list(value = poiData_CI_2017$name), length)
dups <- compCounts[compCounts$count >1,]

#select the right records 
poiData_CI_2017noDupsList <- aggregate(poiData_CI_2017$dt_fr, by = list(poiData_CI_2017$name), max)

#join back the rest of the data
poiData_CI_2017noDups <- inner_join(poiData_CI_2017,poiData_CI_2017noDupsList, by = c("name"= "Group.1", "dt_fr"="x") )

#check there are no duplicates 
compCountsCheck2017 <- aggregate(data.frame(count = poiData_CI_2017noDups$name), list(value = poiData_CI_2017noDups$name), length)
sum(compCountsCheck2017$count)

#counts of large groupings
summary(poiData_CI_2017noDups$indst)


#write to shapefile 
st_write(poiData_CI_2017noDups, "poiData_CI_2017.shp")




#it worked!

#checking entries 
poiData_CI_2017noDups[poiData_CI_2017noDups$indst =="Architecture",]
poiData_CI_2017noDups[poiData_CI_2017noDups$name =="English National Ballet",]

#visualise in KDE Maps by years #############################

#2010
# GGPLOT of data points #######
plotPoints <- ggplot(poiData_CI_2010noDups, aes(x=estng, y=nrthn))+
  geom_point()+ coord_equal()

#I. all groups - KDE estimation of point data
KDE_plot <- plotPoints+stat_density2d(aes(fill = ..level..), geom="polygon") + ggtitle("2010 CI industries")
KDE_plot

#save the plot
ggsave("KDE_2010_allCIgroups.png", plot =KDE_plot)

#II. By industry grouping
#Facet of all the plots together 
facetKDE<-ggplot(poiData_CI_2010noDups, aes(x=estng, y=nrthn))+
  geom_point()+ coord_equal()+ facet_wrap(~indst)+stat_density2d(aes(fill = ..level..), geom="polygon")+ ggtitle("2010 CI industries")
facetKDE      

#save the plot
ggsave("KDE_2010_facetCIgroups.png", plot =facetKDE)


#2017 #######
# GGPLOT of data points 
plotPoints <- ggplot(poiData_CI_2017noDups, aes(x=estng, y=nrthn))+
  geom_point()+ coord_equal()

#I. all groups - KDE estimation of point data
KDE_plot <- plotPoints+stat_density2d(aes(fill = ..level..), geom="polygon") + ggtitle("2017 CI industries")
KDE_plot

#save the plot
ggsave("KDE_2017_allCIgroups.png", plot =KDE_plot)

#II. By industry grouping
#Facet of all the plots together 
facetKDE<-ggplot(poiData_CI_2017noDups, aes(x=estng, y=nrthn))+
  geom_point()+ coord_equal()+ facet_wrap(~indst)+stat_density2d(aes(fill = ..level..), geom="polygon")+ ggtitle("2017 CI industries")
facetKDE      

#save the plot
ggsave("KDE_2017_facetCIgroups.png", plot =facetKDE)



# Misc stuff below- not the core script #################################
#visualise in KDE Maps #############################

# GGPLOT of data points 
plotPoints <- ggplot(poiData_CI, aes(x=easting, y=northing))+
  geom_point()+ coord_equal()

#I. all groups - KDE estimation of point data
KDE_plot <- plotPoints+stat_density2d(aes(fill = ..level..), geom="polygon")
KDE_plot

#II. By industry grouping
#Facet of all the plots together 
facetKDE<-ggplot(poiData_CI, aes(x=easting, y=northing))+
  geom_point()+ coord_equal()+ facet_wrap(~industry)+stat_density2d(aes(fill = ..level..), geom="polygon")
facetKDE      









# Individual KDE Maps for each CI#############################
#1.  Advertising and marketing 

        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Advertising and marketing")
        
        # GGPLOT of data points 
        plotPoints1 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        KDE_plot1 <- plotPoints1 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot1

 #2.  Architecture
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Architecture")
        
        # GGPLOT of data points 
        plotPoints2 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        KDE_plot2 <- plotPoints2 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot2

#3.  Crafts
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Crafts")
        
        # GGPLOT of data points 
        plotPoints3 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
     
        KDE_plot3 <- plotPoints3 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot3
        
#4.  Design: product, graphic and fashion design
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Design: product, graphic and fashion design")
        
        # GGPLOT of data points 
        plotPoints4 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
     
        KDE_plot4 <- plotPoints4 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot4
 
#5.  Film, TV, video, radio and photography
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Film, TV, video, radio and photography")
        
        # GGPLOT of data points 
        plotPoints5 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        
        KDE_plot5 <- plotPoints5 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot5

#6.  IT, software and computer services
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="IT, software and computer services")
        
        # GGPLOT of data points 
        plotPoints6 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        
        KDE_plot6 <- plotPoints6 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot6
        
#7.  Museums, galleries and libraries (2293)
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Museums, galleries and libraries")
        
        # GGPLOT of data points 
        plotPoints7 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        
        KDE_plot7 <- plotPoints7 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot7
        
#8.  Music, performing and visual arts (20306)
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Music, performing and visual arts")
        
        # GGPLOT of data points 
        plotPoints8 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        
        KDE_plot8 <- plotPoints8 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot8        
        
        
#9.  Publishing (14812)
        
        subsetPlot <- subset(poiData_CI, poiData_CI$industry=="Publishing")
        
        # GGPLOT of data points 
        plotPoints9 <- ggplot(subsetPlot, aes(x=easting, y=northing))+
          geom_point()+ coord_equal()
        
        KDE_plot9 <- plotPoints9 +stat_density2d(aes(fill = ..level..), geom="polygon")
        KDE_plot9  
        
        
        

        
