#Script to import and clean the POI data and extract the CIs using lookup file
# part 1: prepare SIC code classifications from DCMS and assign to POI classifications

require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)

#import the lookup file
SIClookUp <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Data/POI/poi_docs/LOOKUPS/POI_CLASS_TO_SIC_LOOKUP.txt", header=TRUE, sep="|")

#cleaning 
#seems to have some questionmarks - get rid of
SIClookUp_1 <- data.frame(lapply(SIClookUp, function(x) {
  gsub("?", "", x, fixed = TRUE)
}))

#lookup of CIs to test it 
testCI <- subset(SIClookUp_1, SIClookUp_1$First.SIC.2007=="7021"|
                   SIClookUp_1$Second.SIC.2007=="7021"|
                   SIClookUp_1$Third.SIC.2007=="7021"|
                   SIClookUp_1$Fourth.SIC.2007=="7021"|
                   SIClookUp_1$Fifth.SIC.2007=="7021"|
                   SIClookUp_1$Sixth.SIC.2007=="7021"|
                 SIClookUp_1$Seventh.SIC.2007=="7021"
                 )

#import the POI data for London (Elsa boundaries)
POI_london <- st_read("/Users/katie/Documents/UCL course/Dissertation/Data/POI/full_shapefile","os_poi_london")

#output the CI file ############################ 

#create list of SIC codes to match based on DCMS document
CI_SIC <- data.frame("Industry Group"= c("Advertising and marketing",
                                         "Advertising and marketing",
                                         "Advertising and marketing",
                                         "Architecture","Crafts","Design: product, graphic and fashion design",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "Film, TV, video, radio and photography",
                                         "IT, software and computer services",
                                         "IT, software and computer services",
                                         "IT, software and computer services",
                                         "IT, software and computer services",
                                         "Publishing","Publishing","Publishing",
                                         "Publishing","Publishing","Publishing",
                                         "Museums, galleries and libraries","Museums, galleries and libraries",
                                         "Music, performing and visual arts",
                                         "Music, performing and visual arts",
                                         "Music, performing and visual arts",
                                         "Music, performing and visual arts",
                                         "Music, performing and visual arts",
                                         "Music, performing and visual arts"),
                     "Industry" = c("Public relations and communication activities", 
                                    "Advertising agencies","Media representation",
                                    "Architectural activities",
                                    "Manufacture of jewellery and related articles",
                                    "Specialised design activities",
                                    "Motion picture, video and television programme production activities", 
                                    "Motion picture, video and television programme post-production",
                                    "Motion picture, video and television programme distribution",
                                    "Motion picture projection activities","Radio broadcasting",
                                    "Television programming and broadcasting activities","Photographic activities","Publishing of computer games","Other software publishing","Computer programming activities","Computer consultancy activities","Book publishing","Publishing of directories and mailing lists","Publishing of newspapers","Publishing of journals and periodicals","Other publishing activities","Translation and interpretation activities","Library and archive activities","Museum activities","Sound recording and music publishing activities","Cultural education","Performing arts","Support activities to performing arts","Artistic creation","Operation of arts facilities"),
                     "SIC" =c("7021","7311","7312",
            "7111","3212", "741", "5911","5912","5913",
            "5914","601", "602","742","5821","5829","6201",
            "6202","5811","5812","5813","5814","5819",
            "743","9101","9102","592", "8552","9001",
            "9002", "9003","9004"))

#export csv for reference 
write.csv(CI_SIC, file = "DCMS_SICcodes.csv")

toMatchSIC <- c("^7021","^7311","^7312",
                "^7111","^3212", "^741", "^5911","^5912","^5913",
                "^5914","^601", "^602","^742","^5821","^5829","^6201",
                "^6202","^5811","^5812","^5813","^5814","^5819",
                "^743","^9101","^9102","^592", "^8552","^9001",
                "^9002", "^9003","^9004")

# find relevent SIC codes in POI lookup in each column

POI_CI_classes1 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                   SIClookUp_1$First.SIC.2007),]
??match
POI_CI_classes2 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Second.SIC.2007),]

POI_CI_classes3 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Third.SIC.2007),]

POI_CI_classes4 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Fourth.SIC.2007),]

POI_CI_classes5 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Fifth.SIC.2007),]

POI_CI_classes6 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Sixth.SIC.2007),]
POI_CI_classes7 <- SIClookUp_1[grep(paste(toMatchSIC,collapse="|"), 
                                    SIClookUp_1$Seventh.SIC.2007),]

# combine them 

POI_CI_classes <- rbind(POI_CI_classes1,POI_CI_classes2,
                        POI_CI_classes3, POI_CI_classes4,
                        POI_CI_classes5,POI_CI_classes6,POI_CI_classes7)

# count if there are duplicate POI codes 
aggregate(data.frame(count = POI_CI_classes$PointX.Classification.Code), list(value = POI_CI_classes$PointX.Classification.Code), length)

#get rid of duplicates
POI_CI_classes <- unique(POI_CI_classes)
#check it worked by using the aggregate function again
#it worked 

#Drop the SIC 2003 codes
POI_CI_classes_final <- POI_CI_classes[,c(1:2,10:16)]

#now match the industry groupings to the table - not sure its working correctly 
POI_CI_classes_final$industry <- CI_SIC$Industry.Group[match(POI_CI_classes_final$First.SIC.2007,CI_SIC$SIC)]

CI_SIC$Industry.Group[match(POI_CI_classes_final$First.SIC.2007,CI_SIC$SIC)]

sum(is.na(POI_CI_classes_final_1$industry))
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)]

POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Second.SIC.2007,CI_SIC$SIC)]
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Third.SIC.2007,CI_SIC$SIC)]
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Fourth.SIC.2007,CI_SIC$SIC)]
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Fifth.SIC.2007,CI_SIC$SIC)]
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Sixth.SIC.2007,CI_SIC$SIC)]
POI_CI_classes_final$industry[is.na(POI_CI_classes_final$industry)] <- CI_SIC$Industry.Group[match(POI_CI_classes_final$Seventh.SIC.2007,CI_SIC$SIC)]

#there were 13 NAs so assign manually 

POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2100177] <-  "Film, TV, video, radio and photography"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2100186] <- "Design: product, graphic and fashion design"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 6340443] <- "IT, software and computer services"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2140214] <- "Design: product, graphic and fashion design"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2040067] <- "Design: product, graphic and fashion design"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2080122] <- "Film, TV, video, radio and photography"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2080131] <- "Music, performing and visual arts"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2080131] <- "Music, performing and visual arts"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 3200263] <- "Music, performing and visual arts"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2080125] <- "Publishing"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 4210276] <- "Music, performing and visual arts"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 9480689] <- "Music, performing and visual arts"


#reassign a few that are obviously incorrectly classified 
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 7370487] <- "Crafts"
POI_CI_classes_final$industry[POI_CI_classes_final$PointX.Classification.Code == 2080129] <- "Publishing"

# delete central gov and signs with na - not relevant 
POI_CI_classes_final_1 <- POI_CI_classes_final %>% na.omit

#the leading 0's have been omitted during the import! need to reinstate 
zeroClasses <- paste0("0", POI_CI_classes_final_1$PointX.Classification.Code)
POI_CI_classes_final_1$PointX.Classification.Code <-zeroClasses

#export csv 
write.csv(POI_CI_classes_final_1, file = "POI_CI_classes.csv")


####################################################################################################################
