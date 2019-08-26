# script to take regression table and prepare for GWR 

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
#install.packages("spgwr")
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)
library(spgwr)


# 2010 data -----------------------------------------------------------

# import the data table 
regressionTable_2010 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2010_2")

#summary ------------------------------------------
summary(regressionTable_2010)
#do a summary of regression table for numerical data
summary2010_r <- do.call(cbind, lapply(regressionTable_2010[,c(5:20,22,24)], summary))
summary2010_r <- t(summary2010_r)
#counts of building period 
summary2010_rC <- do.call(cbind, lapply(regressionTable_2010[,21], summary))

#write csv 
write.csv(summary2010_r, file = "summaryRegressionTable_2010.csv")
write.csv(summary2010_rC, file = "summaryRbuildingPeriods_2010.csv")

# ------------------------------------------


#set projection
BNG = "+init=epsg:27700"
regressionTable_2010 <-st_transform(regressionTable_2010,BNG)

#Log transformation of  CI density 
regressionTable_2010_CI_log <- regressionTable_2010
regressionTable_2010_CI_log$CIlog <-  log10(regressionTable_2010_CI_log$CIdnsty)

#remove Ci density NA's from table
regressionTable_2010_CI_log_1 <- regressionTable_2010_CI_log[!is.na(regressionTable_2010_CI_log$CIlog),]
summary(regressionTable_2010_CI_log_1)

#replace the rest of the NAs in the whole table with 0
regressionTable_2010_CI_log_1[is.na(regressionTable_2010_CI_log_1)] <- 0

#Log the PTAL indicator as it is skewed and there are no 0 values
regressionTable_2010_CI_log_1$AIlog <-  log10(regressionTable_2010_CI_log_1$APTAI20)

#Dont include dev indicators in regression - too many NA values 

# run a test multiple regression to look at residuals

#Get rid of two outliers in Ealing and Barnet, present in both years 
regressionTable_2010_CI_log_1 <-regressionTable_2010_CI_log_1[regressionTable_2010_CI_log_1$LSOA11C!="E01001212",]
regressionTable_2010_CI_log_1 <-regressionTable_2010_CI_log_1[regressionTable_2010_CI_log_1$LSOA11C!="E01000307",]

summary(regressionTable_2010_CI_log_1)

#run model
model1 <- lm(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_201+AIlog, data = regressionTable_2010_CI_log_1)
summary(model1)
plot(model1)

regressionTable_2010_CI_log_1$model_res <- model1$residuals

#plot residuals
qtm(regressionTable_2010_CI_log_1, fill = "model_res")


#GWR -----------------------------------------------------------------
# try setting the bandwidth for the regression model 
#convert to sp
GWR_input_2010 <- as(regressionTable_2010_CI_log_1, 'Spatial')

#calculate the adaptive bandwidth - 0.0131
start.time <- Sys.time()
GWRbandwidth_2010 <- gwr.sel(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_201+AIlog, data=GWR_input_2010, adapt = T) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#calculate the global bandwidth - 2758
start.time <- Sys.time()
GWRbandwidth_global <- gwr.sel(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_201+AIlog, data=GWR_input_2010, adapt = F) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#carry out the GWR
start.time <- Sys.time()
GWRmodel = gwr(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_201+AIlog, data=GWR_input_2010, adapt=GWRbandwidth_2010, hatmatrix=TRUE, se.fit=TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#print results 
GWRmodel

GWRresults<-as.data.frame(GWRmodel$SDF)
head(GWRresults)

#summary of GWR results 
summary2010 <- do.call(cbind, lapply(GWRresults, summary))
summary2010


#save results as a csv (it look a long time to compute!)
#-----------------------------------------------------------
write.csv(GWRresults, file = "GWRoutput_2010.csv")
#summary table
write.csv(summary2010, file = "GWRsummary_2010.csv")
#-----------------------------------------------------------
# -----------------------------------------------------------------

#read in the csv
#-----------------------------------------------------------
GWRresults_1 <- read.csv(file="/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis/GWRoutput_2010.csv", header=TRUE, sep=",")
#-----------------------------------------------------------
head(GWRresults_1)

# attach coefficients to the data table 
regressionTable_2010_CI_log_1$coefmd_bzR2 <- GWRresults_1$md_bzR2
regressionTable_2010_CI_log_1$coefcf_bzFl <- GWRresults_1$cf_bzFl
regressionTable_2010_CI_log_1$bldng_pp1929_54 <- GWRresults_1$bldng_pp1929_54
regressionTable_2010_CI_log_1$bldng_pp1955_99 <- GWRresults_1$bldng_pp1955_99
regressionTable_2010_CI_log_1$bldng_pp2000_09 <- GWRresults_1$bldng_pp2000_09
regressionTable_2010_CI_log_1$bldng_ppre1900  <- GWRresults_1$bldng_ppre1900 
regressionTable_2010_CI_log_1$entropy  <- GWRresults_1$entropy 
regressionTable_2010_CI_log_1$AveBet  <- GWRresults_1$AveBet 
regressionTable_2010_CI_log_1$IMD_201  <- GWRresults_1$IMD_201 
regressionTable_2010_CI_log_1$AIlog  <- GWRresults_1$AIlog 


#plot the variation in coefficients 
tmap_mode("plot")
coefmd_bzR2 <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "coefmd_bzR2", style="jenks",palette = "RdBu")
#coefmd_bzR2
coefcf_bzFl<- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "coefcf_bzFl", style="jenks",palette = "RdBu")
bldng_pp1929_54<- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp1929_54", style="jenks",palette = "RdBu")
bldng_pp1955_99<- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp1955_99", style="jenks",palette = "RdBu")
bldng_pp2000_09<- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp2000_09", style="jenks",palette = "RdBu")
bldng_ppre1900 <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_ppre1900", style="jenks",palette = "RdBu")
entropy <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "entropy", style="jenks",palette = "RdBu")
AveBet <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "AveBet", style="jenks",palette = "RdBu")
IMD_201 <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "IMD_201", style="jenks",palette = "RdBu")
AIlog <- tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "AIlog", style="jenks",palette = "RdBu")

tmap_save(coefmd_bzR2, "GWR_2010_coef_medbzRentm2.png", width=1920, dpi = 300)
tmap_save(coefcf_bzFl, "GWR_2010_coef_cf_bzFl.png", width=1920, dpi = 300)
tmap_save(bldng_pp1929_54, "GWR_2010_coef_bldng_pp1929_54.png", width=1920, dpi = 300)
tmap_save(bldng_pp1955_99, "GWR_2010_coef_bldng_pp1955_99.png", width=1920, dpi = 300)
tmap_save(bldng_pp2000_09, "GWR_2010_coef_bldng_pp2000_09.png", width=1920, dpi = 300)
tmap_save(bldng_ppre1900, "GWR_2010_coef_bldng_ppre1900.png", width=1920, dpi = 300)
tmap_save(entropy, "GWR_2010_coef_entropy.png", width=1920, dpi = 300)
tmap_save(AveBet, "GWR_2010_AveBet.png", width=1920, dpi = 300)
tmap_save(AIlog, "GWR_2010_AIlog.png", width=1920, dpi = 300)
tmap_save(IMD_201, "GWR_2010_IMD_201.png", width=1920, dpi = 300)

#calculate and plot the statistical significance 

#test if coefficient is more than 2 standard errors from 0
#only significant if greater than 0, 
sigTest = abs(GWRresults_1$md_bzR2) - (2 * GWRresults_1$md_bzR2_se) 
sigTest2 = abs(GWRresults_1$cf_bzFl) - (2 * GWRresults_1$cf_bzFl_se) 
sigTest3 = abs(GWRresults_1$bldng_pp1929_54) - (2 * GWRresults_1$bldng_pp1929_54_se) 
sigTest4 = abs(GWRresults_1$bldng_pp1955_99) - (2 * GWRresults_1$bldng_pp1955_99_se) 
sigTest5 = abs(GWRresults_1$bldng_pp2000_09) - (2 * GWRresults_1$bldng_pp2000_09_se) 
sigTest6 = abs(GWRresults_1$bldng_ppre1900) - (2 * GWRresults_1$bldng_ppre1900_se) 
sigTest7 = abs(GWRresults_1$entropy) - (2 * GWRresults_1$entropy_se) 
sigTest8 = abs(GWRresults_1$AveBet) - (2 * GWRresults_1$AveBet_se) 
sigTest9 = abs(GWRresults_1$IMD_201) - (2 * GWRresults_1$IMD_201_se) 
sigTest10 = abs(GWRresults_1$AIlog) - (2 * GWRresults_1$AIlog_se) 

regressionTable_2010_CI_log_1$md_bzR2Sig<-sigTest
regressionTable_2010_CI_log_1$cf_bzFlSig<-sigTest2
regressionTable_2010_CI_log_1$bldng_pp1929_54Sig<-sigTest3
regressionTable_2010_CI_log_1$bldng_pp1955_99Sig<-sigTest4
regressionTable_2010_CI_log_1$bldng_pp2000_09Sig<-sigTest5
regressionTable_2010_CI_log_1$bldng_ppre1900Sig<-sigTest6
regressionTable_2010_CI_log_1$entropySig<-sigTest7
regressionTable_2010_CI_log_1$AveBetSig<-sigTest8
regressionTable_2010_CI_log_1$IMD_201Sig<-sigTest9
regressionTable_2010_CI_log_1$AIlogSig<-sigTest10


#plot and save the map
md_bzR2Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "md_bzR2Sig",style="jenks",palette = "RdYlBu")
cf_bzFlSig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "cf_bzFlSig",style="jenks",palette = "RdYlBu")
bldng_pp1929_54Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp1929_54Sig",style="jenks",palette = "RdYlBu")
bldng_pp1955_99Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp1955_99Sig",style="jenks",palette = "RdYlBu")
bldng_pp2000_09Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_pp2000_09Sig",style="jenks",palette = "RdYlBu")
bldng_ppre1900Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "bldng_ppre1900Sig",style="jenks",palette = "RdYlBu")
entropySig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "entropySig",style="jenks",palette = "RdYlBu")
AveBetSig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "AveBetSig",style="jenks",palette = "RdYlBu")
IMD_201Sig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "IMD_201Sig",style="jenks",palette = "RdYlBu")
AIlogSig <-tm_shape(regressionTable_2010_CI_log_1) +
  tm_fill(col = "AIlogSig",style="jenks",palette = "RdYlBu")

tmap_save(md_bzR2Sig, "GWR_2010_sig_md_bzR2Sig.png", width=1920, dpi = 300)
tmap_save(cf_bzFlSig, "GWR_2010_sig_cf_bzFlSig.png", width=1920, dpi = 300)
tmap_save(bldng_pp1929_54Sig, "GWR_2010_sig_bldng_pp1929_54Sig.png", width=1920, dpi = 300)
tmap_save(cbldng_pp1955_99Sig, "GWR_2010_sig_bldng_pp1955_99Sig.png", width=1920, dpi = 300)
tmap_save(bldng_pp2000_09Sig, "GWR_2010_sig_bldng_pp2000_09Sig.png", width=1920, dpi = 300)
tmap_save(bldng_ppre1900Sig, "GWR_2010_sig_bldng_ppre1900Sig.png", width=1920, dpi = 300)
tmap_save(entropySig, "GWR_2010_sig_entropySig.png", width=1920, dpi = 300)
tmap_save(AveBetSig, "GWR_2010_sig_AveBetSig.png", width=1920, dpi = 300)
tmap_save(IMD_201Sig, "GWR_2010_sig_IMD_201Sig.png", width=1920, dpi = 300)
tmap_save(AIlogSig, "GWR_2010_sig_AIlogSig.png", width=1920, dpi = 300)

# save results to shapefile for analysis
st_write(regressionTable_2010_CI_log_1, "GWRresults_2010.shp",delete_layer=TRUE)

#---------------------------------------------------------------------------------
# 2017 data 
#---------------------------------------------------------------------------

# import the data table 
regressionTable_2017 <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","LSOA_regressionTable_2017_2")

#summary ------------------------------------------
summary(regressionTable_2017)
#do a summary of regression table for numerical data
summary2017_r <- do.call(cbind, lapply(regressionTable_2017[,c(5:20,22,24)], summary))
summary2017_r <- t(summary2017_r)
#counts of building period 
summary2017_rC <- do.call(cbind, lapply(regressionTable_2017[,21], summary))

#write csv 
write.csv(summary2017_r, file = "summaryRegressionTable_2017.csv")
write.csv(summary2017_rC, file = "summaryRbuildingPeriods_2017.csv")
# -------------------------------------------------


#set projection
BNG = "+init=epsg:27700"
regressionTable_2017 <-st_transform(regressionTable_2017,BNG)

#Log of CI density 
regressionTable_2017_CI_log <- regressionTable_2017
regressionTable_2017_CI_log$CIlog <-  log10(regressionTable_2017_CI_log$CIdnsty)

#remove CI density NA lsoas from table
regressionTable_2017_CI_log_1 <- regressionTable_2017_CI_log[!is.na(regressionTable_2017_CI_log$CIlog),]
summary(regressionTable_2017_CI_log_1)

#replace the rest of the variable NAs with 0
regressionTable_2017_CI_log_1[is.na(regressionTable_2017_CI_log_1)] <- 0

#Log PTAL indicator as it is skewed and there are no 0 values
regressionTable_2017_CI_log_1$AIlog <-  log10(regressionTable_2017_CI_log_1$APTAI20)

#dont include dev indicators in regression too - so many NA values 

# run a test multiple regression to look at residuals

#Get rid of two outliers in Ealing and Barnet, present in both years  
regressionTable_2017_CI_log_1 <-regressionTable_2017_CI_log_1[regressionTable_2017_CI_log_1$LSOA11C!="E01001212",]
regressionTable_2017_CI_log_1 <-regressionTable_2017_CI_log_1[regressionTable_2017_CI_log_1$LSOA11C!="E01000307",]



#run the model
model2 <- lm(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_Scr+AIlog, data = regressionTable_2017_CI_log_1)
summary(model2)
plot(model2)

#plot residuals
qtm(regressionTable_2017_CI_log_1, fill = "model_res")


#GWR -----------------------------------------------------------------
# try setting the bandwidth for the regression model 
#convert to sp
GWR_input_2017 <- as(regressionTable_2017_CI_log_1, 'Spatial')

#calculate the adaptive bandwidth - 0.0186
start.time <- Sys.time()
GWRbandwidth_2017 <- gwr.sel(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_Scr+AIlog, data=GWR_input_2017, adapt = T) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#carry out the GWR ---------------------------------------------
start.time <- Sys.time()
GWRmodel_2017 = gwr(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_Scr+AIlog, data=GWR_input_2017, adapt=GWRbandwidth_2017, hatmatrix=TRUE, se.fit=TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#print results 
GWRmodel_2017

GWRresults_2017<-as.data.frame(GWRmodel_2017$SDF)
head(GWRresults_2017)

summary2017 <- do.call(cbind, lapply(GWRresults_2017, summary))
summary2017

#save results as a csv (it look a long time to compute!)
#-----------------------------------------------------------
write.csv(GWRresults_2017, file = "GWRoutput_2017.csv")
#summary table
write.csv(summary2017, file = "GWRsummary_2017.csv")
#-----------------------------------------------------------


# plot the outputs ------------------------------------------------------------


# attach coefficients to the data table 
regressionTable_2017_CI_log_1$coefmd_bzR2 <- GWRresults_2017$md_bzR2
regressionTable_2017_CI_log_1$coefcf_bzFl <- GWRresults_2017$cf_bzFl
regressionTable_2017_CI_log_1$bldng_pp1929_54 <- GWRresults_2017$bldng_pp1929_54
regressionTable_2017_CI_log_1$bldng_pp1955_99 <- GWRresults_2017$bldng_pp1955_99
regressionTable_2017_CI_log_1$bldng_pp2000_09 <- GWRresults_2017$bldng_pp2000_09
regressionTable_2017_CI_log_1$bldng_ppre1900  <- GWRresults_2017$bldng_ppre1900 
regressionTable_2017_CI_log_1$entropy  <- GWRresults_2017$entropy 
regressionTable_2017_CI_log_1$AveBet  <- GWRresults_2017$AveBet 
regressionTable_2017_CI_log_1$IMD_Scr  <- GWRresults_2017$IMD_Scr 
regressionTable_2017_CI_log_1$AIlog  <- GWRresults_2017$AIlog 


#plot the variation in coefficients 
tmap_mode("view")
coefmd_bzR2 <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "coefmd_bzR2", style="jenks",palette = "RdBu")
#coefmd_bzR2
coefcf_bzFl<- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "coefcf_bzFl", style="jenks",palette = "RdBu")
bldng_pp1929_54<- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp1929_54", style="jenks",palette = "RdBu")
bldng_pp1955_99<- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp1955_99", style="jenks",palette = "RdBu")
bldng_pp2000_09<- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp2000_09", style="jenks",palette = "RdBu")
bldng_ppre1900 <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_ppre1900", style="jenks",palette = "RdBu")
entropy <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "entropy", style="jenks",palette = "RdBu")
AveBet <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "AveBet", style="jenks",palette = "RdBu")
IMD_201 <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "IMD_Scr", style="jenks",palette = "RdBu")
AIlog <- tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "AIlog", style="jenks",midpoint = 0, palette = "RdBu")
AIlog

tmap_save(coefmd_bzR2, "GWR_2017_coef_medbzRentm2.png", width=1920, dpi = 300)
tmap_save(coefcf_bzFl, "GWR_2017_coef_cf_bzFl.png", width=1920, dpi = 300)
tmap_save(bldng_pp1929_54, "GWR_2017_coef_bldng_pp1929_54.png", width=1920, dpi = 300)
tmap_save(bldng_pp1955_99, "GWR_2017_coef_bldng_pp1955_99.png", width=1920, dpi = 300)
tmap_save(bldng_pp2000_09, "GWR_2017_coef_bldng_pp2000_09.png", width=1920, dpi = 300)
tmap_save(bldng_ppre1900, "GWR_2017_coef_bldng_ppre1900.png", width=1920, dpi = 300)
tmap_save(entropy, "GWR_2017_coef_entropy.png", width=1920, dpi = 300)
tmap_save(AveBet, "GWR_2017_AveBet.png", width=1920, dpi = 300)
tmap_save(AIlog, "GWR_2017_AIlog.png", width=1920, dpi = 300)
tmap_save(IMD_201, "GWR_2017_IMD_201.png", width=1920, dpi = 300)

#calculate and plot the statistical significance 

#test if coefficient is more than 2 standard errors from 0
#only significant if greater than 0, 
sigTest = abs(GWRresults_2017$md_bzR2) - (2 * GWRresults_2017$md_bzR2_se) 
sigTest2 = abs(GWRresults_2017$cf_bzFl) - (2 * GWRresults_2017$cf_bzFl_se) 
sigTest3 = abs(GWRresults_2017$bldng_pp1929_54) - (2 * GWRresults_2017$bldng_pp1929_54_se) 
sigTest4 = abs(GWRresults_2017$bldng_pp1955_99) - (2 * GWRresults_2017$bldng_pp1955_99_se) 
sigTest5 = abs(GWRresults_2017$bldng_pp2000_09) - (2 * GWRresults_2017$bldng_pp2000_09_se) 
sigTest6 = abs(GWRresults_2017$bldng_ppre1900) - (2 * GWRresults_2017$bldng_ppre1900_se) 
sigTest7 = abs(GWRresults_2017$entropy) - (2 * GWRresults_2017$entropy_se) 
sigTest8 = abs(GWRresults_2017$AveBet) - (2 * GWRresults_2017$AveBet_se) 
sigTest9 = abs(GWRresults_2017$IMD_Scr) - (2 * GWRresults_2017$IMD_Scr_se) 
sigTest10 = abs(GWRresults_2017$AIlog) - (2 * GWRresults_2017$AIlog_se) 

regressionTable_2017_CI_log_1$md_bzR2Sig<-sigTest
regressionTable_2017_CI_log_1$cf_bzFlSig<-sigTest2
regressionTable_2017_CI_log_1$bldng_pp1929_54Sig<-sigTest3
regressionTable_2017_CI_log_1$bldng_pp1955_99Sig<-sigTest4
regressionTable_2017_CI_log_1$bldng_pp2000_09Sig<-sigTest5
regressionTable_2017_CI_log_1$bldng_ppre1900Sig<-sigTest6
regressionTable_2017_CI_log_1$entropySig<-sigTest7
regressionTable_2017_CI_log_1$AveBetSig<-sigTest8
regressionTable_2017_CI_log_1$IMD_201Sig<-sigTest9
regressionTable_2017_CI_log_1$AIlogSig<-sigTest10


#plot and save the map
md_bzR2Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "md_bzR2Sig",style="jenks",palette = "RdYlBu")
cf_bzFlSig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "cf_bzFlSig",style="jenks",palette = "RdYlBu")
bldng_pp1929_54Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp1929_54Sig",style="jenks",palette = "RdYlBu")
bldng_pp1955_99Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp1955_99Sig",style="jenks",palette = "RdYlBu")
bldng_pp2000_09Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_pp2000_09Sig",style="jenks",palette = "RdYlBu")
bldng_ppre1900Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "bldng_ppre1900Sig",style="jenks",palette = "RdYlBu")
entropySig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "entropySig",style="jenks",palette = "RdYlBu")
AveBetSig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "AveBetSig",style="jenks",palette = "RdYlBu")
IMD_201Sig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "IMD_201Sig",style="jenks",palette = "RdYlBu")
AIlogSig <-tm_shape(regressionTable_2017_CI_log_1) +
  tm_fill(col = "AIlogSig",style="jenks",palette = "RdYlBu")
AIlogSig

tmap_save(md_bzR2Sig, "GWR_2017_sig_md_bzR2Sig.png", width=1920, dpi = 300)
tmap_save(cf_bzFlSig, "GWR_2017_sig_cf_bzFlSig.png", width=1920, dpi = 300)
tmap_save(bldng_pp1929_54Sig, "GWR_2017_sig_bldng_pp1929_54Sig.png", width=1920, dpi = 300)
tmap_save(cbldng_pp1955_99Sig, "GWR_2017_sig_bldng_pp1955_99Sig.png", width=1920, dpi = 300)
tmap_save(bldng_pp2000_09Sig, "GWR_2017_sig_bldng_pp2000_09Sig.png", width=1920, dpi = 300)
tmap_save(bldng_ppre1900Sig, "GWR_2017_sig_bldng_ppre1900Sig.png", width=1920, dpi = 300)
tmap_save(entropySig, "GWR_2017_sig_entropySig.png", width=1920, dpi = 300)
tmap_save(AveBetSig, "GWR_2017_sig_AveBetSig.png", width=1920, dpi = 300)
tmap_save(IMD_201Sig, "GWR_2017_sig_IMD_201Sig.png", width=1920, dpi = 300)
tmap_save(AIlogSig, "GWR_2017_sig_AIlogSig.png", width=1920, dpi = 300)

# save results to shapefile for analysis
st_write(regressionTable_2017_CI_log_1, "GWRresults_2017.shp",delete_layer=TRUE)







































#---------------------------------------------------------------------------------
# IGNORE THE BELOW AND DELETE EVENTUALLY
#---------------------------------------------------------------------------
#plot
# take only numericals
LSOAcor_2010 <- regressionTable[, c(5,9,10:12,14:15,17,19,20,22,24,25)]
# set NA's to 0
LSOAcor_2010[is.na( LSOAcor_2010)] <- 0
#remove geometry and make a dataframe
LSOAcor_2010 <- as.data.frame(LSOAcor_2010)
LSOAcor_2010 <- subset(LSOAcor_2010, select =-c(geometry))
summary(LSOAcor_2010)
# plot the table
chart.Correlation(LSOAcor_2010, histogram=TRUE, pch=19, method="spearman")
#try spearman because the data is not normal...

#---------------------------------------------------------------------------------
# log the skewed variables and drop some columns

regressionTable_2010_logged <- regressionTable_2010
regressionTable_2010_logged <-regressionTable_2010_logged[,-c(3:8,10,18,13,14,16,23)]
regressionTable_2010_logged$CIdnsty <-  log10(regressionTable_2010_logged$CIdnsty)
regressionTable_2010_logged$md_bzR2 <-  log10(regressionTable_2010_logged$md_bzR2)
regressionTable_2010_logged$cf_bzFl <-  log10(regressionTable_2010_logged$cf_bzFl)
regressionTable_2010_logged$nmDvPrH <-  log10(regressionTable_2010_logged$nmDvPrH)
regressionTable_2010_logged$AveBet <-  log10(regressionTable_2010_logged$AveBet)
regressionTable_2010_logged$APTAI20 <-  log10(regressionTable_2010_logged$APTAI20)
regressionTable_2010_logged$bzDnsty <-  log10(regressionTable_2010_logged$bzDnsty)

#add the centroids
#regressionTable_2010_logged$LSOAcentroid <- st_centroid(regressionTable_2010_logged)

#get rid of nulls values for CI - omit those LSOAs from the regression?
regressionTable_2010_logged_1 <- regressionTable_2010_logged[!is.na(regressionTable_2010_logged$CIdnsty),]

# set NA's to 0
regressionTable_2010_logged_1[is.na(regressionTable_2010_logged_1)] <- 0
#remove geometry and make a dataframe
regressionTable_2010_logged_1 <- as.data.frame(regressionTable_2010_logged_1)
regressionTable_2010_logged_1 <- subset(regressionTable_2010_logged_1, select =-c(geometry))

# plot the new table
#get rid of categories before plotting
regressionTable_2010_logged_plot <- subset(regressionTable_2010_logged_1, select =-c(1,2,10))

chart.Correlation(regressionTable_2010_logged_plot, histogram=TRUE, pch=19, method="spearman")
#try spearman because the data is not normal...

# try setting the bandwidth for the regression model------------

#convert to sp
GWR_input_2010 <- as(regressionTable_2010_logged , 'Spatial')
GWRbandwidth <- gwr.sel(CIdnsty ~ md_bzR2+cf_bzFl+nmDvPrH+bldng_p+entropy+AveBet+IMD_201+APTAI20+bzDnsty, data=GWR_input_2010, adapt = T) 


#carry out the GWR ---------------------------------------------
GWRmodel_2017 = gwr(CIlog ~ md_bzR2+cf_bzFl+bldng_p+entropy+AveBet+IMD_201+AIlog, data=GWR_input_2010, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
#print results 
GWRmodel_2017

GWRresults<-as.data.frame(GWRmodel$SDF)
head(GWRresults)
#save results as a csv (it look a long time to compute!)