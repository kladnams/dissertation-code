
#Script to do a hdbscan of CI clusters using the cleaned POI data

#import the libraries 
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(sf)
require(broom)
require(dplyr)
require(tmap)
#install.packages("dbscan")
library(dbscan)

#2017 year data 

#import the CI cleaned POI from the shapefile
poiData_CI_2017import <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CI_2017")


# 2017 POI data, by industry ###############################

#1. Advertising and Marketing 

  #subset the data so its just that industry
  poiData_CI_2017_AM <- poiData_CI_2017import[poiData_CI_2017import$indst == "Advertising and marketing",]
  
  #extract the xy data from the CI POIs
  CIcluster_AM <- poiData_CI_2017_AM[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_AM) <- NULL
  
  #run the hdbscan for Ad & Mark
  start.time <- Sys.time()
  CIclusters2017_AM <- hdbscan(CIcluster_AM, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_AM
  #stability scores 
  CIclusters2017_AM$cluster_scores
  
  #add cluster info to the data
  poiData_CI_2017_AM$cluster <- CIclusters2017_AM$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_AM$cluster <- as.character(poiData_CI_2017_AM$cluster) 
  poiData_CI_2017_AM$memberProb <- CIclusters2017_AM$membership_prob
  poiData_CI_2017_AM$outlierScore <- CIclusters2017_AM$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_AM) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_AM_clusterTree_AM.jpg", width = 350)
  plot(CIclusters2017_AM, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # create histograms of characteristics 
      poiData_CI_2017_AM_noGeom <- as.data.frame(poiData_CI_2017_AM)
      poiData_CI_2017_AM_noGeom <- subset(poiData_CI_2017_AM_noGeom, select =-c(geometry))
      poiData_CI_2017_AM_noGeom$cluster <- as.numeric(poiData_CI_2017_AM_noGeom$cluster)
      
      #choose only points in clusters (i.e. not 0 as a cluster = noise point)
      poiData_CI_2017_AM_noGeom_C <- poiData_CI_2017_AM_noGeom[poiData_CI_2017_AM_noGeom$cluster!=0, ] 
      
      AM_histo<-ggplot(poiData_CI_2017_AM_noGeom_C,aes(x=cluster)) + 
        geom_histogram(binwidth=0.5)
      AM_histo1<-ggplot(poiData_CI_2017_AM_noGeom_C,aes(x=memberProb)) + 
        geom_histogram()
      AM_histo2<-ggplot(poiData_CI_2017_AM_noGeom_C,aes(x=outlierScore)) + 
        geom_histogram()
      # save the plots
      ggsave("HDBSCAN_2017_AM_clusterHisto.png", plot = AM_histo)
      ggsave("HDBSCAN_2017_AM_memberProbHisto.png", plot = AM_histo1)
      ggsave("HDBSCAN_2017_AM_outlierHisto.png", plot = AM_histo2)

#write out results to a dataframe for analysis/summary
# export shp for reference 
st_write(poiData_CI_2017_AM, "HDBSCAN_2017_AM_pt30_output1.shp")

#2. Architecture --------------------------------------------------------------

  #subset the data so its just that industry
  poiData_CI_2017_A <- poiData_CI_2017import[poiData_CI_2017import$indst == "Architecture",]
  
  #extract the xy data from the CI POIs
  CIcluster_A <- poiData_CI_2017_A[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_A) <- NULL
  
  #run the hdbscan for Architecture
  start.time <- Sys.time()
  CIclusters2017_A <- hdbscan(CIcluster_A, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_A
  
  #add cluster membership to the data
  poiData_CI_2017_A$cluster <- CIclusters2017_A$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_A$cluster <- as.character(poiData_CI_2017_A$cluster) 
  poiData_CI_2017_A$memberProb <- CIclusters2017_A$membership_prob
  poiData_CI_2017_A$outlierScore <- CIclusters2017_A$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_A) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_A_clusterTree.jpg", width = 350)
  plot(CIclusters2017_A, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # create histograms of characteristics 
    poiData_CI_2017_A_noGeom <- as.data.frame(poiData_CI_2017_A)
    poiData_CI_2017_A_noGeom <- subset(poiData_CI_2017_A_noGeom, select =-c(geometry))
    poiData_CI_2017_A_noGeom$cluster <- as.numeric(poiData_CI_2017_A_noGeom$cluster)
    
    #choose only points in clusters (i.e. not 0 as a cluster = noise point)
    poiData_CI_2017_A_noGeom_C <- poiData_CI_2017_A_noGeom[poiData_CI_2017_A_noGeom$cluster!=0, ] 
    
    A_histo<-ggplot(poiData_CI_2017_A_noGeom_C,aes(x=cluster)) + 
      geom_histogram(binwidth=0.5)
    A_histo1<-ggplot(poiData_CI_2017_A_noGeom_C,aes(x=memberProb)) + 
      geom_histogram()
    A_histo2<-ggplot(poiData_CI_2017_A_noGeom_C,aes(x=outlierScore)) + 
      geom_histogram()
    # save the plots
    ggsave("HDBSCAN_2017_A_clusterHisto.png", plot = A_histo)
    ggsave("HDBSCAN_2017_A_memberProbHisto.png", plot = A_histo1)
    ggsave("HDBSCAN_2017_A_outlierHisto.png", plot = A_histo2)

# export csv/shp for reference 
st_write(poiData_CI_2017_A, "HDBSCAN_2017_A_pt30_output1.shp")


#3. Crafts --------------------------------------------------------------

  #subset the data so its just that industry
  poiData_CI_2017_C <- poiData_CI_2017import[poiData_CI_2017import$indst == "Crafts",]
  
  #extract the xy data from the CI POIs
  CIcluster_C <- poiData_CI_2017_C[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_C) <- NULL
  
  #run the hdbscan for Architecture
  start.time <- Sys.time()
  CIclusters2017_C <- hdbscan(CIcluster_C, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_C
  
  #add cluster membership to the data
  poiData_CI_2017_C$cluster <- CIclusters2017_C$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_C$cluster <- as.character(poiData_CI_2017_C$cluster) 
  poiData_CI_2017_C$memberProb <- CIclusters2017_C$membership_prob
  poiData_CI_2017_C$outlierScore <- CIclusters2017_C$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_C) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_C_clusterTree.jpg", width = 350)
  plot(CIclusters2017_C, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # create histograms of characteristics 
    poiData_CI_2017_C_noGeom <- as.data.frame(poiData_CI_2017_C)
    poiData_CI_2017_C_noGeom <- subset(poiData_CI_2017_C_noGeom, select =-c(geometry))
    poiData_CI_2017_C_noGeom$cluster <- as.numeric(poiData_CI_2017_C_noGeom$cluster)
    
    #choose only points in clusters (i.e. not 0 as a cluster = noise point)
    poiData_CI_2017_C_noGeom_C <- poiData_CI_2017_C_noGeom[poiData_CI_2017_C_noGeom$cluster!=0, ] 
    
    C_histo<-ggplot(poiData_CI_2017_C_noGeom_C,aes(x=cluster)) + 
      geom_histogram(binwidth=0.5)
    C_histo1<-ggplot(poiData_CI_2017_C_noGeom_C,aes(x=memberProb)) + 
      geom_histogram()
    C_histo2<-ggplot(poiData_CI_2017_C_noGeom_C,aes(x=outlierScore)) + 
      geom_histogram()
    # save the plots
    ggsave("HDBSCAN_2017_C_clusterHisto.png", plot = C_histo)
    ggsave("HDBSCAN_2017_C_memberProbHisto.png", plot = C_histo1)
    ggsave("HDBSCAN_2017_C_outlierHisto.png", plot = C_histo2)

# export csv/shp for reference 
st_write(poiData_CI_2017_C, "HDBSCAN_2017_C_pt30_output1.shp")

#4.Film, TV, video, radio and photography --------------------------------------------------------------

  #subset the data so its just that industry
  poiData_CI_2017_FTVRP <- poiData_CI_2017import[poiData_CI_2017import$indst == "Film, TV, video, radio and photography",]
  
  #extract the xy data from the CI POIs
  CIcluster_FTVRP <- poiData_CI_2017_FTVRP[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_FTVRP) <- NULL
  
  #run the hdbscan for Architecture
  start.time <- Sys.time()
  CIclusters2017_FTVRP <- hdbscan(CIcluster_FTVRP, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_FTVRP
  
  #add cluster membership to the data
  poiData_CI_2017_FTVRP$cluster <- CIclusters2017_FTVRP$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_FTVRP$cluster <- as.character(poiData_CI_2017_FTVRP$cluster) 
  poiData_CI_2017_FTVRP$memberProb <- CIclusters2017_FTVRP$membership_prob
  poiData_CI_2017_FTVRP$outlierScore <- CIclusters2017_FTVRP$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_FTVRP) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_FTVRP_clusterTree.jpg", width = 350)
  plot(CIclusters2017_FTVRP, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # create histograms of characteristics 
      poiData_CI_2017_FTVRP_noGeom <- as.data.frame(poiData_CI_2017_FTVRP)
      poiData_CI_2017_FTVRP_noGeom <- subset(poiData_CI_2017_FTVRP_noGeom, select =-c(geometry))
      poiData_CI_2017_FTVRP_noGeom$cluster <- as.numeric(poiData_CI_2017_FTVRP_noGeom$cluster)
      
      #choose only points in clusters (i.e. not 0 as a cluster = noise point)
      poiData_CI_2017_FTVRP_noGeom_C <- poiData_CI_2017_FTVRP_noGeom[poiData_CI_2017_FTVRP_noGeom$cluster!=0, ] 
      
      FTVRP_histo<-ggplot(poiData_CI_2017_FTVRP_noGeom_C,aes(x=cluster)) + 
        geom_histogram(binwidth=0.5)
      FTVRP_histo1<-ggplot(poiData_CI_2017_FTVRP_noGeom_C,aes(x=memberProb)) + 
        geom_histogram()
      FTVRP_histo2<-ggplot(poiData_CI_2017_FTVRP_noGeom_C,aes(x=outlierScore)) + 
        geom_histogram()
      # save the plots
      ggsave("HDBSCAN_2017_FTVRP_clusterHisto.png", plot = FTVRP_histo)
      ggsave("HDBSCAN_2017_FTVRP_memberProbHisto.png", plot = FTVRP_histo1)
      ggsave("HDBSCAN_2017_FTVRP_outlierHisto.png", plot = FTVRP_histo2)
      
  # export csv/shp for reference 
  st_write(poiData_CI_2017_FTVRP, "HDBSCAN_2017_FTVRP_pt30_output1.shp")

#5.Publishing --------------------------------------------------------------
  
  #subset the data so its just that industry
  poiData_CI_2017_P <- poiData_CI_2017import[poiData_CI_2017import$indst == "Publishing",]
  
  #extract the xy data from the CI POIs
  CIcluster_P <- poiData_CI_2017_P[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_P) <- NULL
  
  #run the hdbscan 
  start.time <- Sys.time()
  CIclusters2017_P <- hdbscan(CIcluster_P, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results
  #-------------------------------------
  CIclusters2010_AM
  CIclusters2017_ISC
  
  max(CIclusters2017_ISC$cluster_scores)
  
  CIclusters2017_DPGF
  CIclusters2010_DPGF
  CIclusters2010_ISC$cluster_scores
  CIclusters2010_MGL
  plot(CIclusters2010_ISC, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  plot(CIclusters2017_ISC, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  
  #plot cluster stabilities
  stability <- as.data.frame(CIclusters2010_P$cluster_scores)
  stability$clusternum <- 1:nrow(stability)
  colnames(stability)<- c("clusterScore","cluster")

  p<-ggplot(data=stability, aes(x=cluster, y=clusterScore)) +
    geom_col()+
    geom_text(aes(label=cluster), vjust=1.6, color="white", size=3.5)
  
  # save the plots
  ggsave("HDBSCAN_2010_P_clusterStab.png", plot = p)
  
  #-------------------------------------
  #add cluster membership to the data
  poiData_CI_2017_P$cluster <- CIclusters2017_P$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_P$cluster <- as.character(poiData_CI_2017_P$cluster) 
  poiData_CI_2017_P$memberProb <- CIclusters2017_P$membership_prob
  poiData_CI_2017_P$outlierScore <- CIclusters2017_P$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_P) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_P_clusterTree.jpg", width = 350)
  plot(CIclusters2017_P, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()

  
  # create histograms of characteristics 
  poiData_CI_2017_P_noGeom <- as.data.frame(poiData_CI_2017_P)
  poiData_CI_2017_P_noGeom <- subset(poiData_CI_2017_P_noGeom, select =-c(geometry))
  poiData_CI_2017_P_noGeom$cluster <- as.numeric(poiData_CI_2017_P_noGeom$cluster)
  
  #choose only points in clusters (i.e. not 0 as a cluster = noise point)
  poiData_CI_2017_P_noGeom_C <- poiData_CI_2017_P_noGeom[poiData_CI_2017_P_noGeom$cluster!=0, ] 
  
  P_histo<-ggplot(poiData_CI_2017_P_noGeom_C,aes(x=cluster)) + 
    geom_histogram(binwidth=0.5)
  P_histo1<-ggplot(poiData_CI_2017_P_noGeom_C,aes(x=memberProb)) + 
    geom_histogram()
  P_histo2<-ggplot(poiData_CI_2017_P_noGeom_C,aes(x=outlierScore)) + 
    geom_histogram()
  # save the plots
  ggsave("HDBSCAN_2017_P_clusterHisto.png", plot = P_histo)
  ggsave("HDBSCAN_2017_P_memberProbHisto.png", plot = P_histo1)
  ggsave("HDBSCAN_2017_P_outlierHisto.png", plot = P_histo2)
  
  # export csv/shp for reference 
  st_write(poiData_CI_2017_P, "HDBSCAN_2017_P_pt30_output1.shp")
  
#6.Music, performing and visual arts --------------------------------------------------------------
  
  #subset the data so its just that industry
  poiData_CI_2017_MPVA <- poiData_CI_2017import[poiData_CI_2017import$indst == "Music, performing and visual arts",]
  
  #extract the xy data from the CI POIs
  CIcluster_MPVA <- poiData_CI_2017_MPVA[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_MPVA) <- NULL
  
  #run the hdbscan for Architecture
  start.time <- Sys.time()
  CIclusters2017_MPVA <- hdbscan(CIcluster_MPVA, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_MPVA
  
  #add cluster membership to the data
  poiData_CI_2017_MPVA$cluster <- CIclusters2017_MPVA$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_MPVA$cluster <- as.character(poiData_CI_2017_MPVA$cluster) 
  poiData_CI_2017_MPVA$memberProb <- CIclusters2017_MPVA$membership_prob
  poiData_CI_2017_MPVA$outlierScore <- CIclusters2017_MPVA$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_MPVA) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_MPVA_clusterTree.jpg", width = 350)
  plot(CIclusters2017_P, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # create histograms of characteristics 
  poiData_CI_2017_MPVA_noGeom <- as.data.frame(poiData_CI_2017_MPVA)
  poiData_CI_2017_MPVA_noGeom <- subset(poiData_CI_2017_MPVA_noGeom, select =-c(geometry))
  poiData_CI_2017_MPVA_noGeom$cluster <- as.numeric(poiData_CI_2017_MPVA_noGeom$cluster)
  
  #choose only points in clusters (i.e. not 0 as a cluster = noise point)
  poiData_CI_2017_MPVA_noGeom_C <- poiData_CI_2017_MPVA_noGeom[poiData_CI_2017_MPVA_noGeom$cluster!=0, ] 
  
  MPVA_histo<-ggplot(poiData_CI_2017_MPVA_noGeom_C,aes(x=cluster)) + 
    geom_histogram(binwidth=0.5)
  MPVA_histo1<-ggplot(poiData_CI_2017_MPVA_noGeom_C,aes(x=memberProb)) + 
    geom_histogram()
  MPVA_histo2<-ggplot(poiData_CI_2017_MPVA_noGeom_C,aes(x=outlierScore)) + 
    geom_histogram()
  # save the plots
  ggsave("HDBSCAN_2017_MPVA_clusterHisto.png", plot = MPVA_histo)
  ggsave("HDBSCAN_2017_MPVA_memberProbHisto.png", plot = MPVA_histo1)
  ggsave("HDBSCAN_2017_MPVA_outlierHisto.png", plot = MPVA_histo2)
  
  # export csv/shp for reference 
  st_write(poiData_CI_2017_MPVA, "HDBSCAN_2017_MPVA_pt30_output1.shp")
  

#8.Design: product, graphic and fashion design --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2017_DPGF <- poiData_CI_2017import[poiData_CI_2017import$indst == "Design: product, graphic and fashion design",]

#extract the xy data from the CI POIs
CIcluster_DPGF <- poiData_CI_2017_DPGF[,c(4,5)]
#remove geometry
st_geometry(CIcluster_DPGF) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2017_DPGF <- hdbscan(CIcluster_DPGF, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2017_DPGF

#add cluster membership to the data
poiData_CI_2017_DPGF$cluster <- CIclusters2017_DPGF$cluster
#set data type to factor for visualisation
poiData_CI_2017_DPGF$cluster <- as.character(poiData_CI_2017_DPGF$cluster) 
poiData_CI_2017_DPGF$memberProb <- CIclusters2017_DPGF$membership_prob
poiData_CI_2017_DPGF$outlierScore <- CIclusters2017_DPGF$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2017_DPGF) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2017_DPGF_clusterTree.jpg", width = 350)
plot(CIclusters2017_DPGF, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2017_DPGF_noGeom <- as.data.frame(poiData_CI_2017_DPGF)
poiData_CI_2017_DPGF_noGeom <- subset(poiData_CI_2017_DPGF_noGeom, select =-c(geometry))
poiData_CI_2017_DPGF_noGeom$cluster <- as.numeric(poiData_CI_2017_DPGF_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2017_DPGF_noGeom_C <- poiData_CI_2017_DPGF_noGeom[poiData_CI_2017_DPGF_noGeom$cluster!=0, ] 

DPGF_histo<-ggplot(poiData_CI_2017_DPGF_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
DPGF_histo1<-ggplot(poiData_CI_2017_DPGF_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
DPGF_histo2<-ggplot(poiData_CI_2017_DPGF_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2017_DPGF_clusterHisto.png", plot = DPGF_histo)
ggsave("HDBSCAN_2017_DPGF_memberProbHisto.png", plot = DPGF_histo1)
ggsave("HDBSCAN_2017_DPGF_outlierHisto.png", plot = DPGF_histo2)

# export csv/shp for reference 
st_write(poiData_CI_2017_DPGF, "HDBSCAN_2017_DPGF_pt30_output1.shp")

# 8.IT, software and computer services --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2017_ISC <- poiData_CI_2017import[poiData_CI_2017import$indst == "IT, software and computer services",]

#extract the xy data from the CI POIs
CIcluster_ISC <- poiData_CI_2017_ISC[,c(4,5)]
#remove geometry
st_geometry(CIcluster_ISC) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2017_ISC <- hdbscan(CIcluster_ISC, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2017_ISC

#add cluster membership to the data
poiData_CI_2017_ISC$cluster <- CIclusters2017_ISC$cluster
#set data type to factor for visualisation
poiData_CI_2017_ISC$cluster <- as.character(poiData_CI_2017_ISC$cluster) 
poiData_CI_2017_ISC$memberProb <- CIclusters2017_ISC$membership_prob
poiData_CI_2017_ISC$outlierScore <- CIclusters2017_ISC$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2017_ISC) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2017_ISC_clusterTree.jpg", width = 350)
plot(CIclusters2017_ISC, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2017_ISC_noGeom <- as.data.frame(poiData_CI_2017_ISC)
poiData_CI_2017_ISC_noGeom <- subset(poiData_CI_2017_ISC_noGeom, select =-c(geometry))
poiData_CI_2017_ISC_noGeom$cluster <- as.numeric(poiData_CI_2017_ISC_noGeom$cluster)
poiData_CI_2017_ISC_noGeom$cluster
#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2017_ISC_noGeom_C <- poiData_CI_2017_ISC_noGeom[poiData_CI_2017_ISC_noGeom$cluster!=0, ] 

ISC_histo<-ggplot(poiData_CI_2017_ISC_noGeom_C,aes(x=cluster))+ 
  geom_histogram()  
ISC_histo
ISC_histo1<-ggplot(poiData_CI_2017_ISC_noGeom_C,aes(x=memberProb))+ 
  geom_histogram()
ISC_histo2<-ggplot(poiData_CI_2017_ISC_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2017_ISC_clusterHisto.png", plot = ISC_histo)
ggsave("HDBSCAN_2017_ISC_memberProbHisto.png", plot = ISC_histo1)
ggsave("HDBSCAN_2017_ISC_outlierHisto.png", plot = ISC_histo2)
ISC_histo
# export csv/shp for reference 
st_write(poiData_CI_2017_ISC, "HDBSCAN_2017_ISC_pt30_output1.shp")


# 9. Museums, galleries and libraries --------------------------------------------------------------
  
  #subset the data so its just that industry
  poiData_CI_2017_MGL <- poiData_CI_2017import[poiData_CI_2017import$indst == "Museums, galleries and libraries",]
  
  #extract the xy data from the CI POIs
  CIcluster_MGL <- poiData_CI_2017_MGL[,c(4,5)]
  #remove geometry
  st_geometry(CIcluster_MGL) <- NULL
  
  #run the hdbscan for Architecture
  start.time <- Sys.time()
  CIclusters2017_MGL <- hdbscan(CIcluster_MGL, minPts = 30)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #see the results 
  CIclusters2017_MGL
  
  #add cluster membership to the data
  poiData_CI_2017_MGL$cluster <- CIclusters2017_MGL$cluster
  #set data type to factor for visualisation
  poiData_CI_2017_MGL$cluster <- as.character(poiData_CI_2017_MGL$cluster) 
  poiData_CI_2017_MGL$memberProb <- CIclusters2017_MGL$membership_prob
  poiData_CI_2017_MGL$outlierScore <- CIclusters2017_MGL$outlier_scores
  
  #visualising the clusters
  tmap_mode("view")
  tm_shape(poiData_CI_2017_MGL) +
    tm_dots("cluster")
  
  #Looking at the tree 
  jpeg("HDBSCAN_2017_MGL_clusterTree.jpg", width = 350)
  plot(CIclusters2017_ISC, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()
  
  # no point in histograms because there are no clusters 
  
# export csv/shp for reference 
st_write(poiData_CI_2017_MGL, "HDBSCAN_2017_MGL_pt30_output1.shp")



#-------------------------------------------------------------------------------------------
  # 2010 DATA
#-------------------------------------------------------------------------------------------

#import the CI cleaned POI from the shapefile
poiData_CI_2010import <- st_read("/Users/katie/Documents/UCL course/Dissertation/Coding/R/analysis","poiData_CI_2010")


# 2010 POI data, by industry ###############################

#1. Advertising and Marketing 

#subset the data so its just that industry
poiData_CI_2010_AM <- poiData_CI_2010import[poiData_CI_2010import$indst == "Advertising and marketing",]

#extract the xy data from the CI POIs
CIcluster_AM <- poiData_CI_2010_AM[,c(4,5)]
#remove geometry
st_geometry(CIcluster_AM) <- NULL

#run the hdbscan for Ad & Mark
start.time <- Sys.time()
CIclusters2010_AM <- hdbscan(CIcluster_AM, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_AM
#stability scores 
CIclusters2010_AM$cluster_scores

#add cluster info to the data
poiData_CI_2010_AM$cluster <- CIclusters2010_AM$cluster
#set data type to factor for visualisation
poiData_CI_2010_AM$cluster <- as.character(poiData_CI_2010_AM$cluster) 
poiData_CI_2010_AM$memberProb <- CIclusters2010_AM$membership_prob
poiData_CI_2010_AM$outlierScore <- CIclusters2010_AM$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_AM) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_AM_clusterTree_AM.jpg", width = 350)
plot(CIclusters2010_AM, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_AM_noGeom <- as.data.frame(poiData_CI_2010_AM)
poiData_CI_2010_AM_noGeom <- subset(poiData_CI_2010_AM_noGeom, select =-c(geometry))
poiData_CI_2010_AM_noGeom$cluster <- as.numeric(poiData_CI_2010_AM_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_AM_noGeom_C <- poiData_CI_2010_AM_noGeom[poiData_CI_2010_AM_noGeom$cluster!=0, ] 

AM_histo<-ggplot(poiData_CI_2010_AM_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
AM_histo1<-ggplot(poiData_CI_2010_AM_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
AM_histo2<-ggplot(poiData_CI_2010_AM_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_AM_clusterHisto.png", plot = AM_histo)
ggsave("HDBSCAN_2010_AM_memberProbHisto.png", plot = AM_histo1)
ggsave("HDBSCAN_2010_AM_outlierHisto.png", plot = AM_histo2)

#add stability scores for shp for visualisation
CIclusters2010_AM_stab <- as.data.frame(CIclusters2010_AM$cluster_scores)
CIclusters2010_AM_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_AM_stab)))
colnames(CIclusters2010_AM_stab) <- c("cluster_score","cluster")

poiData_CI_2010_AM_1<-left_join(poiData_CI_2010_AM, CIclusters2010_AM_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_AM_1, "HDBSCAN_2010_AM_pt30_output1.shp",delete_layer = T)

#2. Architecture --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_A <- poiData_CI_2010import[poiData_CI_2010import$indst == "Architecture",]

#extract the xy data from the CI POIs
CIcluster_A <- poiData_CI_2010_A[,c(4,5)]
#remove geometry
st_geometry(CIcluster_A) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_A <- hdbscan(CIcluster_A, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_A

#add cluster membership to the data
poiData_CI_2010_A$cluster <- CIclusters2010_A$cluster
#set data type to factor for visualisation
poiData_CI_2010_A$cluster <- as.character(poiData_CI_2010_A$cluster) 
poiData_CI_2010_A$memberProb <- CIclusters2010_A$membership_prob
poiData_CI_2010_A$outlierScore <- CIclusters2010_A$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2017_A) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_A_clusterTree.jpg", width = 350)
plot(CIclusters2010_A, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_A_noGeom <- as.data.frame(poiData_CI_2010_A)
poiData_CI_2010_A_noGeom <- subset(poiData_CI_2010_A_noGeom, select =-c(geometry))
poiData_CI_2010_A_noGeom$cluster <- as.numeric(poiData_CI_2010_A_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_A_noGeom_C <- poiData_CI_2010_A_noGeom[poiData_CI_2010_A_noGeom$cluster!=0, ] 

A_histo<-ggplot(poiData_CI_2010_A_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
A_histo1<-ggplot(poiData_CI_2010_A_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
A_histo2<-ggplot(poiData_CI_2010_A_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_A_clusterHisto.png", plot = A_histo)
ggsave("HDBSCAN_2010_A_memberProbHisto.png", plot = A_histo1)
ggsave("HDBSCAN_2010_A_outlierHisto.png", plot = A_histo2)

#add stability scores for shp for visualisation
CIclusters2010_A_stab <- as.data.frame(CIclusters2010_A$cluster_scores)
CIclusters2010_A_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_A_stab)))
colnames(CIclusters2010_A_stab) <- c("cluster_score","cluster")

poiData_CI_2010_A_1<-left_join(poiData_CI_2010_A, CIclusters2010_A_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_A_1, "HDBSCAN_2010_A_pt30_output1.shp",delete_layer = T)


#3. Crafts --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_C <- poiData_CI_2010import[poiData_CI_2010import$indst == "Crafts",]

#extract the xy data from the CI POIs
CIcluster_C <- poiData_CI_2010_C[,c(4,5)]
#remove geometry
st_geometry(CIcluster_C) <- NULL

#run the hdbscan 
start.time <- Sys.time()
CIclusters2010_C <- hdbscan(CIcluster_C, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_C

#add cluster membership to the data
poiData_CI_2010_C$cluster <- CIclusters2010_C$cluster
#set data type to factor for visualisation
poiData_CI_2010_C$cluster <- as.character(poiData_CI_2010_C$cluster) 
poiData_CI_2010_C$memberProb <- CIclusters2010_C$membership_prob
poiData_CI_2010_C$outlierScore <- CIclusters2010_C$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_C) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_C_clusterTree.jpg", width = 350)
plot(CIclusters2010_C, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_C_noGeom <- as.data.frame(poiData_CI_2010_C)
poiData_CI_2010_C_noGeom <- subset(poiData_CI_2010_C_noGeom, select =-c(geometry))
poiData_CI_2010_C_noGeom$cluster <- as.numeric(poiData_CI_2010_C_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_C_noGeom_C <- poiData_CI_2010_C_noGeom[poiData_CI_2010_C_noGeom$cluster!=0, ] 

C_histo<-ggplot(poiData_CI_2010_C_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
C_histo1<-ggplot(poiData_CI_2010_C_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
C_histo2<-ggplot(poiData_CI_2010_C_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_C_clusterHisto.png", plot = C_histo)
ggsave("HDBSCAN_2010_C_memberProbHisto.png", plot = C_histo1)
ggsave("HDBSCAN_2010_C_outlierHisto.png", plot = C_histo2)

#add stability scores for shp for visualisation
CIclusters2010_C_stab <- as.data.frame(CIclusters2010_C$cluster_scores)
CIclusters2010_C_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_C_stab)))
colnames(CIclusters2010_C_stab) <- c("cluster_score","cluster")

poiData_CI_2010_C_1<-left_join(poiData_CI_2010_C, CIclusters2010_C_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_C_1, "HDBSCAN_2010_C_pt30_output1.shp",delete_layer = T)

#4.Film, TV, video, radio and photography --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_FTVRP <- poiData_CI_2010import[poiData_CI_2010import$indst == "Film, TV, video, radio and photography",]

#extract the xy data from the CI POIs
CIcluster_FTVRP <- poiData_CI_2010_FTVRP[,c(4,5)]
#remove geometry
st_geometry(CIcluster_FTVRP) <- NULL

#run the hdbscan 
start.time <- Sys.time()
CIclusters2010_FTVRP <- hdbscan(CIcluster_FTVRP, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_FTVRP

#add cluster membership to the data
poiData_CI_2010_FTVRP$cluster <- CIclusters2010_FTVRP$cluster
#set data type to factor for visualisation
poiData_CI_2010_FTVRP$cluster <- as.character(poiData_CI_2010_FTVRP$cluster) 
poiData_CI_2010_FTVRP$memberProb <- CIclusters2010_FTVRP$membership_prob
poiData_CI_2010_FTVRP$outlierScore <- CIclusters2010_FTVRP$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_FTVRP) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_FTVRP_clusterTree.jpg", width = 350)
plot(CIclusters2010_FTVRP, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_FTVRP_noGeom <- as.data.frame(poiData_CI_2010_FTVRP)
poiData_CI_2010_FTVRP_noGeom <- subset(poiData_CI_2010_FTVRP_noGeom, select =-c(geometry))
poiData_CI_2010_FTVRP_noGeom$cluster <- as.numeric(poiData_CI_2010_FTVRP_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_FTVRP_noGeom_C <- poiData_CI_2010_FTVRP_noGeom[poiData_CI_2010_FTVRP_noGeom$cluster!=0, ] 

FTVRP_histo<-ggplot(poiData_CI_2010_FTVRP_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
FTVRP_histo1<-ggplot(poiData_CI_2010_FTVRP_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
FTVRP_histo2<-ggplot(poiData_CI_2010_FTVRP_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_FTVRP_clusterHisto.png", plot = FTVRP_histo)
ggsave("HDBSCAN_2010_FTVRP_memberProbHisto.png", plot = FTVRP_histo1)
ggsave("HDBSCAN_2010_FTVRP_outlierHisto.png", plot = FTVRP_histo2)

#add stability scores for shp for visualisation
CIclusters2010_FTVRP_stab <- as.data.frame(CIclusters2010_FTVRP$cluster_scores)
CIclusters2010_FTVRP_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_FTVRP_stab)))
colnames(CIclusters2010_FTVRP_stab) <- c("cluster_score","cluster")

poiData_CI_2010_FTVRP_1<-left_join(poiData_CI_2010_FTVRP, CIclusters2010_FTVRP_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_FTVRP_1, "HDBSCAN_2010_FTVRP_pt30_output1.shp",delete_layer = T)

#5.Publishing --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_P <- poiData_CI_2010import[poiData_CI_2010import$indst == "Publishing",]

#extract the xy data from the CI POIs
CIcluster_P <- poiData_CI_2010_P[,c(4,5)]
#remove geometry
st_geometry(CIcluster_P) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_P <- hdbscan(CIcluster_P, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_P

#add cluster membership to the data
poiData_CI_2010_P$cluster <- CIclusters2010_P$cluster
#set data type to factor for visualisation
poiData_CI_2010_P$cluster <- as.character(poiData_CI_2010_P$cluster) 
poiData_CI_2010_P$memberProb <- CIclusters2010_P$membership_prob
poiData_CI_2010_P$outlierScore <- CIclusters2010_P$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_P) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_P_clusterTree.jpg", width = 350)
plot(CIclusters2010_P, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_P_noGeom <- as.data.frame(poiData_CI_2010_P)
poiData_CI_2010_P_noGeom <- subset(poiData_CI_2010_P_noGeom, select =-c(geometry))
poiData_CI_2010_P_noGeom$cluster <- as.numeric(poiData_CI_2010_P_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_P_noGeom_C <- poiData_CI_2010_P_noGeom[poiData_CI_2010_P_noGeom$cluster!=0, ] 

P_histo<-ggplot(poiData_CI_2010_P_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
P_histo1<-ggplot(poiData_CI_2010_P_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
P_histo2<-ggplot(poiData_CI_2010_P_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_P_clusterHisto.png", plot = P_histo)
ggsave("HDBSCAN_2010_P_memberProbHisto.png", plot = P_histo1)
ggsave("HDBSCAN_2010_P_outlierHisto.png", plot = P_histo2)

#add stability scores for shp for visualisation
CIclusters2010_P_stab <- as.data.frame(CIclusters2010_P$cluster_scores)
CIclusters2010_P_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_P_stab)))
colnames(CIclusters2010_P_stab) <- c("cluster_score","cluster")

poiData_CI_2010_P_1<-left_join(poiData_CI_2010_P, CIclusters2010_P_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_P_1, "HDBSCAN_2010_P_pt30_output1.shp",delete_layer = T)

#6.Music, performing and visual arts --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_MPVA <- poiData_CI_2010import[poiData_CI_2010import$indst == "Music, performing and visual arts",]

#extract the xy data from the CI POIs
CIcluster_MPVA <- poiData_CI_2010_MPVA[,c(4,5)]
#remove geometry
st_geometry(CIcluster_MPVA) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_MPVA <- hdbscan(CIcluster_MPVA, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_MPVA

#add cluster membership to the data
poiData_CI_2010_MPVA$cluster <- CIclusters2010_MPVA$cluster
#set data type to factor for visualisation
poiData_CI_2010_MPVA$cluster <- as.character(poiData_CI_2010_MPVA$cluster) 
poiData_CI_2010_MPVA$memberProb <- CIclusters2010_MPVA$membership_prob
poiData_CI_2010_MPVA$outlierScore <- CIclusters2010_MPVA$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_MPVA) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_MPVA_clusterTree.jpg", width = 350)
plot(CIclusters2010_P, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_MPVA_noGeom <- as.data.frame(poiData_CI_2010_MPVA)
poiData_CI_2010_MPVA_noGeom <- subset(poiData_CI_2010_MPVA_noGeom, select =-c(geometry))
poiData_CI_2010_MPVA_noGeom$cluster <- as.numeric(poiData_CI_2010_MPVA_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_MPVA_noGeom_C <- poiData_CI_2010_MPVA_noGeom[poiData_CI_2010_MPVA_noGeom$cluster!=0, ] 

MPVA_histo<-ggplot(poiData_CI_2010_MPVA_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
MPVA_histo1<-ggplot(poiData_CI_2010_MPVA_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
MPVA_histo2<-ggplot(poiData_CI_2010_MPVA_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_MPVA_clusterHisto.png", plot = MPVA_histo)
ggsave("HDBSCAN_2010_MPVA_memberProbHisto.png", plot = MPVA_histo1)
ggsave("HDBSCAN_2010_MPVA_outlierHisto.png", plot = MPVA_histo2)

#add stability scores for shp for visualisation
CIclusters2010_MPVA_stab <- as.data.frame(CIclusters2010_MPVA$cluster_scores)
CIclusters2010_MPVA_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_MPVA_stab)))
colnames(CIclusters2010_MPVA_stab) <- c("cluster_score","cluster")

poiData_CI_2010_MPVA_1<-left_join(poiData_CI_2010_MPVA, CIclusters2010_MPVA_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_MPVA_1, "HDBSCAN_2010_MPVA_pt30_output1.shp", delete_layer = T)


#8.Design: product, graphic and fashion design --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_DPGF <- poiData_CI_2010import[poiData_CI_2010import$indst == "Design: product, graphic and fashion design",]

#extract the xy data from the CI POIs
CIcluster_DPGF <- poiData_CI_2010_DPGF[,c(4,5)]
#remove geometry
st_geometry(CIcluster_DPGF) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_DPGF <- hdbscan(CIcluster_DPGF, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_DPGF

#add cluster membership to the data
poiData_CI_2010_DPGF$cluster <- CIclusters2010_DPGF$cluster
#set data type to factor for visualisation
poiData_CI_2010_DPGF$cluster <- as.character(poiData_CI_2010_DPGF$cluster) 
poiData_CI_2010_DPGF$memberProb <- CIclusters2010_DPGF$membership_prob
poiData_CI_2010_DPGF$outlierScore <- CIclusters2010_DPGF$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_DPGF) +
  tm_dots("cluster")

#Looking at the tree 
  jpeg("HDBSCAN_2010_DPGF_clusterTree.jpg", width = 350)
  plot(CIclusters2010_DPGF, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
  dev.off()

# create histograms of characteristics 
poiData_CI_2010_DPGF_noGeom <- as.data.frame(poiData_CI_2010_DPGF)
poiData_CI_2010_DPGF_noGeom <- subset(poiData_CI_2010_DPGF_noGeom, select =-c(geometry))
poiData_CI_2010_DPGF_noGeom$cluster <- as.numeric(poiData_CI_2010_DPGF_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_DPGF_noGeom_C <- poiData_CI_2010_DPGF_noGeom[poiData_CI_2010_DPGF_noGeom$cluster!=0, ] 

DPGF_histo<-ggplot(poiData_CI_2010_DPGF_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
DPGF_histo1<-ggplot(poiData_CI_2010_DPGF_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
DPGF_histo2<-ggplot(poiData_CI_2010_DPGF_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_DPGF_clusterHisto.png", plot = DPGF_histo)
ggsave("HDBSCAN_2010_DPGF_memberProbHisto.png", plot = DPGF_histo1)
ggsave("HDBSCAN_2010_DPGF_outlierHisto.png", plot = DPGF_histo2)


#add stability scores for shp for visualisation
CIclusters2010_DPGF_stab <- as.data.frame(CIclusters2010_DPGF$cluster_scores)
CIclusters2010_DPGF_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_DPGF_stab)))
colnames(CIclusters2010_DPGF_stab) <- c("cluster_score","cluster")

poiData_CI_2010_DPGF_1<-left_join(poiData_CI_2010_DPGF, CIclusters2010_DPGF_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_DPGF_1, "HDBSCAN_2010_DPGF_pt30_output1.shp", delete_layer = T)

# 8.IT, software and computer services --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_ISC <- poiData_CI_2010import[poiData_CI_2010import$indst == "IT, software and computer services",]

#extract the xy data from the CI POIs
CIcluster_ISC <- poiData_CI_2010_ISC[,c(4,5)]
#remove geometry
st_geometry(CIcluster_ISC) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_ISC <- hdbscan(CIcluster_ISC, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_ISC

#add cluster membership to the data
poiData_CI_2010_ISC$cluster <- CIclusters2010_ISC$cluster
#set data type to factor for visualisation
poiData_CI_2010_ISC$cluster <- as.character(poiData_CI_2010_ISC$cluster) 
poiData_CI_2010_ISC$memberProb <- CIclusters2010_ISC$membership_prob
poiData_CI_2010_ISC$outlierScore <- CIclusters2010_ISC$outlier_scores

CIclusters2010_ISC$cluster_scores
#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_ISC) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_ISC_clusterTree.jpg", width = 350)
plot(CIclusters2010_ISC, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# create histograms of characteristics 
poiData_CI_2010_ISC_noGeom <- as.data.frame(poiData_CI_2010_ISC)
poiData_CI_2010_ISC_noGeom <- subset(poiData_CI_2010_ISC_noGeom, select =-c(geometry))
poiData_CI_2010_ISC_noGeom$cluster <- as.numeric(poiData_CI_2010_ISC_noGeom$cluster)

#choose only points in clusters (i.e. not 0 as a cluster = noise point)
poiData_CI_2010_ISC_noGeom_C <- poiData_CI_2010_ISC_noGeom[poiData_CI_2010_ISC_noGeom$cluster!=0, ] 

ISC_histo<-ggplot(poiData_CI_2010_ISC_noGeom_C,aes(x=cluster)) + 
  geom_histogram(binwidth=0.5)
ISC_histo1<-ggplot(poiData_CI_2010_ISC_noGeom_C,aes(x=memberProb)) + 
  geom_histogram()
ISC_histo2<-ggplot(poiData_CI_2010_ISC_noGeom_C,aes(x=outlierScore)) + 
  geom_histogram()
# save the plots
ggsave("HDBSCAN_2010_ISC_clusterHisto.png", plot = ISC_histo)
ggsave("HDBSCAN_2010_ISC_memberProbHisto.png", plot = ISC_histo1)
ggsave("HDBSCAN_2010_ISC_outlierHisto.png", plot = ISC_histo2)

#add stability scores for shp for visualisation
CIclusters2010_ISC_stab <- as.data.frame(CIclusters2010_ISC$cluster_scores)
CIclusters2010_ISC_stab$cluster <- as.character(seq.int(nrow(CIclusters2010_ISC_stab)))
colnames(CIclusters2010_ISC_stab) <- c("cluster_score","cluster")

poiData_CI_2010_ISC_1<-left_join(poiData_CI_2010_ISC, CIclusters2010_ISC_stab, by="cluster")

# export csv/shp for reference 
st_write(poiData_CI_2010_ISC_1, "HDBSCAN_2010_ISC_pt30_output1.shp", delete_layer = TRUE)


# 9. Museums, galleries and libraries --------------------------------------------------------------

#subset the data so its just that industry
poiData_CI_2010_MGL <- poiData_CI_2010import[poiData_CI_2010import$indst == "Museums, galleries and libraries",]

#extract the xy data from the CI POIs
CIcluster_MGL <- poiData_CI_2010_MGL[,c(4,5)]
#remove geometry
st_geometry(CIcluster_MGL) <- NULL

#run the hdbscan for Architecture
start.time <- Sys.time()
CIclusters2010_MGL <- hdbscan(CIcluster_MGL, minPts = 30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#see the results 
CIclusters2010_MGL

#add cluster membership to the data
poiData_CI_2010_MGL$cluster <- CIclusters2010_MGL$cluster
#set data type to factor for visualisation
poiData_CI_2010_MGL$cluster <- as.character(poiData_CI_2010_MGL$cluster) 
poiData_CI_2010_MGL$memberProb <- CIclusters2010_MGL$membership_prob
poiData_CI_2010_MGL$outlierScore <- CIclusters2010_MGL$outlier_scores

#visualising the clusters
tmap_mode("view")
tm_shape(poiData_CI_2010_MGL) +
  tm_dots("cluster")

#Looking at the tree 
jpeg("HDBSCAN_2010_MGL_clusterTree.jpg", width = 350)
plot(CIclusters2010_MGL, gradient = c("purple", "blue", "green", "yellow"), show_flat = T)
dev.off()

# no point in histograms because there are no clusters 

# export csv/shp for reference 
st_write(poiData_CI_2010_MGL, "HDBSCAN_2010_MGL_pt30_output1.shp")

#--------------------------------------------------------------------------------------
# Analyse cluster stability scores 
#--------------------------------------------------------------------------------------

#stability scores - compare years 
clusterScores_AM <- as.data.frame(cbind(CIclusters2017_AM$cluster_scores,CIclusters2010_AM$cluster_scores))

clusterScore_histo<-ggplot(clusterScores2017,aes(x=CIclusters2010_AM$cluster_scores)) + 
  geom_histogram()
clusterScore_histo
CIclusters2010_A$cluster_scores

