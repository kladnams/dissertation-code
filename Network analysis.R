#Script to compute the betweenness centrality of the London road network

library(igraph)
require(rgeos)
require(rgdal)
require(ggplot2)
require(sp)
require(broom)
require(dplyr)

# step 1: read in and get network 

## Import the files 
roadFile_network = "/Users/katie/Documents/UCL course/Dissertation/Data/Street Network_Elsa/London_edgelist_clean.csv"
coordsFile = "/Users/katie/Documents/UCL course/Dissertation/Data/Street Network_Elsa/london_coordinates_clean.csv"
roadsData = read.csv(roadFile_network,header=TRUE)
nodeCoords = read.csv(coordsFile,header=TRUE)

nodeCoords$start_point <- nodeCoords$id
roadsData<- left_join(roadsData, nodeCoords, by = "start_point", copy = FALSE)

#check for self loops 
roadsData[roadsData$start_point==roadsData$end_point,]
#there are self loops - get rid of them
roadsData_nl<-roadsData[!(roadsData$start_point==roadsData$end_point),]
roadsData_nl[roadsData_nl$start_point==roadsData_nl$end_point,] # it worked 

## Create an plot the graph 
roadGraph = graph.data.frame(roadsData_nl,directed=FALSE)
V(roadGraph)$name[1:5] 

#check no of clusters
clusters(roadGraph)$no

#attach coordinate information
V(roadGraph)$x = roadsData[match(as.numeric(V(roadGraph)$name),roadsData$start_point),6]
V(roadGraph)$y = roadsData[match(as.numeric(V(roadGraph)$name),roadsData$start_point),7]

#plot not working because not all x and y coordinates in the graph have been assigned using the coordinate data
sum(is.na(V(roadGraph)$y))
sum(is.na(V(roadGraph)$x))

#delete the vertices with no coordinates (seems to be a small proportion)
roadGraph2 = delete.vertices(roadGraph, V(roadGraph)[is.na(V(roadGraph)$y== TRUE)])
roadGraph2 = delete.vertices(roadGraph, V(roadGraph)[is.na(V(roadGraph)$x== TRUE)])

#check it worked 
sum(is.na(V(roadGraph2)$x))
sum(V(roadGraph))

#plot to see if it looks okay - it looks like London! (not percolation london)
plot(roadGraph2,vertex.label=NA,vertex.size=.01)

#compute node betweenness centrality (easier to aggregate by LSOA than edge centrality)
bet_centralitiesNodes = estimate_betweenness (roadGraph2,vids = V(roadGraph2),cutoff=450)
#calc starts @ 10.46, ended at 12.50

#write it out to a csv file
write.csv(bet_centralitiesNodes, file = "londonRoads_betCentralitiesNodes.csv")

#normalise the values - the bet values are numeric vectors
normalised_betNodes=(bet_centralitiesNodes-min(bet_centralitiesNodes))/(max(bet_centralitiesNodes)-min(bet_centralitiesNodes))

normalised_betNodes_df<-cbind(read.table(text=names(normalised_betNodes)), normalised_betNodes)
normalised_betNodes_df$id <- normalised_betNodes_df$V1
normalised_betNodes_df[1:5,]


#match the values to the roadsData file file with xy coords so it be mapped
roadsData_betValues <- left_join(roadsData_nl, normalised_betNodes_df, by="id", copy = FALSE)

#export to csv so can read in QGIS - does it look right?
write.csv(roadsData_betValues, file = "roadsData_betNodeValues.csv")

#THOUGHTS FROM LOOKING ON QGIS ##############
#looks okay but a lot of very low values..
#remove nodes with a degree of degree 2 or less so it just looks at junctions?
#identifies roads with congestion but not really on a neighboorhood scale.. 
#still seems to show variation correctly by LSOA...
#


#create a new file with betweeness values aggregated by LSOA 
LSOAroadsData_betValues <- aggregate(roadsData_betValues$normalised_betNodes, list(roadsData_betValues$LSOA11CD),mean)
#change the column names 
colnames(LSOAroadsData_betValues) <-c("LSOA", "AveBet")
#write out the csv file 
write.csv(LSOAroadsData_betValues, file = "LSOAroadsData_betValues.csv")



#------------------------------------------------
#let us now compute closeness centrality
#closeness1 =closeness.estimate(roadGraph2,cutoff=1000)

#compute edge betweenness centrality 
#bet_centralities = estimate_edge_betweenness (roadGraph2,e = E(roadGraph2),cutoff=1000)
#try with a lower cutoff threshold
#bet_centralities2 = estimate_edge_betweenness (roadGraph2,e = E(roadGraph2),cutoff=450)


#took hours to compute, so save the output in a csv:
#write.csv(bet_centralities, file = "londonRoads_betCentralities.csv")
write.csv(bet_centralities2, file = "londonRoads_betCentralities2.csv")



#plot the betweenness centralities

#get the colours and the weights
palette_edges=hsv(h=1-((bet_centralities/max(bet_centralities)*2/3)+1/3),s = 1,v=1)
width_lines=6

plot(roadGraph2,vertex.size=.1,vertex.label=NA,edge.width=bet_centralities/max(bet_centralities)*width_lines,edge.color=palette_edges)

#######
#other type of visualisation
#let us define some nice colours for our nodes in the map
getHSVColors=function(values,s=1,v=1){
  values=(values[values!=-1])
  vnorm=(values-min(values))/(max(values)-min(values))
  h=-2/3*vnorm+2/3
  colors=values
  colors[values!=-1]= hsv(h,s,v)
  colors[values==-1]= hsv(0,0,1)
  return (colors)
}

plot(roadGraph2,vertex.size=0.1,edge.color=getHSVColors(bet_centralities),vertex.label=NA)

## weight the graph by distance
#E(roadGraph)$weight=roadsData$length

