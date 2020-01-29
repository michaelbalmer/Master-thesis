##read street shapefile
streets_shp<-readOGR(dsn = "./streets_network_data",
                    layer = "street_data_rail_tourist", stringsAsFactors = F)

##turn street IDs into num
streets_shp$ORIG_FID <- as.numeric(streets_shp$ORIG_FID)

##create list of nodes and edges
sfNEL<-readshpnw(streets_shp, ELComputed=TRUE, Detailed=F)

#Create graph network and add the edge length as the weight for graph and attributes
igr<-nel2igraph(sfNEL[[2]],sfNEL[[3]],weight=sfNEL[[4]], eadf = sfNEL[[5]])

##betweenness centrality
V(igr)$betweenness <- betweenness(igr, v = V(igr), directed = F, weights = NULL,
                           nobigint = TRUE, normalized = F)
##Edge betweenness centrality
E(igr)$edge_between <- edge_betweenness(igr, e = E(igr), directed = F,
                 weights = NULL)
##Closeness centrality
V(igr)$closeness <- closeness(igr, vids = V(igr), mode = c("out", "in", "all",
                                                           "total"), weights = NULL, normalized = F)
##degree centrality (number of adjecent edges for each node)
V(igr)$degree_centr <- degree(igr, v = V(igr), mode = c("all", "out", "in", "total"),
       loops = TRUE, normalized = FALSE)
##Eigenvector centrality
V(igr)$eigen_centr <- eigen_centrality(igr, directed = FALSE, scale = TRUE,
                                       weights = NULL)$vector
##Alpha centrality
V(igr)$alpha_centr <- alpha_centrality(igr, nodes = V(igr), alpha = 1, loops = FALSE,
                 exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE)

##Page rank centrality
V(igr)$page_rank <- page_rank(igr, algo = c("prpack", "arpack", "power"),
          vids = V(igr), directed = TRUE, damping = 0.85,
          personalized = NULL, weights = NULL, options = NULL)$vector

##convert graph to df
igr_df <- as_long_data_frame(igr)

##only include occurences with ORIG_FID>0 (parking segments)
igr_park_seg <- igr_df[!is.na(igr_df$road_seg),]
igr_park_seg <- igr_park_seg[!igr_park_seg$EdgeID==15019,]


##create a dataframe with road_segment ID, edge_id, and vertices
dist_df <- data.frame(igr_park_seg$ORIG_FID, igr_park_seg$road_seg , igr_park_seg$EdgeID,
                      igr_park_seg$from, igr_park_seg$to)
colnames(dist_df) <- c("ORIG_FID","road_seg","EdgeID","from","to")
##copy for rail and tourist distances
rail_dist_df <- dist_df
tourist_dist_df <- dist_df

##edge tails of tourist and rail 
tourist_edge_tail <- igr_df$from[!igr_df$tourist_id==0]
rail_edge_tail <- igr_df$from[!is.na(igr_df$rail)]


count=0
##iterate through all tourist edge tails and compute distances to park segments
for (i in tourist_edge_tail){
  distance <- distances(igr, v= igr_park_seg$from,
                           to = i, mode="all")
  count=count+1
  tourist_dist_df[,count+5] <- distance
  colnames(tourist_dist_df)[count+5] <- paste("Dis_Ed",i,sep = "_")
}

count=0
##iterate through all rail edge tails and compute distances to park segments
for (i in rail_edge_tail){
  distance <- distances(igr, v= igr_park_seg$from,
                        to = i, mode="all")
  count=count+1
  rail_dist_df[,count+5] <- distance
  colnames(rail_dist_df)[count+5] <- paste("Dis_Ed",i,sep = "_")
}