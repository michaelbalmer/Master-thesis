##read residential index data
res_index_100 <- read.csv("./geographic_data/residential_index/resunits_servicearea100.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
res_index_200 <- read.csv("./geographic_data/residential_index/resunits_servicearea200.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
res_index_300 <- read.csv("./geographic_data/residential_index/resunits_servicearea300.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
res_index_400 <- read.csv("./geographic_data/residential_index/resunits_servicearea400.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
res_index_500 <- read.csv("./geographic_data/residential_index/resunits_servicearea500.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")


##read office index data
office_index_100 <- read.csv("./geographic_data/office_index/office_servicearea100.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
office_index_200 <- read.csv("./geographic_data/office_index/office_servicearea200.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
office_index_300 <- read.csv("./geographic_data/office_index/office_servicearea300.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
office_index_400 <- read.csv("./geographic_data/office_index/office_servicearea400.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")
office_index_500 <- read.csv("./geographic_data/office_index/office_servicearea500.csv", 
                          header = TRUE, stringsAsFactors = F, sep=";")

##read industrial index data
industrial_index_100 <- read.csv("./geographic_data/industrial_index/industrial_servicearea100.csv", 
                             header = TRUE, stringsAsFactors = F, sep=";")
industrial_index_200 <- read.csv("./geographic_data/industrial_index/industrial_servicearea200.csv", 
                             header = TRUE, stringsAsFactors = F, sep=";")
industrial_index_300 <- read.csv("./geographic_data/industrial_index/industrial_servicearea300.csv", 
                             header = TRUE, stringsAsFactors = F, sep=";")
industrial_index_400 <- read.csv("./geographic_data/industrial_index/industrial_servicearea400.csv", 
                             header = TRUE, stringsAsFactors = F, sep=";")
industrial_index_500 <- read.csv("./geographic_data/industrial_index/industrial_servicearea500.csv", 
                             header = TRUE, stringsAsFactors = F, sep=";")

##read restaurant data
restaurants_100 <- read.csv("./geographic_data/restaurants/rest_service_area100.csv", 
                                 header = TRUE, stringsAsFactors = F, sep=";")
restaurants_200 <- read.csv("./geographic_data/restaurants/rest_service_area200.csv", 
                                 header = TRUE, stringsAsFactors = F, sep=";")
restaurants_300 <- read.csv("./geographic_data/restaurants/rest_service_area300.csv", 
                                 header = TRUE, stringsAsFactors = F, sep=";")
restaurants_400 <- read.csv("./geographic_data/restaurants/rest_service_area400.csv", 
                                 header = TRUE, stringsAsFactors = F, sep=";")
restaurants_500 <- read.csv("./geographic_data/restaurants/rest_service_area500.csv", 
                                 header = TRUE, stringsAsFactors = F, sep=";")


##read business data
businesses_100 <- read.csv("./geographic_data/businesses/busi_service_area100.csv", 
                            header = TRUE, stringsAsFactors = F, sep=";")
businesses_200 <- read.csv("./geographic_data/businesses/busi_service_area200.csv", 
                            header = TRUE, stringsAsFactors = F, sep=";")
businesses_300 <- read.csv("./geographic_data/businesses/busi_service_area300.csv", 
                            header = TRUE, stringsAsFactors = F, sep=";")
businesses_400 <- read.csv("./geographic_data/businesses/busi_service_area400.csv", 
                            header = TRUE, stringsAsFactors = F, sep=";")
businesses_500 <- read.csv("./geographic_data/businesses/busi_service_area500.csv", 
                            header = TRUE, stringsAsFactors = F, sep=";")


##residential_index
res_index <- join_all(list(res_index_500,res_index_400,res_index_300,res_index_200,res_index_100), 
               by = 'ORIG_FID')
res_index <- res_index[,-c(1,4,6,8,10)]
names(res_index) <- c("ORIG_FID","resunits_500","resunits_400","resunits_300","resunits_200",
                      "resunits_100")

##office_index
office_index <- join_all(list(office_index_500[,c("ORIG_FID","area_sqft")],office_index_400[,c("ORIG_FID","area_sqft")]
                              ,office_index_300[,c("ORIG_FID","area_sqft")],office_index_200[,c("ORIG_FID","area_sqft")]
                              ,office_index_100[,c("ORIG_FID","area_sqft")]), by = 'ORIG_FID')
names(office_index) <- c("ORIG_FID","office_sqft_500","office_sqft_400","office_sqft_300",
                      "office_sqft_200","office_sqft_100")

##industrial index
industrial_index <- join_all(list(industrial_index_500[,c("ORIG_FID","area_sqft")],
                                  industrial_index_400[,c("ORIG_FID","area_sqft")],
                                  industrial_index_300[,c("ORIG_FID","area_sqft")],
                                  industrial_index_200[,c("ORIG_FID","area_sqft")],
                                  industrial_index_100[,c("ORIG_FID","area_sqft")]), by = 'ORIG_FID')
names(industrial_index) <- c("ORIG_FID","industrial_sqft_500","industrial_sqft400",
                             "industrial_sqft300","industrial_sqft200","industrial_sqft100")

##restaurant index
restaurant_index <- join_all(list(restaurants_500[,c("ORIG_FID","Join_Count")],
                                  restaurants_400[,c("ORIG_FID","Join_Count")],
                                  restaurants_300[,c("ORIG_FID","Join_Count")],
                                  restaurants_200[,c("ORIG_FID","Join_Count")],
                                  restaurants_100[,c("ORIG_FID","Join_Count")]), by = 'ORIG_FID')
names(restaurant_index) <- c("ORIG_FID","restaurant_count_500","restaurant_count_400",
                             "restaurant_count_300","restaurant_count_200",
                             "restaurant_count_100")

##business index
businesses_index <- join_all(list(businesses_500[,c("ORIG_FID","Join_Count")],
                                  businesses_400[,c("ORIG_FID","Join_Count")],
                                  businesses_300[,c("ORIG_FID","Join_Count")],
                                  businesses_200[,c("ORIG_FID","Join_Count")],
                                  businesses_100[,c("ORIG_FID","Join_Count")]), by = 'ORIG_FID')
names(businesses_index) <- c("ORIG_FID","businesses_count_500","businesses_count_400",
                             "businesses_count_300","businesses_count_200",
                             "businesses_count_100")


##create min & mean values for tourist distance
mean_tourist_dist <- apply(tourist_dist_df[,6:length(tourist_dist_df[1,])], 1, FUN=mean)
min_tourist_dist <- apply(tourist_dist_df[,6:length(tourist_dist_df[1,])], 1, FUN=min)

tourist_dist_df$mean_dist <- mean_tourist_dist
tourist_dist_df$min_dist <- min_tourist_dist


##create min & mean values for railway distance
mean_rail_dist <- apply(rail_dist_df[,6:length(rail_dist_df[1,])], 1, FUN=mean)
min_rail_dist <- apply(rail_dist_df[,6:length(rail_dist_df[1,])], 1, FUN=min)

rail_dist_df$mean_dist <- mean_rail_dist
rail_dist_df$min_dist <- min_rail_dist


##aggregate centrality indices
centrality_indices <- igr_park_seg[,(names(igr_park_seg) %in% c("ORIG_FID","road_seg",
                                                                "edge_between","from_betweenness",
                                                                "from_closeness","from_degree_centr",
                                                                "from_eigen_centr","from_alpha_centr",
                                                                "to_betweenness","to_closeness",
                                                                "to_degree_centr","to_eigen_centr",
                                                                "to_alpha_centr","from_page_rank",
                                                                "to_page_rank"))]

##compute mean values of vertice centrality indices
mean_betweenness <- apply(centrality_indices[,c("from_betweenness","to_betweenness")], 1, FUN=mean)
mean_closeness <- apply(centrality_indices[,c("from_closeness","to_closeness")], 1, FUN=mean)
mean_degree_centr <- apply(centrality_indices[,c("from_degree_centr","to_degree_centr")], 1, FUN=mean)
mean_eigen_centr <- apply(centrality_indices[,c("from_eigen_centr","to_eigen_centr")], 1, FUN=mean)
mean_alpha_centr <- apply(centrality_indices[,c("from_alpha_centr","to_alpha_centr")], 1, FUN=mean)
mean_page_rank <-apply(centrality_indices[,c("from_page_rank","to_page_rank")], 1, FUN=mean)


centrality_indices[,c("mean_betweenness","mean_closeness","mean_degree","mean_eigen",
                      "mean_alpha", "mean_page")] <- c(mean_betweenness,mean_closeness,mean_degree_centr,
                                          mean_eigen_centr,mean_alpha_centr,mean_page_rank)

centrality_indices <- centrality_indices[,(names(centrality_indices) %in% c("ORIG_FID","road_seg",
                                                                "edge_between","mean_betweenness",
                                                                "mean_closeness","mean_degree",
                                                                "mean_eigen","mean_alpha",
                                                                "mean_page"))]

##join all data into one dataframe
all_geo_data <- centrality_indices
all_geo_data <- join_all(list(all_geo_data,rail_dist_df[,c(1,2,length(rail_dist_df[1,])-1,length(rail_dist_df[1,]))],
                              tourist_dist_df[,c(1,2,length(tourist_dist_df[1,])-1,length(tourist_dist_df[1,]))],
                              res_index,office_index,industrial_index,restaurant_index,businesses_index),
                              by = 'ORIG_FID')

all_geo_data <- all_geo_data[,-c(10,13)]

all_geo_data <- rename(all_geo_data, c("mean_dist" = "rail_mean_dist", "min_dist" = "rail_min_dist",
                       "mean_dist.1" = "tourist_mean_dist", "min_dist.1" = "tourist_min_dist"))

##turn NAs into 0 values
all_geo_data[is.na(all_geo_data)] = 0


##normalize geo data (values from 0 to 1)
all_geo_data[,3:length(all_geo_data[1,])] <- normalize(all_geo_data[,3:length(all_geo_data[1,])], 
                          method = "range", range = c(0, 1))

##write file
write.csv(all_geo_data, file = "./geographic_data/all_geo_data.csv")