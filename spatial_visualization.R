##all streets blocks with block id and name
##add mean occ rate and park capacity
streets_coords <- dataAMJ %>%
  dplyr::group_by(STREET_BLOCK, BLOCK_ID,PM_DISTRICT_NAME) %>%
  dplyr::summarise(mean(OCC_RATE),mean(PARK_CAPACITY))


#############
#merge with coordinates

streets_coords <- merge(streets_coords,all_segs_coords,by='STREET_BLOCK')
streets_coords <- streets_coords[,-6]

##merge with MAE values
##RF 1h & 5h prediction horizon for FS1 & 8
streets_coords <- merge(streets_coords,street_seg_error_rf[,c('BLOCK_ID','mae1hfs1',
                                                              'mae1hfs8','mae5hfs1',
                                                              'mae5hfs8')],
                        by='BLOCK_ID')

##percentage improvement from FS1 - FS8
streets_coords$mae1h_per_diff_rf <- streets_coords$mae1hfs1 - 
  streets_coords$mae1hfs8
streets_coords$mae1h_per_diff_rf <- streets_coords$mae1h_per_diff/streets_coords$mae1hfs1*100

streets_coords$mae5h_per_diff_rf <- streets_coords$mae5hfs1 - 
  streets_coords$mae5hfs8
streets_coords$mae5h_per_diff_rf <- streets_coords$mae5h_per_diff/streets_coords$mae5hfs1*100


colnames(streets_coords)[c(10:13)] <- c("mae1hfs1_rf",'mae1hfs8_rf','mae5hfs1_rf',
                                              'mae5hfs8_rf')

colnames(streets_coords)[4:5] <- c('mean_occ_rate','park_capacity')



##duplicate each row
streets_coords <- streets_coords[rep(seq_len(nrow(streets_coords)), 
                                               each = 2), ]
##build new columns (group, id)
streets_coords$id <- c(1:nrow(streets_coords))
streets_coords$group <- rep(1:((nrow(streets_coords))/2), each=2)

##build new columns (POINT_X, POINT_Y)
streets_coords$POINT_X <- streets_coords$POINT_X_1
streets_coords$POINT_Y <- streets_coords$POINT_Y_1

streets_coords[seq(2, nrow(streets_coords), 2),'POINT_X' ] <- NA
streets_coords[seq(2, nrow(streets_coords), 2),'POINT_Y' ] <- NA

for (i in 1:nrow(streets_coords)){
  if (is.na(streets_coords[i,'POINT_X'])){
    streets_coords[i,'POINT_X'] <- streets_coords[i,'POINT_X_2']
    streets_coords[i,'POINT_Y'] <- streets_coords[i,'POINT_Y_2']
  }
}

streets_coords <- streets_coords[,-c(6:9)]

rownames(streets_coords) <- streets_coords$id

###remove outlier streets
streets_coords <- streets_coords[!streets_coords$BLOCK_ID %in% c(83000,83002,46002,41321),]

##remove West Portal
streets_coords <- streets_coords[!streets_coords$PM_DISTRICT_NAME == 'West Portal',]

######################################################################

##load shp files service area & POIs

##points
business_poi = readOGR(dsn="./service_area_poi_output/", layer="business_poi")
touristic_poi = readOGR(dsn="./service_area_poi_output/", layer="touristic_poi")
public_transport_poi = readOGR(dsn="./service_area_poi_output/", layer="public_transport_poi")

##polygons
resident_landuse = readOGR(dsn="./service_area_poi_output/", layer="resident_landuse")
office_landuse = readOGR(dsn="./service_area_poi_output/", layer="office_landuse")
industrial_landuse = readOGR(dsn="./service_area_poi_output/", layer="industrial_landuse")
hayesst300_servicearea = readOGR(dsn="./service_area_poi_output/", layer="hayesst300_servicearea")
all_industrial_civic = readOGR(dsn="./service_area_poi_output/", layer="all_industrial_civic")
all_office_civic = readOGR(dsn="./service_area_poi_output/", layer="all_office_civic")
all_resident_civic = readOGR(dsn="./service_area_poi_output/", layer="all_resident_civic")

##lines
segment_hayesst300 = readOGR(dsn="./service_area_poi_output/", layer="segment_hayesst300")

##points as df
business_poi <- as.data.frame(business_poi)
touristic_poi <- as.data.frame(touristic_poi)
public_transport_poi <- as.data.frame(public_transport_poi)

##fortify polygons / lines
resident_landuse <- fortify(resident_landuse)
office_landuse <- fortify(office_landuse)
industrial_landuse <- fortify(industrial_landuse)
hayesst300_servicearea <- fortify(hayesst300_servicearea)
all_industrial_civic <- fortify(all_industrial_civic)
all_office_civic <- fortify(all_office_civic)
all_resident_civic <- fortify(all_resident_civic)

segment_hayesst300 <- fortify(segment_hayesst300)

resident_landuse$group <- 'Residential'
office_landuse$group <- 'Office'
industrial_landuse$group <- 'Industrial'
resident_landuse$id <- as.numeric(resident_landuse$id)
office_landuse$id <- as.numeric(office_landuse$id)+(1 + max(resident_landuse$id))
industrial_landuse$id <- as.numeric(industrial_landuse$id)+(1 + max(office_landuse$id))


all_industrial_civic$group <- 'Industrial'
all_office_civic$group <- 'Office'
all_resident_civic$group <- 'Residential'
all_industrial_civic$id <- as.numeric(all_industrial_civic$id)
all_office_civic$id <- as.numeric(all_office_civic$id)+(1+max(all_industrial_civic$id))
all_resident_civic$id <- as.numeric(all_resident_civic$id)+(1+max(all_office_civic$id))

landuse_polygons <- rbind(resident_landuse,office_landuse,industrial_landuse)
landuse_polygons_civic <- rbind(all_resident_civic,all_office_civic,all_industrial_civic)


business_poi <- business_poi[,c(1,18,17)]
touristic_poi <- touristic_poi[,c(1:3)]
public_transport_poi <- public_transport_poi[,c(1,5,6)]
business_poi$poi <- 'Business'
touristic_poi$poi <- 'Touristic'
public_transport_poi$poi <- 'Public Transport'
names(business_poi) <- c('id','long','lat','poi')
names(touristic_poi) <- c('id','long','lat','poi')
names(public_transport_poi) <- c('id','long','lat','poi')

poi_points <- rbind(business_poi,touristic_poi,public_transport_poi)
touristic_pt_points <- rbind(touristic_poi,public_transport_poi)


#############################################################

##all streets (centrality index)

all_streets_centr <- igr_df

##duplicate each row
all_streets_centr <- all_streets_centr[rep(seq_len(nrow(all_streets_centr)), 
                                     each = 2), ]

##build new columns (POINT_X, POINT_Y)
all_streets_centr$POINT_X <- all_streets_centr$from_x
all_streets_centr$POINT_Y <- all_streets_centr$from_y

all_streets_centr[seq(2, nrow(all_streets_centr), 2),'POINT_X' ] <- NA
all_streets_centr[seq(2, nrow(all_streets_centr), 2),'POINT_Y' ] <- NA

for (i in 1:nrow(all_streets_centr)){
  if (is.na(all_streets_centr[i,'POINT_X'])){
    all_streets_centr[i,'POINT_X'] <- all_streets_centr[i,'to_x']
    all_streets_centr[i,'POINT_Y'] <- all_streets_centr[i,'to_y']
  }
}

##get rid of redundant cols
all_streets_centr <- all_streets_centr[ , (names(all_streets_centr) %in% 
                         c('EdgeID','edge_between',
                           'from_alpha_centr','to_alpha_centr',
                           'from_closeness','to_closeness','POINT_X',
                           'POINT_Y'))]
all_streets_centr$closeness <- 
  rowMeans(all_streets_centr[c('from_closeness', 'to_closeness')], na.rm=TRUE)
all_streets_centr$alpha <- 
  rowMeans(all_streets_centr[c('from_alpha_centr', 'to_alpha_centr')], na.rm=TRUE)

all_streets_centr <- all_streets_centr[ , (names(all_streets_centr) %in% 
                                             c('EdgeID','edge_between',
                                               'closeness','alpha','POINT_X',
                                               'POINT_Y'))]

####################################
#centrality maps

##remove outliers of closeness
all_streets_centr <- all_streets_centr[!all_streets_centr$closeness<2e-6,]
##set threshold for alpha
all_streets_centr[all_streets_centr$alpha>1.01,'alpha'] <- 1.01
##set threshold for edge_between
all_streets_centr[all_streets_centr$edge_between>3000000,'edge_between'] <- 3000000

##edge betweenness map
edge_between_map <- ggmap(map_sf,darken = c(.8, "white"))+
  geom_path(data = all_streets_centr, aes(x = POINT_X, y = POINT_Y, 
                group = EdgeID ,color=(edge_between)^(1/3)),size=.3)+
  scale_color_gradient(low="blue",
                       high="red")+
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

##closeness map
closeness_map <- ggmap(map_sf,darken = c(0.8, "white"))+
  geom_path(data = all_streets_centr, aes(x = POINT_X, y = POINT_Y, 
                                          group = EdgeID ,color=(closeness)^(1/1)),size=.3)+
  scale_color_gradient(low="blue",
                       high="red")+
  theme(legend.position = "none",axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


#alpha map
alpha_map <- ggmap(map_sf,darken = c(0.8, "white"))+
  geom_path(data = all_streets_centr, aes(x = POINT_X, y = POINT_Y, 
                                          group = EdgeID ,color=(alpha)^(1/1)),size=.3)+
  scale_color_gradient(low="blue",
                       high="red")+
  theme(legend.position = "none",axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


legend_color_map <- ggplot()+
  geom_path(data = all_streets_centr, aes(x = POINT_X, y = POINT_Y, 
                                          group = EdgeID ,color=(alpha)^(1/1)),size=.3)+
  scale_color_gradient(low="blue",
                       high="red",limits=c(1, 1.01),breaks = c(1,1.01),
                       labels = c('low','high'))+
  theme(legend.position = "right",axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(vjust=-0.5,hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text=element_text(size=15))


legend_map <- ggmap(map_sf,darken = c(1, "white"))+
  labs(x = "", y = "")+
  theme(legend.position = c(1,1),axis.text = element_blank(),
        axis.ticks = element_blank(),panel.border = element_blank())+
  scalebar(dist = 2, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.055,border.size = .5,height = 0.015,
           x.min=-122.52057,x.max = -122.35,y.min =37.7051,y.max= 37.81895,
           anchor = c(x=-122.46,y=37.8))

##north arrow
north <- north2(legend_map,x = 0.3, y = 0.3, scale = 0.35, symbol = 3)

###arrange four plots into one

plot_grid(
  alpha_map, edge_between_map,closeness_map,legend_map,align = "hv", ncol = 2,nrow = 2,
  labels = c('(a) Alpha', '(b) Betweenness', '(c) Closeness'),label_size = 12,
  label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -0.5,label_fontface = 'plain'
)

#####################################################

##Google API key
#register_google(key='insert key here')

### Get a map

#map SF
map <- get_map(c(-122.475,37.75,-122.38,37.815) ,zoom=13,
               maptype = 'terrain', source = "stamen")

#map examplary service area
map_service_area <- get_map(c(-122.4284,37.7727,-122.4151,37.7816) ,zoom=16,
                            maptype = 'terrain', source = "stamen")

##Civic Center map
civic_center_map <- get_map(c(-122.4275,37.7745,-122.4125,37.7835) ,zoom=16,
                            maptype = 'terrain', source = "stamen")

##Fishermans Wharf map
fishermans_wharf_map <- get_map(c(-122.4270,37.8030,-122.4093,37.8128) ,zoom=16,
                                maptype = 'terrain', source = "stamen")


### map with MAE RF
###RF 1h ahead prediction / FS8
map_1h_fs8 <- ggmap(map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae1hfs8_rf),
                                                        size=1.4)+
  labs(x = "", y = "",color='MAE')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(0.04, 0.16), 
                       breaks =seq(0.04,0.16,0.03),expand=c(0,0))+
  theme(legend.position = c(0.09,0.21),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = .5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))

###RF 5h ahead prediction / FS8
map_5h_fs8 <- ggmap(map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae5hfs8_rf),
            size=1.4)+
  labs(x = "", y = "",color='MAE')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(0.06, 0.3), 
                       breaks =seq(0.06,0.3,0.06),expand=c(0,0))+
  theme(legend.position = c(0.09,0.21),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = .5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))

### map with percentage differences
###1h ahead
map_1h_diff <- ggmap(map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae1h_per_diff_rf),
                                                        size=1.4)+
  labs(x = "", y = "",color='Improvement \n(% MAE)')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(-10, 32), 
                       breaks =seq(-10,32,10),expand=c(0,0))+
  theme(legend.position = c(0.13,0.23),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = .5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))


### map with percentage differences
###5h ahead
map_5h_diff <- ggmap(map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae5h_per_diff_rf),
            size=1.4)+
  labs(x = "", y = "",color='Improvement \n(% MAE)')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(-15, 55), 
                       breaks =seq(-15,55,15),expand=c(0,0))+
  theme(legend.position = c(0.13,0.23),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = 0.5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))


##MAE map of 1 and 5h ahead prediction
plot_grid(
  map_1h_fs8, map_5h_fs8,align = "hv", ncol = 2,
  labels = c('(a) 1 step ahead', '(b) 5 step ahead'),
  label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -8,label_fontface = 'plain'
)

##MAE improvement map of 1 and 5h ahead prediction
plot_grid(
  map_1h_diff, map_5h_diff,align = "hv", ncol = 2,
  labels = c('(a) 1 step ahead', '(b) 5 step ahead'),
  label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -8,label_fontface = 'plain'
)


####histogram with improvements (1h ahead & 5h ahead)

melted_streets_coords <- melt(streets_coords[,c('BLOCK_ID','mae1h_per_diff_rf','mae5h_per_diff_rf')],
     id='BLOCK_ID')

toDelete <- seq(1, nrow(melted_streets_coords), 2)
melted_streets_coords <- melted_streets_coords[ toDelete ,]

####histogram with improvements (1h ahead & 5h ahead)
average_vals <- ddply(melted_streets_coords, "variable", summarise, grp.mean=mean(value))

hist_dist_improvement<-ggplot(melted_streets_coords, aes(x=value, fill=variable,
                                                         color=variable)) +
  geom_histogram(position="identity", alpha=0.4,binwidth=1)+
  theme_bw()+
  scale_fill_manual('Prediction horizon (steps)',values=c('#e41a1c','#377eb8'),
                    labels=c("1", "5"))+
  scale_colour_manual('Prediction horizon (steps)',values=c('#e41a1c','#377eb8'),
                      labels=c("1", "5"))+
  scale_y_continuous(limits=c(0,35),breaks=seq(0,35,5),expand = c(0,0))+
  scale_x_continuous(limits=c(-20,60),breaks=seq(-20,60,10),expand=c(0,0))+
  labs(x = "Performance Improvement (% MAE)", y = "Count")+
  geom_vline(data=average_vals, aes(xintercept=grp.mean, color=variable),
             linetype="dashed")+
  theme(axis.text.x = element_text(size=10),
        legend.position = c(0.87,0.88),
        legend.box.background = element_rect(colour = "black"),
        plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        legend.text=element_text(size=8),
        legend.key.size = unit(.6,"cm"),
        legend.spacing.y = unit(0.5, 'cm'))


###map with district locations
ggmap(map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", 
            aes(x = POINT_X, y = POINT_Y, group = group,color=PM_DISTRICT_NAME),size=1.3)+
  theme_bw()+
  labs(x = "", y = "",color='District')+
  scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a',
                               '#984ea3','#ff7f00','#2F4F4F',
                               '#a65628','#f781bf','#999999'))+
  theme(legend.position = c(0.13,0.22),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.direction = 'vertical',
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        legend.key.size = unit(1, 'lines'))+
  guides(colour = guide_legend(title.position = "top"))+
  scalebar(dist = .5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))



################################################################################

##Civic Center Map ##5h ahead MAE
map_civic_center_mae <- ggmap(civic_center_map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                  group = group,color=mae5hfs8_rf),size=2.5)+
  geom_point(data=public_transport_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=15,color='#e31a1c')+
  geom_point(data=touristic_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=17,color='#e31a1c')+
  geom_point(data=business_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=1.5,shape=20,color="black")+
  labs(x = "", y = "",color='MAE')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(0.06, 0.3), 
                       breaks =seq(0.06,0.3,0.06),expand=c(0,0))+
  theme(legend.position = c(0.09,0.76),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = 100, transform = TRUE, model = 'WGS84',dist_unit = 'm',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.4275,x.max = -122.4125,y.min =37.7745,y.max= 37.7835,
           anchor = c(x=-122.4139,y=37.7754))


##Civic Center Map Improvement ##5h ahead improvement
map_civic_center_impr <- ggmap(civic_center_map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae5h_per_diff_rf),size=2.5)+
  geom_point(data=public_transport_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=15,color='#e31a1c')+
  geom_point(data=touristic_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=17,color='#e31a1c')+
  geom_point(data=business_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=1.5,shape=20,color="black")+
  labs(x = "", y = "",color='Performance\nimprovement\n(% MAE)')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(-15, 55), 
                       breaks =seq(-15,55,15),expand=c(0,0))+
  theme(legend.position = c(0.125,0.71),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = 100, transform = TRUE, model = 'WGS84',dist_unit = 'm',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.4275,x.max = -122.4125,y.min =37.7745,y.max= 37.7835,
           anchor = c(x=-122.4139,y=37.7754))


###5h ahead
plot_grid(
  map_civic_center_mae, map_civic_center_impr,align = "hv", ncol = 2,
  labels = c('(a) Performance feature set 8', '(b) Relative performance improvement'),
  label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -9,label_fontface = 'plain'
)



####inset map
###civic center
ggmap(map,darken = c(0.3, "white"))+
  ##Service Area
  geom_rect(aes(xmin=-122.4275,xmax = -122.4125,ymin =37.7745,ymax= 37.7835),color='black',
            fill=NA,size=1.5)+
  ##all SF
  geom_rect(aes(xmin=-122.475,xmax=-122.38,ymin=37.75,ymax=37.815),color='black',
            fill=NA,size=2.5)+
  labs(x = "", y = "",color='')+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


##################################################################################

##Fisherman's Wharf Map ##5h ahead MAE
map_fishermans_wharf_mae <- ggmap(fishermans_wharf_map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae5hfs8_rf),size=2.5)+
  geom_point(data=public_transport_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=15,color='#e31a1c')+
  geom_point(data=touristic_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=17,color='#e31a1c')+
  geom_point(data=business_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=1.5,shape=20,color="black")+
  labs(x = "", y = "",color='MAE')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(0.07, 0.2), 
                       breaks =seq(0.08,0.2,0.04),expand=c(0,0))+
  theme(legend.position = c(0.09,0.745),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = 150, transform = TRUE, model = 'WGS84',dist_unit = 'm',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.4270,x.max = -122.4093,y.min =37.8030,y.max= 37.8128,
           anchor = c(x=-122.411,y=37.804))


##Fishermans Wharf Map Improvement ##5h ahead improvement
map_fishermans_wharf_impr <- ggmap(fishermans_wharf_map,darken = c(0.6, "white"))+
  geom_path(data = streets_coords,lineend="round", aes(x = POINT_X, y = POINT_Y, 
                                                       group = group,color=mae5h_per_diff_rf),size=2.5)+
  geom_point(data=public_transport_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=15,color='#e31a1c')+
  geom_point(data=touristic_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=3,shape=17,color='#e31a1c')+
  geom_point(data=business_poi,aes(x = as.numeric(long), y = as.numeric(lat))
             ,size=1.5,shape=20,color="black")+
  labs(x = "", y = "",color='Performance\nimprovement\n(% MAE)')+
  scale_color_gradient(low="blue",
                       high="red",limits = c(-5, 50.1), 
                       breaks =seq(-5,50,10),expand=c(0,0))+
  theme(legend.position = c(0.125,0.695),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  scalebar(dist = 150, transform = TRUE, model = 'WGS84',dist_unit = 'm',
         box.fill = c("black", "white"), st.color = "black",st.size = 4,
         st.dist=0.03,border.size = .5,height = 0.015,
         x.min=-122.4270,x.max = -122.4093,y.min =37.8030,y.max= 37.8128,
         anchor = c(x=-122.411,y=37.804))

###5h ahead
plot_grid(
  map_fishermans_wharf_mae, map_fishermans_wharf_impr,align = "hv", ncol = 2,
  labels = c('(a) Performance feature set 8', '(b) Relative performance improvement'),
  label_size = 12,label_x = 0.5, label_y = 0,
  hjust = 0.5, vjust = -10,label_fontface = 'plain'
)


####inset map
##fisherman's wharf
ggmap(map,darken = c(0.3, "white"))+
  ##Service Area
  geom_rect(aes(xmin=-122.4270,xmax = -122.4093,ymin =37.8030,ymax= 37.8128),color='black',
            fill=NA,size=1.5)+
  ##all SF
  geom_rect(aes(xmin=-122.475,xmax=-122.38,ymin=37.75,ymax=37.815),color='black',
            fill=NA,size=2.5)+
  labs(x = "", y = "",color='')+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


#######################Map of Service area and inset

##Hayes ST 300 with service area & land use
ggmap(map_service_area,darken = c(0.6, "white"))+
  geom_path(data = segment_hayesst300, aes(x = long, y = lat,group=id),color='black',
            size=2.5)+
  geom_polygon(data = landuse_polygons, aes(x = long, y = lat, 
                      group = id,fill=group),colour='black')+
  geom_polygon(data = hayesst300_servicearea, aes(x = long, y = lat),
               colour='#565656',fill='',size=1)+
  labs(x = "", y = "",fill='Land Use')+
  scale_fill_manual(values=c('#e41a1c','#377eb8','#4daf4a'))+
  theme(legend.position = c(0.09,0.88),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key.size = unit(.7, "cm"),
        legend.title = element_text(size=12),
        legend.spacing.y = unit(0.4, 'cm'))+
  scalebar(dist = 100, transform = TRUE, model = 'WGS84',dist_unit = 'm',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.42854,x.max = -122.4151,y.min =37.7727,y.max= 37.7816,
           anchor = c(x=-122.416,y=37.7737))


###inset map service area
ggmap(map,darken = c(0.3, "white"))+
  ##service area
  geom_rect(aes(xmin=-122.4284,xmax=-122.4151,ymin=37.7727,ymax=37.7816),color='black',
            fill=NA,size=1.5)+
  ##all SF
  geom_rect(aes(xmin=-122.475,xmax=-122.38,ymin=37.75,ymax=37.815),color='black',
            fill=NA,size=2.5)+
  labs(x = "", y = "",color='')+
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


#######################Map of POIs

ggmap(map,darken = c(0.3, "white"))+
  geom_point(data=touristic_pt_points,aes(x = long, y = lat, 
                                colour=poi),size=2.5,shape=15)+
  theme_bw()+
  labs(x = "", y = "",colour='POI')+
  scale_colour_manual(values=c('#e41a1c','#377eb8'))+
  theme(legend.position = c(0.105,0.91),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.key.size = unit(.7, "cm"),
        legend.title = element_text(size=12,hjust = 0.08),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'))+
  scalebar(dist = .5, transform = TRUE, model = 'WGS84',dist_unit = 'km',
           box.fill = c("black", "white"), st.color = "black",st.size = 4,
           st.dist=0.03,border.size = .5,height = 0.015,
           x.min=-122.475,x.max = -122.38,y.min =37.75,y.max= 37.815,
           anchor = c(x=-122.385,y=37.755))


# ggplot(streets_coords, aes(park_capacity,mae5hfs8_rf))+
#   geom_point(size=.7)+
#   theme_bw()+
#   #scale_colour_manual(values=c('#808080','#0000FF','#ff0000','#008000','#252525'))+
#   labs(x = "Parking capacity", y = "MAE") +
#   scale_x_continuous(limits=c(0,70),breaks = seq(0, 70, 10),expand = c(0,0)) +
#   scale_y_continuous(limits=c(0.05,0.3),breaks=seq(0.05,0.3,0.05),expand = c(0,0))+
#   theme(plot.title = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

#################################################################

