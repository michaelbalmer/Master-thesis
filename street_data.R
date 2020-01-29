##load street data with street names, addresses, coordinates for start and end points
##assign for each (relevant) street segment X&Y for start and end point of road
##create coordinate points for missing street segments (some 30)

#read street data
street_data <- read.csv("street_points.csv", header = TRUE)

#drop unnecessary columns
drops <- c("accepted","active","classcode","date_date_","time_date_","date_date1","time_date1","date_date2","time_date2","f_node_cnn","gds_chg_id","gds_chg__1","gds_chg__2","jurisdicti","layer",
           "street_na1","streetna_1","street_gc","t_node_cnn","zip_code")
street_data <- street_data[ , !(names(street_data) %in% drops)]

## create new columns from/to address
street_data$address <- NA

# set all 0 values for address left/right to NA
street_data[c("rt_fadd", "rt_toadd","lf_fadd","lf_toadd")][(street_data[c("rt_fadd", "rt_toadd","lf_fadd","lf_toadd")]==0)] <- NA


addmin <- apply(street_data[c("rt_fadd", "rt_toadd","lf_fadd","lf_toadd")], 1, min, na.rm=TRUE)
addmax <- apply(street_data[c("rt_fadd", "rt_toadd","lf_fadd","lf_toadd")], 1, max, na.rm=TRUE)

street_data$addmin <- addmin
street_data$addmax <- addmax

street_data[seq(1, nrow(street_data), 2),"address"] <- 
  street_data[seq(1, nrow(street_data), 2),"addmin"]
street_data[seq(2, nrow(street_data), 2),"address"] <- 
  street_data[seq(2, nrow(street_data), 2),"addmax"]

drops2 <- c("addmin","addmax")
street_data <- street_data[ , !(names(street_data) %in% drops2)]

###########

##create data with all unique street segments from parking occupancy data
all_segments <- data[!duplicated(data$STREET_BLOCK), ]

## drop all unnecessary cols
undrops <- c("BLOCK_ID","STREET_NAME","BLOCK_NUM","STREET_BLOCK","PM_DISTRICT_NAME")
all_segments <- all_segments[ , (names(all_segments) %in% undrops)]


## save unique streets from street data
## street data with only streets left occurring in occupancy data
street_data <- street_data[(street_data$streetname) %in% all_segments$STREET_NAME, ]


## remove streets without address
street_data <- street_data[!is.infinite(street_data$address), ]

## only keep addresses ending in 00,01,99
street_data <- street_data[(street_data$address %% 100 == 0) | (street_data$address %% 100 == 1) | (street_data$address %% 100 == 99), ]

street_data <- street_data[ , !(names(street_data) %in% c("add_beg"))]

##create STREET_BLOCK column
street_data$STREET_BLOCK <- paste (street_data$streetname,((street_data$address %/% 100)*100), sep = " ", collapse = NULL)

street_data$block_beg <- NA

##determine beginning or end of block
street_data$block_beg <- ifelse((street_data$address) %% 100 == 1 | (street_data$address) %% 100 == 0 , T, F)



##merge all_segments & street_data
all_segments_copy <- merge(all_segments, street_data, 
                           by.x = "STREET_BLOCK", by.y = "STREET_BLOCK" )
all_segments_copy <- all_segments_copy[ , (names(all_segments_copy) %in% 
                                             c("BLOCK_ID","STREET_NAME","BLOCK_NUM",
                                               "STREET_BLOCK","PM_DISTRICT_NAME",
                                               "address","block_beg","POINT_X","POINT_Y"))]

all_segments_true <- all_segments_copy[all_segments_copy$block_beg == T,]
all_segments_false <- all_segments_copy[all_segments_copy$block_beg == F,]

##merge true and false
all_segments_merge <- merge(all_segments_true, all_segments_false, 
                            by.x = "STREET_BLOCK", by.y = "STREET_BLOCK" )

##drop redundant cols
all_segments_merge <- all_segments_merge[ , (names(all_segments_merge) %in% 
                                               c("BLOCK_ID.x","STREET_BLOCK","STREET_NAME.x",
                                                 "BLOCK_NUM.x","PM_DISTRICT_NAME.x",
                                                 "address.x","address.y","POINT_X.x","POINT_Y.x",
                                                 "POINT_X.y","POINT_Y.y"))]

##rename cols
names(all_segments_merge) <- c("STREET_BLOCK","BLOCK_ID","STREET_NAME",
                               "BLOCK_NUM","PM_DISTRICT_NAME","POINT_X_1","POINT_Y_1",
                               "ADDRESS_FROM","POINT_X_2","POINT_Y_2","ADDRESS_TO"
)

##drop duplicates
all_segments_merge <- all_segments_merge[!duplicated(all_segments_merge$STREET_BLOCK), ]

##bind STREET_BLOCK cols of all_segments_merge & all_segments
try <- all_segments_merge[1]
try2 <- all_segments[4]
all_segs2 <- rbind(try,try2)

segs_dupl <- all_segs2 %>% 
  group_by(all_segs2$STREET_BLOCK) %>%
  summarise(no_rows = length(STREET_BLOCK))%>%
  print(n=1000)

##missing streets
segs_dupl <- segs_dupl[segs_dupl$no_rows==1,]
segs_dupl <- segs_dupl[,1]
segs_dupl <- segs_dupl[-1,]
names(segs_dupl) <- c("STREET_BLOCK")
segs_dupl$POINT_X_1 <- NA
segs_dupl$POINT_Y_1 <- NA
segs_dupl$POINT_X_2 <- NA
segs_dupl$POINT_Y_2 <- NA

## manually add missing street coordinates of street blocks
segs_dupl[1,2:5] <- c(-122.388422,37.784762,-122.3898, 37.78361)
segs_dupl[2,2:5] <- c(-122.3896, 37.78622,-122.390149, 37.785940)
segs_dupl[3,2:5] <- c(-122.391259, 37.784943,-122.3936, 37.78308)
segs_dupl[4,2:3] <- c(-122.435005, 37.789104)
segs_dupl[5,2:5] <- c(-122.398013, 37.795397,-122.3991, 37.79516)
segs_dupl[6,2:5] <- c(-122.417204, 37.805217,-122.4188, 37.80624)
segs_dupl[7,2:5] <- c(-122.412628, 37.808843, -122.413857, 37.808887)
segs_dupl[8,2:5] <- c(-122.413857, 37.808887 , -122.414746, 37.808917)
segs_dupl[9,2:5] <- c(-122.414746, 37.808917,-122.415844, 37.808974)
segs_dupl[10,2:5] <- c(-122.394298, 37.794916, -122.392618, 37.793716)
segs_dupl[11,2:5] <- c(-122.391232 ,37.792362, -122.390155, 37.790748)
segs_dupl[12,2:5] <- c(-122.390155, 37.790748, -122.388608, 37.789479)
segs_dupl[13,2:5] <- c(-122.388608, 37.789479, -122.388010, 37.787109)
segs_dupl[14,2:5] <- c(-122.388010, 37.787109, -122.388209, 37.784767)
segs_dupl[15,2:5] <- c(-122.388209, 37.784767, -122.388441, 37.781814)
segs_dupl[16,2:5] <- c(-122.390161, 37.790745, -122.390994, 37.790101)
segs_dupl[17,2:5] <- c(-122.4217, 37.77896, -122.421923, 37.779900)
segs_dupl[18,2:5] <- c(-122.454608, 37.802197, -122.454878, 37.801036)
segs_dupl[19,2:5] <- c(-122.412550, 37.808614,-122.414192, 37.808435)
segs_dupl[20,2:5] <- c(-122.414192, 37.808435, -122.415821, 37.808243)
segs_dupl[21,2:5] <- c(-122.415821, 37.808243, -122.4174, 37.80801)
segs_dupl[22,2:5] <- c(-122.400853, 37.771148,-122.402349, 37.769933)
segs_dupl[23,2:5] <- c(-122.418171, 37.777999,-122.419749, 37.777784)
segs_dupl[24,2:5] <- c(-122.4180, 37.77751, -122.418259, 37.778498)
segs_dupl[25,2:5] <- c(-122.418259, 37.778498,-122.418622, 37.780287)
segs_dupl[26,2:5] <- c(-122.418622, 37.780287,-122.418830, 37.781252)
segs_dupl[27,2:5] <- c(-122.3977, 37.78634,-122.398189, 37.786019)
segs_dupl[28,2:5] <- c(-122.4199, 37.77825, -122.420263, 37.780092)
segs_dupl[29,2:5] <- c(-122.420263, 37.780092, -122.420463, 37.781043)
segs_dupl[30,2:5] <- c(-122.467017, 37.739735,-122.4683, 37.74002)
segs_dupl[31,2:5] <- c(-122.3982, 37.79619, -122.400535, 37.795890)
segs_dupl[32,2:5] <- c(-122.400535, 37.795890,-122.401686, 37.795734)
segs_dupl[33,2:5] <- c(-122.4670, 37.73972, -122.467983, 37.738910)
segs_dupl[34,2:5] <- c(-122.467983, 37.738910 ,-122.4691, 37.73797)
segs_dupl <- segs_dupl[-4,]
##create df with all street blocks and coords
all_segs_coords <- all_segments_merge[ , (names(all_segments_merge) %in% 
                                            c("STREET_BLOCK","POINT_X_1","POINT_Y_1",
                                              "POINT_X_2","POINT_Y_2"))]
all_segs_coords <- rbind(all_segs_coords, segs_dupl)

all_segs_coords2 <- inner_join(all_segs_coords, dataAMJ, by="STREET_BLOCK")

df_merged <- inner_join(data1, data2, by = 'KEY')

#write csv file with all street names and coordinates

#write.csv(all_segs_coords, file = "all_segs_coords.csv")