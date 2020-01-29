##mean error values (averaged over all training dataset sizes)
##one hour ahead prediction

##RF
##MAE
mean_mae_fs1 <- run_featureset_two[[1]]$mean_mae
mean_mae_fs2 <- run_featureset_four[[1]]$mean_mae
mean_mae_fs3 <- run_featureset_six[[1]]$mean_mae
mean_mae_fs4 <- run_featureset_eight[[1]]$mean_mae
mean_mae_fs5 <- run_featureset_eleven[[1]]$mean_mae
mean_mae_fs6 <- run_featureset_twelve[[1]]$mean_mae
mean_mae_fs7 <- run_featureset_thirteen[[1]]$mean_mae
mean_mae_fs8 <- run_featureset_ten[[1]]$mean_mae

for (i in 2:10){
  mean_mae_fs1 <- cbind.data.frame(mean_mae_fs1,run_featureset_two[[i]]$mean_mae)
  mean_mae_fs2 <- cbind.data.frame(mean_mae_fs2,run_featureset_four[[i]]$mean_mae)
  mean_mae_fs3 <- cbind.data.frame(mean_mae_fs3,run_featureset_six[[i]]$mean_mae)
  mean_mae_fs4 <- cbind.data.frame(mean_mae_fs4,run_featureset_eight[[i]]$mean_mae)
  mean_mae_fs5 <- cbind.data.frame(mean_mae_fs5,run_featureset_eleven[[i]]$mean_mae)
  mean_mae_fs6 <- cbind.data.frame(mean_mae_fs6,run_featureset_twelve[[i]]$mean_mae)
  mean_mae_fs7 <- cbind.data.frame(mean_mae_fs7,run_featureset_thirteen[[i]]$mean_mae)
  mean_mae_fs8 <- cbind.data.frame(mean_mae_fs8,run_featureset_ten[[i]]$mean_mae)
}

mean_mae_fs1 <- rowMeans(mean_mae_fs1)
mean_mae_fs2 <- rowMeans(mean_mae_fs2)
mean_mae_fs3 <- rowMeans(mean_mae_fs3)
mean_mae_fs4 <- rowMeans(mean_mae_fs4)
mean_mae_fs5 <- rowMeans(mean_mae_fs5)
mean_mae_fs6 <- rowMeans(mean_mae_fs6)
mean_mae_fs7 <- rowMeans(mean_mae_fs7)
mean_mae_fs8 <- rowMeans(mean_mae_fs8)


mean_mae_rf <- cbind.data.frame(c(1:10),mean_mae_fs1,mean_mae_fs2,mean_mae_fs3,
                              mean_mae_fs4,mean_mae_fs5,mean_mae_fs6,mean_mae_fs7,
                              mean_mae_fs8)

names(mean_mae_rf) <- c('horizon','1','2','3','4','5','6','7','8')

typeof(mean_mae_rf[5,5])

##########################################################

##MAE
mean_mse_fs1 <- run_featureset_two[[1]]$mean_mse
mean_mse_fs2 <- run_featureset_four[[1]]$mean_mse
mean_mse_fs3 <- run_featureset_six[[1]]$mean_mse
mean_mse_fs4 <- run_featureset_eight[[1]]$mean_mse
mean_mse_fs5 <- run_featureset_eleven[[1]]$mean_mse
mean_mse_fs6 <- run_featureset_twelve[[1]]$mean_mse
mean_mse_fs7 <- run_featureset_thirteen[[1]]$mean_mse
mean_mse_fs8 <- run_featureset_ten[[1]]$mean_mse


for (i in 2:10){
  mean_mse_fs1 <- cbind.data.frame(mean_mse_fs1,run_featureset_two[[i]]$mean_mse)
  mean_mse_fs2 <- cbind.data.frame(mean_mse_fs2,run_featureset_four[[i]]$mean_mse)
  mean_mse_fs3 <- cbind.data.frame(mean_mse_fs3,run_featureset_six[[i]]$mean_mse)
  mean_mse_fs4 <- cbind.data.frame(mean_mse_fs4,run_featureset_eight[[i]]$mean_mse)
  mean_mse_fs5 <- cbind.data.frame(mean_mse_fs5,run_featureset_eleven[[i]]$mean_mse)
  mean_mse_fs6 <- cbind.data.frame(mean_mse_fs6,run_featureset_twelve[[i]]$mean_mse)
  mean_mse_fs7 <- cbind.data.frame(mean_mse_fs7,run_featureset_thirteen[[i]]$mean_mse)
  mean_mse_fs8 <- cbind.data.frame(mean_mse_fs8,run_featureset_ten[[i]]$mean_mse)
}

mean_mse_fs1 <- rowMeans(mean_mse_fs1)
mean_mse_fs2 <- rowMeans(mean_mse_fs2)
mean_mse_fs3 <- rowMeans(mean_mse_fs3)
mean_mse_fs4 <- rowMeans(mean_mse_fs4)
mean_mse_fs5 <- rowMeans(mean_mse_fs5)
mean_mse_fs6 <- rowMeans(mean_mse_fs6)
mean_mse_fs7 <- rowMeans(mean_mse_fs7)
mean_mse_fs8 <- rowMeans(mean_mse_fs8)


mean_mse_rf <- cbind.data.frame(c(1:10),mean_mse_fs1,mean_mse_fs2,mean_mse_fs3,
                                mean_mse_fs4,mean_mse_fs5,mean_mse_fs6,mean_mse_fs7,
                                mean_mse_fs8)

names(mean_mse_rf) <- c('horizon','1','2','3','4','5','6','7','8')


##########################################################

##R2
mean_rsq_fs1 <- run_featureset_two[[1]]$mean_rsq
mean_rsq_fs2 <- run_featureset_four[[1]]$mean_rsq
mean_rsq_fs3 <- run_featureset_six[[1]]$mean_rsq
mean_rsq_fs4 <- run_featureset_eight[[1]]$mean_rsq
mean_rsq_fs5 <- run_featureset_eleven[[1]]$mean_rsq
mean_rsq_fs6 <- run_featureset_twelve[[1]]$mean_rsq
mean_rsq_fs7 <- run_featureset_thirteen[[1]]$mean_rsq
mean_rsq_fs8 <- run_featureset_ten[[1]]$mean_rsq


for (i in 2:10){
  mean_rsq_fs1 <- cbind.data.frame(mean_rsq_fs1,run_featureset_two[[i]]$mean_rsq)
  mean_rsq_fs2 <- cbind.data.frame(mean_rsq_fs2,run_featureset_four[[i]]$mean_rsq)
  mean_rsq_fs3 <- cbind.data.frame(mean_rsq_fs3,run_featureset_six[[i]]$mean_rsq)
  mean_rsq_fs4 <- cbind.data.frame(mean_rsq_fs4,run_featureset_eight[[i]]$mean_rsq)
  mean_rsq_fs5 <- cbind.data.frame(mean_rsq_fs5,run_featureset_eleven[[i]]$mean_rsq)
  mean_rsq_fs6 <- cbind.data.frame(mean_rsq_fs6,run_featureset_twelve[[i]]$mean_rsq)
  mean_rsq_fs7 <- cbind.data.frame(mean_rsq_fs7,run_featureset_thirteen[[i]]$mean_rsq)
  mean_rsq_fs8 <- cbind.data.frame(mean_rsq_fs8,run_featureset_ten[[i]]$mean_rsq)
}

mean_rsq_fs1 <- rowMeans(mean_rsq_fs1)
mean_rsq_fs2 <- rowMeans(mean_rsq_fs2)
mean_rsq_fs3 <- rowMeans(mean_rsq_fs3)
mean_rsq_fs4 <- rowMeans(mean_rsq_fs4)
mean_rsq_fs5 <- rowMeans(mean_rsq_fs5)
mean_rsq_fs6 <- rowMeans(mean_rsq_fs6)
mean_rsq_fs7 <- rowMeans(mean_rsq_fs7)
mean_rsq_fs8 <- rowMeans(mean_rsq_fs8)


mean_rsq_rf <- cbind.data.frame(c(1:10),mean_rsq_fs1,mean_rsq_fs2,mean_rsq_fs3,
                                mean_rsq_fs4,mean_rsq_fs5,mean_rsq_fs6,mean_rsq_fs7,
                                mean_rsq_fs8)

names(mean_rsq_rf) <- c('horizon','1','2','3','4','5','6','7','8')


###############################################################################
#ANN
##MAE
mean_mae_fs1_nn <- run_featureset_two_nn[[1]]$mean_mae
mean_mae_fs2_nn <- run_featureset_four_nn[[1]]$mean_mae
mean_mae_fs3_nn <- run_featureset_six_nn[[1]]$mean_mae
mean_mae_fs4_nn <- run_featureset_eight_nn[[1]]$mean_mae
mean_mae_fs8_nn <- run_featureset_ten_nn[[1]]$mean_mae

for (i in 2:10){
  mean_mae_fs1_nn <- cbind.data.frame(mean_mae_fs1_nn,run_featureset_two_nn[[i]]$mean_mae)
  mean_mae_fs2_nn <- cbind.data.frame(mean_mae_fs2_nn,run_featureset_four_nn[[i]]$mean_mae)
  mean_mae_fs3_nn <- cbind.data.frame(mean_mae_fs3_nn,run_featureset_six_nn[[i]]$mean_mae)
  mean_mae_fs4_nn <- cbind.data.frame(mean_mae_fs4_nn,run_featureset_eight_nn[[i]]$mean_mae)
  mean_mae_fs8_nn <- cbind.data.frame(mean_mae_fs8_nn,run_featureset_ten_nn[[i]]$mean_mae)
}

mean_mae_fs1_nn <- rowMeans(mean_mae_fs1_nn)
mean_mae_fs2_nn <- rowMeans(mean_mae_fs2_nn)
mean_mae_fs3_nn <- rowMeans(mean_mae_fs3_nn)
mean_mae_fs4_nn <- rowMeans(mean_mae_fs4_nn)
mean_mae_fs8_nn <- rowMeans(mean_mae_fs8_nn)

mean_mae_nn <- cbind.data.frame(c(1:10),mean_mae_fs1_nn,mean_mae_fs2_nn,mean_mae_fs3_nn,
                              mean_mae_fs4_nn,mean_mae_fs8_nn)

names(mean_mae_nn) <- c('horizon','1','2','3','4','8')

###########################################
#ANN
##MSE
mean_mse_fs1_nn <- run_featureset_two_nn[[1]]$mean_mse
mean_mse_fs2_nn <- run_featureset_four_nn[[1]]$mean_mse
mean_mse_fs3_nn <- run_featureset_six_nn[[1]]$mean_mse
mean_mse_fs4_nn <- run_featureset_eight_nn[[1]]$mean_mse
mean_mse_fs8_nn <- run_featureset_ten_nn[[1]]$mean_mse

for (i in 2:10){
  mean_mse_fs1_nn <- cbind.data.frame(mean_mse_fs1_nn,run_featureset_two_nn[[i]]$mean_mse)
  mean_mse_fs2_nn <- cbind.data.frame(mean_mse_fs2_nn,run_featureset_four_nn[[i]]$mean_mse)
  mean_mse_fs3_nn <- cbind.data.frame(mean_mse_fs3_nn,run_featureset_six_nn[[i]]$mean_mse)
  mean_mse_fs4_nn <- cbind.data.frame(mean_mse_fs4_nn,run_featureset_eight_nn[[i]]$mean_mse)
  mean_mse_fs8_nn <- cbind.data.frame(mean_mse_fs8_nn,run_featureset_ten_nn[[i]]$mean_mse)
}

mean_mse_fs1_nn <- rowMeans(mean_mse_fs1_nn)
mean_mse_fs2_nn <- rowMeans(mean_mse_fs2_nn)
mean_mse_fs3_nn <- rowMeans(mean_mse_fs3_nn)
mean_mse_fs4_nn <- rowMeans(mean_mse_fs4_nn)
mean_mse_fs8_nn <- rowMeans(mean_mse_fs8_nn)

mean_mse_nn <- cbind.data.frame(c(1:10),mean_mse_fs1_nn,mean_mse_fs2_nn,mean_mse_fs3_nn,
                                mean_mse_fs4_nn,mean_mse_fs8_nn)

names(mean_mse_nn) <- c('horizon','1','2','3','4','8')


###########################################
#ANN
##R2
mean_rsq_fs1_nn <- run_featureset_two_nn[[1]]$mean_rsq
mean_rsq_fs2_nn <- run_featureset_four_nn[[1]]$mean_rsq
mean_rsq_fs3_nn <- run_featureset_six_nn[[1]]$mean_rsq
mean_rsq_fs4_nn <- run_featureset_eight_nn[[1]]$mean_rsq
mean_rsq_fs8_nn <- run_featureset_ten_nn[[1]]$mean_rsq

for (i in 2:10){
  mean_rsq_fs1_nn <- cbind.data.frame(mean_rsq_fs1_nn,run_featureset_two_nn[[i]]$mean_rsq)
  mean_rsq_fs2_nn <- cbind.data.frame(mean_rsq_fs2_nn,run_featureset_four_nn[[i]]$mean_rsq)
  mean_rsq_fs3_nn <- cbind.data.frame(mean_rsq_fs3_nn,run_featureset_six_nn[[i]]$mean_rsq)
  mean_rsq_fs4_nn <- cbind.data.frame(mean_rsq_fs4_nn,run_featureset_eight_nn[[i]]$mean_rsq)
  mean_rsq_fs8_nn <- cbind.data.frame(mean_rsq_fs8_nn,run_featureset_ten_nn[[i]]$mean_rsq)
}

mean_rsq_fs1_nn <- rowMeans(mean_rsq_fs1_nn)
mean_rsq_fs2_nn <- rowMeans(mean_rsq_fs2_nn)
mean_rsq_fs3_nn <- rowMeans(mean_rsq_fs3_nn)
mean_rsq_fs4_nn <- rowMeans(mean_rsq_fs4_nn)
mean_rsq_fs8_nn <- rowMeans(mean_rsq_fs8_nn)

mean_rsq_nn <- cbind.data.frame(c(1:10),mean_rsq_fs1_nn,mean_rsq_fs2_nn,mean_rsq_fs3_nn,
                                mean_rsq_fs4_nn,mean_rsq_fs8_nn)

names(mean_rsq_nn) <- c('horizon','1','2','3','4','8')



###########################################################
#performance improvement of geographic information

###percent change for each prediction horizon
###RF
change_table_rf <- rbind.data.frame('2','3','4','5','6','7','8')
names(change_table_rf) <- 'feature_set'


for (row in 1:nrow(mean_mae_rf)){
  fs2dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'2']
  fs2per_change <- fs2dec/mean_mae_rf[row,'1']*100
  fs3dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'3']
  fs3per_change <- fs3dec/mean_mae_rf[row,'1']*100
  fs4dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'4']
  fs4per_change <- fs4dec/mean_mae_rf[row,'1']*100
  fs5dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'5']
  fs5per_change <- fs5dec/mean_mae_rf[row,'1']*100
  fs6dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'6']
  fs6per_change <- fs6dec/mean_mae_rf[row,'1']*100
  fs7dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'7']
  fs7per_change <- fs7dec/mean_mae_rf[row,'1']*100
  fs8dec <- mean_mae_rf[row,'1'] - 
    mean_mae_rf[row,'8']
  fs8per_change <- fs8dec/mean_mae_rf[row,'1']*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs5per_change,
                  fs6per_change,fs7per_change,fs8per_change)
  change_table_rf <- cbind(change_table_rf,change_vec)
}
colnames(change_table_rf) <- c("id",as.character(1:10))


##############################
###percent change for each prediction horizon
###NN
change_table_nn <- rbind.data.frame('2','3','4','8')
names(change_table_nn) <- 'feature_set'


for (row in 1:nrow(mean_mae_nn)){
  fs2dec <- mean_mae_nn[row,'1'] - 
    mean_mae_nn[row,'2']
  fs2per_change <- fs2dec/mean_mae_nn[row,'1']*100
  fs3dec <- mean_mae_nn[row,'1'] - 
    mean_mae_nn[row,'3']
  fs3per_change <- fs3dec/mean_mae_nn[row,'1']*100
  fs4dec <- mean_mae_nn[row,'1'] - 
    mean_mae_nn[row,'4']
  fs4per_change <- fs4dec/mean_mae_nn[row,'1']*100
  fs8dec <- mean_mae_nn[row,'1'] - 
    mean_mae_nn[row,'8']
  fs8per_change <- fs8dec/mean_mae_nn[row,'1']*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs8per_change)
  change_table_nn <- cbind(change_table_nn,change_vec)
}
colnames(change_table_nn) <- c("id",as.character(1:10))








###########################################################
###percent change for each training data input (1 hour ahead prediction)
###RF
change_table_10hp_rf <- rbind.data.frame('2','3','4','5','6','7','8')
names(change_table_10hp_rf) <- 'feature_set'

for (i in 1:10){
  fs2dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_four[[i]]$mean_mae[10]
  fs2per_change <- fs2dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs3dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_six[[i]]$mean_mae[10]
  fs3per_change <- fs3dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs4dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_eight[[i]]$mean_mae[10]
  fs4per_change <- fs4dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs5dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_eleven[[i]]$mean_mae[10]
  fs5per_change <- fs5dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs6dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_twelve[[i]]$mean_mae[10]
  fs6per_change <- fs6dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs7dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_thirteen[[i]]$mean_mae[10]
  fs7per_change <- fs7dec/run_featureset_two[[i]]$mean_mae[10]*100
  fs8dec <- run_featureset_two[[i]]$mean_mae[10] - 
    run_featureset_ten[[i]]$mean_mae[10]
  fs8per_change <- fs8dec/run_featureset_two[[i]]$mean_mae[10]*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs5per_change,fs6per_change,
                  fs7per_change,fs8per_change)
  change_table_10hp_rf <- cbind(change_table_10hp_rf,change_vec)
}
colnames(change_table_10hp_rf) <- c("id",as.character(1:10))


############################################
###percent change for each training data input (1 hour ahead prediction)
###ANN
change_table_10hp_nn <- rbind.data.frame('2','3','4','8')
names(change_table_10hp_nn) <- 'feature_set'

for (i in 1:10){
  fs2dec <- run_featureset_two_nn[[i]]$mean_mae[10] - 
    run_featureset_four_nn[[i]]$mean_mae[10]
  fs2per_change <- fs2dec/run_featureset_two_nn[[i]]$mean_mae[10]*100
  fs3dec <- run_featureset_two_nn[[i]]$mean_mae[10] - 
    run_featureset_six_nn[[i]]$mean_mae[10]
  fs3per_change <- fs3dec/run_featureset_two_nn[[i]]$mean_mae[10]*100
  fs4dec <- run_featureset_two_nn[[i]]$mean_mae[10] - 
    run_featureset_eight_nn[[i]]$mean_mae[10]
  fs4per_change <- fs4dec/run_featureset_two_nn[[i]]$mean_mae[10]*100
  fs8dec <- run_featureset_two_nn[[i]]$mean_mae[10] - 
    run_featureset_ten_nn[[i]]$mean_mae[10]
  fs8per_change <- fs8dec/run_featureset_two_nn[[i]]$mean_mae[10]*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs8per_change)
  change_table_10hp_nn <- cbind(change_table_10hp_nn,change_vec)
}
colnames(change_table_10hp_nn) <- c("id",as.character(1:10))









###########################################################
###percent change for each training data input (5 hour ahead prediction)
###RF
change_table_5hp_rf <- rbind.data.frame('2','3','4','5','6','7','8')
names(change_table_5hp_rf) <- 'feature_set'

for (i in 1:10){
  fs2dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_four[[i]]$mean_mae[5]
  fs2per_change <- fs2dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs3dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_six[[i]]$mean_mae[5]
  fs3per_change <- fs3dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs4dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_eight[[i]]$mean_mae[5]
  fs4per_change <- fs4dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs5dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_eleven[[i]]$mean_mae[5]
  fs5per_change <- fs5dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs6dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_twelve[[i]]$mean_mae[5]
  fs6per_change <- fs6dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs7dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_thirteen[[i]]$mean_mae[5]
  fs7per_change <- fs7dec/run_featureset_two[[i]]$mean_mae[5]*100
  fs8dec <- run_featureset_two[[i]]$mean_mae[5] - 
    run_featureset_ten[[i]]$mean_mae[5]
  fs8per_change <- fs8dec/run_featureset_two[[i]]$mean_mae[5]*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs5per_change,fs6per_change,
                  fs7per_change,fs8per_change)
  change_table_5hp_rf <- cbind(change_table_5hp_rf,change_vec)
}
colnames(change_table_5hp_rf) <- c("id",as.character(1:10))


############################################
###percent change for each training data input (5 hour ahead prediction)
###ANN
change_table_5hp_nn <- rbind.data.frame('2','3','4','8')
names(change_table_5hp_nn) <- 'feature_set'

for (i in 1:10){
  fs2dec <- run_featureset_two_nn[[i]]$mean_mae[5] - 
    run_featureset_four_nn[[i]]$mean_mae[5]
  fs2per_change <- fs2dec/run_featureset_two_nn[[i]]$mean_mae[5]*100
  fs3dec <- run_featureset_two_nn[[i]]$mean_mae[5] - 
    run_featureset_six_nn[[i]]$mean_mae[5]
  fs3per_change <- fs3dec/run_featureset_two_nn[[i]]$mean_mae[5]*100
  fs4dec <- run_featureset_two_nn[[i]]$mean_mae[5] - 
    run_featureset_eight_nn[[i]]$mean_mae[5]
  fs4per_change <- fs4dec/run_featureset_two_nn[[i]]$mean_mae[5]*100
  fs8dec <- run_featureset_two_nn[[i]]$mean_mae[5] - 
    run_featureset_ten_nn[[i]]$mean_mae[5]
  fs8per_change <- fs8dec/run_featureset_two_nn[[i]]$mean_mae[5]*100
  change_vec <- c(fs2per_change,fs3per_change,fs4per_change,fs8per_change)
  change_table_5hp_nn <- cbind(change_table_5hp_nn,change_vec)
}
colnames(change_table_5hp_nn) <- c("id",as.character(1:10))



#################################################################################
##error for each segment (all train days, 1h ahead)
##RF
run_featureset_two[[1]]$mean_dist_error[1][[1]][[1]]
##street segment ids
run_featureset_two[[1]]$mean_dist_error[2]

run_featureset_two[[1]]$mean_dist_error[1][[1]][[1]]

##RF
##MAE
dist_mean_mae_fs1 <- run_featureset_two[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs2 <- run_featureset_four[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs3 <- run_featureset_six[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs4 <- run_featureset_eight[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs5 <- run_featureset_eleven[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs6 <- run_featureset_twelve[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs7 <- run_featureset_thirteen[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs8 <- run_featureset_ten[[1]]$mean_dist_error[1][[1]][[1]]


for (i in 2:10){
  dist_mean_mae_fs1 <- cbind.data.frame(dist_mean_mae_fs1,run_featureset_two[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs2 <- cbind.data.frame(dist_mean_mae_fs2,run_featureset_four[[i]]$mean_dist_error[1][[1]][[1]])  
  dist_mean_mae_fs3 <- cbind.data.frame(dist_mean_mae_fs3,run_featureset_six[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs4 <- cbind.data.frame(dist_mean_mae_fs4,run_featureset_eight[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs5 <- cbind.data.frame(dist_mean_mae_fs5,run_featureset_eleven[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs6 <- cbind.data.frame(dist_mean_mae_fs6,run_featureset_twelve[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs7 <- cbind.data.frame(dist_mean_mae_fs7,run_featureset_thirteen[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs8 <- cbind.data.frame(dist_mean_mae_fs8,run_featureset_ten[[i]]$mean_dist_error[1][[1]][[1]])
}


dist_mean_mae_fs1 <- rowMeans(dist_mean_mae_fs1)
dist_mean_mae_fs2 <- rowMeans(dist_mean_mae_fs2)
dist_mean_mae_fs3 <- rowMeans(dist_mean_mae_fs3)
dist_mean_mae_fs4 <- rowMeans(dist_mean_mae_fs4)
dist_mean_mae_fs5 <- rowMeans(dist_mean_mae_fs5)
dist_mean_mae_fs6 <- rowMeans(dist_mean_mae_fs6)
dist_mean_mae_fs7 <- rowMeans(dist_mean_mae_fs7)
dist_mean_mae_fs8 <- rowMeans(dist_mean_mae_fs8)


##one hour ahead prediction, averaged over all training data amounts
street_seg_error_rf <- cbind.data.frame(run_featureset_two[[1]]$mean_dist_error[2],dist_mean_mae_fs1,dist_mean_mae_fs2,
                                        dist_mean_mae_fs3,dist_mean_mae_fs4,
                                        dist_mean_mae_fs5,dist_mean_mae_fs6,
                                        dist_mean_mae_fs7,dist_mean_mae_fs8)

names(street_seg_error_rf) <- c('BLOCK_ID','mae1hfs1','mae1hfs2','mae1hfs3','mae1hfs4',
                                'mae1hfs5','mae1hfs6','mae1hfs7','mae1hfs8')


street_seg_error_rf <- merge(street_seg_error_rf,streets_coords[,c(1,3)],by='BLOCK_ID')
street_seg_error_rf <- street_seg_error_rf[!duplicated(street_seg_error_rf$BLOCK_ID), ]
street_seg_error_rf$algorithm <- 'RF'

##############################################

##ANN
##MAE
dist_mean_mae_fs1 <- run_featureset_two_nn[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs2 <- run_featureset_four_nn[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs3 <- run_featureset_six_nn[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs4 <- run_featureset_eight_nn[[1]]$mean_dist_error[1][[1]][[1]]
dist_mean_mae_fs8 <- run_featureset_ten[[1]]$mean_dist_error[1][[1]][[1]]


for (i in 2:10){
  dist_mean_mae_fs1 <- cbind.data.frame(dist_mean_mae_fs1,run_featureset_two_nn[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs2 <- cbind.data.frame(dist_mean_mae_fs2,run_featureset_four_nn[[i]]$mean_dist_error[1][[1]][[1]])  
  dist_mean_mae_fs3 <- cbind.data.frame(dist_mean_mae_fs3,run_featureset_six_nn[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs4 <- cbind.data.frame(dist_mean_mae_fs4,run_featureset_eight_nn[[i]]$mean_dist_error[1][[1]][[1]])
  dist_mean_mae_fs8 <- cbind.data.frame(dist_mean_mae_fs8,run_featureset_ten_nn[[i]]$mean_dist_error[1][[1]][[1]])
}


dist_mean_mae_fs1 <- rowMeans(dist_mean_mae_fs1)
dist_mean_mae_fs2 <- rowMeans(dist_mean_mae_fs2)
dist_mean_mae_fs3 <- rowMeans(dist_mean_mae_fs3)
dist_mean_mae_fs4 <- rowMeans(dist_mean_mae_fs4)
dist_mean_mae_fs8 <- rowMeans(dist_mean_mae_fs8)

dist_mean_mae_fs5 <- NA
dist_mean_mae_fs6 <- NA
dist_mean_mae_fs7 <- NA

##one hour ahead prediction, averaged over all training data amounts
street_seg_error_nn <- cbind.data.frame(run_featureset_two_nn[[1]]$mean_dist_error[2],dist_mean_mae_fs1,dist_mean_mae_fs2,
                                        dist_mean_mae_fs3,dist_mean_mae_fs4,dist_mean_mae_fs5,dist_mean_mae_fs6,
                                        dist_mean_mae_fs7,dist_mean_mae_fs8)

names(street_seg_error_nn) <- c('BLOCK_ID','mae1hfs1','mae1hfs2','mae1hfs3','mae1hfs4',
                                'mae1hfs5','mae1hfs6','mae1hfs7','mae1hfs8')


street_seg_error_nn <- merge(street_seg_error_nn,streets_coords[,c(1,3)],by='BLOCK_ID')
street_seg_error_nn <- street_seg_error_nn[!duplicated(street_seg_error_nn$BLOCK_ID), ]
street_seg_error_nn$algorithm <- 'ANN'

#########

##combine error of RF & NN
street_seg_error_rfnn <- rbind(street_seg_error_rf,street_seg_error_nn)



######################################################################################
##error for each segment (all train days, 5h ahead)
##RF



##RF
##MAE
dist_mean_mae_fs1 <- run_featureset_two[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs2 <- run_featureset_four[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs3 <- run_featureset_six[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs4 <- run_featureset_eight[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs5 <- run_featureset_eleven[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs6 <- run_featureset_twelve[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs7 <- run_featureset_thirteen[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs8 <- run_featureset_ten[[1]]$mean_dist_error[1][[1]][[5]]


for (i in 2:10){
  dist_mean_mae_fs1 <- cbind.data.frame(dist_mean_mae_fs1,run_featureset_two[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs2 <- cbind.data.frame(dist_mean_mae_fs2,run_featureset_four[[i]]$mean_dist_error[1][[1]][[5]])  
  dist_mean_mae_fs3 <- cbind.data.frame(dist_mean_mae_fs3,run_featureset_six[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs4 <- cbind.data.frame(dist_mean_mae_fs4,run_featureset_eight[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs5 <- cbind.data.frame(dist_mean_mae_fs5,run_featureset_eleven[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs6 <- cbind.data.frame(dist_mean_mae_fs6,run_featureset_twelve[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs7 <- cbind.data.frame(dist_mean_mae_fs7,run_featureset_thirteen[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs8 <- cbind.data.frame(dist_mean_mae_fs8,run_featureset_ten[[i]]$mean_dist_error[1][[1]][[5]])
}


dist_mean_mae_fs1 <- rowMeans(dist_mean_mae_fs1)
dist_mean_mae_fs2 <- rowMeans(dist_mean_mae_fs2)
dist_mean_mae_fs3 <- rowMeans(dist_mean_mae_fs3)
dist_mean_mae_fs4 <- rowMeans(dist_mean_mae_fs4)
dist_mean_mae_fs5 <- rowMeans(dist_mean_mae_fs5)
dist_mean_mae_fs6 <- rowMeans(dist_mean_mae_fs6)
dist_mean_mae_fs7 <- rowMeans(dist_mean_mae_fs7)
dist_mean_mae_fs8 <- rowMeans(dist_mean_mae_fs8)


##one hour ahead prediction, averaged over all training data amounts
street_seg_error_rf2 <- cbind.data.frame(run_featureset_two[[1]]$mean_dist_error[2],dist_mean_mae_fs1,dist_mean_mae_fs2,
                                        dist_mean_mae_fs3,dist_mean_mae_fs4,
                                        dist_mean_mae_fs5,dist_mean_mae_fs6,
                                        dist_mean_mae_fs7,dist_mean_mae_fs8)

names(street_seg_error_rf2) <- c('BLOCK_ID','mae5hfs1','mae5hfs2','mae5hfs3','mae5hfs4',
                                'mae5hfs5','mae5hfs6','mae5hfs7','mae5hfs8')

##join street_seg_error
street_seg_error_rf <- merge(street_seg_error_rf,street_seg_error_rf2,by='BLOCK_ID')

##remove street_seg_error_rf2
rm(street_seg_error_rf2)

##############################################

##ANN
##MAE
dist_mean_mae_fs1 <- run_featureset_two_nn[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs2 <- run_featureset_four_nn[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs3 <- run_featureset_six_nn[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs4 <- run_featureset_eight_nn[[1]]$mean_dist_error[1][[1]][[5]]
dist_mean_mae_fs8 <- run_featureset_ten[[1]]$mean_dist_error[1][[1]][[5]]


for (i in 2:10){
  dist_mean_mae_fs1 <- cbind.data.frame(dist_mean_mae_fs1,run_featureset_two_nn[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs2 <- cbind.data.frame(dist_mean_mae_fs2,run_featureset_four_nn[[i]]$mean_dist_error[1][[1]][[5]])  
  dist_mean_mae_fs3 <- cbind.data.frame(dist_mean_mae_fs3,run_featureset_six_nn[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs4 <- cbind.data.frame(dist_mean_mae_fs4,run_featureset_eight_nn[[i]]$mean_dist_error[1][[1]][[5]])
  dist_mean_mae_fs8 <- cbind.data.frame(dist_mean_mae_fs8,run_featureset_ten_nn[[i]]$mean_dist_error[1][[1]][[5]])
}


dist_mean_mae_fs1 <- rowMeans(dist_mean_mae_fs1)
dist_mean_mae_fs2 <- rowMeans(dist_mean_mae_fs2)
dist_mean_mae_fs3 <- rowMeans(dist_mean_mae_fs3)
dist_mean_mae_fs4 <- rowMeans(dist_mean_mae_fs4)
dist_mean_mae_fs8 <- rowMeans(dist_mean_mae_fs8)

dist_mean_mae_fs5 <- NA
dist_mean_mae_fs6 <- NA
dist_mean_mae_fs7 <- NA

##one hour ahead prediction, averaged over all training data amounts
street_seg_error_nn2 <- cbind.data.frame(run_featureset_two_nn[[1]]$mean_dist_error[2],dist_mean_mae_fs1,dist_mean_mae_fs2,
                                         dist_mean_mae_fs3,dist_mean_mae_fs4,
                                         dist_mean_mae_fs5,dist_mean_mae_fs6,
                                         dist_mean_mae_fs7,dist_mean_mae_fs8)

names(street_seg_error_nn2) <- c('BLOCK_ID','mae5hfs1','mae5hfs2','mae5hfs3','mae5hfs4',
                                 'mae5hfs5','mae5hfs6','mae5hfs7','mae5hfs8')

##join street_seg_error
street_seg_error_nn <- merge(street_seg_error_nn,street_seg_error_nn2,by='BLOCK_ID')

##remove street_seg_error_rf2
rm(street_seg_error_nn2)

#########

##combine error of RF & NN
street_seg_error_rfnn <- rbind(street_seg_error_rf,street_seg_error_nn)

##change order of RF and ANN levels
street_seg_error_rfnn$algorithm <- factor(street_seg_error_rfnn$algorithm,
                       levels = c('RF','ANN'),ordered = TRUE)

######################################################

##percentage improvement from FS1 - FS8
street_seg_error_rf$mae1h_per_diff_rf <- street_seg_error_rf$mae1hfs1 - 
  street_seg_error_rf$mae1hfs8
street_seg_error_rf$mae1h_per_diff_rf <- street_seg_error_rf$mae1h_per_diff/
  street_seg_error_rf$mae1hfs1*100

street_seg_error_rf$mae5h_per_diff_rf <- street_seg_error_rf$mae5hfs1 - 
  street_seg_error_rf$mae5hfs8
street_seg_error_rf$mae5h_per_diff_rf <- street_seg_error_rf$mae5h_per_diff/
  street_seg_error_rf$mae5hfs1*100


