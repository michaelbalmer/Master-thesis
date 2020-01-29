#install Tensorflow backend as follows:
#install.packages('devtools')
# library(devtools)
# library(reticulate)

# ##1
# use_virtualenv('/Users/Michael/venv')
# 
# ##install tensorflow & keras from github
# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/keras")
# library(tensorflow)
# library(keras)
# 
# ##connect to python virtual environment venv
# install_tensorflow(envname = '/Users/Michael/venv')
# install_keras(envname = '/Users/Michael/venv')
# 
# ##python configuration
# reticulate::py_config()

##create NN dataset
dataAMJnn <- dataAMJ

##left outer join with all geographic variables
dataAMJnn <- merge(x = dataAMJnn, y = all_geo_data, by.x = "STREET_BLOCK",
                   by.y="road_seg", all.x = TRUE)

dataAMJnn[is.na(dataAMJnn)] = 0

##callback
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 100 == 0) cat("\n")
    cat(".")
  }
)  

#patience: amount of epochs to check for improvement
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)


input_data <- dataAMJnn
#train_days <- 1
#weekday="monday"
horizon <- 1
features <- c('TIME_OF_DAY','OCC_RATE_t1',centrality_features,poi_features,landuse_features)
shuffle=F

X_train <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$X_train
y_train <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_train
X_test <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$X_test
y_test <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_test
y_test_district <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_test_district

# # To matrices
X_train <- as.matrix(X_train)
y_train <- as.matrix(y_train)
X_test <- as.matrix(X_test)
y_test <- as.matrix(y_test)
y_test_district <- as.matrix(y_test_district)


epochs <- 5
build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 264, activation = "relu",
                input_shape = ncol(X_train)) %>%
    layer_dense(units = 264, 
                activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=0.001),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()

model %>% summary()
set.seed(seed)
history <- model %>% fit(
  X_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(print_dot_callback)
)

#plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
#  coord_cartesian(ylim = c(0, 2))


print(history)

# score <- model %>% evaluate(
#   X_test, y_test,
#   verbose = 0
# )
# 
# score

predictions <- model %>% predict(X_test)

y_test_district

# test performance
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))
print(paste0('MAE: ' , mae(y_test,predictions) ))


##set up for tuning
par <- list(
  neurons1 = c(128,256,512),
  neurons2 = c(128,256,512),
  lr = c(0.001)
)

set.seed(seed)
runs <- tuning_run('nn_runs.R', runs_dir = './nn_runs', sample = 1, flags = par)


# find the best evaluation accuracy
runs <- runs[order(runs$metric_mean_absolute_error, decreasing = F), ]

runs_5d_tod_occ1_all <- runs

save(runs_5d_tod_occ1_all, file="./A training_runs_nn/runs_5d_tod_occ1_all.RData")


##function to run model
nn_run <- function(feature_set,neurons1,neurons2,learning_rate=0.001,
                   eps=50){
  rsq1_test <- c()
  mae1_test <- c()
  mse1_test <- c()
  mean_rsq_test <- c()
  mean_mae_test <- c()
  mean_mse_test <- c()
  error_dist_list <- list()
  mean_error_dist_list <- list()
  days_list <- list()
  for (train_day in train_days){
    for (el in feature_set){
      for (day in all_weekdays){
        X_train <- prepare_train_test(input_data,train_day,day,el,shuffle)$X_train
        y_train <- prepare_train_test(input_data,train_day,day,el,shuffle)$y_train
        X_test <- prepare_train_test(input_data,train_day,day,el,shuffle)$X_test
        y_test <- prepare_train_test(input_data,train_day,day,el,shuffle)$y_test
        y_test_dis <- prepare_train_test(input_data,train_day,day,
                                         el,shuffle)$y_test_district
        # # To matrices
        X_train <- as.matrix(X_train)
        y_train <- as.matrix(y_train)
        X_test <- as.matrix(X_test)
        y_test <- as.matrix(y_test)
        y_test_dis <- as.matrix(y_test_dis)
        
        #model architecture
        model <- keras_model_sequential() %>%
          layer_dense(units = neurons1, activation = "relu",
                      input_shape = ncol(X_train)) %>%
          layer_dense(units = neurons2, 
                      activation = "relu") %>%
          layer_dense(units = 1)
        
        model <- model %>% compile(
          loss = "mse",
          optimizer = optimizer_rmsprop(lr=learning_rate),
          metrics = list("mean_absolute_error"))
        
        set.seed(seed)
        history <- model %>% fit(
          X_train,
          y_train,
          epochs= eps,
          validation_split = 0.2,
          verbose = 0,
          callbacks = list(early_stop)
        )
        predictions <- model %>% predict(X_test)
        
        print(paste0('MAE: ' , mae(y_test,predictions) ))
        rsq1_test <- c(rsq1_test,unname(caret::postResample(predictions , y_test)['Rsquared']))
        mae1_test <- c(mae1_test,(mae(y_test,predictions)))
        mse1_test <- c(mse1_test,unname((caret::postResample(predictions , y_test)['RMSE']^2)))
        
        ##add error for each district to list
        ####
        segment <- c()
        abs_error <- c()
        dist_error <- list("district"=y_test_dis[,2],
                           'block_id'=y_test_dis[,3],
                           'y_test'=y_test_dis[,1], 
                           'predictions'=predictions)
        
        dist_error <- as.data.frame(dist_error)
        dist_error$district <- as.factor(dist_error$district)
        dist_error$block_id <- as.factor(dist_error$block_id)
        dist_error$y_test <- as.numeric(as.character(dist_error$y_test))
        dist_error$predictions <- as.numeric(as.character(dist_error$predictions))
        
        dist_error$abs_error <- abs(dist_error$y_test - dist_error$predictions)
        
        error_per_block <- dist_error %>%
          group_by (dist_error$block_id) %>%
          summarise(mean(abs_error))
        names(error_per_block) <- c('segment','abs_error')
        error_per_block <- error_per_block[order(error_per_block$segment),]
        segment <- error_per_block$segment
        abs_error <- error_per_block$abs_error
        new_list <- list("segment"=segment, "abs_error"=abs_error)
        ######
        error_dist <- new_list
        error_dist_list <- list.append(error_dist_list, c(error_dist$abs_error))
        #print(mean(error_dist$abs_error))
        #print(error_dist_list)
        
      }
      mean_rsq_test <- c(mean_rsq_test,mean(rsq1_test))
      mean_mse_test <- c(mean_mse_test,mean(mse1_test))
      mean_mae_test <- c(mean_mae_test,mean(mae1_test))
      mae1_test <- c()
      mse1_test <- c()
      rsq1_test <- c()
      print(mean_mae_test)
      
      mean_error_dist <- do.call(cbind, error_dist_list)
      mean_error_dist <- rowMeans(mean_error_dist)
      #print(mean(mean_error_dist))
      mean_error_dist_list <- list.append(mean_error_dist_list,c(mean_error_dist))
      #print(mean_error_dist_list)
      error_dist_list <- list()
      
    }
    print(paste0('Train day finished: ' , train_day ))
    #print(mean_error_dist_list)
    days_list <- list.append(days_list,list("train_days"=train_day,"mean_mae"=mean_mae_test,
                                            "mean_mse"=mean_mse_test,"mean_rsq"=mean_rsq_test,
                                            "mean_dist_error"=list(c(mean_error_dist_list),
                                                                   levels(error_dist$segment))))
    mean_mse_test <- c()
    mean_mae_test <- c()
    mean_rsq_test <- c()
    mean_error_dist_list <- list()
  }
  return(days_list)
}

train_days <- c(1:10)
run_featureset_eight_nn <- nn_run(feature_set_eight,128,128,0.001,100)

save(run_featureset_eight_nn, file="./A nn_runs_lists/run_featureset_eight_nn.RData")
