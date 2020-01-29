seed=10
seed2=11

##geographic features
poi_features <- c('rail_mean_dist','tourist_min_dist','businesses_count_500')
all_poi <- c('rail_mean_dist','rail_min_dist','tourist_mean_dist','tourist_min_dist',
             'businesses_count_500','businesses_count_400',
             'businesses_count_300','businesses_count_200','businesses_count_100','restaurant_count_500',
             'restaurant_count_400','restaurant_count_300','restaurant_count_200','restaurant_count_100')
centrality_features <- c('edge_between','mean_closeness',
                    'mean_alpha')
all_centrality <- c('edge_between','mean_betweenness','mean_closeness','mean_degree',
                    'mean_eigen','mean_alpha','mean_page')

landuse_features <- c('resunits_500','office_sqft_500','industrial_sqft_500')
all_landuse <- c('resunits_500','resunits_400','resunits_300','resunits_200','resunits_100',
                      'office_sqft_500','office_sqft_400','office_sqft_300','office_sqft_200',
                      'office_sqft_100','industrial_sqft_500','industrial_sqft400','industrial_sqft300',
                      'industrial_sqft200','industrial_sqft100')

all_features_uncorr <- c('resunits_500','office_sqft_500','industrial_sqft_500',
                         'rail_mean_dist','tourist_min_dist','edge_between','mean_alpha')

##prediction horizons
all_horizons <- c('OCC_RATE_t1','OCC_RATE_t2','OCC_RATE_t3','OCC_RATE_t4','OCC_RATE_t5',
                  'OCC_RATE_t6','OCC_RATE_t7','OCC_RATE_t8','OCC_RATE_t9','OCC_RATE_t10')

all_weekdays <- c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')


##create feature sets
feature_set_one <- 'TIME_OF_DAY'
feature_set_three <- c('TIME_OF_DAY',centrality_features)
feature_set_five <- c('TIME_OF_DAY',poi_features)
feature_set_seven <- c('TIME_OF_DAY',landuse_features)
feature_set_nine <- c('TIME_OF_DAY',centrality_features,poi_features,
                      landuse_features)

feature_set_c_p <- c('TIME_OF_DAY',centrality_features,poi_features)
feature_set_p_l <- c('TIME_OF_DAY',poi_features,landuse_features)
feature_set_c_l <- c('TIME_OF_DAY',centrality_features,landuse_features)

feature_set_two <- list()
feature_set_four <- list()
feature_set_six <- list()
feature_set_eight <- list()
feature_set_ten <- list()

feature_set_eleven <- list()
feature_set_twelve <- list()
feature_set_thirteen <- list()

for (hor in all_horizons){
  feature_set_two <- list.append(feature_set_two,c(feature_set_one,hor))
  feature_set_four <- list.append(feature_set_four,c(feature_set_three,hor))
  feature_set_six <- list.append(feature_set_six,c(feature_set_five,hor))
  feature_set_eight <- list.append(feature_set_eight,c(feature_set_seven,hor))
  feature_set_ten <- list.append(feature_set_ten,c(feature_set_nine,hor))
  
  feature_set_eleven <- list.append(feature_set_eleven,c(feature_set_c_p,hor))
  feature_set_twelve <- list.append(feature_set_twelve,c(feature_set_p_l,hor))
  feature_set_thirteen <- list.append(feature_set_thirteen,c(feature_set_c_l,hor))
}


##create RF dataset
dataAMJrf <- dataAMJ

dataAMJrf$TIME_OF_DAY <- as.factor(dataAMJrf$TIME_OF_DAY)
dataAMJrf$WEEKDAY_INT <- as.factor(dataAMJrf$WEEKDAY_INT)

##generate random sample
dataAMJrf$random_sample <- runif(n=length(dataAMJrf[,1]),min=0,max=1)

sequence_daily <- seq(min(dataAMJrf$CAL_DATE), max(dataAMJrf$CAL_DATE),by="day")
##cut date of dataAMJrf (choose timeframe)

##left outer join with all geographic variables
dataAMJrf <- merge(x = dataAMJrf, y = all_geo_data, by.x = "STREET_BLOCK",
                   by.y="road_seg", all.x = TRUE)

dataAMJrf[is.na(dataAMJrf)] = 0

###

##function to prepare data and split it into train and test set
prepare_train_test <- function(input_data,train_days,weekday,features,shuffle=F){
  if (weekday=="monday"){
    df <- input_data[input_data$WEEKDAY_INT==2,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else if (weekday=="tuesday"){
    df <- input_data[input_data$WEEKDAY_INT==3,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else if (weekday=="wednesday"){
    df <- input_data[input_data$WEEKDAY_INT==4,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else if (weekday=="thursday"){
    df <- input_data[input_data$WEEKDAY_INT==5,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else if (weekday=="friday"){
    df <- input_data[input_data$WEEKDAY_INT==6,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else if (weekday=="saturday"){
    df <- input_data[input_data$WEEKDAY_INT==7,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  else {
    df <- input_data[input_data$WEEKDAY_INT==1,]
    dates <- sort(unique(df$CAL_DATE))
    train_sequence <- dates[1:train_days]
    test_sequence <- dates[(train_days+1)]
  }
  input_data <- input_data[order(input_data$START_TIME_DT),]
  if (shuffle==T){
    set.seed(seed)
    rows <- sample(nrow(input_data))
    input_data <- input_data[rows, ]
  }
  train <- input_data[input_data$CAL_DATE %in% train_sequence,]
  test <- input_data[input_data$CAL_DATE %in% test_sequence,]
  X_train <- train %>% dplyr::select(features)
  y_train <- train$OCC_RATE
  X_test <- test %>% dplyr::select(features)
  y_test <- test$OCC_RATE
  y_test_district <- test %>% dplyr::select('OCC_RATE','PM_DISTRICT_NAME','BLOCK_ID')
  newList <- list("X_train"=X_train,"y_train"=y_train,"X_test"=X_test,"y_test"=y_test,"y_test_district"=y_test_district)
  return(newList)
}

input_data <- dataAMJrf
train_days <- 10
weekday="monday"
horizon <- 1
features <- c('TIME_OF_DAY',centrality_features,poi_features,'OCC_RATE_t1')
shuffle=F
mnodes=NA

X_train <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$X_train
y_train <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_train
X_test <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$X_test
y_test <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_test
y_test_district <- prepare_train_test(input_data,train_days,weekday,features,shuffle)$y_test_district


##function to run RF experiments
##output: list of performance measures for each train period and prediction horizon
rf_run <- function(feature_set,ntree,mtry,maxnodes){
  rsq1_test <- c()
  mae1_test <- c()
  mse1_test <- c()
  importance_incmse <- list()
  mean_importance_incmse <- list()
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
        #
        set.seed(seed)
        regr <- randomForest(x = X_train, y = y_train,maxnodes=maxnodes, ntree = ntree,
                             mtry=mtry,importance=T)
        predictions <- predict(regr, X_test)
        print(paste0('MAE: ' , mae(y_test,predictions) ))
        rsq1_test <- c(rsq1_test,unname(caret::postResample(predictions , y_test)['Rsquared']))
        mae1_test <- c(mae1_test,(mae(y_test,predictions)))
        mse1_test <- c(mse1_test,unname((caret::postResample(predictions , y_test)['RMSE']^2)))
        
        importance_incmse <- list.append(importance_incmse,regr$importance[,1])
        #print(importance_incmse)
        
        ##add error for each district to list
        ####
        segment <- c()
        abs_error <- c()
        dist_error <- list("district"=y_test_dis$PM_DISTRICT_NAME,
                           'block_id'=y_test_dis$BLOCK_ID,
                           'y_test'=y_test_dis$OCC_RATE, 
                           'predictions'=predictions)
        
        dist_error <- as.data.frame(dist_error)
        dist_error$district <- as.factor(dist_error$district)
        dist_error$block_id <- as.factor(dist_error$block_id)
        
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
      
      mean_importance_incmse <- list.append(mean_importance_incmse, 
                                            colMeans(do.call(rbind, importance_incmse)))
      importance_incmse <- list()
      #print(mean_importance_incmse)
      
      mean_error_dist <- do.call(cbind, error_dist_list)
      mean_error_dist <- rowMeans(mean_error_dist)
      #print(mean(mean_error_dist))
      mean_error_dist_list <- list.append(mean_error_dist_list,c(mean_error_dist))
      error_dist_list <- list()
      
    }
    print(paste0('Train day finished: ' , train_day ))
    #print(mean_error_dist_list)
    days_list <- list.append(days_list,list("train_days"=train_day,"mean_mae"=mean_mae_test,
                                            "mean_mse"=mean_mse_test,"mean_rsq"=mean_rsq_test,
                                            "mean_dist_error"=list(c(mean_error_dist_list),
                                                                   levels(error_dist$segment)),
                                            "imp_incmse"=mean_importance_incmse))
    mean_mse_test <- c()
    mean_mae_test <- c()
    mean_rsq_test <- c()
    mean_error_dist_list <- list()
    mean_importance_incmse <- list()
  }
  return(days_list)
}

train_days <- c(1:10)    

##actually run model
run_featureset_thirteen <- rf_run(feature_set_thirteen,200,4,500)

##save list object as file
save(run_featureset_thirteen, file="./A rf_runs_lists/run_featureset_thirteen.RData")
#load("./rf_runs_lists/run_featureset_two.RData")

##actually train model with adjusted hyperparameters
set.seed(seed)
regr <- randomForest(x = X_train, y = y_train , ntree = 200, mtry=5,maxnodes = 500,
                     importance=T)

# Make prediction on test data set
predictions <- predict(regr, X_test)

# train performance
mean(regr$mse) ##mean squared error
mean(regr$rsq) ##r squared


# test performance
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))
print(paste0('MAE: ' , mae(y_test,predictions) ))


metric<-'RMSE'

##build a custom Random Forest model to obtain the best set of parameters 
##for model and compare the output for various combinations of the parameters

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree", "mtry"), class = 
                                    rep("numeric", 3), label = c("maxnodes", "ntree", "mtry"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, mtry = param$mtry, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters
control <- trainControl(method="cv", number=10, search='grid')

# Outline the grid of parameters
tunegrid <- expand.grid(.maxnodes=c(500), .ntree=c(200),
                        .mtry=c(3,4,5))

# Train the model
set.seed(seed)
rf_gridsearch <- caret::train(x=X_train, y=y_train, method=customRF, metric=metric, 
                       tuneGrid=tunegrid, trControl=control)

warnings()

#plot combination of parameters vs performance measure
plot(rf_gridsearch)
print(rf_gridsearch)

##best parameter combination
rf_gridsearch$bestTune

##feature importance
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')

###


# Build scatterplot of real vs predicted values
ggplot(  ) +
  geom_point( aes(x = X_test$TIME_OF_DAY, y = y_test, color = 'red', alpha = 0.5) ) +
  geom_point( aes(x = X_test$TIME_OF_DAY , y = predictions, color = 'blue',  alpha = 0.5)) +
  labs(x = "TIME", y = "OCC", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red"))

ggplot(  ) +
  geom_point( aes(x = X_test$OCC_RATE_t1, y = y_test, color = 'red', alpha = 0.5) ) +
  geom_point( aes(x = X_test$OCC_RATE_t1 , y = predictions, color = 'blue',  alpha = 0.5)) +
  labs(x = "TIME", y = "OCC", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red"))


###correlation matrix of features

cormat <- round(cor(X_train[,-1]),2)
melted_cormat <- melt(cormat)
melted_cormat

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# summarize the correlation matrix
print(cormat)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(cormat, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
