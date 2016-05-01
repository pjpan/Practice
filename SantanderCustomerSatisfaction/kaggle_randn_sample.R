train_feature <- createDataPartition(y = df_train_x$TARGET, p = 0.7,
                                         list = FALSE,
                                         times = 1)

df.train <- df_train_x[train_feature,-1]
df.test <- df_train_x[-train_feature,-1]

df.test%>%count(TARGET)%>%mutate(pct =n/sum(n)) # 0.03876173
df.train%>%count(TARGET)%>%mutate(pct =n/sum(n)) # 0.03991431
X_test_predictions <- c()

for(i in 1:90) {
  # i <- 1
  start_time <- proc.time()
  set.seed(234 * i)
  rate <- 0.8
  
  feature_set_index <- createDataPartition(1:(ncol(df.train)-1), p = rate,
                                           list = FALSE,
                                           times = 1)
  
  feature_set_index <- feature_set_index[,1]
  X_sp_ <- df.train[, c(feature_set_index,307)]
  X_test_sp_ <- df.test[, c(feature_set_index,307)]
  X_all_feature_ <- names(X_sp_)
  
  dx_xgb <- sparse.model.matrix(TARGET ~ ., data = X_sp_)
  dx_xgb_test <- sparse.model.matrix(TARGET ~ ., data = X_test_sp_)
  
  dX_xgb_ <- xgb.DMatrix(dx_xgb, label = X_sp_$TARGET)
  
  param <- list(  objective           = "binary:logistic", 
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = 0.02,
                  max_depth           = 5,
                  subsample           = 0.7,
                  colsample_bytree    = 0.7, # auc0.842956 
                  min_child_weight    = 1,
                  nthread             = 24)
  
  # Run Cross Valication
  cv.Folds = 5
  cv.nround = 1200
  bst.cv = xgb.cv(param = param,
                  data = dX_xgb_, 
                  # label = X_sp_$TARGET,  # if matrix,不用设置；
                  nfold = cv.Folds,
                  nrounds = cv.nround,
                  verbose = 1,
                  early.stop.round = 20,
                  maximize = T)
  
  if(max(bst.cv$test.auc.mean) > 0.8411){
    print(paste0("the best cv auc:", max(bst.cv$test.auc.mean)))
    dX_xgb_test_ <- xgb.DMatrix(dx_xgb_test, label = X_test_sp_$TARGET)
    watchlist <- list(train=dX_xgb_, eval = dX_xgb_test_)
    
    # train xgboost
    xgb.rand <- xgb.train(params= param, 
                           data = dX_xgb_, 
                           nrounds = which.max(bst.cv$test.auc.mean), 
                           verbose = 1,
                           watchlist  = watchlist,
                           maximize = T)
    
    # **************************************
    # calculate XGBoost importance
    # **************************************
    # X_all_imp <- xgb.importance(X_all_feature$feature_name, model=xgb)
    # saveRDS(X_all_imp_, paste0("cache/",folder,"test/X_all_imp",i,".RData"))
    
    # predict values in test set
    y_pred <- predict(xgb.rand, dX_xgb_test_)
    
    X_test_predictions <- as.data.frame(cbind(
      df_train_x[-train_feature, 1],
      y_pred ))
    
    saveRDS(X_test_predictions, paste0("cache/X_test_predictions_",i,".RData"))
    saveRDS(X_all_feature_, paste0("cache/X_test_features_",i,".RData"))
  }
}

elapse_time <- proc.time() - start_time
print(paste0("time pass", elapse_time))
  