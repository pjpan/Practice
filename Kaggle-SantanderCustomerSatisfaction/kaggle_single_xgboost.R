train_feature <- createDataPartition(y = df_train_x$TARGET, p = 0.7,
                                 list = FALSE,
                                 times = 1)

df.train <- df_train_x[train_feature,-1]
df.test <- df_train_x[-train_feature,-1]

df.test%>%count(TARGET)%>%mutate(pct =n/sum(n)) # 0.03876173
df.train%>%count(TARGET)%>%mutate(pct =n/sum(n)) # 0.03991431
X_test_predictions <- c()
best_auc <- 0

start_time <- proc.time()
set.seed(234)

dx_xgb <- sparse.model.matrix(TARGET ~ ., data = df.train)
dx_xgb_test <- sparse.model.matrix(TARGET ~ ., data = df.test)

dX_xgb_ <- xgb.DMatrix(dx_xgb, label = df.train$TARGET)
dX_xgb_test_ <- xgb.DMatrix(dx_xgb_test, label = df.test$TARGET)

for (depth in c(1,5,7,9,15)){
param <- list(  objective           = "binary:logistic", 
              booster             = "gbtree",
              eval_metric         = "auc",
              eta                 = 0.02,
              max_depth           = depth,
              subsample           = 0.7,
              colsample_bytree    = 0.75, # auc0.842956 
              min_child_weight    = 1,
              nthread             = 8)

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

cat("the best cv auc:", max(bst.cv$test.auc.mean), "\n")
watchlist <- list(train=dX_xgb_, eval = dX_xgb_test_)

if(best_auc<max(bst.cv$test.auc.mean))
{
  # train xgboost
  xgb.single <- xgb.train(params= param, 
                        data = dX_xgb_, 
                        nrounds = which.max(bst.cv$test.auc.mean), # which.max(bst.cv$test.auc.mean),  # 0.847216
                        verbose = 1,
                        watchlist  = watchlist,
                        maximize = T)
  
  
  # **************************************
  # calculate XGBoost importance
  # **************************************
  # X_all_imp <- xgb.importance(X_all_feature$feature_name, model=xgb)
  # saveRDS(X_all_imp_, paste0("cache/",folder,"test/X_all_imp",i,".RData"))
  
  # predict values in test set
  y_pred <- predict(xgb.single, dX_xgb_test_)
  
  X_test_predictions <- as.data.frame(cbind(
    df_train_x[-train_feature, 1],
    y_pred ))
  
  saveRDS(xgb.single, paste0("cache/train/xgb_d,"depth, "_s0.7_c0.75.RData"))
  # saveRDS(X_test_predictions, paste0("cache/single_xgb_d7_s0.7_c0.75_predictions.RData"))
  }
}
elapse_time <- proc.time() - start_time
print(paste0("time pass", elapse_time, "\n"))
