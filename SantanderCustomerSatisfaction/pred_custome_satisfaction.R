library(xgboost)
library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(readr)
library(bit64)
library(caret)

set.seed(112)
# *************
# load data 
# *************
df.train.raw <- fread('./input/train.csv', data.table = F, na.strings = "NULL", stringsAsFactors = F)
df.test.raw <- fread('./input/test.csv', data.table = F, na.strings = "NULL", stringsAsFactors = F)
df_train_y <- df.train.raw$TARGET
df_train_x <- df.train.raw[,!colnames(df.train.raw)%in%c('ID','TARGET')]
df_test_x <- df.test.raw[,!colnames(df.test.raw)%in%c('ID')]

# **********************************
# Removing constant features
# **********************************
cat("\n## Removing the constants features.\n")
for (f in names(df_train_x)) {
  if (length(unique(df_train_x[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    df_train_x[[f]] <- NULL
    df_test_x[[f]] <- NULL
  }
}
# **********************************
# Removing identical features
# **********************************
features_pair <- combn(names(df_train_x), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(df_train_x[[f1]] == df_train_x[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}
# delete useless features
feature.names <- setdiff(names(df_train_x), toRemove)
df_train_x <- df_train_x[, feature.names]
df_test_x <- df_test_x[, feature.names]

# 查看变量是否超过10个类别，如果在train和test有共同数值，则为类别，否则为稀疏数值；
tocategory <- c()
tosparsefeature <- c()
for(i in 1:(ncol(df_train_x)))
{
  nval <- length(unique(df_train_x[,i]))
  if(nval<=10 & length(setdiff(unique(df_train_x[,i]),unique(df_test_x[,i])))==0)
  {
    tocategory <- c(tocategory, colnames(df_train_x)[i])
    cat(paste0(colnames(df_train_x)[i]," 类别相同：",nval,"\n"))
  }else{
    tosparsefeature <- c(tosparsefeature, colnames(df_train_x)[i])
    cat(paste0(colnames(df_train_x)[i]," 类别不同：",nval,"\n"))
  }
}

# 先区分出训练集，训练集合区分train+cv和测试集合
df_train_x$TARGET <- df_train_y
train <- caret::createDataPartition(df_train_x$TARGET, p = 0.8, list =F)
dtrain_x <- df_train_x[train, ]
dtrain_cv <- df_train_x[-train, ]

# 对train区分训练和cv集合
df.train.x.sparse <- dtrain_x[, names(dtrain_x) %in% c(tosparsefeature, "TARGET")]
df.train.x.category <- dtrain_x[, names(dtrain_x) %in% c(tocategory, "TARGET")]

df.train.cv.sparse <- dtrain_cv[, names(dtrain_cv) %in% c(tosparsefeature, "TARGET")]
df.train.cv.category <- dtrain_cv[, names(dtrain_cv) %in% c(tocategory, "TARGET")]

# 给test集合也进行处理；
df.test.sparse <- df_test_x[, names(df_test_x) %in% tosparsefeature]
df.test.category <- df_test_x[, names(df_test_x) %in% tocategory]

# apply(df.train.sparse, 2, function(x){return(quantile(x, 0.995))})  # 有很多变量都是零，缺失值很多；
# apply(df.train.category, 2, function(x){return(quantile(x, 0.995, na.rm = T))})
# saldo_medio_var29_hace3 ，num_trasp_var17_out_ult1， imp_reemb_var17_hace3，imp_reemb_var33_ult1只有一个有效值
# df.train.raw%>%count(num_aport_var33_ult1, TARGET)

# 剔除缺失值很多的变量
# 稀疏变量的结果
toRemove.sparse<- c()
for(i in 1:length(df.train.x.sparse)) {
  if(quantile(df.train.x.sparse[,i], 0.995)==0)
  {
    toRemove.sparse <- c(toRemove.sparse, names(df.train.x.sparse)[i])
  }
}
# 类别变量的结果
toRemove.category<- c()
for(i in 1:length(df.train.x.category)) {
  if(quantile(df.train.x.category[,i], 0.995)==0)
  {
    toRemove.category <- c(toRemove.category, names(df.train.x.category)[i])
  }
}
# 数据集合中剔除无效的变量
feature.names.sparse <- setdiff(names(df.train.x.sparse), toRemove.sparse)
feature.names.category <- setdiff(names(df.train.x.category), toRemove.category)

feature.names.sparse.test <- setdiff(names(df.test.sparse), toRemove.sparse)
feature.names.category.test <- setdiff(names(df.test.category), toRemove.category)

# 稀疏变量的训练集合
df.train.x.sparse <- df.train.x.sparse[, feature.names.sparse]
df.train.cv.sparse <- df.train.cv.sparse[, feature.names.sparse]

df.test.sparse <- df.test.sparse[, feature.names.sparse.test]
df.test.sparse$TARGET <- -1

# 类别变量的训练集合
df.train.x.category <- df.train.x.category[, feature.names.category]
df.train.cv.category <- df.train.cv.category[, feature.names.category]

df.test.category <- df.test.category[, feature.names.category.test]
df.test.category$TARGET <- -1

# **********************************
# train  full model
# **********************************
# 
# dtrain <- xgb.DMatrix(data=as.matrix(dtrain_x[,1:308]), label=dtrain_x$TARGET)
# dtest <- xgb.DMatrix(data=as.matrix(dtrain_cv[,1:308]), label=dtrain_cv$TARGET)

# ***************************
# train model
# ***************************
dtrain <- sparse.model.matrix(TARGET ~ ., data = dtrain_x)
dtrain_cv_x <- sparse.model.matrix(TARGET ~ ., data = dtrain_cv)

df_test_x$TARGET <- -1
dtest_x <- sparse.model.matrix(TARGET ~ ., data = df_test_x)

dtrain_ <- xgb.DMatrix(data = dtrain, label = dtrain_x$TARGET)
dtrain_cv_ <- xgb.DMatrix(data = dtrain_cv_x, label = dtrain_cv$TARGET)
dtest_ <- xgb.DMatrix(data = dtest_x, label = df_test_x$TARGET)

watchlist <- list(train=dtrain_, eval = dtrain_cv_)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 6,
                subsample           = 0.7,
                colsample_bytree    = 0.6,
                min_child_weight    = 1,
                seed                = 100086
)

# # do cross validation
cv.nround <- 800
cv.nfold <- 3
xgb.cv <- xgb.cv( params = param, 
                  nrounds = cv.nround,
                  nfold = cv.nfold, 
                  data = dtrain_, 
                  label= dtrain_x$TARGET, 
                  verbose = 1,
                  early.stop.round = 20, 
                  maximize = T)

# train the finnal model
xgb.clf <- xgb.train(params              = param, 
                     data                = dtrain_, 
                     nrounds             = which.max(xgb.cv$test.auc.mean), 
                     verbose             = 2,
                     watchlist           = watchlist,
                     maximize            = T ,  # 
)

# 存储结果；
saveRDS(xgb.clf, file = "cache/xgb.clf.full.Rdata")

# 查看变量重要性；
feature.names <-  dtrain@Dimnames[[2]]
imp.feature <- xgb.importance(feature_names = feature.names, model = xgb.clf)
xgb.plot.importance(imp.feature[1:20,])

# pred test result
preds_test <- predict(xgb.clf, dtest_)
preds_cv <- predict(xgb.clf, dtrain_cv_)
gbm::gbm.roc.area(dtrain_cv$TARGET, preds_cv) # 0.8539731
summary(preds_test)
summary(preds_cv)
# **********************************
#  train the category model
# **********************************
cat("train the category model\n")
dtrain_category <- sparse.model.matrix(TARGET ~ ., data = df.train.x.category)
dtrain_cv_x_category <- sparse.model.matrix(TARGET ~ ., data = df.train.cv.category)
dtrain_category_ <- xgb.DMatrix(data = dtrain_category, label = df.train.x.category$TARGET)
dtrain_cv_category_ <- xgb.DMatrix(data = dtrain_cv_x_category, label = df.train.cv.category$TARGET)

dtest_category <- sparse.model.matrix(TARGET ~ ., data = df.test.category)
dtest_category_ <- xgb.DMatrix(data = dtest_category, label = df.test.category$TARGET)

watchlist <- list(train=dtrain_category_, eval = dtrain_cv_category_)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.7,
                colsample_bytree    = 0.8,
                min_child_weight    = 2,
                seed                = 123
)

# # do cross validation
cv.nround <- 800
cv.nfold <- 5
xgb.cv.category <- xgb.cv( params = param, 
                           nrounds = cv.nround,
                           nfold = cv.nfold, 
                           data = dtrain_category_, 
                           label= df.train.x.category$TARGET, 
                           verbose = 2,
                           early.stop.round = 20, 
                           maximize = T)

# train the finnal model
xgb.clf.category <- xgb.train(params     = param, 
                              data                = dtrain_category_, 
                              nrounds             = which.max(xgb.cv.category$test.auc.mean), 
                              verbose             = 2,
                              watchlist           = watchlist,
                              maximize            = T ,  # 
)

# 存储结果；
saveRDS(xgb.clf.category, file = "cache/xgb.clf.category.Rdata")
# 查看变量重要性；
feature.names <-  dtrain_category@Dimnames[[2]]
imp.feature <- xgb.importance(feature_names = feature.names, model = xgb.clf.category)
xgb.plot.importance(imp.feature[1:20,])

# pred result
preds_test_category <- predict(xgb.clf.category, dtest_category_)
preds_cv_category <- predict(xgb.clf.category, dtrain_cv_category_)
summary(preds_cv_category)
summary(preds_test_category)
gbm::gbm.roc.area(df.train.cv.category$TARGET, preds_cv_category) # 0.740699

# **********************************
# train sparse model
# **********************************
dtrain_sparse <- sparse.model.matrix(TARGET ~ ., data = df.train.x.sparse)
dtrain_cv_x_sparse <- sparse.model.matrix(TARGET ~ ., data = df.train.cv.sparse)
dtrain_sparse_ <- xgb.DMatrix(data = dtrain_sparse, label = df.train.x.sparse$TARGET)
dtrain_cv_sparse_ <- xgb.DMatrix(data = dtrain_cv_x_sparse, label = df.train.cv.sparse$TARGET)

dtest_sparse <- sparse.model.matrix(TARGET ~ ., data = df.test.category)
dtest_sparse_ <- xgb.DMatrix(data = dtest_sparse, label = df.test.category$TARGET)


watchlist <- list(train=dtrain_sparse_, eval = dtrain_cv_sparse_)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 6,
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                min_child_weight    = 2,
                seed                = 124
)

# # do cross validation
cv.nround <- 1000
cv.nfold <- 5
xgb.cv.sparse <- xgb.cv( params = param, 
                         nrounds = cv.nround,
                         nfold = cv.nfold, 
                         data = dtrain_sparse_, 
                         label= df.train.x.sparse$TARGET, 
                         verbose = 1,
                         early.stop.round = 20, 
                         maximize = T)

# train the finnal model
xgb.clf.sparse <- xgb.train(params     = param, 
                            data                = dtrain_sparse_, 
                            nrounds             = which.max(xgb.cv.sparse$test.auc.mean), 
                            verbose             = 2,
                            watchlist           = watchlist,
                            maximize            = T ,  # 
)

# 存储结果；
saveRDS(xgb.clf.sparse, file = "cache/xgb.clf.sparse.Rdata")
# 查看变量重要性；
feature.names.sparse <-  dtrain_sparse@Dimnames[[2]]
imp.feature.sparse <- xgb.importance(feature_names = feature.names.sparse, model = xgb.clf.sparse)
xgb.plot.importance(imp.feature.sparse[1:20,])

# pred result
preds_test_sparse <- predict(xgb.clf.sparse, dtest_sparse_)
preds_cv_sparse <- predict(xgb.clf.sparse, dtrain_cv_sparse_)
summary(preds_cv_sparse)
summary(preds_test_sparse)
gbm::gbm.roc.area(dtrain_cv_sparse$TARGET, preds_cv_sparse) # 0.852089

# to model ensemble,combine all the three results.
df.2nd.layer <- as.data.frame(cbind(preds_cv, preds_cv_category, TARGET = as.integer(dtrain_cv$TARGET)))
head(df.2nd.layer)
# train the gbm.model
lm.fit <- lm(TARGET~., data = df.2nd.layer)

summary(lm.fit)
gbm.best.iter <- gbm::gbm.perf(gbm.final)
summary(gbm.final)

preds_final <- df.2nd.layer$preds_cv*1.103144-0.001895-0.023270*df.2nd.layer$preds_cv_category
summary(preds_cv)
gbm::gbm.roc.area(dtrain_cv$TARGET, preds_final) # 0.8543975

# 利用三个融合的模型进行预测；
readRDS('cache/xgb.clf.full.Rdata')
readRDS('cache/xgb.clf.category.Rdata')
readRDS('cache/xgb.clf.sparse.Rdata')
# full model
test.2nd.layer <- as.data.frame(cbind(preds_cv = preds_test, preds_cv_category=preds_test_category))
head(test.2nd.layer)
summary(preds_test)

preds_test_final <- test.2nd.layer$preds_cv*1.103144-0.001895-0.023270*test.2nd.layer$preds_cv_category
preds_test_final <- ifelse(preds_test_final<0,0,preds_test_final)
summary(preds_test_final)

# submission result
submissionv6 <- data.frame(ID=df.test.raw$ID, TARGET=preds_test_final)
cat("saving the submission file\n")
write.csv(submissionv6, "submissionv6.csv", row.names = F)
