library(xgboost)
library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(readr)
library(bit64)
library(caret)
library(ROCR)

set.seed(112)
# *************
# load data 
# *************
df_train_raw <- fread('./input/train.csv', data.table = F, na.strings = "NULL", stringsAsFactors = F)
df_test_raw <- fread('./input/test.csv', data.table = F, na.strings = "NULL", stringsAsFactors = F)
df_train_y <- df_train_raw[,c("ID", "TARGET")]
df_train_x <- df_train_raw
df_test <- df_test_raw
df_test$TARGET <- -1
df_test_y <- as.data.frame(cbind(ID = df_test$ID,TARGET = rep(-1,nrow(df_test))))

# **********************************
# Removing constant features
# **********************************
cat("\n## Removing the constants features.\n")
for (f in names(df_train_x)) {
  if (length(unique(df_train_x[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    df_train_x[[f]] <- NULL
    df_test[[f]] <- NULL
  }
}

# **********************************
# Removing identical features:from other scripts
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
df_test <- df_test[, feature.names]

# 替换极端大的数值为 -1 
df_train_x[df_train_x== 9999999999] <- -1
df_train_x[df_train_x<= -1000000] <- -1
df_train_x[df_train_x<= -999999] <- -1

df_test[df_train_x== 9999999999] <- -1
df_test[df_train_x<= -1000000] <- -1
df_test[df_train_x<= -999999] <- -1

# 删除掉那些极端分布的结果；
sparsefeature <- c()
for(i in 2:(ncol(df_train_x)-1)){
  cat(names(df_train_x)[i], "0.999percenttile is ", quantile(df_train_x[, i], 0.999),"\n")
  if(quantile(df_train_x[, i], 0.999)==0){
    sparsefeature <- c(sparsefeature, names(df_train_x)[i])
  }}


# delete sparse features
feature.names <- setdiff(names(df_train_x), sparsefeature)
df_train_x <- df_train_x[, feature.names]
df_test <- df_test[, feature.names]

# 画出每个变量的分布图
for(i in 2:ncol(df_train_x)){
  data_image <- paste0("distribute/",names(df_train_x)[i],"_",i,".jpg")
  jpeg(file = data_image, bg ="transparent", width=1024, height=768, units="px", quality=75, pointsize=20)
  p <- ggplot(df_train_x,aes(df_train_x[,i] ,fill = factor(TARGET)))+xlab(names(df_train_x)[i])+geom_histogram()
  plot(p)
  dev.off()
  cat(names(df_train_x)[i], "is done. \n")
}

# num_meses_var5_ult3和target正相关；
# df_train_x%>%select(num_var35, TARGET)%>%count(num_var35,TARGET)%>%mutate(pct =n/sum(n))
# df_train_x%>%select(num_var1_0,num_var1)%>%count(num_var1_0,num_var1)%>%mutate(pct =n/sum(n))
# 
# cor(df_train_x$num_meses_var13_largo_ult3, df_train_x$TARGET) # -0.03436733
# cor(df_train_x$num_meses_var5_ult3, df_train_x$TARGET) # -0.1482529
# cor(df_train_x$num_meses_var39_vig_ult3, df_train_x$TARGET) # -0.011169
# cor(df_train_x$num_sal_var16_ult1, df_train_x$TARGET) # 0.009215761

# dummies
# num_meses_var39_vig_ult3
# num_sal_var16_ult1

#变量转换：
totransform <- c("num_meses_var8_ult3", "num_meses_var13_largo_ult3", "num_op_var40_comer_ult1")
df_train_x <- mutate(df_train_x,
                     num_meses_var8_ult3_trans = ifelse(num_meses_var8_ult3>0, 1, num_meses_var8_ult3),
                     num_meses_var13_largo_ult3_trans = ifelse(num_meses_var13_largo_ult3>0, 1, num_meses_var13_largo_ult3),
                     num_op_var40_comer_ult1_trans = num_op_var40_comer_ult1/3
                    )

df_test <- mutate(df_test,
                     num_meses_var8_ult3_trans = ifelse(num_meses_var8_ult3>0, 1, num_meses_var8_ult3),
                     num_meses_var13_largo_ult3_trans = ifelse(num_meses_var13_largo_ult3>0, 1, num_meses_var13_largo_ult3),
                     num_op_var40_comer_ult1_trans = num_op_var40_comer_ult1/3
)
# 删除变换前的变量；
df_train_x <- df_train_x[, !colnames(df_train_x)%in%c(totransform)]
df_test <- df_test[, !colnames(df_test)%in%c(totransform)]

# 查看变量是否超过10个类别，如果在train和test有共同数值，则为类别，否则为稀疏数值；
# tocategory <- c()
# tosparsefeature <- c()
# for(i in 1:(ncol(df_train_x)))
# {
#   nval <- length(unique(df_train_x[,i]))
#   if(nval<=10 & length(setdiff(unique(df_train_x[,i]),unique(df_test[,i])))==0)
#   {
#     tocategory <- c(tocategory, colnames(df_train_x)[i])
#     # eval(parse(text = paste0("impact_",colnames(df_train_x)[i],"_mod <- impactModel(df_train_x[,",i,"], df_train_x$TARGET)")))
#     cat(paste0(colnames(df_train_x)[i]," 类别相同：",nval,"\n"))
#   }else{
#     tosparsefeature <- c(tosparsefeature, colnames(df_train_x)[i])
#     cat(paste0(colnames(df_train_x)[i]," 类别不同：",nval,"\n"))
#   }
# }

# 进行impactcoding, 放弃这么复杂的变换；
# df_train_category <- df_train_x[, names(df_train_x) %in% c(tocategory,"TARGET")]
# df_test_category <- df_test[, names(df_test) %in% c(tocategory,"TARGET")]
# for(s in 1:(length(df_train_category)-1)){
#   eval(parse(text = paste0("impact_",colnames(df_train_category)[s],"_mod <- impactModel(df_train_category[,",s,"], df_train_category$TARGET)"))) 
#   eval(parse(text = paste0("df_train_category$impact_",colnames(df_train_category)[s]," <- applyImpactModel(impact_",colnames(df_train_category)[s],"_mod,df_train_category$",colnames(df_train_category)[s],")")))
#   eval(parse(text = paste0("df_test_category$impact_",colnames(df_test_category)[s]," <- applyImpactModel(impact_",colnames(df_test_category)[s],"_mod, df_test_category$",colnames(df_test_category)[s],")")))
#   cat(paste0(colnames(df_train_category)[s],"is finished\n"))
# }

# df_train_x[,names(df_train_x)%in%tocategory]%>%head()
# df_train_x%>%count(num_var44_0, TARGET)%>%mutate(pct = n/sum(n))
# impactModel(df_train_x$num_var44_0, df_train_x$TARGET)

# 先区分出训练集，训练集合区分train+cv和测试集合
train <- caret::createDataPartition(df_train_x$TARGET, p = 0.8, list =F)
dtrain <- df_train_x[train, ]
dtrain_cv <- df_train_x[-train, ]

# apply(df.train.sparse, 2, function(x){return(quantile(x, 0.995))})  # 有很多变量都是零，缺失值很多；
# apply(df.train.category, 2, function(x){return(quantile(x, 0.995, na.rm = T))})
# saldo_medio_var29_hace3 ，num_trasp_var17_out_ult1， imp_reemb_var17_hace3，imp_reemb_var33_ult1只有一个有效值
# df.train.raw%>%count(num_aport_var33_ult1, TARGET)
# # 数据集合中剔除无效的变量
# feature.names.sparse <- setdiff(names(df.train.x.sparse), toRemove.sparse)
# feature.names.category <- setdiff(names(df.train.x.category), toRemove.category)
# #集合类别和稀疏变量集合进行预测；
# dtrain_x <- dtrain_x[,colnames(dtrain_x)%in%c(feature.names.category[1:72], feature.names.sparse)]
# dtrain_cv <- dtrain_cv[,colnames(dtrain_cv)%in%c(feature.names.category[1:72], feature.names.sparse)]
# df_test_x <- df_test_x[,colnames(df_test_x)%in%c(feature.names.category[1:72], feature.names.sparse)]

# **********************************
# train  full model
# **********************************
# to 
# dtrain_all <- sparse.model.matrix(TARGET ~ ., data = df_train)
# # dtrain <- sparse.model.matrix(TARGET ~ ., data = dtrain_x)
# # dtrain_cv_x <- sparse.model.matrix(TARGET ~ ., data = dtrain_cv)
dtest_x <- sparse.model.matrix(TARGET ~ ., data = df_test)
# 
# # dtrain_raw_ <- xgb.DMatrix(data = dtrain_raw, label = df_train_raw$TARGET)
# dtrain_all_ <- xgb.DMatrix(data = dtrain_all, label = df_train$TARGET)
# # dtrain_ <- xgb.DMatrix(data = dtrain, label = dtrain_x$TARGET)
# # dtrain_cv_ <- xgb.DMatrix(data = dtrain_cv_x, label = dtrain_cv$TARGET)
dtest_ <- xgb.DMatrix(data = dtest_x, label = df_test$TARGET)



