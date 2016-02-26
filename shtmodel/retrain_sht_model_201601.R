library(gbm)
library(data.table)
library(caret)
library(dplyr)
library(xgboost)

options(stringsAsFactors = F)
shtraw <- fread(input ="./shtmodel1101_0106.txt", header = T, na.strings = "NULL", sep="\t", stringsAsFactors = F)

shtparameter <- strsplit(shtraw$modelinputparameter,',')
shtdata <- as.data.frame(do.call(rbind, shtparameter))

# 给变量命名
names(shtdata) <- c(
  "orderid",
  "cityi",
  "gcity",
  "gintense",
  "gzone",
  "intense",
  "zonei",
  "zonestari",
  "htlnoroomorders_7d",
  "htlnoroomrate_7d",
  "last3droom_sht",
  "last3droompct",
  "last7dnoroom",
  "last7droom_sht",
  "htlnoroomrate_60d",
  "htlorders_60d",
  "noroomrate_60d",
  "shtnoroomrate_60d",
  "shtorders_60d",
  "zonenoroomrate_3d",
  "ordhour",
  "ordprior",
  "recommendlevel",      
  "goldstar",
  "star",
  "roomquantity",
  "freesale",
  "canuserooms",
  "firstunitprice"
)

# View(head(shtdata))
shtdata <- cbind(shtdata, cancelreason = shtraw$cancelreason, score= shtraw$score, cityid = shtraw$cityid)
shtdata <- mutate(shtdata, isfullroom =ifelse(cancelreason =='NOROOM',1,0))

# ggplot(shtdata)+geom_histogram(aes(x=score))

# 把低星级的酒店拿出来进行统计；
data <- filter(shtdata, star<=3, ordprior>=0)

# # string -> numeric
# string -> numeric 
data$cityi <- as.numeric(data$cityi)
data$gcity <- as.numeric(data$gcity)
data$gintense <- as.numeric(data$gintense)
data$gzone <- as.numeric(data$gzone)
data$intense <- as.numeric(data$intense)
data$zonei <- as.numeric(data$zonei)
data$zonestari <- as.numeric(data$zonestari)
data$htlnoroomrate_7d <- as.numeric(data$htlnoroomrate_7d)
data$shtnoroomrate_60d <- as.numeric(data$shtnoroomrate_60d)
data$noroomrate_60d <- as.numeric(data$noroomrate_60d)
data$last3droompct <- as.numeric(data$last3droompct)
data$htlnoroomrate_60d <- as.numeric(data$htlnoroomrate_60d)    
data$zonenoroomrate_3d <- as.numeric(data$zonenoroomrate_3d)

# # string -> integer 
data$htlnoroomorders_7d <- as.integer(data$htlnoroomorders_7d)
data$last3droom_sht <- as.integer(data$last3droom_sht)
data$last7dnoroom <- as.integer(data$last7dnoroom)    
data$last7droom_sht <- as.integer(data$last7droom_sht)  
data$htlorders_60d <- as.integer(data$htlorders_60d)
data$roomquantity <- as.integer(data$roomquantity)

data$canuserooms <- as.integer(data$canuserooms)
data$firstunitprice <- as.integer(data$firstunitprice)
data$shtorders_60d <- as.integer(data$shtorders_60d)

# # string -> character 
data$ordhour <- as.character(data$ordhour)
data$star <- as.character(data$star)
data$goldstar <- as.character(data$goldstar)
data$recommendlevel <- as.character(data$recommendlevel)

data$ordprior <- as.integer(data$ordprior)
data <- mutate(data, ordprior2 = ifelse(ordprior>60,61,ordprior))

# transform metrics
data$last3droompct <- ifelse(data$last3droompct > 1, 1, data$last3droompct)
# data$last3droom_sht <- ifelse(data$last3droom_sht >= 7 , 7, data$last3droom_sht)
# data$bin_hasreserved <- ifelse(data$canuserooms>0,1,0)
# data$bin_roomhasnoroom <- ifelse(data$last7dnoroom>0,1,0) # 看酒店和房型历史是否出现过满房订单；
data <- mutate(data, canuserooms=ifelse(canuserooms<0,-1, canuserooms)
               , htlnoroomrate_7d = ifelse(htlnoroomrate_7d>1,1,htlnoroomrate_7d)
               , star=ifelse(star<0,0,star)
               , goldstar=ifelse(goldstar<0,2,goldstar)
               , price=ifelse(firstunitprice>1200,12,0)
               , price=ifelse(firstunitprice>800 & firstunitprice<=1200 ,8,price)
               , price=ifelse(firstunitprice>600 & firstunitprice<=800 ,6,price)
               , price=ifelse(firstunitprice>400 & firstunitprice<=600,4,price)
               , price=ifelse(firstunitprice>300 & firstunitprice<=400,3,price)
               , price=ifelse(firstunitprice>200 & firstunitprice<=300,2,price)
               , price=ifelse(firstunitprice<=200,1,price)
)
data[is.na(data)] <- -1         # # 把一些因子的NA变成 -1

# data%>%count(canuserooms,isfullroom)%>%mutate(pct=n/sum(n))

# transform onehot-encode
ohe_feats = c('goldstar', 'star', 'recommendlevel', 'ordhour','freesale')
excluedvars = c('orderid','cityid','score','price','cancelreason')
# paste0(ohe_feats, collapse ='+')
dummies <- dummyVars(~ goldstar+star+recommendlevel+ordhour, data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)
df_all_X <- df_all_combined[,-c(which(colnames(df_all_combined) %in% excluedvars))]

# 查看历史出现通入住天的满房的情况下的拒单率
# quantile(df_all_X$htlnoroomorders_7d,seq(from=0, to=1,by=0.01))
# data%>%count(htlnoroomorders_7d>0,isfullroom)%>%mutate(pct=n/sum(n))
# htlnoroomorders_7d > 0 isfullroom      n       pct
# 1                  FALSE          0 187461 0.8851853
# 2                  FALSE          1  24315 0.1148147
# 3                   TRUE          0   5847 0.6703738
# 4                   TRUE          1   2875 0.3296262

# split train & test set
train <- createDataPartition(df_all_X$isfullroom, p = 0.7, list = F)
SHTtrain <- df_all_X[train,]
SHTtest <- df_all_X[-train,]

# 查看所有变量的直方图
# d <- reshape2::melt(data[,-c(1,27,29)])
# ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
train_x <- SHTtrain[,-24]
train_y <- SHTtrain[,'isfullroom']
test_x <- SHTtest[,-24]
test_y <- SHTtest[,'isfullroom']

virtualroom.sht2 <- xgboost(data = data.matrix(train_x), 
                                  label = train_y, 
                                  eta = 0.1,
                                  max_depth = 10, 
                                  nround=100, 
                                  subsample = 0.5,
                                  colsample_bytree = 0.5,
                                  seed = 1,
                                  objective = "binary:logistic",
                                  eval_metric = "auc"
                                  
)

# xgboost with cv；
virtualroom.sht3 <- xgb.cv(data = data.matrix(train_x), 
                            label = train_y, 
                            eta = 0.1,
                            max_depth = 5, 
                            nround=7, 
                            subsample = 0.5,
                            colsample_bytree = 0.5,
                            seed = 1,
                            objective = "binary:logistic",
                            eval_metric = "auc",
                            nfold = 3
                            
)

importance_matrix <- xgb.importance(model = virtualroom.sht2)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

pred_test_y <- predict(virtualroom.sht3, data.matrix(test_x))

# retrain with new data ,first with gbm;
mono <- cor(train_x, train_y, method = 'spearman') / abs(cor(train_x, train_y, method = 'spearman'))

virtualroom.sht <- gbm.fit(x= train_x, y = train_y
               ,distribution = "adaboost"               
               ,n.trees = 2500               
               ,shrinkage = 0.02               
               ,interaction.depth = 3               
               ,bag.fraction = 0.6             
               ,train.fraction = 0.5
               ,keep.data = F
             #  ,var.monotone = mono
)

# png('./featureeffect.png', width = 1000, height = 1000)
par(mfrow = c(5,5), mar = c(1, 1, 1, 1), pty = "s")
for (i in 1:24)
{
  plot.gbm(virtualroom.sht, 19, best.virtualroom.sht);
}

# dev.off()

best.virtualroom.sht <- gbm.perf(virtualroom.sht, method='test')

imp <- summary(virtualroom.sht, n.trees = best.virtualroom.sht, plotit = T)
barplot(imp[, 2], col = gray(0:(ncol(train_x)) / (ncol(train_x))),
        names.arg = imp[, 1], yaxt = "n", cex.names = 1)
title(main = list("Importance Rank of Predictors", font = 4, cex = 1.5))

# load(file = "./ProdScripts/ReservedRoomForVendor.RData")
# Roc curve
SHTtest$pred2 <- predict(virtualroom.sht, newdata= SHTtest, n.trees = best.virtualroom.sht, type="response")
SHTtest$pred <- pred_test_y

SHTtest <- mutate(SHTtest, gap = pred2-pred)
p <-ggplot()+geom_density(data=SHTtest, aes(x = pred ), colour ="blue")
p+ geom_density(data=SHTtest, aes(x = pred2 ), colour ="red")

# SHTtest$pred2 <- predict(virtualroom.sht, newdata= SHTtest, n.trees = best.virtualroom.sht, type="response")
# SHTtest$pred <- ifelse(SHTtest$htlnoroomorders_7d > 5, 1, SHTtest$pred)
# SHTtest$pred <- ifelse(SHTtest$last7dnoroom >= 1, 1, SHTtest$pred)  # last7d无房的房量高于0
gbm.roc.area(SHTtest$isfullroom, SHTtest$pred2)  # 0.7816964   0.7621193
# gbm.roc.area(SHTtest$isfullroom, SHTtest$score)  # 0.6940663

# 指标评估  ,precision & recall的评估结果；
obj <- SHTtest$isfullroom
pred.gbm <- SHTtest$pred2
accuracy = recall = specificity = sensitivity = NULL
cutoffPoint=seq(0.01,1,0.001)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.gbm >=c1)
  specificity[i] <- sum((pred==1) & (obj == 1))/sum(pred == 1)
  sensitivity[i] <- sum((pred==1) & (obj==1))/sum(obj==1)
  recall[i] <- sum((pred==0) & (obj==0))/sum(obj==0)
  accuracy[i] <- sum((pred==0) & (obj==0))/sum(pred==0)
}
roc.result <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity)
roc.xg <- roc.result

p <-ggplot()+geom_line(data=roc.gbm,aes(x=recall, y = accuracy, fill = c()), colour ="blue")
p+geom_line(data=roc.xg,aes(x=recall, y = accuracy), colour = "red")

save(virtualroom.sht,best.virtualroom.sht,roc.xg,roc.gbm,virtualroom.sht2,file="./testScripts/ReservedRoomForVendor_0125.RData")












