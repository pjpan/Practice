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

virtualroom.sht <- xgboost(data = data.matrix(train_x), 
                                  label = train_y, 
                                  eta = 0.1,
                                  max_depth = 5, 
                                  nround=100, 
                                  subsample = 0.5,
                                  colsample_bytree = 0.5,
                                  seed = 1,
                                  objective = "binary:logistic"
)

pred_test_y <- predict(virtualroom.sht, data.matrix(test_x))

confusiontab <- as.data.frame(cbind(pred= pred_test_y, truth= test_y))

summary(pred_test_y)
# retrain with new data ,first with gbm;
virtualroom.sht <- gbm(isfullroom ~
                ordprior
               +factor(goldstar)
               +factor(recommendlevel)
               +factor(star)
               +factor(ordhour)
              # +factor(freesale)   # 新增变量,重要性不高
             #  +factor(price)    # 价格段
               +firstunitprice   #  稀疏性
               +canuserooms        # 现付是否有保留房
               +htlorders_60d
               +shtnoroomrate_60d
               +zonenoroomrate_3d
               +gintense
               +gcity
               +gzone
             #  +last3droom_sht   #  与last7droom_sht重复
               +cityi
               +htlnoroomorders_7d   # 37.0360131
               +zonestari
               +zonei
               +last7droom_sht
             #  +intense    # 与zonestari的相关度太高；
               +last3droompct    # 同房型的入住率
             #  +htlnoroomrate_7d
               +htlnoroomrate_60d  # 11.7423540
               +noroomrate_60d
               ,data = SHTtrain
               ,distribution = "adaboost"               
               ,n.trees = 3000               
               ,shrinkage = 0.02               
               ,interaction.depth = 3               
               ,bag.fraction = 0.6             
               ,train.fraction = 0.5              
               ,cv.folds = 3
               ,keep.data = F
               # ,n.minobsinnode = 5
)

summary(virtualroom.sht)
best.virtualroom.sht <- gbm.perf(gbm.sht, method='cv')

# load(file = "./ProdScripts/ReservedRoomForVendor.RData")
# Roc curve
SHTtest$pred <- predict(virtualroom.sht, newdata= SHTtest, n.trees = best.virtualroom.sht, type="response")

# SHTtest$pred2 <- predict(virtualroom.sht, newdata= SHTtest, n.trees = best.virtualroom.sht, type="response")
# SHTtest$pred <- ifelse(SHTtest$htlnoroomorders_7d > 5, 1, SHTtest$pred)
# SHTtest$pred <- ifelse(SHTtest$last7dnoroom >= 1, 1, SHTtest$pred)  # last7d无房的房量高于0
gbm.roc.area(SHTtest$isfullroom, SHTtest$pred)  # 0.7475905
# gbm.roc.area(SHTtest$isfullroom, SHTtest$score)  # 0.6940663


# 指标评估  ,precision & recall的评估结果；
obj <- SHTtest$isfullroom
pred.gbm <- SHTtest$pred
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
ggplot(data = roc.result)+geom_line(aes(x= recall , y=  accuracy))
View(roc.result)

rocv1.0<- roc.result
rocv2.0<- roc.result
rocv3.0<- roc.result
rocv4.0<- roc.result  # add price feature
rocv5.0<- roc.result  # add price feature,delete noroomrate_7d

View(rocv3.0)
View(cbind(rocv1.0,rocv5.0))

p <-ggplot()+geom_line(data=rocv1.0,aes(x=recall, y = accuracy), colour ="blue")
p+geom_line(data=rocv5.0,aes(x=recall, y = accuracy), colour = "black")+geom_line(data=rocv4.0,aes(x=recall, y = accuracy), colour = "green")

save(virtualroom.sht,best.virtualroom.sht,file="./ProdScripts/ReservedRoomForVendor.RData")













