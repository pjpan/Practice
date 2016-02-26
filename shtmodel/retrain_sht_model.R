library(gbm)
library(RJDBC)
library(rJava)
setwd('/home/pjpan')
sqljar <-c("./Jar/sqljdbc4.jar")
sqldriveclass<-c("com.microsoft.sqlserver.jdbc.SQLServerDriver")
drv <- JDBC(driverClass=sqldriveclass,sqljar)
sqldwconn <- dbConnect(drv, 'jdbc:sqlserver://10.8.91.147:28747', 'un_pjpan','xxxx')

# import data
sql<-paste0("select * from bi_htl..ppj_virtualroomlog_sht_training_20150608")  
res <- dbSendQuery(sqldwconn, sql)
shtraw <- fetch(res,n=-1)

shtparameter <- strsplit(shtraw$ModelInputParameter,',')
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
  "roomquantity"
)

data <- cbind(shtdata, cancelreason = shtraw$CancelReason, score= shtraw$Score, freesale = shtraw$Freesale,canuserooms= shtraw$CanUseRooms, firstunitprice= shtraw$FirstUnitPrice)
View(data)

# # string -> numeric
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

# # string -> factor 
data$ordhour <- as.integer(data$ordhour)
data$star <- as.integer(data$star)
data$goldstar <- as.integer(data$goldstar)
data$recommendlevel <- as.integer(data$recommendlevel)
data$ordprior <- as.integer(data$ordprior)
data$shtorders_60d <- as.integer(data$shtorders_60d)

data[is.na(data)] <- 0 
# transform metrics
# data$ordroomnum <- ifelse(data$ordroomnum >= 6, -1,data$ordroomnum)  # 超过6的归结为一类，与其他的category来进行平衡；

data$recommendlevel <- ifelse(data$recommendlevel>= 8 | data$recommendlevel <= 0 | is.na(data$recommendlevel), -1 
                              , data$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；
# data$level <- ifelse(data$level>= 3 & data$level <=4, -2, data$level)  # 中间档的酒店订单量太小，合并

data$star <- ifelse(data$star<=3, 3, data$star)  # 供应商高星级的酒店居多

# data$ordprior <- ifelse(data$ordprior>=1 &　data$ordprior<=7,7
#                         ,ifelse(data$ordprior>7 & data$ordprior <=14,14
#                                 ,ifelse(data$ordprior>14 & data$ordprior <=21,21
#                                         ,ifelse(data$ordprior>21 & data$ordprior <=30,30,
#                                                 ifelse(data$ordprior >30, 35, data$ordprior)
#                                         ))))

# data$last3droompct <- ifelse(data$roomquantity > 0, data$last3droom/data$roomquantity, -1)
data$last3droompct <- ifelse(data$last3droompct > 1, 1, data$last3droompct)
data$last3droom_sht <- ifelse(data$last3droom_sht >= 20 , 20, data$last3droom_sht)
data$ordhour <- ifelse(data$ordhour <= 7 , 7, data$ordhour)

# data$htlnoroomorders_7d <- ifelse(data$htlnoroomorders_7d >= 9 , 9, data$htlnoroomorders_7d)  #  同子酒店的无房订单量
data[is.na(data)] <- -1         # # 把一些因子的NA变成 -1

data$isfullroom <- ifelse(data$cancelreason=='NOROOM',1,0)

# 看酒店和房型历史是否出现过满房订单；
data$ishtlhasnoroom <- ifelse(data$htlnoroomrate_7d>0,1,0) 
data$isroomhasnoroom <- ifelse(data$last7dnoroom>0,1,0)

table(data$ishtlhasnoroom,data$isfullroom)

# # View(dplyr::filter(data,shtorders_60d>3000))
# table(data$ordprior, data$isfullroom)

# split train & test set
train <- rbinom(dim(data)[[1]],size=1 ,prob = 0.7)==1
SHTtrain <- data[train,]
SHTtest <- data[!train,]

# 查看所有变量的直方图
# d <- reshape2::melt(data[,-c(1,27,29)])
# ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()

# rf.sht <- randomForest::randomForest(factor(isfullroom) ~ 
#                         cityi
#                        +gcity
#                        +gzone
#                        +intense
#                        +htlnoroomrate_7d
#                        +htlnoroomorders_7d
#                        +star
#                        +ordprior
#                        +recommendlevel
#                        +goldstar
#                        +roomquantity
#                        +I(htlnoroomrate_7d>0)
#                        ,data= shttrain
#                        ,importance=TRUE
#                        ,ntree=500
#                        ,proximity=F
#                        ,keep.forest=T
#                        )


# randomForest::importance(rf.sht)
# 
# pred <- predict(rf.sht, shttest,type = 'response')
# 
# View(pred)
# 
# shttest$pred.fullroom <- ifelse(pred[,1]>=0.8,0,1)
# shttest$pred.fullroom <- ifelse(pred[,2]>=0.8,1,0)
# 
# table(shttest$pred.fullroom, shttest$isfullroom)

# retrain with new data
gbm.sht <- gbm(isfullroom ~
                ordprior
               +factor(goldstar)
               +recommendlevel
               +factor(star)
               +ordhour
               +factor(freesale)   # 新增变量
               +canuserooms        # 现付是否有保留房
               +factor(ishtlhasnoroom)
               +factor(isroomhasnoroom)
               +htlorders_60d
               +shtnoroomrate_60d
               +zonenoroomrate_3d
               +gintense
               +gcity
               +gzone
               +last3droom_sht
               +cityi
               +htlnoroomorders_7d   # 37.0360131
               +zonestari
               +zonei
               +last7droom_sht
               +intense
               +last3droompct    # 同房型的入住率
               +htlnoroomrate_7d
               +htlnoroomrate_60d  # 11.7423540
               +noroomrate_60d
               ,data = SHTtrain
               ,distribution = "adaboost"               
               ,n.trees = 500               
               ,shrinkage = 0.02               
               ,interaction.depth = 2               
               ,bag.fraction = 0.6             
               ,train.fraction = 0.5              
               ,cv.folds = 3
               ,keep.data = FALSE
               # ,n.minobsinnode = 5
)

summary(gbm.sht)
best.raw <- gbm.perf(gbm.sht, method='cv')
# Roc curve
SHTtest$pred <- predict(gbm.sht, newdata= SHTtest, n.trees = best.raw, type="response")
# SHTtest$pred <- ifelse(SHTtest$htlnoroomorders_7d > 5, 1, SHTtest$pred)
# SHTtest$pred <- ifelse(SHTtest$last7dnoroom >= 1, 1, SHTtest$pred)  # last7d无房的房量高于0
gbm.roc.area(SHTtest$isfullroom, SHTtest$pred)  # 0.7211884

# write.csv(SHTtest, file = "D:/project/【HTL】Offline虚拟保留房模型/供应商模型/sht.version.csv")
# misclass <- filter(SHTtest, isfullroom ==1, pred<=0.03)

# 指标评估  ,precision & recall的评估结果；
pred.gbm <- SHTtest$pred
accuracy = recall = specificity = sensitivity = NULL
cutoffPoint=seq(0.01,1,0.001)
for (i in 1:length(cutoffPoint))
{
  c1 <- cutoffPoint[i]
  pred <- 1*(pred.gbm >=c1)
  specificity[i] <- sum((pred==1) & (SHTtest$isfullroom == 1))/sum(pred == 1)
  sensitivity[i] <- sum((pred==1) & (SHTtest$isfullroom==1))/sum(SHTtest$isfullroom==1)
  recall[i] <- sum((pred==0) & (SHTtest$isfullroom==0))/sum(SHTtest$isfullroom==0)
  accuracy[i] <- sum((pred==0) & (SHTtest$isfullroom==0))/sum(pred==0)
}
roc.result <- data.frame(cutoffPoint, accuracy, recall, specificity, sensitivity)
ggplot(data = roc.result)+geom_line(aes(x= recall , y=  accuracy))
View(roc.result)

View(filter(data,shtorders_60d>4500))
















