library(xgboost)
library(dplyr)
library(DT)
library(lubridate)
library(GGally)
library(ggplot2)

# metric ，每个样本的价格偏差；
evalute_rmse <- function( pred , obj){
  n <- nrow(pred)
  return(round(sum(abs(pred-obj), na.rm = T)/n,0))
}

# load data
car_license <- read.csv(file = 'car_license.csv', stringsAsFactors = F, header = T)
datatable(car_license)
View(car_license)
# transform feature
# car_license <- mutate(car_license, time = format(as.Date(time), "%Y-%m"))

# split train and test data
car_license_train <- car_license[1:28,]
car_license_test <- car_license[29:31,]
# 查看每个变量与目标变量的分布
# png(file="feature_distribution.png", bg="transparent")
# ggpairs(car_license)
# dev.off()
# 训练模型
# names(car_license_train)
# 时间系列
# p <- ggplot( data = car_license, aes(x = time, y = price_avg, color = factor(quarter)))+geom_point()+ylim(c(0,87000))+ggtitle("time with price")
# p <- p + geom_point(data = car_license, aes(x = time, y = alarm_price))
# p
# 预测结果；
# 线性回归
# lm_model <- lm( price_avg ~ people+car_license+licenseperpeople+alarm_price+license_MoM+People_MoM+lastquarter_price+month+quarter
#                 ,data = car_license_train)
# 
# summary(lm_model)
# 
# # 查看多重共线性
# car::vif(lm_model)

# ######## 看一下拍牌人数和中标率和高于警示价的关系 Begin#############  #
# 查看拍牌人数和超出警示价的关系
ggplot(data = car_license) + geom_line(aes(people, price_min))
ggplot() + geom_line(data = car_license, aes(lastmonth_price, price_min), color = "red")

#  ####### 用GLM训练，直接预测最后可成交的价格#########
x_lable <- c("alarm_price", "lastmonth_price", "people", "car_license","licenseperpeople")
train_x <- as.matrix(car_license_train[ ,x_lable])
train_y <- as.matrix(car_license_train$price_min)
glm_cv <- glmnet::cv.glmnet(x = train_x, y = train_y, family = "gaussian", alpha = 0.001, nfold = 3)
# plot(glm_cv)
# coef(glm_cv)
save(glm_cv,file = './best_cv_v2.RData')
# 提取最终模型
model_final <- glm_cv$glmnet.fit
# 取得简洁模型的参数系数
model_coef <- coef(glm_cv$glmnet.fit, s = glm_cv$lambda.1se)
# 取得原始模型的参数系数
all_coef <- coef(glm_cv$glmnet.fit, s =  min(model_final$lambda))

# 预测最后的结果；
test_x <- as.matrix(car_license_test[ ,x_lable])
car_license_test$pred <- round(predict(glm_cv, test_x, s=glm_cv$lambda.1se)/100,0)*100
# 查看一下预测结果；
datatable(select(car_license_test,price_min, pred, time))
car_license_test$price_gap <- car_license_test$pred-car_license_test$price_min
#查看预测的偏差；
p <- ggplot(data = car_license_test) + geom_point(aes(time,pred))+geom_line(aes(time,pred, group=1))+geom_point(aes(time,price_min),color = "red")
p + geom_line(aes(time,price_min, group =1),color = "red")
summary(car_license_test$price_gap)
# 每个平均误差
evalute_rmse(car_license_test$pred, car_license_test$price_min)
ggplot(data = car_license_test) + geom_point(aes(time, pred)) + geom_line(aes(time,pred, group =1))
#  ####### 用GLM训练，直接预测最后可成交的价格#########

# 利用xgboost进行预测
param = list(
  objective = "objective",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "mae"                                                          # evaluation/loss metric
)

xgb.cv()


