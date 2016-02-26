# version 2.0 ,add price,factor(star),delete rules
# old: 0.057436224  new：0.06423432
# calcReservedRoomForVendor("1565187397,0.0826599970459938,0.0,0.0,0.0,0.0826599970459938,0.0,0.0,-1,0.0, 0, -1.0, 0, 0, 0.1450381679389313, 131, 0.11627906976744186, 0.024390243902439025, 41, 0.15789473684210525, 16,0, 3,2,3,25, F, -1, 596.0")

calcReservedRoomForVendor <- function(inParam)
{
  # package申明；
  if(!require(gbm)) install.packages("gbm")
  if(!require(dplyr)) install.packages("dplyr")
  #package的引用
  library(gbm)
  library(dplyr)
  options(stringsAsFactors = FALSE) #  char类型不要自动转化成 factor
  # setwd(getwd())
  # rm(list=ls())
  
  # debug 
  load("/home/pjpan/shtmodel/ProdScripts/ReservedRoomForVendor.RData")
  
  # load data-Versions1
  if(is.character(inParam)) 
  {
    rawdata <- strsplit(c(inParam), ",")
    data <- as.data.frame(do.call(rbind, rawdata))
  }
  
  if(length(data) == 29)
  {
    names(data) <- c(
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
    # View(data)
    # transform the data
    
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
    
    data$freesale <- as.character(data$freesale)
    
    # # string -> integer 
    data$htlnoroomorders_7d <- as.integer(data$htlnoroomorders_7d)
    data$last3droom_sht <- as.integer(data$last3droom_sht)
    data$last7dnoroom <- as.integer(data$last7dnoroom)    
    data$last7droom_sht <- as.integer(data$last7droom_sht)  
    data$htlorders_60d <- as.integer(data$htlorders_60d)
    data$roomquantity <- as.integer(data$roomquantity)
    
    data$canuserooms <- as.integer(data$canuserooms)
    data$firstunitprice <- as.integer(data$firstunitprice)
    
    # # string -> factor 
    data$ordhour <- as.integer(data$ordhour)
    data$star <- as.integer(data$star)
    data$goldstar <- as.integer(data$goldstar)
    data$recommendlevel <- as.integer(data$recommendlevel)
    data$ordprior <- as.integer(data$ordprior)
    data$shtorders_60d <- as.integer(data$shtorders_60d)
    
    # transform metrics
    data$recommendlevel <- ifelse(data$recommendlevel>= 11 | data$recommendlevel <= 0 | is.na(data$recommendlevel), -1 
                                  , data$recommendlevel)  #  超过10 和小于0的看做一类别，跟其他类别的量级持平；

    data$last3droompct <- ifelse(data$last3droompct > 1, 1, data$last3droompct)
    # data$last3droom_sht <- ifelse(data$last3droom_sht >= 7 , 7, data$last3droom_sht)
    # data$bin_hasreserved <- ifelse(data$canuserooms>0,1,0)
    # data$bin_roomhasnoroom <- ifelse(data$last7dnoroom>0,1,0) # 看酒店和房型历史是否出现过满房订单；
    data[is.na(data)] <- -1         # # 把一些因子的NA变成 -1
    

    data <- mutate(data, ifelse(canuserooms<0,-1, canuserooms)
                   , htlnoroomrate_7d = ifelse(htlnoroomrate_7d>1 , 1 ,htlnoroomrate_7d)
                   , star=ifelse(star<0, 0, star)
                   , goldstar=ifelse(goldstar<0, 2, goldstar)
                   , price=ifelse(firstunitprice>1200, 12, 0)
                   , price=ifelse(firstunitprice>800 & firstunitprice<=1200 ,8,price)
                   , price=ifelse(firstunitprice>600 & firstunitprice<=800 ,6,price)
                   , price=ifelse(firstunitprice>400 & firstunitprice<=600,4,price)
                   , price=ifelse(firstunitprice>300 & firstunitprice<=400,3,price)
                   , price=ifelse(firstunitprice>200 & firstunitprice<=300,2,price)
                   , price=ifelse(firstunitprice<=200,1,price)
    )    
    
    # 预测最后的的结果 ，只用一种模型来进行预测；
    data$pred <- tryCatch(predict(virtualroom.sht, newdata= data, n.trees = best.virtualroom.sht, type="response") ,warning = 'predict error')
  #  data$pred <- ifelse(data$htlnoroomorders_7d>=1, 1, data$pred)  # 同母酒店的现、预付酒店last7d无房的房量高于0 
  #  data$pred <- ifelse(data$last7dnoroom>=1, 1, data$pred)  # 同子酒店的last7d无房的房量高于0
    
    return(list(orderid= data$orderid, score= data$pred))
    
  }  else  {
    return(list(orderid= -1, score= 99))
  }
  
}