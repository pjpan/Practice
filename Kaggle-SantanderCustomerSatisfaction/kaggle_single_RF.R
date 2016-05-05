library(randomForest)
df.train_ <- df_train_x
df.train_$TARGET <- as.factor(df.train_$TARGET)
single.rf <- randomForest(TARGET~ .,
                          data =df.train_,
                          keep.forest = T,
                          importance = T, 
                          proximity = F,
                          ntree = 500,
                          na.action = na.omit)


save(single_rf, 'cache/single.rf.model.RData')