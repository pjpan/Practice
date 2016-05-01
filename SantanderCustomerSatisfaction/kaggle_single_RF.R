library(randomForest)
df.train_ <- df.train
df.train_$TARGET <- as.factor(df.train_$TARGET)
single.rf <- randomForest(TARGET~ .,
                          data =df.train_,
                          keep.forest = T,
                          importance = T, 
                          proximity = F,
                          ntree = 500,
                          na.action = na.omit)

sapply(X = df.train, FUN = summary)

quantile(df.train_$imp_op_var40_ult1, seq(from= 0 ,to = 1,by=0.01))

df.train_%>%count(imp_op_var40_ult1>700, TARGET)%>%mutate(pct = n/sum(n))
