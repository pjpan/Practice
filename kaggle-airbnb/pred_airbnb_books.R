#  import data
library(data.table)
library(dplyr)
library(ggplot2)
library(xgboost)
# country info
countries <- fread(input = './countries.csv', stringsAsFactors = F)
# use behaviour
sessions <- fread(input = './sessions.csv', stringsAsFactors = F)
# trainnning data
trainuse <- fread(input = './train_users_2.csv', stringsAsFactors = F)
#test data
testuse <- fread(input = './test_users.csv', stringsAsFactors = F)
# 
age_gender_bkts <- fread(input = './age_gender_bkts.csv', stringsAsFactors = F)

trainuse%>%group_by(gender)%>%count(gender)%>%mutate(pct=n/sum(n))

testuse%>%group_by(gender)%>%count(gender)%>%mutate(pct=n/sum(n))

# 统计出每个变量的分布
d <- melt(trainuse)
ggplot(d,aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()





