network%>%left_join(network,network, by = c("sourceName" = "targetName", "targetName" = "sourceName" ) ,copy = F)


network2 <- mutate(network
                   ,
                   )



data <- read.table('./data/NewBI.txt'
                   , sep='\t'
                   , header = T
                   , stringsAsFactors = F
                   , encoding = "UTF8" 
                   )


network <- data
#  名字和部门对应；
namedept <- distinct(network[,c('targetName','dep')])

# 生成随机的颜色；
colormap <- as.data.frame(cbind(dep=unique(data$dep),color=rainbow(length(unique(data$dep)))),stringsAsFactors = F)

# 切换dept，从source变成target；
network$dep <- left_join(network, namedept,by = c("targetName"="targetName"))$dep  #  替换部门，从source变成target;
save(network,colormap,mapp,file ='./conf/utils.RData')

