library(dplyr)
library(networkD3)

# 读取原数据；
network <- read.table('./data/NewBI.txt'
                      , sep='\t'
                      , header = T
                      , stringsAsFactors = F
                      , encoding = "UTF8" 
                      , fileEncoding = "UTF8"
)

Links <- network

Nodes <- read.table('./data/酒店BI社交网络图 (Nodes)_无向节点.csv'
                      , sep=','
                      , header = T
                      , stringsAsFactors = F
                      , encoding = "UTF8" 
)

# 出发和目的地join
networkall <- network%>%left_join(network,network, by = c("sourceName" = "targetName", "targetName" = "sourceName" ) ,copy = T)
names(Links)
Links[,1]%in%Links[,2]

#  单向评分的节点关系；
networksingle <- filter(networkall,is.na(weight.y) == T)
#  双向的评分节点关系；
networkdouble <- mutate(filter(networkall,is.na(weight.y) == F)
                        ,sourcetgt = paste(sourceName, targetName)
                        ,tgtsource = paste(targetName, sourceName)
                       )


write.table(networkdouble,file = './data/networkdouble.txt',row.names = F, fileEncoding = 'UTF8')
# 查看 tgtsource 里面是否存在了 sourcetgt；

# 读取原数据；
networknetworkdouble2 <- read.table('./data/networkdouble.txt'
                      , sep='\t'
                      , header = T
                      , stringsAsFactors = F
                      , encoding = "UTF8" 
)

#  拼接两部分数据；
networklast <- as.data.frame(rbind(networksingle, networknetworkdouble2[,1:12]))
network <- networklast[,1:7]  # 取出前2名；
names(network) <- names

#  名字和部门对应；
namedept <- distinct(network[,c('targetName','dep')])

# 生成随机的颜色；
colormap <- as.data.frame(cbind(dep=unique(data$dep),color=rainbow(length(unique(data$dep)))),stringsAsFactors = F)

# 切换dept，从source变成target；
network$dep <- left_join(network, namedept,by = c("targetName"="targetName"))$dep  #  替换部门，从source变成target;

save(Links,colormap,Nodes,namedept,file ='./conf/utils.RData')





