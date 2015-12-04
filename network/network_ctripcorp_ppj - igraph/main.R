network%>%left_join(network,network, by = c("sourceName" = "targetName", "targetName" = "sourceName" ) ,copy = F)

network <- read.table('./data/network.txt'
                   , sep=' '
                   , header = T
                   , stringsAsFactors = F
                   )

Nodes <- read.table('./data/nodes.txt'
                    , sep=' '
                    , header = T
                    , stringsAsFactors = F
)

# 存储结果；
save(network,Nodes,colormap,file = './conf/utils.RData')

network <- Links
#  名字和部门对应；
namedept <- distinct(network[,c('targetName','dep')])

# 生成随机的颜色；
colormap <- as.data.frame(cbind(dep=unique(data$dep),color=rainbow(length(unique(data$dep)))),stringsAsFactors = F)

# 切换dept，从source变成target；
network$dep <- left_join(network, namedept,by = c("targetName"="targetName"))$dep  #  替换部门，从source变成target;

save(network,colormap,Nodes,file ='./conf/utils.RData')

g <- make_ring(10)
vertex_attr(g) <- list(name = LETTERS[1:10],
                       color = rep("yellow", gorder(g)))
vertex_attr(g, "label") <- V(g)$name
g
plot(g)
