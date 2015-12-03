library(dplyr)
library(networkD3)
library(stringi)
## 读取文件并转化编码
nodes = read.csv("data/Nodes_BI_1123.csv",
                 encoding = "GBK",
                 stringsAsFactors = F)
nodes$Label = stri_enc_toutf8(nodes$Label)

edges =  read.csv("data/Edges_BI.csv")
tolower(names(edges))->names(edges)
tolower(names(nodes))->names(nodes)

### 计算每个节点的链接数量作为点的大小
num1 = edges %>% group_by(source) %>% summarise(num1 = n())
num2 = edges %>% group_by(target) %>% summarise(num2 = n())
nodes$id = nodes$id -1
nodes = nodes %>% left_join(num1,by=c("id"="source")) %>%
        left_join(num2, by = c("id" = "target")) 
nodes[is.na(nodes)] = 0
nodes = mutate(nodes, size = num1 + num2 + 1)
nodes$group =sample(5,171,replace = T)


edges$source = edges$source-1
edges$target = edges$target - 1
## 保存文件
save(nodes,edges,file = data/network.Rdata)


## 本地调试
## MyClickScript是一个定义好的JS脚本,作为点击出发后的操作
MyClickScript <- 'alert("You clicked " + d.name + " which is in row " +
       (d.index + 1) +  " of your original R data frame");'
forceNetwork(Links = edges, Nodes = nodes, Source = "source",
             Target = "target", Value = "weight", NodeID = "label",
             Group = "group", Nodesize = "size",opacity = 1, zoom = T,
             fontFamily="微软雅黑",
             clickAction =MyClickScript)
