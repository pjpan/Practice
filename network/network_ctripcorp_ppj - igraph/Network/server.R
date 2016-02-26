library(shiny)
library(igraph)
library(dplyr)

# Define server logic required to summarize and view the selected dataset
load('./conf/utils.RData')

# filter(Nodes,Label =='赵华')$PageRank   Nodes[109,11] <- 0.01124424

groupnum <- length(unique(Nodes$Modularity.Class))   # 看一下社团类别
groupcolor <- as.data.frame(cbind(color=rainbow(groupnum),group = as.integer(unique(Nodes$Modularity.Class))),stringsAsFactors = F) # 生成不同社团的随机颜色；    

shinyServer(function(input, output) {
  
  # Generate a summary of the dataset
  output$Plot <- renderPlot({
    
    if(gsub(" ","",input$name) == "all"){
      data <- network
      data <- data %>% 
                arrange(desc(weight)) %>% 
              #  filter(weight>=5) %>%
                head(n = input$obs)
    
    }else{
      data <- subset(network, network$sourceName == gsub(" ","",input$name)|network$targetName == gsub(" ","",input$name))  # 单项节点，非双向的；                  
      data <- data %>% arrange(desc(weight)) %>%
                           #    filter(weight >=5)%>%
                                 head(n = input$obs)
    }
        
    # debug
    # data <- network

    # 剔除掉那些没有存在的节点信息；
    Nodes <- Nodes
    allnodes <- union(data$sourceName,data$targetName)
    Nodes$isexists <- Nodes$Label %in% allnodes
    Nodes <- Nodes %>% filter(isexists == TRUE)%>% select(Label:Modularity.Class)%>% mutate(Modularity.Class = as.character(Modularity.Class))
    Nodes <- mutate(Nodes, color = left_join(Nodes, groupcolor, by = c("Modularity.Class"="group"))$color) # 给每个节点赋颜色属性；
    
    data <- network  
    g <- graph_from_data_frame(data, directed= F , vertices=Nodes)
    
    
    #  改进点：
    #  1、线的宽度表示连接的大小；
    #  2、放大图片；
    #  3、改成无方向的结果，输出的table里面只显示targetname结果；
    #  3、同一个部分是同一个颜色；
    #  4、圆圈的大小目前是degree，改成pagerank计算出来的权重；
    #  5、圆圈大小用 pagerank来表示，圆圈的颜色用Modularity Class来表示；
    # 
        
    try(plot(g,
        main='Ctrip Organizational network Demo',
        vertex.size = 10*((vertex_attr(g,"PageRank")-min(vertex_attr(g,"PageRank")))/((max(vertex_attr(g,"PageRank")))-(min(vertex_attr(g,"PageRank"))))),   # 节点的大小用pagerank来衡量；
        vertex.shape='circle',                        #   节点的形状；
        vertex.label=V(g)$Label,
        vertex.label.dist=0,                      # puts the name labels slightly off the dots
        vertex.label.cex=1,                        #   节点字体的大小；
        vertex.label.color='black',                  #   节点字体颜色
        vertex.color = vertex_attr(g,"color"),                  # mapp(degree(g),c(1,20)),  E(g)$color  #  node的颜色；原来的结果:
        vertex.label.font = 2,
        layout=layout.fruchterman.reingold,          # layout.auto 自适应,尽量不重叠
        edge.color = 'black',                      # 线的颜色
        edge.arrow.size = 0,                          #  调整有方向箭头的大小
        edge.width = 1.5,                     #  调整线的宽度；E(g)$weight
        asp = 0
    #    edge.label = E(g)$weight,                    # 线的标签显示什么；
        ), silent =T)
    }
     ,width = 1000 , height = 800
    )

  # Show the first "n" observations
  output$view <- renderTable({
    if(gsub(" ","",input$name) == "all"){
      data <- network
      data <- data %>% 
        arrange(desc(weight)) %>% 
      #  filter(weight>=5) %>%
        head(n = input$obs) %>%
        select(sourceName,targetName,weight)
      
    }else{
      data <- subset(network, network$sourceName == gsub(" ","",input$name)|network$targetName == gsub(" ","",input$name))  # 单项节点，非双向的；                  
      data <- data %>% arrange(desc(weight))%>%
                        head(n = input$obs)
      
      
      # 调整一下位置；如果和输入的source名字不同，把source和target换位置；
      for(i in 1:nrow(data)) 
      {
        if(data[i,"sourceName"]==gsub(" ","",input$name)){
          data$target[i] <- data[i,"targetName"]
        }else {
          data$target[i] <- data[i,"sourceName"]
        }
      }
      
      data <- data%>%select(target,weight)
                   
   
  }
    
  })
})

# runApp(,port=4033)









