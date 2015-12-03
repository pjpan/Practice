library(shiny)
library(igraph)
library(dplyr)

# Define server logic required to summarize and view the selected dataset
load('./conf/utils.RData')


shinyServer(function(input, output) {
  
  # Return the requested dataset
  dataInput <- reactive({
    
    if(input$name == "all"){
      data <- network
      data <- filter(head(data[order(data[,"weight"],decreasing = T),], n = input$obs)
                     ,weight>=5)
      
    }else{
      data <- subset(network 
                     , network$sourceName == input$name|network$sourceName == input$name  # 单项节点，非双向的；                  
                     )
            
      data <- filter(head(data[order(data[,"weight"],decreasing = T),], n = input$obs)
                     ,weight>=5
                     )
    }
        
  }) 

  # Generate a summary of the dataset
  output$Plot <- renderPlot({
    
    data <- dataInput() 
        
    # debug
    # data1 <- data    
    g <- graph.data.frame(data, directed = F )

    E(g)$weight <- data$weight   # 线的粗细当做权重；
    
    V(g)
    
    
    E(g)$color <- left_join(data,colormap,c("dep"="dep"))$color
    
    par(mai=c(0,0,1,0))
        
    #  改进点：
    #  1、线的宽度表示连接的大小；
    #  2、放大图片；
    #  3、改成无方向的结果，输出的table里面只显示targetname结果；
    #  3、同一个部分是同一个颜色；
    #  4、圆圈的大小目前是degree，改成pagerank计算出来的权重；
    #  5、圆圈大小用 pagerank来表示，圆圈的颜色用Modularity Class来表示；
    # 

        
    try(plot(g,
        vertex.size = mapp(degree(g),c(1,20)),        #  节点的大小用度来衡量；
        vertex.shape='circle',                       #   节点的形状；
        vertex.label.cex=1.5,                        #   节点字体的大小；
        vertex.label.color='black',                  #   节点字体颜色
        vertex.color = E(g)$color,                  # mapp(degree(g),c(1,20)),  E(g)$color  #  node的颜色；原来的结果:
        vertex.label.font = 2,
        layout=layout.fruchterman.reingold,          # layout.auto 自适应,尽量不重叠
        edge.color = E(g)$color,                      # 线的颜色
        edge.arrow.size = 0,                          #  调整有方向箭头的大小
        edge.width = E(g)$weight,                     #  调整线的宽度；
        asp = 0
    #    edge.label = E(g)$weight,                    # 线的标签显示什么；
        ), silent =T)
    }
     ,width = 1000 , height = 800
    )
  #
  # Show the first "n" observations
  output$view <- renderTable({
    data <- dataInput()
    data <- data[order(data[,6],decreasing = T),]
    select(head(data, n = input$obs),targetName,dep,weight)
    
  })
})














