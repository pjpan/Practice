library(shiny)
library(networkD3)
library(igraph)
library(dplyr)


# Define server logic required to summarize and view the selected dataset
load('./conf/utils.RData')

shinyServer(function(input, output) {
  
  # Return the requested dataset
#   dataInput <- reactive({
#     
#     if(input$name == "all"){
#       data <- network
#       data <- filter(head(data[order(data[,"weight"],decreasing = T),], n = input$obs)
#                      ,weight>=5)
#       
#     }else{
#       data <- subset(network 
#                      , network$sourceName == input$name|network$sourceName == input$name  # 单项节点，非双向的；                  
#                      )
#             
#       data <- filter(head(data[order(data[,"weight"],decreasing = T),], n = input$obs)
#                      ,weight>=5
#                      )
#     }
#         
#   }) 

  # Generate a summary of the dataset
  # 不同的输出在ui.R上面要做相应的调整；
  output$Plot <- renderSimpleNetwork({
    
    if(input$name == "all"){
      
      Links <- filter(head(Links[order(Links[,"weight"],decreasing = T),], n = input$obs))
      
      Nodes <- Nodes
      
    } else {
      Links <- subset(Links 
                     ,Links$sourceName == input$name|Links$targetName == input$name  # 单项节点，非双向的                    
                      )
      Nodes <- filter(Nodes, Label == input$name)      
        }  
    
    #  改进点：
    #  1、线的宽度表示连接的大小；
    #  2、放大图片；
    #  3、改成无方向的结果，输出的table里面只显示targetname结果；
    #  3、同一个部分是同一个颜色；
    #  4、圆圈的大小目前是degree，改成pagerank计算出来的权重；
    #  5、圆圈大小用 pagerank来表示，圆圈的颜色用Modularity Class来表示；
    # debug
    # data <- network
    # 无法显示字体；
#     forceNetwork(Links = Links, Nodes = Nodes, Source = 'sourceName'
#                  ,Target = 'targetName',Value = 'weight', NodeID = 'Label'
#                  ,Nodesize = 'PageRank', Group = 'Modularity.Class'
#                  ,height = NULL, width = NULL, colourScale = JS("d3.scale.category20()")
#                  ,fontSize = 10, fontFamily = "微软雅黑"
#                  ,linkDistance = 50
#                  ,linkWidth = JS("function(d) { return Math.sqrt(d.value); }")
#                  ,radiusCalculation = JS(" Math.sqrt(d.nodesize)+8")
#                  ,charge = -120
#                  ,linkColour = "black", opacity = 1, zoom = T, legend = T
#                  ,bounded = T, opacityNoHover = 0, clickAction = NULL
#                  )

  simpleNetwork(Data= Links, Source = 'sourceName', Target = 'targetName', height = 800,
                width = 1000, linkDistance = 100, charge = -200, fontSize = 15,
                fontFamily = "微软雅黑", linkColour = "#666", nodeColour = "#3182bd",
                nodeClickColour = "#3182bd", textColour = "#3182bd", opacity = 5,
                zoom = T)

    }
    )
  #
  # Show the first "n" observations
  output$view <- renderTable({
    if(input$name == "all"){
      
      Links <- filter(head(Links[order(Links[,"weight"],decreasing = T),], n = input$obs))
      
    } else {
      Links <- subset(Links 
                      , Links$sourceName == input$name|Links$targetName == input$name)  # 单项节点，非双向的                    
      
    } 
    Links <- Links[order(Links[,6],decreasing = T),]
    select(head(Links, n = input$obs),targetName,dep,weight)  
  })
})














