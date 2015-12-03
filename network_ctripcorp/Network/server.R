library(shiny)


# Define server logic required to summarize and view the selected dataset

network<-NewBI

shinyServer(function(input, output) {
  
  # Return the requested dataset
  dataInput <- reactive({
    
    if(input$variable == "全网络"){
      data <- network
    }else{
      data <- subset(network,network$sourceName == input$variable|network$targetName == input$variable,
                     selsect = sourceName:weight)
    }
  })
  
  # Generate a summary of the dataset
  output$Plot <- renderPlot({
    data <- dataInput()
    
    library(igraph)
    data <- data[order(data[,6],decreasing = T),]
    data1 <- head(data, n = input$obs)
    #data1 <- subset(data, data$weight == input$range,select = sourceName:weight)
    g<-graph.data.frame(data1)
    E(g)$curved<-0.2
    plot(g,
        vertex.size = mapp(degree(g),c(1,20)),
        layout=layout.fruchterman.reingold,
        vertex.shape='circle',vertex.label.cex=1.0,vertex.label.color='black',
        vertex.color = mapp(degree(g),c(1,20)),
        edge.arrow.size = 1,edge.width = 2,asp = 0,edge.arrow.size = 2,
        edge.label = data1$weight,
        vertex.label.font = 3
        )
    },width = 1000,height = 850)
  #
  # Show the first "n" observations
  output$view <- renderTable({
    data <- dataInput()
    data <- data[order(data[,6],decreasing = T),]
    head(data, n = input$obs)
    #data <- subset(data, data$weight == input$range,select = sourceName:weight)
    #head(data, n = input$range)
  })
})
