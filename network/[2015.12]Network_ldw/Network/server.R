
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(networkD3)
library(dplyr)
source("utils.R")
load("network.Rdata")

server <- function(input, output) {
  
 
  
  output$force <- renderForceNetwork({
    if(input$variable=="None"){
      edges1 = edges
      nodes1 = nodes
    }else{
      list = change_data(edges,nodes,input$variable)
      nodes1 = list[[1]]
      edges1 = list[[2]]
    }
 
       
       forceNetwork(Links = edges1, Nodes = nodes1, Source = "source",
                    Target = "target", Value = "weight", NodeID = "label",
                    Group = "group", Nodesize = "size",opacity = input$opacity, zoom = T,
                    fontFamily="微软雅黑")
  })
  
  output$table = renderDataTable({
    if(input$variable=="None"){
      edges1 = edges
      nodes1 = nodes
    }else{
      list = change_data(edges,nodes,input$variable)
      nodes1 = list[[1]]
      edges1 = list[[2]]
    }
    
    
    if(input$variable=="None"){
      output = edges1 %>% 
        left_join(nodes1[,1:2],by=c("source"="id")) %>%
        left_join(nodes1[,1:2],by=c("target"="id")) %>%
        arrange(desc(weight)) %>% select(Source = label.y,
                                         Target = label,
                                         Weight = weight) 
      # %>% head(10)
    }else{
      output = edges1 %>% 
        left_join(nodes1[,1:2],by=c("source"="id")) %>%
        left_join(nodes1[,1:2],by=c("target"="id")) %>%
        arrange(desc(weight)) %>% select(Source = label.y,
                                         Target = label.x,
                                         Weight = weight) 
      # %>% head(10)
    }
    
    
    
    as.data.frame(output)
      
  })
  
}