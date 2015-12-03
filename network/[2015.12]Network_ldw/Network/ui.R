
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(networkD3)
load("network.Rdata")

shinyUI(fluidPage(
  
  titlePanel("Shiny networkD3 "),
  
  sidebarLayout(
    sidebarPanel(
      a(href = "ctrip.com",target="_blank",
        img(src = "http://pic.c-ctrip.com/common/c_logo2013.png")),br(),
      h1("Ctrip关系网络demo"),
      sliderInput("opacity", "Opacity (not for Sankey)", 0.6, min = 0.1,
                  max = 1, step = .1),
      selectInput("variable", "选择姓名:",
                  c("None",nodes$label,selectlize = T)),
      p("langdw@Ctrip.com")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Force Network", forceNetworkOutput("force")),
        
        tabPanel("列表信息", dataTableOutput("table"))
      )
    )
  )
))