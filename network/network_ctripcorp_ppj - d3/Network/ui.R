
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Social Network"),
  
    sidebarPanel(
      a(href = "ctrip.com",target="_blank",
      img(src = "http://pic.c-ctrip.com/common/c_logo2013.png"))
      ,br(),
      h1("Ctrip Social Network Demo"),
    
      textInput("name", "Please Enter a Name:", 
                value = c("all")),
    
    
#     sliderInput("obs", "Number of observations to view:", 
#                 min=1, max=200, value=20),
    
      numericInput("obs", "Number of observations to view:", 30),
    
      helpText("Note: while the data view will show only the specified",
             "number of observations, the summary will still be based",
             "on the full dataset."),
    
      submitButton("Update View"),
      tabPanel("Observations",tableOutput("view")),  # 把输出结果挪到了左边，侧边栏；
    
      p("如有问题和建议，请联系：zhuzheng@Ctrip.com")
      ,width = 4
    ),
      
  mainPanel(
#     tabsetPanel(     #  设置两个tab当做一个输出界面；
     tabPanel("Plot",simpleNetworkOutput("Plot",width = "100%", height = "800px"))
    
#     )
  )
  )
)






