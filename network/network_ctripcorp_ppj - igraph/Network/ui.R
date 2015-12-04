
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
    
      numericInput("obs", "Number of observations to view:", 50),
    
      helpText("Note: 1） 社会网络图中圆圈越大代表该节点在社会网络中与直接或间接连接的人关系强度越大  
                      2）社会网络图中同一种颜色节点代表这些节点处于同一个社区。"),
    
      submitButton("Update View"),
      tabPanel("Observations",tableOutput("view")),  # 把输出结果挪到了左边，侧边栏；
    
      p("如有问题和建议，请联系：zhuzheng@Ctrip.com")
      ,width = 4
    ),
      
  mainPanel(
#     tabsetPanel(     #  设置两个tab当做一个输出界面；
    tabPanel("Plot",plotOutput("Plot"))
    
#     )
  )
  )
)






