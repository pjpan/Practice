library(shiny)


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel("Social Network"),
  
    sidebarPanel(
    a(href = "ctrip.com",target="_blank",
      img(src = "http://pic.c-ctrip.com/common/c_logo2013.png")),br(),
    h1("Ctrip Social Network Demo"),
    
    textInput("variable", "Please Enter a Name:", 
                value = c("全网络",as.character(Nodes$label))),
    
    
    numericInput("obs", "Number of observations to view:", 20),
    
    helpText("Note: while the data view will show only the specified",
             "number of observations, the summary will still be based",
             "on the full dataset."),
    
    submitButton("Update View"),
    
    p("zhuzheng@Ctrip.com")
    
    ),
  
  mainPanel(
    tabsetPanel(
    tabPanel("Plot",plotOutput("Plot")),
    tabPanel("Observations",tableOutput("view"))
    )
  )
  )
)