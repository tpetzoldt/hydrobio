library("rhandsontable")
library("plotly")


shinyUI(fluidPage(
  headerPanel("Autocorrelation"),
  sidebarLayout(
    sidebarPanel(
      
      h3("Input data"),
      
      rHandsontableOutput("hot"),
      checkboxInput("arrow", "show shift", FALSE),
      h4("Lag"),
      sliderInput("lag", "Autocorrelcation lag", min = 0, max = 15, value = 0),
      h4("Actions"),
      actionButton("addRows", "+10 rows"),
      actionButton("clrBtn", "Clear")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                plotOutput("plot"),
                plotOutput("plot2"),
                plotOutput("plot3")
                 #verbatimTextOutput("summary")
        ),
        tabPanel("Info",
                 includeHTML("info.html")
        )
        )
      )
    )
  )
)