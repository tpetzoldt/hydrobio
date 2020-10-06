library("rhandsontable")


shinyUI(fluidPage(
  HTML('<span lang = "en">'), # worakround; improve this when shiny supports global lang tag
  includeHTML("www/header_ihb_en.html"),
  headerPanel("Autocorrelation"),
  sidebarLayout(
    sidebarPanel(

      h1("Input data"),

      rHandsontableOutput("hot"),
      checkboxInput("arrow", "show shift", FALSE),
      checkboxInput("comp", "show pairs", FALSE),
      checkboxInput("cor", "show correlation", FALSE),
      h2("Lag"),
      sliderInput("lag", "Autocorrelcation lag", min = 0, max = 15, value = 0),
      h2("Add random noise"),
      sliderInput("noise", "Standard deviation of noise relative to range of y", min = 0, max = 2, step = 0.01, value = 0),
      h2("Actions"),
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
    ),
  includeHTML("www/footer_en.html"), # <---
  HTML("</span>")
  )
)
