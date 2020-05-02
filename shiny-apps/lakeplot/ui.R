library(rhandsontable)

shinyUI(fluidPage(
  headerPanel("Lake Profile Plot"),
  sidebarLayout(
    sidebarPanel(

      h3("Input data"),

      rHandsontableOutput("hot"),

      #h3("Start parameters"),

      #fluidRow(
      #column(3, numericInput("inpMean", label = "Mean", value = NA)),
      #column(3, numericInput("inpSd", label = "SD", value = NA)),
      #column(3, numericInput("inpK", label = "K", value = NA))
      #),
      #checkboxInput("bxAutomatic", "Automatic", TRUE),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Multi Parameter Probe",
          plotOutput("multiprobe"),
          #verbatimTextOutput("summary")
        ),
        tabPanel("Light Sensor",
          plotOutput("light")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel("Info",
          includeHTML("info.html")
        )
      )
    )
  )
))
