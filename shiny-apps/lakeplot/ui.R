library(rhandsontable)
library(plotly)
library(dplyr)
library(rLakeAnalyzer)
library(reshape2)

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
      checkboxInput("10Ciso", "plot 10°C Isotherme", FALSE),
      checkboxInput("thermo", "plot Thermocline", FALSE),
      checkboxInput("light1p", "plot Tiefe in der noch 1% Licht", FALSE),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Multi Parameter Probe",
          plotlyOutput("multiprobe"),
          #verbatimTextOutput("summary")
        ),
        tabPanel("Light Sensor",
          plotlyOutput("light1"),
          plotlyOutput("light2")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel("Info",
          includeHTML("info.html")
        )
      )
    )
  )
))
