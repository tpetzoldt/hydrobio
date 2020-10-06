library("rhandsontable")
library("plotly")
library("dplyr")
library("rLakeAnalyzer")
library("reshape2")

shinyUI(fluidPage(
  HTML('<span lang = "en">'), # worakround; improve this when shiny supports global lang tag
  includeHTML("www/header_ihb_en.html"),
  headerPanel("Lake Profile Plot"),
  sidebarLayout(
    sidebarPanel(

      h1("Input data"),

      rHandsontableOutput("hot"),
      h2("Measurement units"),
      p("Depth: m, Temp: °C, pH: -, Conductivity: mS/m, Turb: NTU, Light: Iz/I0 (percent)"),
      h2("Options"),
      checkboxInput("10Ciso", "plot 10°C isotherme", FALSE),
      checkboxInput("thermo", "plot thermocline", FALSE),
      checkboxInput("light1p", "plot 1% light depth", FALSE),

      actionButton("addRows", "+10 rows"),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Multi Parameter Probe",
          plotlyOutput("multiprobe1"),
          plotlyOutput("multiprobe2"),
          tableOutput("sumTable1")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel("Light Sensor",
          plotlyOutput("light1"),
          plotlyOutput("light2"),
          tableOutput("sumTable2")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel("Exercises (EN)",
          #withMathJax(includeHTML("methods.html")) # mysteriosly breaks rhandsontable
          withMathJax(includeMarkdown("methods.md")) # works, but has issues with references and some math

        ),
        tabPanel("Aufgaben (DE)",
                 withMathJax(includeMarkdown("methods_de.md")) # works, but has issues with references and some math

        ),
        tabPanel("Info",
          includeHTML("info.html")
        )
      )
    )
  ),
  includeHTML("www/footer_en.html"), # <---
  HTML("</span>")
))
