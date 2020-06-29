library("rhandsontable")
library("plotly")
library("Kendall")

shinyUI(fluidPage(
  headerPanel("Temperature trend test"),
  sidebarLayout(
    sidebarPanel(

      h3("Input data"),

      rHandsontableOutput("hot"),
      h4("Input units"),
      p("Date must be in ISO 8601 format (YYYY-mm-dd), Temp: °C"),
      h4("Actions"),
      actionButton("addRows", "+10 rows"),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Linear temperature trend",
                 plotlyOutput("temp_ts"),
                 plotlyOutput("acf"),
                 tableOutput("sumTable")
          #verbatimTextOutput("summary")
        ),

        tabPanel("Aufgaben (DE)",
                 withMathJax(includeMarkdown("methods_de.md")) # works, but has issues with references and some math

        ),
        tabPanel("Info",
          includeHTML("info.html")
        )
      )
    )
  )
))
