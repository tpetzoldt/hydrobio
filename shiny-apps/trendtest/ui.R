library("rhandsontable")
library("plotly")
library("Kendall")

shinyUI(fluidPage(
  headerPanel("Trend test"),
  sidebarLayout(
    sidebarPanel(

      h3("Input data"),

      rHandsontableOutput("hot"),
      radioButtons("smooth", label = c("select smoother"), selected = "lm",
                   choiceNames = list("linear", "loess"),
                   choiceValues = list("lm", "loess")),
      h4("Input units"),
      p("Date must be either nummeric or character in ISO 8601 format (YYYY-mm-dd)"),
      h4("Actions"),
      actionButton("addRows", "+10 rows"),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
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
