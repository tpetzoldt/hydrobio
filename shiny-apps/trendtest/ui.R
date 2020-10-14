library("rhandsontable")
library("plotly")
library("Kendall")

shinyUI(fluidPage(
  HTML('<span lang = "en">'), # worakround; improve this when shiny supports global lang tag
  includeHTML("www/header_ihb_en.html"),
  headerPanel("Trend test"),
  sidebarLayout(
    sidebarPanel(

      h1("Input data"),

      rHandsontableOutput("hot"),
      radioButtons("smooth", label = c("select smoother"), selected = "lm",
                   choiceNames = list("linear", "loess"),
                   choiceValues = list("lm", "loess")),
      h2("Input"),
      p("x can be either nummeric or a date as character in ISO 8601 format (YYYY-mm-dd)"),
      h2("Actions"),
      actionButton("addRows", "+10 rows"),
      actionButton("runBtn", "Plot"),
      actionButton("clrBtn", "Clear")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 plotlyOutput("temp_ts"),
                 plotlyOutput("acf"),
                 plotlyOutput("resdist"),
                 tableOutput("sumTable")
          #verbatimTextOutput("summary")
        ),

        tabPanel("Hintergrund",
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
