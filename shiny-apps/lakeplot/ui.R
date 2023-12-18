ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  tags$div(
    style='float: right;color: white; font-family: OpenSans;',
    selectInput(
      inputId='selected_language',
      label=i18n$t("Language"),
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  ),
  HTML(paste0('<span lang = "en">')), # worakround; improve this when shiny supports global lang tag
  includeHTML("www/header_ihb_en.html"),
  headerPanel(i18n$t("Lake Profile Plot")),
  sidebarLayout(
    sidebarPanel(

      h1(i18n$t("Input data")),

      rHandsontableOutput("hot"),
      h2(i18n$t("Measurement units")),
      p(i18n$t("Depth"), " m, ", i18n$t("Temp"), ": °C, ",
        i18n$t("Oxygen"), ": mg/L, ", i18n$t("pH"), ": - ",
        i18n$t("Conductivity"), ": mS/m, ",
        i18n$t("Chlorophyl-a"), ": mug/L, ", i18n$t("Turb"), ": NTU ",
        i18n$t("Light"), ": Iz/I0 (", i18n$t("percent"), ")"),
      h2(i18n$t("Options")),
      checkboxInput("10Ciso", i18n$t("plot 10°C isotherme"), FALSE),
      checkboxInput("thermo", i18n$t("plot thermocline"), FALSE),
      checkboxInput("light1p", i18n$t("plot 1% light depth"), FALSE),
      checkboxInput("plotST", i18n$t("add Secchi depth to plot"), FALSE),
      conditionalPanel(condition = "input.plotST==true",
                       numericInput("ST", i18n$t("Secchi depth (m)"), NA)),
      actionButton("addRows", i18n$t("+10 rows")),
      actionButton("runBtn", i18n$t("Plot")),
      actionButton("clrBtn", i18n$t("Clear"))

    ),
    mainPanel(
      tabsetPanel(
        tabPanel(i18n$t("Multi Parameter Probe"),
          plotlyOutput("multiprobe1"),
          plotlyOutput("multiprobe2"),
          tableOutput("sumTable1")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel(i18n$t("Light Sensor"),
          plotlyOutput("light1"),
          plotlyOutput("light2"),
          tableOutput("sumTable2")#,
          #verbatimTextOutput("summary")
        ),
        tabPanel(i18n$t("Exercises"),
          uiOutput("methods")
        ),
        # tabPanel("Aufgaben (DE)",
        #          withMathJax(includeMarkdown("methods_de.md")) # works, but has issues with references and some math
        # 
        # ),
        tabPanel(i18n$t("Informations"),
          uiOutput("info")
        )
      )
    )
  ),
  includeHTML("www/footer_en.html"), # <---
  HTML("</span>")
)
