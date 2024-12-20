library("rhandsontable")
library("plotly")
library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")
library("shiny.i18n")
library("rLakeAnalyzer")
library("shiny")

lastClrBtn   <- 0
lastAddRows  <- 0
lastInputRun <- 0

## DF structure and example data
DF <- data.frame(Depth=0:10,
                 Temp=c(20,20,20,17,16,16,15,10,8,7,6),
                 Oxygen=c(10, 12, 9, 8, 7, 5,5,4, 4, 3, 1),
                 pH=c(7.5,8,8.2,7,7,7.1,6.9,7,7,7,6.9),
                 Cond=runif(11,300,320),
                 Chla=c(3.4,3.6,4.4,5.1,3.4,2.7,2,1.1,0.3,0,0),
                 Turb=c(0.91, 1.13, 1.06, 1.31, 1.15, 0.93, 0.50, 0.44, 0.42, 0.17, 0.19),
                 ## exponential curve with some relative error
                 Light = 100*exp(-0.5 * (0:10)) * runif(11, min=0.8, max=1.2)
)

# path to the translator files
i18n <- Translator$new(translation_json_path = "translation/translation.json")

i18n$set_translation_language("en")

##--------------------------- ui ---------------------------------------

ui <- fluidPage(
  title='Lake Profile Plotter',
  shiny.i18n::usei18n(i18n),
  tags$div(
    style='float: right;color: white; font-family: Open Sans;',
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

##------------------- server -------------------------------------
server <- function(input, output, session) {

  observeEvent(input$selected_language, {
    update_lang(session = session, language = input$selected_language)
  })


  get_eps <- reactive({
    if(is.null(input$hot)) return(NA) # emergency exit during initialization
    DF <- hot_to_r(input$hot)
    DF_valid <- na.omit(DF[c("Depth", "Light")])
    if (nrow(DF_valid) > 2) {
      m <- lm(log(Light) ~ Depth, data = DF_valid)
      coef(m)[2]
    } else {
      NA
    }
  })

  get_iso10 <- reactive({
    if(is.null(input$hot)) return(NA) # emergency exit during initialization
    DF <- hot_to_r(input$hot)
    DF_valid <- na.omit(DF[c("Depth", "Temp")])

    if(any(DF_valid$Temp > 10) & nrow(DF_valid) > 2) {
      z_iso10 <- approx(DF_valid$Temp, DF_valid$Depth, 10, ties = mean)$y
    } else {
      z_iso10 <- NA
    }
    return(z_iso10)
  })

  get_ST <- reactive(input$ST)

  get_thermo <- reactive({
    if(is.null(input$hot)) return(NA) # emergency exit during initialization
    DF <- hot_to_r(input$hot)
    DF_valid <- na.omit(DF[c("Depth", "Temp")])
    z_thermo <- NA
    z_thermo <- thermo.depth(DF_valid$Temp, DF_valid$Depth)
    return(z_thermo)
  })

  get_analysis <- reactive({
    # do some calculations
  })

  output$hot <- renderRHandsontable({

    add10 <- input$addRows

    if(input$clrBtn > lastClrBtn) {
      lastClrBtn <<- input$clrBtn

      DF <- isolate(hot_to_r(input$hot))
      DF[,] <- as.numeric(NA) # as.numeric to avoid boolean
    } else  if (add10 > lastAddRows) {
      lastAddRows <<- add10

      DF <- isolate(hot_to_r(input$hot))
      nm <- names(DF)
      DF2 <- as.data.frame(matrix(NA, nrow=10, ncol=length(nm)))
      colnames(DF2) <- nm

      DF <- rbind(DF, DF2)
    } else {
      if (!is.null(input$hot)) {
        DF <- isolate(hot_to_r(input$hot))
      } else {
        print("startup")
        lastClrBtn   <<- input$clrBtn
        lastAddRows  <<- input$addRows
        lastInputRun <<- input$runBtn
      }
    }

    #print("hot triggered")

    rhandsontable(DF, height=400)  %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  sumTable <-  reactive({

    input$runBtn

    z_iso10 <- get_iso10()
    z_thermo <- get_thermo()
    eps <- get_eps()
    z_light <- log(0.01)/eps
    setNames(data.frame(z_iso10, z_thermo, z_light, eps),
             c(i18n$t("10 °C isotherme"),
               i18n$t("thermocline"),
               i18n$t("1% light depth"), "epsilon"))
  })

  output$sumTable1 <- renderTable(sumTable())
  output$sumTable2 <- renderTable(sumTable())

  output$multiprobe1 <- renderPlotly({

    input$runBtn
    input$plotST
    input$thermo
    input$`10Ciso`
    input$light1p
    input$selected_language
    ST <- get_ST()

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        df2 <- merge(df2, data.frame(variable = c("Temp", "Oxygen", "pH", "Cond"),
                                     plot = c("1: Temp & O2 (°C, mg/L)",
                                              "1: Temp & O2 (°C, mg/L)",
                                              "2: pH (-)",
                                              i18n$t("3: Conductivity (mS/m)"))))
                                              # paste unfortunately not possible here
        dfp1 <- subset(df2, df2$variable %in% c("Temp", "Oxygen", "pH", "Cond"))
        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) + geom_line() +
          geom_point() + coord_flip()  +
          theme(legend.position="bottom") +
          xlab(paste(i18n$t("Depth"), "(m)")) +
          ylab(i18n$t("value")) +
          scale_x_continuous(trans = "reverse") +
          facet_grid(.~plot, scales = "free") +
          scale_color_discrete(i18n$t("variable"))

        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10,
                                                  variable = i18n$t("10 °C isotherme")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light,
                                                  variable = i18n$t("1% light depth")),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }


        if(input$thermo) {

          z_thermo <- get_thermo()

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo,
                                                  variable = i18n$t("thermocline")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$plotST) {
          datST <- aggregate(list(y = dfp1$value), by = list(plot = dfp1$plot), quantile, 0.1)
          datST$x <- ST
          datST$variable <- i18n$t("Secchi depth")
          p1 <- p1 + geom_segment(data = datST,
                                  aes(x = 0, y = y, xend = x, yend = y, col = variable),
                                  arrow = arrow(angle = 90, length = unit(0.2, "cm")))
        }
        ggplotly(p1)

      } else {
        NULL # return NULL if input$hot is not yet initialized
      }
    })
  })

  output$multiprobe2 <- renderPlotly({

    input$runBtn
    input$plotST
    input$thermo
    input$`10Ciso`
    input$light1p
    input$selected_language
    ST <- get_ST()

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        DF$O2_sat <- DF$Oxygen/(exp(7.7117 - 1.31403 * log(DF$Temp + 45.93)))*100
        df2 <- melt(DF, id.vars = "Depth")
        df2 <- merge(df2, data.frame(variable = c("O2_sat", "Chla", "Turb"),
                                     plot = c(i18n$t("4: Oxygen saturation (%)"),
                                              "5: Chlorophyl-a (mug/L)",
                                              i18n$t("6: Turbidity (NTU)"))))
        dfp1 <- subset(df2, df2$variable %in% c("O2_sat", "Chla", "Turb"))

        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) + geom_line() +
          geom_point() + coord_flip()  + facet_grid(.~plot, scales = "free") +
          theme(legend.position="bottom") +
          xlab(paste(i18n$t("Depth"), "(m)")) +
          ylab(i18n$t("value")) +
          scale_x_continuous(trans = "reverse") +
          scale_color_discrete(i18n$t("variable"))

        if(input$`10Ciso`) {
          z_iso10 <- get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10,
                                                  variable = i18n$t("10 °C isotherme")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light,
                                                  variable = i18n$t("1% light depth")),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }


        if(input$thermo) {

          z_thermo <- get_thermo()

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo,
                                                  variable = i18n$t("thermocline")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$plotST) {
          datST <- aggregate(list(y = dfp1$value), by = list(plot = dfp1$plot), quantile, 0.1)
          datST$x <- ST
          datST$variable <- i18n$t("Secchi depth")
          p1 <- p1 + geom_segment(data = datST,
                                  aes(x = 0, y = y, xend = x, yend = y, col = variable),
                                  arrow = arrow(angle = 90, length = unit(0.2, "cm")))
        }
        ggplotly(p1)

      } else {
        # placeholder, do nothing
      }
    })
  })

  output$light1 <- renderPlotly({
    input$runBtn
    input$light1p
    input$thermo
    input$`10Ciso`
    input$plotST
    input$selected_language
    ST <- get_ST()

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        dfp1 <- subset(df2, df2$variable %in% c("Light"))
        if(input$light1p) {
          dfp1$value <- dfp1$value/max(dfp1$value, na.rm = TRUE)
        }

        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) +
          geom_line() + geom_point() + coord_flip() +
          theme(legend.position="bottom") +
          xlab(paste(i18n$t("Depth"), "(m)")) +
          ylab(i18n$t("value")) +
          scale_x_continuous(trans = "reverse") +
          ggtitle(ifelse(input$light1p,
                         i18n$t("Light (relative)"),
                         i18n$t("Light"))) +
          scale_color_discrete(i18n$t("variable"))


        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10,
                                                  variable = i18n$t("10 °C isotherme")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light,
                                                  variable = i18n$t("1% light depth")),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }

        if(input$thermo) {

          z_thermo <- get_thermo()

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo,
                                                  variable = i18n$t("thermocline")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }


        if(input$`10Ciso`) {
          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10,
                                                  variable = i18n$t("10 °C isotherme")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$plotST) {
          datST <- aggregate(list(y = dfp1$value), by = list(plot = dfp1$variable), quantile, 0.1)
          datST$x <- ST
          datST$variable <- i18n$t("Secchi depth")
          p1 <- p1 + geom_segment(data = datST,
                                  aes(x = 0, y = y, xend = x, yend = y, col = variable),
                                  arrow = arrow(angle = 90, length = unit(0.2, "cm")))
        }

        ggplotly(p1)
      } else {
        # placeholder, do nothing
      }
    })
  })


  output$light2 <- renderPlotly({
    input$runBtn
    input$light1p
    input$thermo
    input$`10Ciso`
    input$plotST
    input$selected_language
    ST <- get_ST()

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        dfp1 <- subset(df2, df2$variable %in% c("Light"))
        dfp1$value <- log10(dfp1$value)

        #print(str(DF))
        #analysis <- get_analysis()

        # linear fit
        #m <- lm(log(DF$Light) ~ DF$Depth)

        DF_valid <- na.omit(DF[c("Depth", "Light")])
        if (nrow(DF_valid) > 1) {
          m <- lm(log(Light) ~ Depth, data = DF_valid)
        }
        else {
          return(NULL) # emergency exit from the function
        }

        #eps <- coef(m)[2]
        funtext <- data.frame(
          xpos = 0,
          ypos =  0,
          annotateText = paste0("y = ", round(coef(m)[1], 2), " ",
                                round(coef(m)[2], 2), " * x"),
          variable = "linear fit")


        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable), na.omit = TRUE) +
          geom_point() + coord_flip() +
          theme(legend.position="bottom") +
          xlab(paste(i18n$t("Depth"), "(m)")) +
          ylab(i18n$t("value")) +
          scale_x_continuous(trans = "reverse") +
          geom_smooth(method = "lm", formula = y ~ x, aes(col = "linear fit")) +
          geom_text(data = funtext, aes(x = xpos, y = ypos,
                                        label = annotateText, col = variable),
                    parse = TRUE) +
          ggtitle(i18n$t("log(light) with linear fit")) +
          scale_color_discrete(i18n$t("variable"))

        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10,
                                                  variable = i18n$t("10 °C isotherme")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light,
                                                  variable = i18n$t("1% light depth")),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }

        if(input$thermo) {

          z_thermo <- get_thermo()

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo,
                                                  variable = i18n$t("thermocline")),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$plotST) {
          datST <- aggregate(list(y = dfp1$value), by = list(plot = dfp1$variable), quantile, 0.1)
          datST$x <- ST
          #datST$variable <- "Secchi depth"
          p1 <- p1 + geom_segment(data = datST,
                                  aes(x = 0, y = y, xend = x, yend = y,
                                      col = i18n$t("Secchi depth")),
                                  arrow = arrow(angle = 90, length = unit(0.2, "cm")))
        }

        ggplotly(p1)
      } else {
        # placeholder, do nothing
      }
    })
  })

  output$info <- renderUI({
    if(input$selected_language == "de") {
      includeHTML("info_de.html")
    } else {
      includeHTML("info.html")
    }
  })

  output$methods <- renderUI({
    if(input$selected_language == "de") {
      #withMathJax(includeHTML("methods.html")) # mysteriously breaks rhandsontable
      withMathJax(includeMarkdown("methods_de.md")) # works, but has issues with references and some math
    } else {
      withMathJax(includeMarkdown("methods.md"))
    }
  })
}

shinyApp(ui, server)
