library("rhandsontable")
library("plotly")
library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")

lastClrBtn <- 0
lastAddRows <- 0

## DF structure and example data
DF <- data.frame(Depth=0:10,
                 Temp=c(20,20,20,17,16,16,15,10,8,7,6),
                 Oxygen=c(10, 12, 9, 8, 7, 5,5,4, 4, 3, 1),
                 pH=c(7.5,8,8.2,7,7,7.1,6.9,7,7,7,6.9),
                 Cond=runif(11,300,320),
                 ## exponential curve with some relative error
                 Light = 100*exp(-0.5 * (0:10)) * runif(11, min=0.8, max=1.2)
                 )

shinyServer(function(input, output, session) {

  get_analysis <- reactive({
    # do some calculations
  })

  #observeEvent(input$addRows, {
  #  cat("test", input$addRows, "\n")
  #})

  #new_df <- eventReactive(input$addRows, {
  #  cat("event")

  #  nm <- names(DF)
  #  DF2 <- as.data.frame(matrix(NA, nrow=10, ncol=length(nm)))
  #  colnames(DF2) <- nm
  #  rbind(DF, DF2)
  #})

  output$hot <- renderRHandsontable({
    if(input$clrBtn > lastClrBtn) {
      lastClrBtn <<- input$clrBtn
      print(lastClrBtn)
      DF <- hot_to_r(input$hot)
      DF[,] <- as.numeric(NA) # as.numeric to avoid boolean

    } else {
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
      } else {
        print("startup")
      }
    }

    if (input$addRows > lastAddRows) {
      lastAddRows <<- input$addRows
      print(lastAddRows)

      DF <- hot_to_r(input$hot)
      nm <- names(DF)
      DF2 <- as.data.frame(matrix(NA, nrow=10, ncol=length(nm)))
      colnames(DF2) <- nm

      DF <- rbind(DF, DF2)
    }

    rhandsontable(DF, height=600)  %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$multiprobe <- renderPlotly({
    input$runBtn
    input$thermo
    input$`10Ciso`
    input$light1p

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        df2 <- merge(df2, data.frame(variable = c("Temp", "Oxygen", "pH", "Cond", "Light"),
                                     plot = c(1, 1, 2, 3, 4)))
        dfp1 <- subset(df2, df2$variable %in% c("Temp", "Oxygen", "pH", "Cond"))

        print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) + geom_line() +
          geom_point() + coord_flip()  + facet_grid(.~plot, scales = "free") +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse")

        if(input$`10Ciso`) {
          z_iso10 <- approx(DF$Temp, DF$Depth, 10)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }


        if(input$thermo) {
          z_thermo <- thermo.depth(DF$Temp, DF$Depth)

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
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


    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        dfp1 <- subset(df2, df2$variable %in% c("Light"))
        if(input$light1p) {
         dfp1$value <- dfp1$value/max(dfp1$value, na.rm = TRUE)
        }
        print(str(DF))


        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) +
          geom_line() + geom_point() + coord_flip() +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse") +
          ggtitle(ifelse(input$light1p, "Light relative", "Light"))



        if(input$`10Ciso`) {
          z_iso10 <- approx(DF$Temp, DF$Depth, 10)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$thermo) {
          z_thermo <- thermo.depth(DF$Temp, DF$Depth)

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }


        if(input$`10Ciso`) {
          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
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
    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        dfp1 <- subset(df2, df2$variable %in% c("Light"))
        dfp1$value <- log10(dfp1$value)

        print(str(DF))
        #analysis <- get_analysis()

        # linear fit
        m <- lm(log(DF$Light) ~ DF$Depth)
        eqt <- paste0("y = ", round(m$coefficients[1],2), " ",
                      round(m$coefficients[2],2), " * x")



        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) +
          geom_point() + coord_flip() +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse") +
          geom_smooth(method = "lm", aes(col = "linear fit")) +
          geom_text(data = data.frame(Depth = 1, value = 0.1, variable = "linear fit"),
                    parse = TRUE, label = eqt) + ggtitle("log(light) with linear fit")


        if(input$`10Ciso`) {
          z_iso10 <- approx(DF$Temp, DF$Depth, 10)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$thermo) {
          z_thermo <- thermo.depth(DF$Temp, DF$Depth)

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        ggplotly(p1)


      } else {
        # placeholder, do nothing
      }
    })
  })

})
