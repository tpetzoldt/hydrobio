library("rhandsontable")
library("plotly")
library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")

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

shinyServer(function(input, output, session) {

  get_eps <- reactive({
    DF <- hot_to_r(input$hot)
    m <- lm(log(DF$Light) ~ DF$Depth)
    coef(m)[2]
  })

  get_iso10 <- reactive({
    DF <- hot_to_r(input$hot)
    z_iso10 <- approx(DF$Temp, DF$Depth, 10)$y
    if(all(DF$Temp <= 10)) {
      z_iso10 <- NA
    }
    return(z_iso10)
  })
  
  get_thermo <- reactive({
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

    rhandsontable(DF, height=600)  %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  sumTable <-  reactive({

    input$runBtn

    z_iso10 <- get_iso10()
    z_thermo <- get_thermo()
    eps <- get_eps()
    z_light <- log(0.01)/eps
    setNames(data.frame(z_iso10, z_thermo, z_light, eps),
            c("10C isotherme", "thermocline depth", "1% light depth", "epsilon"))
  })
  
  output$sumTable1 <- renderTable(sumTable()) 
  output$sumTable2 <- renderTable(sumTable())  
    
  output$multiprobe1 <- renderPlotly({

    input$runBtn

    input$thermo
    input$`10Ciso`
    input$light1p

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        df2 <- melt(DF, id.vars = "Depth")
        df2 <- merge(df2, data.frame(variable = c("Temp", "Oxygen", "pH", "Cond"),
                                     plot = c("1: Temp & O2 (deg C, mg/L)", "1: Temp & O2 (deg C, mg/L)", "2: pH", "3: Conductivity (mS/m)")))
        dfp1 <- subset(df2, df2$variable %in% c("Temp", "Oxygen", "pH", "Cond"))
        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) + geom_line() +
          geom_point() + coord_flip()  +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse") +
          facet_grid(.~plot, scales = "free")

        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }


        if(input$thermo) {
         
          z_thermo <- get_thermo()

          p1 <- p1 + geom_vline(data = data.frame(x = z_thermo, variable = "thermocline"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }
        ggplotly(p1)

      } else {
        # placeholder, do nothing
      }
    })
  })

  output$multiprobe2 <- renderPlotly({

    input$runBtn

    input$thermo
    input$`10Ciso`
    input$light1p

    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        DF$O2_sat <- DF$Oxygen/(exp(7.7117 - 1.31403 * log(DF$Temp + 45.93)))*100
        df2 <- melt(DF, id.vars = "Depth")
        df2 <- merge(df2, data.frame(variable = c("O2_sat", "Chla", "Turb"),
                                     plot = c("4: Oxygen saturation (%)",
                                              "5: Chlorophyl-a (mug/L)",
                                              "6: Turbidity (NTU)")))
        dfp1 <- subset(df2, df2$variable %in% c("O2_sat", "Chla", "Turb"))

        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) + geom_line() +
          geom_point() + coord_flip()  + facet_grid(.~plot, scales = "free") +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse")

        if(input$`10Ciso`) {
          z_iso10 <- get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }


        if(input$thermo) {
         
          z_thermo <- get_thermo()

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
        #print(str(DF))

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) +
          geom_line() + geom_point() + coord_flip() +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse") +
          ggtitle(ifelse(input$light1p, "Light relative", "Light"))


        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }

        if(input$thermo) {

          z_thermo <- get_thermo()

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

        #print(str(DF))
        #analysis <- get_analysis()

        # linear fit
        m <- lm(log(DF$Light) ~ DF$Depth)
        #eps <- coef(m)[2]
        funtext <- data.frame(
          xpos = 0,
          ypos =  0,
          annotateText = paste0("y = ", round(coef(m)[1], 2), " ",
                                round(coef(m)[2], 2), " * x"),
          variable = "linear fit")
        

        p1 <- ggplot(dfp1, aes(x = Depth, y = value, col = variable)) +
          geom_point() + coord_flip() +
          theme(legend.position="bottom") + xlab("Depth (m)")  +
          scale_x_continuous(trans = "reverse") +
          geom_smooth(method = "lm", aes(col = "linear fit")) +
          geom_text(data = funtext, aes(x = xpos, y = ypos,
                                        label = annotateText, col = variable),
                    parse = TRUE) + 
          ggtitle("log(light) with linear fit")

        if(input$`10Ciso`) {
          z_iso10 <-get_iso10()

          p1 <- p1 + geom_vline(data = data.frame(x = z_iso10, variable = "10 °C isotherme"),
                                aes(xintercept = x, col = variable), linetype = "dashed")
        }

        if(input$light1p) {
          ## thpe: use extinction formula instead of interpolation
          #z_light <- approx(DF$Light/max(DF$Light, na.rm = TRUE), DF$Depth, 0.01)$y
          z_light <- log(0.01) / get_eps()

          p1 <- p1 + geom_vline(data = data.frame(x = z_light, variable = "1% light depth"),
                                aes(xintercept = x, col = variable), linetype = "dotted")
        }

        if(input$thermo) {
       
          z_thermo <- get_thermo()

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
