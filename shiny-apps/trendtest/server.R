library("rhandsontable")
library("plotly")
library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")
library("Kendall")

lastClrBtn   <- 0
lastAddRows  <- 0
lastInputRun <- 0

## DF structure and example data
DF <- data.frame(Date = c(paste0("200", 0:9, "-01-01"), paste0("20", 10:15, "-01-01")),
                 Temp = 9 + 0:15*0.04 + rnorm(16,0, 0.5)
                 )

shinyServer(function(input, output, session) {



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

  output$temp_ts <- renderPlotly({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        DF$Date <- as.POSIXct(DF$Date)
        DF$mode <- "Obs"
        DF$tile <- "1 - Temperature"
        
        lm <- lm(Temp ~ Date, DF)
        
        DF_l <- rbind(DF, data.frame(Date = DF$Date,
                                   Temp = predict(lm, DF),
                                   mode = rep("lm", length(DF$Date)),
                                   tile = rep("1 - Temperature", length(DF$Date))))
        
        DF_l <- rbind(DF_l, data.frame(Date = DF$Date,
                                       Temp = residuals(lm),
                                       mode = rep("res", length(DF$Date)),
                                       tile = rep("2 - Residuals", length(DF$Date))))

        funtext <- data.frame(
          xpos = quantile(DF$Date, 0.2),
          ypos =  quantile(DF$Temp, 0.95),
          annotateText = paste0("y = ", signif(coef(lm)[1], 2),
                                " + ",
                                signif(coef(lm)[2]*86400*365.25, 2), " * x"),
          mode = "lm",
          tile = "1 - Temperature")
        
        p1 <- ggplot() +
          geom_hline(data = data.frame(tile = "2 - Residuals"), aes(yintercept = 0)) +
          geom_point(data = DF_l[DF_l$mode != "lm", ], aes(x = Date, y = Temp, col = mode)) +
          geom_line(data = DF_l[DF_l$mode != "res", ], aes(x = Date, y = Temp, col = mode)) +
          facet_wrap(~tile, scales = "free") + 
          geom_text(data = funtext, aes(x = xpos, y = ypos,
                                        label = annotateText, col = mode),
                    parse = TRUE) 
        ggplotly(p1)
      } else {
        NULL # return NULL if input$hot is not yet initialized
      }
    })
  })


  
  output$acf <- renderPlotly({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        DF$Date <- as.POSIXct(DF$Date)
        
        lm <- lm(Temp ~ Date, DF)
        
        DF$lm <- predict(lm, DF)
        
        DF$res <- residuals(lm)
        AC <- acf(DF$res, plot = FALSE)
        conf.level <- 0.95
        ciline <- qnorm((1 - conf.level)/2)/sqrt(length(DF$Temp))
      p2 <- ggplot(data.frame(acf = AC$acf,
                              lag = AC$lag), aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) +
        geom_segment(mapping = aes(xend = lag, yend = 0)) + 
        geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
        geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
        ggtitle("Autocorelation residuals")
      ggplotly(p2)
      } else {
        NULL # return NULL if input$hot is not yet initialized
      }
    })
  })
  
  
  sumTable <-  reactive({
    
    input$runBtn
    DF <- hot_to_r(input$hot)
    DF$Date <- as.POSIXct(DF$Date)
    ts <- ts(DF$Temp, deltat = mean(diff(as.numeric(DF$Date)))/(365.25*24*3600))
    ken <- MannKendall(ts)
    lm <- lm(Temp ~ Date, DF)
    
    setNames(data.frame(c("Linear trend",
                          "Mann Kendall test"),
                        c("Slope", "Tau"),
                        c(lm$coefficients[2]*(365.25*24*3600), ken$tau[1]),
                        c(summary(lm)$coefficients[2,4], ken$sl[1])),
             c("Method", "Measure","Value", "p.value"))
  
  })
  
  output$sumTable <- renderTable(sumTable(), digits = 4)
  
})
