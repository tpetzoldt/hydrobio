library("rhandsontable")


lastClrBtn   <- 0
lastAddRows  <- 0
lastInputRun <- 0

## DF structure and example data
DF <- data.frame(x = 0:60,
                 y = sin(seq(0, 6 * pi, length.out = 61))
)

shinyServer(function(input, output, session) {
  
  
  
  output$hot <- renderRHandsontable({
    
    add10 <- input$addRows
    
    if(input$clrBtn > lastClrBtn) {
      lastClrBtn <<- input$clrBtn
      
      DF <- isolate(hot_to_r(input$hot))
      DF <- data.frame(x = as.character(""),
                       y = as.numeric(NA)) # as.numeric to avoid boolean
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
  
  
  get_DF <- reactive({
    DF <- hot_to_r(input$hot)
    DF <- na.exclude(DF)
    # check if input is in iso8601 date format
    is_date <- grepl("\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d.*", DF$x[1])
    if(is_date) {
      DF <- DF[DF$x != "", ]
    }
    if(is_date) {
      DF$x <- as.POSIXct(DF$x)
    } else {
      DF$x <- as.numeric(DF$x)
    }
    
    return(DF)
  })
  
  output$plot <- renderPlot({
    
    dat <- get_DF()
    lag <- input$lag
    plot(dat$x, dat$y, pch = 16, xlab = "x", ylab = "y", type = "b")
    lines(dat$x[(1+lag):length(dat$x)], dat$y[1:(length(dat$x) - lag)], col = 2)
    points(dat$x[(1+lag):length(dat$x)], dat$y[1:(length(dat$x) - lag)], col = 2, pch = 16)
    if(input$arrow){
      arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[1:(length(dat$x) - lag)],
            y0 = dat$y[1:(length(dat$x) - lag)], y1 = dat$y[1:(length(dat$x) - lag)],
            col = "blue", alpha = 0.5, length = 0.1)
    }
    
  })
  
  output$plot2 <- renderPlot({
    
    dat <- get_DF()
    lag <- input$lag
    par(col.sub = 2)
    plot(dat$y[1:(length(dat$x) - lag)], dat$y[(1+lag):length(dat$x)], pch = 16,
         ylab = "y", xlab = "y shifted by lag", xaxt = "n",
         sub = paste0("Correlation = ",round(cor(dat$y[1:(length(dat$x) - lag)],
                                                 dat$y[(1+lag):length(dat$x)]), 4)))  
    axis(1, col = 2, col.ticks = 2, col.axis = 2)
    abline(0, 1)
    
  })
  
  
  output$plot3 <- renderPlot({
    
    dat <- get_DF()
    lag <- input$lag
    ac <- acf(dat$y, main = "")
    ac_lag <- as.vector(ac$acf)[lag + 1]
    points(lag, ac_lag, pch = 16, col = 2)
  })
  
  
})
