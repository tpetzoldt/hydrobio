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
    if(length(dat$x) == 0) {
      NULL
    } else {
      lag <- input$lag
      par(mar = c(4,4,4,4))
      plot(dat$x, dat$y, pch = 16, xlab = "x", ylab = "y", type = "b")
      lines(dat$x[(1+lag):length(dat$x)], dat$y[1:(length(dat$x) - lag)], col = 2)
      points(dat$x[(1+lag):length(dat$x)], dat$y[1:(length(dat$x) - lag)], col = 2, pch = 16)
      if(input$arrow) {
        arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[1:(length(dat$x) - lag)],
              y0 = dat$y[1:(length(dat$x) - lag)], y1 = dat$y[1:(length(dat$x) - lag)],
              col = "blue", alpha = 0.5, length = 0.1)
      }
      if (input$comp) {
        arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[(1+lag):length(dat$x)],
               y0 = dat$y[1:(length(dat$x) - lag)], y1 = dat$y[(1+lag):length(dat$x)],
               col = "blue", alpha = 0.5, length = 0, lty = 1)
      }
      if (input$cor) {
        y <- dat$y[1:(length(dat$x) - lag)]
        yl <- dat$y[(1+lag):length(dat$x)]
        abline(h = mean(dat$y))
        arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[(1+lag):length(dat$x)],
               y0 = mean(dat$y), y1 = dat$y[(1+lag):length(dat$x)],
               col = 1, length = 0, lty = 17)
        arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[(1+lag):length(dat$x)],
               y0 = mean(dat$y), y1 = dat$y[1:(length(dat$x) - lag)],
               col = 2, length = 0, lty = 15)
        aco <- cumsum((y - mean(dat$y))*(yl - mean(dat$y)))/sum((dat$y - mean(dat$y))^2)
        par(new = TRUE)
        aco <- c(rep(NA, length(dat$x) - length(y)), aco)
        scl <- 0.5*length(dat$x)
        x <- dat$x[1:length(dat$x)]
        plot(x, aco, 'l', xaxt = "n", yaxt = "n", col ="green3", lwd = 2,
             ylim = c(-1, 1), xlab = "", ylab = "")
        axis(4, col = 3, col.ticks = 3, col.axis = 3)
        abline(h = 0, lwd = 1, col = 3, lty = 17)
        arrows(x1 = dat$x[(1+lag):length(dat$x)], x0 = dat$x[(1+lag):length(dat$x)],
               y0 = 0, y1 = (y - mean(dat$y))*(yl - mean(dat$y))/sum((dat$y - mean(dat$y))^2) * scl,
               col = 3, length = 0.1, lwd = 2)
        mtext("correlation", 4, col =3, line = 2)
        par(new = FALSE)
      }
    }
  })

  output$plot2 <- renderPlot({

    dat <- get_DF()
    if(length(dat$x) == 0) {
      NULL
    } else {
      lag <- input$lag
      y <- dat$y[1:(length(dat$x) - lag)]
      yl <- dat$y[(1+lag):length(dat$x)]
      lim <- c(min(c(y, yl)), max(c(y, yl)))
      par(mar = c(4,4,4,4))
      plot(y, yl, pch = 16, xlim = lim, ylim = lim,
           ylab = "y", xlab = "y shifted by lag", xaxt = "n", col = 4,)
      axis(1, col = 2, col.ticks = 2, col.axis = 2)
      mtext(paste0("Correlation = ",round(sum((y - mean(dat$y))*(yl - mean(dat$y)))/sum((dat$y - mean(dat$y))^2), 4)), 1, line = 1.75, col = 3)
      abline(0, 1)
    }

  })


  output$plot3 <- renderPlot({

    dat <- get_DF()
    if(length(dat$x) == 0) {
      NULL
    } else {
      lag <- input$lag
      par(mar = c(4,4,4,4))
      ac <- acf(dat$y, main = "")
      ac_lag <- as.vector(ac$acf)[lag + 1]
      points(lag, ac_lag, pch = 16, col = 3)
    }
  })


})
