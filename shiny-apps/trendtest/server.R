library("rhandsontable")
library("plotly")
library("dplyr")
library("reshape2")
library("ggplot2")
library("gridExtra")
library("Kendall")
library("xts")
library("mgcv")

lastClrBtn   <- 0
lastAddRows  <- 0
lastInputRun <- 0

## DF structure and example data
DF <- data.frame(
  x = c(paste0("200", 0:9, "-01-01"), paste0("20", 10:15, "-01-01")),
  y = 9 + 0:15*0.04 + rnorm(16,0, 0.5)
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
    DF$mode <- "Obs"
    DF$tile <- "1 - Time series"

    if(input$smooth == "lm") {
      lm <- lm(y ~ x, DF)
    } else if(input$smooth == "loess"){
      if(is_date) {
        DFn <- DF
        DFn$x <- as.numeric(DFn$x)
        lm <- loess(y ~ x, DFn)
      } else {
        lm <- loess(y ~ x, DF)
      }
    }


    DF <- rbind(DF, data.frame(x = DF$x,
                               y = residuals(lm),
                               mode = rep("res", length(DF$x)),
                               tile = rep("2 - Residuals", length(DF$x))))

    return(DF)
  })

  output$temp_ts <- renderPlotly({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- get_DF()

        p1 <- ggplot() +
          geom_hline(data = data.frame(tile = "2 - Residuals"), aes(yintercept = 0), col = "grey") +
          geom_point(data = DF, aes(x = x, y = y, col = mode)) +
          geom_line(data = DF[DF$mode == "Obs", ], aes(x = x, y = y, col = mode)) +
          geom_smooth(data = DF[DF$mode == "Obs", ],aes(x = x, y = y), method = input$smooth) +
          facet_wrap(~tile, scales = "free") + theme(legend.position = "none")
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
        DF <- get_DF()

        AC <- acf(DF$y[DF$mode == "res"], plot = FALSE)
        conf.level <- 0.95
        ciline <- qnorm((1 - conf.level)/2)/sqrt(length(DF$y))
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


  output$resdist <- renderPlotly({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- get_DF()

        dist <- data.frame(x = seq(min(DF$y[DF$mode == "res"]),
                                   max(DF$y[DF$mode == "res"]),
                                   length.out = 200),
                           y = dnorm(seq(min(DF$y[DF$mode == "res"]),
                                         max(DF$y[DF$mode == "res"]),
                                         length.out = 200),
                                     mean(DF$y[DF$mode == "res"]),
                                     sd(DF$y[DF$mode == "res"])))

        p <- ggplot(DF[DF$mode == "res", ], aes(x = y)) +
        geom_histogram(aes(y = ..density..), bins = round(length(DF$x[DF$mode == "Obs"])/3),
                       fill = "bisque3", col = "darkgrey") +
        geom_density(aes(y=..density..), col = "blue4", lwd = 1.2) +
        geom_line(data = dist, aes(x, y), col = "green4", lty = "dashed", lwd = 1.2) +
        xlab("Residuals") + ggtitle("Distribution residuals")
        ggplotly(p)
      } else {
        NULL # return NULL if input$hot is not yet initialized
      }
    })
  })


  sumTable <-  reactive({

    DF <- get_DF()
    if(is.numeric(DF$x)) {
      ts <- xts(DF$y[DF$mode == "Obs"], as.POSIXct(DF$x[DF$mode == "Obs"],
                                                   origin = "2000-01-01"))
    } else {
      ts <- xts(DF$y[DF$mode == "Obs"], DF$x[DF$mode == "Obs"])
    }

    ken <- MannKendall(ts)
    lm <- lm(y ~ x, DF[DF$mode == "Obs", ])

    setNames(data.frame(c("Linear trend", "Mann Kendall test"),
                        c("Slope", "Tau"),
                        c(lm$coefficients[2] *
                            ifelse(is.numeric(DF$x), 1, (365.25*24*3600)),
                          ken$tau[1]),
                        c(summary(lm)$coefficients[2, 4], ken$sl[1])),
             c("Method", "Measure","Value", "p.value"))
  })

  output$sumTable <- renderTable({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        sumTable()
      } else {
        NULL
      }
     }
  )}, digits = 4)
})
