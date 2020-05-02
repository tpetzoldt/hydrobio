library("rhandsontable")

lastClrBtn <- 0

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

  output$hot = renderRHandsontable({
    if(input$clrBtn) {
      print(lastClrBtn)
      DF[,] <- as.numeric(NA) # as.numeric to avoid boolean
    } else {
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
      } else {
        print("startup")
      }
    }

    rhandsontable(DF)  %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) #%>%
      #hot_col(c("Depth"), type="numeric") %>%
      #hot_col(c("Temp"), type="numeric") %>%
      #hot_col("Oxygen", type="numeric")
  })

  output$multiprobe <- renderPlot({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        print(str(DF))

        par(mfrow=c(1, 3), las=1)
        columns <- c("Temp", "Oxygen")

        plot(0,0, ylim = rev(na.omit(range(DF$Depth))), xlim=range(na.omit(as.vector(DF[columns]))),
             type = "n", ylab="z (m)", xlab=paste(columns, collapse=", "))


        lapply(1:length(columns), function(i) {
          lines(DF[, columns[i]], DF$Depth, col=i)
        })
        legend("bottomright", legend=columns, col=1:length(columns), lty=1)

        plot(DF$pH, DF$Depth, xlab="pH", ylab="z (m)",
             xlim = range(c(na.omit(DF$pH), 6.5, 7.5)),
             ylim=rev(na.omit(range(DF$Depth))), type="l")
        abline(v=7, lty="dashed", col="grey")

        plot(DF$Cond, DF$Depth, xlab="Cond", ylab="z (m)",
             xlim = range(c(na.omit(DF$Cond))),
             ylim=rev(na.omit(range(DF$Depth))), type="l")
      } else {
        # placeholder, do nothing
      }
    })
  })

  output$light <- renderPlot({
    input$runBtn
    isolate({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
        #analysis <- get_analysis()

        par(mfrow=c(1, 2), las=1)
        plot(DF$Light, DF$Depth, xlab="Light", ylab="z (m)",
             xlim = range(c(na.omit(DF$Light))),
             ylim=rev(na.omit(range(DF$Depth))), type="l")

        plot(log(DF$Light), DF$Depth, xlab="Light", ylab="z (m)",
             xlim = log(c(0.1, 100)),
             ylim=rev(na.omit(range(DF$Depth))), type="p", axes=FALSE)

        axis(2)
        xtic <- 10^(-2:2)
        axis(1, label=xtic, at=log(xtic))
        box()

        m <- lm(log(Light) ~ Depth, data=DF)

        x <- range(DF$Depth)
        y <- coef(m)[1] + coef(m)[2] * x

        lines(y, x)
        ab <- round(coef(m), 2)
        legend("topleft", lty=1, paste("I =", ab[1], ab[2], "* z"))

      } else {
        # placeholder, do nothing
      }
    })
  })

})

