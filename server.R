# install.packages("shiny")
# install.packages("DT")
library(shiny)
shinyServer(function(input, output) {
  ####################
  # Agg by Sta Data: DataTable with pagination, search and filter; -1 means no pagination;
  ####################
  withProgress(message = 'Please wait ... Loading DataTable', value = 0, {
    # Number of times we'll go through the loop
    n <- 10
    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      senior.agg.by.sta.all.weather <-
        read.csv("senior.agg.by.sta.all.weather.csv",head = T)[,-1]
      # Increment the progress bar, and update the detail text.
      incProgress(1 / n, detail = paste("Doing part", i))
    }
  })
  
  output$ex1 <-
    DT::renderDataTable(DT::datatable(
      senior.agg.by.sta.all.weather[,-c(2,3,5,6,7,15)], options = list(lengthMenu = list(c(5, 25,-1), c('5', '25', 'All')),
                                                                       pageLength = 25)
    ))
  ####################
  # Graph traffic view
  ####################
  output$selectedVal <- reactive({
    input$region
  })
  # Fill in the spot we created for a plot
  output$AggByStaPlot <- renderPlot({
    if (input$region == "全部")
      dataToPlot <- senior.agg.by.sta.all.weather
    else
      dataToPlot <-
        senior.agg.by.sta.all.weather[unlist(senior.agg.by.sta.all.weather$station) == input$region,]
    
    if (input$viewby == "d" & input$region != "全部") {
      dataToPlot <- dataToPlot[order(dataToPlot$month, dataToPlot$day),]
      # render by date barplot
      scal <-
        ceiling(max(dataToPlot$countInDay) / max(dataToPlot$temp))
      maxY <-  max(dataToPlot$countInDay,dataToPlot$temp * scal)
      barplot(
        rbind(
          dataToPlot$countInMorning, dataToPlot$countInDay - dataToPlot$countInMorning
        ),
        names.arg = paste0(dataToPlot$month,"/", dataToPlot$day),
        ylim = c(0, maxY),
        col = c("grey50", "grey80"),
        ylab = "Number of People",
        xlab = "Date"
      )
    }
    
    else if (input$viewby == "m" & input$region != "全部") {
      dm1 <-
        aggregate(dataToPlot$countInDay ~ dataToPlot$station + dataToPlot$month, FUN = sum)
      dm2 <-
        aggregate(dataToPlot$countInMorning ~ dataToPlot$station + dataToPlot$month, FUN = sum)
      dm3 <-
        aggregate(dataToPlot$temp ~ dataToPlot$station + dataToPlot$month, FUN = mean)
      dataToPlot <-
        merge(
          dm3, merge(
            dm1, dm2, all.x = T, all.y = F, by = c("dataToPlot$station","dataToPlot$month")
          ),all.x = T, all.y = F, by = c("dataToPlot$station","dataToPlot$month")
        )
      colnames(dataToPlot) <-
        c("station", "month", "temp","countInDay","countInMorning")
      dataToPlot <- dataToPlot[order(dataToPlot$month),]
      dataToPlot$countInDay <- as.numeric(dataToPlot$countInDay)
      dataToPlot$countInMorning <-
        as.numeric(dataToPlot$countInMorning)
      dataToPlot$temp <- as.numeric(dataToPlot$temp)
      # render by month barplot
      scal <-
        ceiling(max(dataToPlot$countInDay) / max(dataToPlot$temp))
      maxY <-  max(dataToPlot$countInDay,dataToPlot$temp * scal)
      barplot(
        rbind(
          dataToPlot$countInMorning, dataToPlot$countInDay - dataToPlot$countInMorning
        ),
        names.arg = paste0(dataToPlot$month),
        ylim = c(0, maxY),
        col = c("grey50", "grey80"),
        ylab = "Number of People",
        xlab = "Month"
      )
    }
    
    else if (input$viewby == "d" & input$region == "全部") {
      dm1 <-
        aggregate(dataToPlot$countInDay ~ dataToPlot$month + dataToPlot$day, FUN = sum)
      dm2 <-
        aggregate(dataToPlot$countInMorning ~ dataToPlot$month + dataToPlot$day, FUN = sum)
      dm3 <-
        aggregate(dataToPlot$temp ~ dataToPlot$month + dataToPlot$day, FUN = mean)
      dataToPlot <-
        merge(
          dm3, merge(
            dm1, dm2, all.x = T, all.y = F,  by = c("dataToPlot$month","dataToPlot$day")
          ),all.x = T, all.y = F, by = c("dataToPlot$month","dataToPlot$day")
        )
      colnames(dataToPlot) <-
        c("month", "day", "temp","countInDay","countInMorning")
      dataToPlot <-
        dataToPlot[order(dataToPlot$month,dataToPlot$day),]
      # render by date barplot
      scal <-
        ceiling(max(dataToPlot$countInDay) / max(dataToPlot$temp))
      maxY <-  max(dataToPlot$countInDay,dataToPlot$temp * scal)
      barplot(
        rbind(
          dataToPlot$countInMorning, dataToPlot$countInDay - dataToPlot$countInMorning
        ),
        names.arg = paste0(dataToPlot$month,"/", dataToPlot$day),
        ylim = c(0, maxY),
        col = c("grey50", "grey80"),
        ylab = "Number of People",
        xlab = "Date"
      )
    }
    
    
    else{
      # month # all
      dm1 <-
        aggregate(dataToPlot$countInDay ~ dataToPlot$month, FUN = sum)
      dm2 <-
        aggregate(dataToPlot$countInMorning ~ dataToPlot$month, FUN = sum)
      dm3 <-
        aggregate(dataToPlot$temp ~ dataToPlot$month, FUN = mean)
      dataToPlot <-
        merge(
          dm3, merge(
            dm1, dm2, all.x = T, all.y = F, by = "dataToPlot$month"
          ),all.x = T, all.y = F, by = c("dataToPlot$month")
        )
      colnames(dataToPlot) <-
        c("month", "temp","countInDay","countInMorning")
      dataToPlot <- dataToPlot[order(dataToPlot$month),]
      dataToPlot$countInDay <- as.numeric(dataToPlot$countInDay)
      dataToPlot$countInMorning <-
        as.numeric(dataToPlot$countInMorning)
      dataToPlot$temp <- as.numeric(dataToPlot$temp)
      # render by month barplot
      scal <-
        ceiling(max(dataToPlot$countInDay) / max(dataToPlot$temp))
      maxY <-  max(dataToPlot$countInDay,dataToPlot$temp * scal)
      barplot(
        rbind(
          dataToPlot$countInMorning, dataToPlot$countInDay - dataToPlot$countInMorning
        ),
        names.arg = paste0(dataToPlot$month),
        ylim = c(0, maxY),
        col = c("grey50", "grey80"),
        ylab = "Number of People",
        xlab = "Month"
      )
    }
    
    # append temp in plot
    lines(dataToPlot$temp * scal, col = "green")
    axis(4, at = seq(0, ceiling(max(
      dataToPlot$temp
    )) * scal,  by = scal),
    labels = seq(0,ceiling(max(
      dataToPlot$temp
    ))))
    mtext(side = 4, line = -1, "Temperature (Celcius)")
    legend(
      "topright",
      legend = c("morning", "afternoon"),
      fill = c("grey50", "grey80")
    )
  })
  
})