library(shiny)
library(datasets)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(Hmisc)

wd.datapath = paste0(getwd(),"/data")
wd.init = getwd()
setwd(wd.datapath)

shinyServer(function(input, output){
    filedata <- reactive({
      infile <- input$data
      data.frame(read.csv(paste(input$data, '_final_val.csv')))
    })
  
    output$summary <- renderText({
      paste('Most recent', nrow(filedata()), 'tweet sentiments of', input$data) 
    })
    
    output$sentiment_plot <- renderPlot({
      brand = filedata()
      brand = brand[0:input$obs[2],]
      brand$hrs = ymd_hms(brand$time)
      ggplot(brand, aes(hrs, score)) + geom_point() +
        stat_summary(fun.data = 'mean_cl_boot', mult = 1, geom = 'smooth') +
        ggtitle(paste(input$data, as.Date(brand$created))) + scale_y_continuous(limits=c(1, 9), breaks=c(1:9))
    })      
      
    output$top5 <- renderDataTable({
      brand = head(subset(filedata(), select=c('score', 'text')))
      #head(brand, n=5)
  }, options = list(lengthMenu = c(5, 20, 50), pageLength = 5))

})
