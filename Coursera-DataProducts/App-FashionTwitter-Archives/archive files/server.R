library(shiny)
library(shinyIncubator)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  # Return the requested dataset
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    isolate({
      withProgress(session, {
        setProgress(message = "Processing tweets...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    dist <- datasetInput$dist
    n <- datasetInput$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderText({
    'I will write a summary of results here'
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$wordcloud <- renderPlot({
    v <- terms()
    set.seed(1589)
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 3, max.words=Input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
})
