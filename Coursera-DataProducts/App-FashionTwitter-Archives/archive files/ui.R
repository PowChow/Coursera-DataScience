library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("Fashion on Twitter"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose Twitter Brand:", 
                  choices = c("@WhoWhatWear", "@refinery29", "@REVOLVEclothing", '@SteveMadden',
                              '@Fashionista_com', '@DailyFrontRow', '@Singer22')),
      actionButton("update", "Update"), 
      br(), #line break
      sliderInput("max", 
            "Maximum Number of Tweets:", 
            min = 1,  max = 300,  value = 100)
    ),
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Summary", htmlOutput("summary")),                   
        tabPanel('WordCloud', plotOutput('wordcloud')),          
        tabPanel('Sentiment Plot by Time', plotOutput("plot")),
        tabPanel("Sentiment Tweet Table", tableOutput("table"))
      )
    ) 
  )
))
