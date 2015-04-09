library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fashion on Twitter"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a brand:"),
      
      selectInput("data", 
                  label = "Choose a twitter handle to display:",
                  choices = list("@WhoWhatWear", "@Fashionista_com",
                                 "@refinery29"), 
                            selected = "@WhoWhatWear"),
      
      sliderInput("obs", 
                  label = "Number of Observations:",
                  min = 0, max = 2000, value = c(0, 2000)),
      
      br(),
      a(href = "www.pauline.chow.com", "Source code")
    ),
    
  # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
    mainPanel(
      textOutput("summary"),
      plotOutput('sentiment_plot'),
      dataTableOutput("top5")  
#       dataTableOutput('bottom5')
    )
  )
))
