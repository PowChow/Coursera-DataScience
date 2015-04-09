library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fashion on Twitter"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("caption", "Report Title:", "Data Summary"),
      
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("@WhoWhatWear", "@SteveMadden", "@refinery29")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
	 # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      verbatimTextOutput("summary"), 
      
      tableOutput("view")
    )
  )
))
