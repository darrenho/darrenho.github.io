if (!require('shiny')){install.packages("shiny");require(shiny)}
###
# More complicated design
###
if (!require('ggplot2')) install.packages("ggplot2")

ui = fluidPage(
  
  # User input: number of bins for histogram
  sidebarLayout(
    sidebarPanel(
      #This is a "widget"
      #   The first two arguments are always 
      #      - inputId: A unique character 
      #      - label:  what does the user see as a description?
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # this directs where to send the user input to
    #  -> Send user input to "main panel" on site
    mainPanel(
      # Define what type of output should server expect
      #  (example: plot, html, image, table, ..)
      #   outputId: a character used to connect UI input to server output
      plotOutput(outputId = "distPlot")
    )
  )
  
)


server = function(input, output) {
  
  # renderPlot refreshed the display whenever the user input changes
  #   input:   has an object "bins" which is 
  #            chosen by the user w/ widget
  #   output:  The UI directs the user input to be displayed at "distPlot"
  output$distPlot = renderPlot({
    
    ggplot(faithful, aes(x = waiting)) +
      geom_histogram(bins = input$bins, colour = "white")
    
  })
  
}

shinyApp(ui = ui, server = server,
         options = list(height = 500))