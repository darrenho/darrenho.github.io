if (!require('shiny')) install.packages("shiny")
###
# Class Example
###
if (!require('ggplot2')) install.packages("ggplot2")

ui = fluidPage(
  titlePanel("Central Limit Theorem"),
  # User input: number of bins for histogram
  sidebarLayout(
    sidebarPanel(
      #This is a "widget"
      #   The first two arguments are always 
      #      - inputId: A unique character 
      #      - label:  what does the user see as a description?
      sliderInput(inputId = "nReps",
                  label = "Number of replications:",
                  min = 1,
                  max = 100,
                  value = 1),
      sliderInput(inputId = "nObs",
                  label = "Number of observations:",
                  min = 1,
                  max = 100,
                  value = 30),
      radioButtons(inputId     = "distType",
                  label        = "Population Distribution:",
                  choiceNames  = c('Normal','Exponential'),
                  choiceValues = c('normal','exponential')),
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
    if(input$distType == 'normal'){
      sampAndMeanF = function(nObs){
        return(mean(rnorm(nObs)))
      }
    }
    if(input$distType == 'exponential'){
      sampAndMeanF = function(nObs){
        return(mean(rexp(nObs)))
      }
    }
    nRepsGrid        = rep(input$nObs,input$nReps)
    sampleMeans      = sapply(nRepsGrid,sampAndMeanF)
    sampleMeans_df   = data.frame('sampleMeans'=sampleMeans)
    ggplot(sampleMeans_df,aes(x = sampleMeans)) +    
      geom_histogram(bins = input$bins, colour = "white")
  })
}

shinyApp(ui = ui, server = server,
         options = list(height = 500))