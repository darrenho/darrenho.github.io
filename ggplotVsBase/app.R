if (!require('shiny')){install.packages("shiny");require(shiny)}
###
# Class Example
###
if (!require('ggplot2')) install.packages("ggplot2");require('ggplot2')

ui = fluidPage(
  titlePanel("ggplot"),
  # User input: number of bins for histogram
  sidebarLayout(
    sidebarPanel(
      #This is a "widget"
      #   The first two arguments are always 
      #      - inputId: A unique character 
      #      - label:  what does the user see as a description?
      radioButtons(inputId     = "plotType",
                  label        = "Type of Plot:",
                  choiceNames  = c('Histogram','Boxplot','Scatter Plot'),
                  choiceValues = c('histogram','boxplot','scatter'))
    ),
    
    # this directs where to send the user input to
    #  -> Send user input to "main panel" on site
    mainPanel("",
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
              )
    )
  )
)


server = function(input, output) {
  
  # renderPlot refreshed the display whenever the user input changes
  #   input:   has an object "bins" which is 
  #            chosen by the user w/ widget
  #   output:  The UI directs the user input to be displayed at "output"
  amNew = rep('automatic',nrow(mtcars))
  amNew[mtcars$am == 1] = 'manual'
  transmission = factor(amNew)
  baseScatterColor = rep('red',nrow(mtcars))
  baseScatterColor[transmission == 'manual'] = 'blue'
  pt_base = reactive({if(input$plotType == 'histogram'){
                    return(hist(mtcars$wt))
                  }else if(input$plotType == 'scatter'){
                    plot(mtcars$wt,mtcars$mpg,col=baseScatterColor)
                    abline(lm(mtcars$mpg~mtcars$wt,subset = transmission == 'manual'),col='blue')
                    abline(lm(mtcars$mpg~mtcars$wt,subset = transmission == 'automatic'),col='red')
                    return(NULL)
                  }else if(input$plotType == 'boxplot'){
                    return(boxplot(mtcars$mpg~transmission))
                  }
                })
  pt_ggplot = reactive({if(input$plotType == 'histogram'){
                  return(ggplot(mtcars, aes(x=mpg, fill = transmission)) + geom_histogram(bins = 10) + ggtitle("Histogram of MPG!"))
                }else if(input$plotType == 'scatter'){
                  return(ggplot(mtcars, aes(x=wt, y=mpg, colour = transmission)) + 
                           geom_point(size=2, shape=23) +
                           #geom_text(label=rownames(mtcars)) +
                           geom_smooth(method = "lm") +
                           ggtitle("Scatter and Linear Regression of MPG vs. WT")
                         )
                }else if(input$plotType == 'boxplot'){
                  fill <- "#4271AE"
                  line <- "#1F3552"
                  return(ggplot(mtcars, aes(x=transmission, y = mpg, fill = transmission)) + 
                           geom_boxplot(colour = line) + 
                           ggtitle('Boxplot of mpg by transmission') +
                           geom_jitter())
                }
                })
  output$plotgraph1 = renderPlot({pt_ggplot()})
  output$plotgraph2 = renderPlot({pt_base()})
}

shinyApp(ui = ui, server = server,
         options = list(height = 500))