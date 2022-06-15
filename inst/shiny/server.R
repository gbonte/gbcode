
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
D<-NULL
BOUND1=2
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(input$mean-BOUND1,input$mean+BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$mean,input$variance),
         ylab="density")
    
  })
  
  output$uniPlotH <- renderPlot( {
    
    D<<-rnorm(input$N,input$mean,input$variance)
    hist(D)
    
    
  })
  
  output$uniPlotD <- renderPlot( {
      input$mean
      input$variance
      plot(D,0*D)
    
    
  })
  
})
