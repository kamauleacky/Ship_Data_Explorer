#List of packages to load
 
PkgList <- list('shiny', 'vroom', 'tidyverse', 'leaflet')
lapply(PkgList, function(x)do.call('require', list(x)))

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  
  output$footnote <- renderText({
   paste('The stats for the selected ship are as follows...') 
  }  )

})
