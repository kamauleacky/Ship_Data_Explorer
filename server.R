#List of packages to load
 
PkgList <- list('shiny', 'vroom', 'tidyverse', 'leaflet')
lapply(PkgList, function(x)do.call('require', list(x)))

#Load ship data once across sessions
shipdata <- vroom('Data/ships.csv')
names(shipdata) <- tolower(names(shipdata))
sort(names(shipdata))
names(shipdata)[25] <- 'portfr'


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  # points <- eventReactive(input$bins , {
  #   cbind(shipdata[1, c("lon", "lat")] )
  # }, ignoreNULL = FALSE)
  
  points <-reactive( shipdata %>% dplyr::filter(lon==min(lon)|lat==min(lat)))
  
  output$shiproute <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  
  
  output$footnote1 <- renderText({
    paste('The stats for the selected ship are as follows...') 
  }  )
  
  output$footnote2 <- renderText({
    paste('The stats for the selected ship are as follows...') 
  }  )
  
  output$footnote3 <- renderText({
    paste('The stats for the selected ship are as follows...') 
  }  )
  
})
