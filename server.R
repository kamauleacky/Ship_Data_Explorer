#List of packages to load
 
PkgList <- list('shiny', 'vroom', 'tidyverse', 'leaflet')
lapply(PkgList, function(x)do.call('require', list(x)))

#Load ship data once across sessions
shipdata <- vroom('Data/ships.csv')
names(shipdata) <- tolower(names(shipdata))
sort(names(shipdata))
names(shipdata)[25] <- 'portfr'


shinyServer(function(input, output) {

  # update header section
  
  output$header1 <- renderInfoBox({
    val <- shipdata %>% distinct(ship_id) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Unique ships', value=val, icon = icon("ship"), color = "blue", width = 5, size = "")
  }  )
  
  output$header2 <- renderInfoBox({
    val <- shipdata %>% distinct(ship_type) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Unique types of ships', value=val, icon = icon("cart-arrow-down"), color = "green", width = 5, size = "")
  }  )
  
  
  output$header3 <- renderInfoBox({
    val <- shipdata %>% distinct(ship_id) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Maritime flags', value=val, icon = icon("flag"), color = "red", width = 5, size = "")
  }  )
  
  
  output$header4 <- renderInfoBox({
    val <- shipdata %>% distinct(ship_id) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Destinations', value=val, icon = icon("map-marker-alt"), color = "purple", width = 5, size = "")
  }  )
  
  
  #Update Map area
  # points <- eventReactive(input$bins , {
  #   cbind(shipdata[1, c("lon", "lat")] )
  # }, ignoreNULL = FALSE)
  
  points <-reactive( shipdata %>% dplyr::distinct(flag, .keep_all=T)#(lon==min(lon)|lat==min(lat))
                     )
  
  output$shiproute <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  
  
  #Update footer section
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
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
