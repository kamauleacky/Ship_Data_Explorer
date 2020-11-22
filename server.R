#Load ship data once across sessions
if(!file.exists('Data/ships.feather')){
shipdata <- vroom::vroom('Data/ships.csv')
names(shipdata) <- tolower(names(shipdata))
sort(names(shipdata))
names(shipdata)[25] <- 'portfr'#Fix duplicate names

feather::write_feather(shipdata, 'Data/ships.feather')
}else{
  shipdata <- feather::read_feather('Data/ships.feather')
}

shinyServer(function(input, output, session) {
  
#status indicator
  status <- reactiveValues(value='map initializing...', gpscord='')
  
  
#Update Side bar area
#Vessel type
  shipdrop_choice <- shipdata %>% distinct(shiptype) %>%
    arrange(shiptype) %>% pull()
    
 shiptypeselect <-  ShipDropdownServer("shiptype", choiceselect = shipdrop_choice)
   
  #Vessel Name
  shipnameselect <- reactive({
  req(shiptypeselect())
  validate(
    need(is.numeric(as.numeric(gsub('[^0-9]','', shiptypeselect()))),
         'Please select a vessel type')
  )

   choicenames <-  shipdata %>% dplyr::filter(shiptype==as.numeric(shiptypeselect())) %>%
      distinct(shipname) %>% arrange(shipname) %>% pull() %>%
      as.character()

 return(
    ShipDropdownServer("shipname", choiceselect = choicenames)
 )
    })
  
  
observeEvent(input$shipreport, {
output$shiproute <- renderLeaflet({
      status$value <- "<font color=\"#FF00FF\"><b>consecutive points with largest distance travelled</b></font>"
 RenderLeafletMap(latlon = vesselshipreport()[[1]])
        
     })
shinyjs::hide(input$ship)
     
   })

 
  #Display info on selected vessel/ship
vesselshipreport <- reactive({
    req(InputChecker(shiptypeselect()),
        InputChecker(shipnameselect()())
    )

outvessel <- VesselTypeData(selectedvessel = shiptypeselect(), shipdatafile = shipdata)

outship <-  ShipSelectedData(selectedship = shipnameselect()(), vesseltypedata = outvessel)

outreport <- ShipReportData(shipseldata = outship)


gpscord <- paste0('For the selected vessel,
                        the longest distance travelled between two consecutive coordinates was ',
                        round(unique(outreport$distcord),4), 'm. This trip occured most recently on ',
                        unique(outreport$datetime), '. To show the location on the map,
                  please click on the corresponding button.')
    return(list(report=outreport, reporttext=gpscord)) 
  })
#   
  
  
  # update header section
  
  output$header1 <- renderUI({
    val <- shipdata %>% distinct(ship_id) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Unique ships', value=val, icon = shiny::icon("ship"), color = "blue", width = 2, size = "")
  }  )
  
  output$header2 <- renderUI({
    val <- shipdata %>% distinct(ship_type) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Unique types of ships', value=val, icon = shiny::icon("cart-arrow-down"),  color = "green", width = 2, size = "")
  }  )
  
  
  output$header3 <- renderUI({
    val <- shipdata %>% distinct(flag) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Maritime flags', value=val, icon = shiny::icon("flag"), color = "red", width = 2, size = "")
  }  )
  
  
  output$header4 <- renderUI({
    val <- shipdata %>% distinct(destination) %>% dplyr::summarise(n()) %>% pull()
    infoBox('Destinations', value=val, icon = shiny::icon("map-marker-alt"),color = "purple", width = 2, size = "")
  }  )
  
  
  #Update Map area 
  #Update map with selected ship data
#Default map shows distinct maritime flags locations... upon user input, update map accordingly

  shipcords <- reactive({
  if(!grepl('<select one>|<none available>',tolower(shipnameselect()()))){
      # status$value <- "<font color=\"#FF0000\"><b>distinct locations of selected vessel</b></font>"
   out <-   shipdata %>%
    dplyr::filter(shiptype==as.numeric(shiptypeselect())&
                  shipname==shipnameselect()()
                  ) %>% dplyr::distinct(lon, lat, .keep_all=T)
   # status$gpscord <- out
   
} else{
  status$value <- "<font color=\"#426cf5\"><b>unique destinations across all data</b></font>"
 out <-  shipdata %>% dplyr::distinct(destination, .keep_all=T)
 # status$gpscord <- out
 }
  return(out)
  
})
 ##Render map
#- Default render
output$shiproute <- renderLeaflet({
    RenderLeafletMap(latlon = shipcords())
  })
  
# - Updated with selected vessel info
observeEvent(vesselshipreport(),{
  status$value <- "<font color=\"#FF0000\"><b>distinct locations of selected vessel</b></font>"
  
  output$shiproute <- renderLeaflet({
    RenderLeafletMap(latlon = shipcords())
  })
})
  
  #Update footer section
   
  output$footnote1 <- renderText({
    paste('Currently showing location(s) of:', status$value) 
  }  )
  
  output$footnote2 <- renderText({
    vesselshipreport()$reporttext
    }  )
  
  # output$footnote3 <- renderText({
  #   paste('The stats for the selected ship are as follows...') 
  # }  )
  
})
