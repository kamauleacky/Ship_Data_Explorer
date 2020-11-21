
# Define functions/objects exposed to both UI and Server
#List of packages to load
PkgList <- list('shiny', 'shiny.semantic', 'semantic.dashboard', 'shinyjs',
                'tidyverse', 'leaflet', 'geosphere')
# 'feather', vroom: called respective functions directly

lapply(PkgList, function(x)do.call('require', list(x)))


#Define module for dropdowns
# https://stackoverflow.com/questions/46134469/shiny-dynamic-tabs-dependent-on-other-tabs-how-to-create-them-simultaneously

ShipDropdown <- function(id, label = "shipselect", choiceselect='<None Available>') {
  ns <- NS(id)
  
  tagList(
    selectizeInput(ns("shipselect")
                   , label =label
                   , choices = choiceselect
    )
    
  )
}

ShipDropdownServer <- function(id, choiceselect) {
  moduleServer(
    id,
    function(input, output, session) {
       updateSelectizeInput(session = getDefaultReactiveDomain()
                           , inputId = "shipselect"
                           # ,label = label
                           , choices =c('<Select one>', choiceselect)
                           ,selected = '<Select one>'
      )
      
      choicelist <- reactive(input$shipselect)
      return(choicelist)
      
     }
  )
}


