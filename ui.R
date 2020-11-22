# 
# library(shiny)
# library(shiny.semantic)
# library(leaflet)
# library(semantic.dashboard)

shinyUI(semanticPage(

  # Application title
  titlePanel("Ship Data Explorer"),

   sidebar_layout(
    sidebar_panel(
   
      ShipDropdown("shiptype", label = "Select vessel type"),
      
      ShipDropdown("shipname", label = "Select vessel name"),
      
      vertical_layout(
      # dataTableOutput('shipstats'),
      useShinyjs(),
      (
      action_button('shipreport', 'Show maximum distance location for selection on map')
      )
      
      
    ,
    downloadButton('download', 'Download markdown report (to implement)')
    )
    ),

    # Show a plot of the generated distribution
    main_panel(
 fluidRow(
      split_layout(
        uiOutput('header1'),
        uiOutput('header2'),
        uiOutput('header3'),
        uiOutput('header4')
      )
)
      ,
      
      fluidRow(
        leafletOutput('shiproute')

      ),
      
fluidRow(
        split_layout(        
        htmlOutput('footnote1'),
        
        htmlOutput('footnote2')
        
        # textOutput('footnote3')
        
      )
)
    )
  )
))
