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
      
      dataTableOutput('shipstats'),
      textOutput('shiptext')
      
      
      
    ),

    # Show a plot of the generated distribution
    main_panel(
 fluidRow(
      split_layout(
        infoBoxOutput('header1'),
        
        infoBoxOutput('header2'),
        infoBoxOutput('header3'),
        infoBoxOutput('header4')
        
      )
)
      ,
      
      fluidRow(
        leafletOutput('shiproute')

      ),
      
fluidRow(
        split_layout(        
        textOutput('footnote1'),
        
        textOutput('footnote2'),
        
        textOutput('footnote3')
        
      )
)
    )
  )
))
