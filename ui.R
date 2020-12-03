
shinyUI(semanticPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebar_layout(
    sidebar_panel (
      
      ShipDropdownUI("shiptype"),
      ShipDropdownUI("shipname")
      
      ,
      
      vertical_layout(
        useShinyjs(),
        (
          action_button('shipreport', 'Show maximum distance location for selection on map')
        ),
        useShinyjs(),
        (
          action_button('generatereport', 'Generate report for download')
        )
        ,
        
        shinyjs::disabled(
          downloadButton('download', 'Download Report')
        )
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
      ),
      
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
