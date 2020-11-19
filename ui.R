
library(shiny)
library(shiny.semantic)
library(leaflet)

shinyUI(semanticPage(

  # Application title
  titlePanel("Ship Data Explorer"),

   sidebar_layout(
    sidebar_panel(
      slider_input("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    main_panel(
              h1('Stats'),

      split_layout(
        dropdown_input("simple_dropdown", LETTERS[1:5], value = "A", type = "selection multiple"),
    
        dropdown_input("simple_dropdown2", LETTERS[1:5], value = "A", type = "selection multiple"),
        dropdown_input("simple_dropdown", LETTERS[1:5], value = "A", type = "selection multiple"),
        dropdown_input("simple_dropdown", LETTERS[1:5], value = "A", type = "selection multiple")
      
        
      ),
      
      split_layout(
        leafletOutput('shiproute')

      ),
      split_layout(        
        textOutput('footnote1'),
        
        textOutput('footnote2'),
        
        textOutput('footnote3')
        
      )
    )
  )
))
