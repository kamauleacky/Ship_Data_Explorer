library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(tidyverse)
library(leaflet)
library(geosphere)
library(shinyjs)
library(rmarkdown)

ShipDropdownUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('shipselect'))
  )
}


ShipDropdownServer <- function(id,label, choices) {
  moduleServer(id, function(input, output, session) {
    output$shipselect <- renderUI({
      ns <- session$ns
      selectInput(ns('shipselect'), label = label, choices = choices)
    })
    return(reactive(input$shipselect))
  })
}