#R helper functions
#Upon vessel type selection, number of available ships
VesselShip <- function(selectedvessel, shipdatafile){
  stopifnot(!is.na(as.numeric(selectedvessel)))
  shipdatafile %>% dplyr::filter(shiptype==selectedvessel)
}

ShipStatsSelected <- function(selectedship, shipdatafile){
  
}
  
