#R helper functions
InputChecker <- function(var){
  if(grepl('<select one>|<none available>', tolower(as.character(var)))){
    return(NULL)
  }else{
    return(TRUE)
  }
}

#Render map

RenderLeafletMap <-function(latlon){
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = latlon)

}

#Upon vessel type selection, number of available ships


VesselTypeData <- function(selectedvessel, shipdatafile){
  stopifnot(!is.na(as.numeric(selectedvessel)))
  shipdatafile %>% dplyr::filter(shiptype==selectedvessel)
}

ShipSelectedData <- function(selectedship, vesseltypedata){
  shipdata <- vesseltypedata %>% dplyr::filter(shipname==selectedship)
}

ShipReportData <- function(shipseldata){
  shipselout <- shipseldata %>% 
    dplyr::mutate(latprev=lag(lat), lonprev=lag(lon),
                  latprev=case_when(is.na(latprev)~lat, TRUE~latprev),
                  lonprev=case_when(is.na(lonprev)~lon, TRUE~lonprev)) %>% 
    rowwise() %>% 
    dplyr::mutate(
                  distcord=c(distm(x=c(lon, lat), y=c(lonprev, latprev), fun=distGeo) ),
                  distcord=round(distcord, 4)
  ) %>% data.frame()
  
  shipmaxdist <- shipselout %>% dplyr::filter(distcord==max(distcord)) %>% 
    arrange(desc(datetime)) %>% top_n(1, datetime)
  
  shipreport <- shipmaxdist %>% dplyr::select(-c(lonprev, latprev)) %>% 
    dplyr::mutate(position='Current') %>% bind_rows(
      shipmaxdist %>% dplyr::select(-c(lon, lat)) %>%
        dplyr::rename(lon=lonprev, lat=latprev) %>% 
        dplyr::mutate(position='Previous')
    )

  return(shipreport)
  
  }  
