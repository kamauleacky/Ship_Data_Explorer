#R helper functions
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
  stopifnot(length(selectedvessel)>0&!is.na(as.character(selectedvessel)))
  shipdatafile %>% dplyr::filter(ship_type==as.character(selectedvessel))
}

ShipSelectedData <- function(selectedship, vesseltypedata){
  shipdata <- vesseltypedata %>% dplyr::filter(shipname==selectedship)
}

ShipReportData <- function(shipseldata){
  # Sys.sleep(2)
  shipseldata <- shipseldata %>% 
    dplyr::mutate(latprev=lag(lat), lonprev=lag(lon),
                  latprev=case_when(is.na(latprev)~lat, TRUE~latprev),
                  lonprev=case_when(is.na(lonprev)~lon, TRUE~lonprev))
  
  shipselout <- shipseldata%>% 
    rowwise() %>%
    dplyr::mutate(
      distcord= c(distm(x=cbind(lon, lat), y=cbind(lonprev, latprev), fun=distGeo) ),
      # distcord= c(distm(x=cbind(0, 0), y=cbind(0, 0), fun=distGeo) ),
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
