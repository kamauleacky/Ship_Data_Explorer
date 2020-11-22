library(testthat)
source('../../R/Ship_Statistics.R')

geodist_testdata <- data.frame(lon=c(-8.29258, -8.29315,
                                 -18.34152,-8.29380,
                                 -8.293152, -8.29315, 
                                 -18.34152),
                               lat=c(41.44531, 41.44155,
                                     36.85493, 41.44150,
                                     41.44450, 41.44155,
                                     36.85493)) %>% 
  dplyr::mutate_if(is.numeric,round, 5) %>% 
  dplyr::mutate(datetime=as.POSIXct(c( "2020-11-22 22:46:58 CET", "2020-11-23 22:46:58 CET",
                           "2020-11-24 22:46:58 CET","2020-11-25 22:46:58 CET",
                           "2020-11-26 22:46:58 CET", "2020-11-27 22:46:58 CET",
                           "2020-11-28 22:46:58 CET")),
                location=1:n())

geodist_testdata
shipseldata=geodist_testdata

ShipReportData(geodist_testdata)
#Largest distance between location 2 and 3 which is equal to that of 6 and 7..
#Show that function returns the most recent date in such situations as well 
# 2020-11-28 22:46:58 CET

test_that('Geographic distance',{
  
  geodist <- ShipReportData(shipseldata =geodist_testdata)
  
  expect_s3_class(geodist, 'data.frame')

  expect_equal(unique(geodist$datetime), as.POSIXct("2020-11-28 22:46:58 CET"))
  
})
