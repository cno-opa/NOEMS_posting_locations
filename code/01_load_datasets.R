source("code/functions/spatial.R")
source("code/functions/refreshData.R")


incidents <- loadIncidentsFromFile() 

spatial_call_data <- incidents[!is.na(incidents$Latitude) & !is.na(incidents$Longtitude) & !is.na(incidents$ArrivedTime), ] %>% 
  spatialize_incidents()

###Exclude February b/c mardi gras
calls_since_2016 <- spatial_call_data[ year(spatial_call_data$IncidentDate) == 2016 & 
                                         month(spatial_call_data$IncidentDate) != 2, ]

calls_since_2016@data <- calls_since_2016@data %>% 
  mutate(DayShift = factor(if_else(between(hour(IncidentDate),
                                           7,
                                           18),
                                   "Day",
                                   "Night")))

read_polygons <- function(directory, layer_name) {
  ps <- readOGR(directory, layer = layer_name)
  
  ps <- ps[ which( !grepl("martin luther king", ps@data$Address, ignore.case = TRUE  )),] %>% 
    spTransform( NOLA.proj)
}
###10-minute drive-time polygons
# polygons_low_traffic <- readOGR("data/algorithm/polygons", layer = "cleaned_points_low_traffic_polygons")
# 
# polygons_high_traffic <- readOGR("data/algorithm/polygons", layer = "cleaned_points_high_traffic_polygons")
# 
# 
# ###Exclude MLK from algorithm (didn't work in trial)
# polygons_low_traffic <- polygons_low_traffic[ which( !grepl("martin luther king", polygons_low_traffic@data$Address, ignore.case = TRUE  )),]
# 
# 
# polygons_high_traffic <- polygons_high_traffic[ which( !grepl("martin luther king", polygons_high_traffic@data$Address, ignore.case = TRUE  )),]
