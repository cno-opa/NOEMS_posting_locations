###Turn EMS incidents into a spatialpointsdataframe 
nola_proj <- function() CRS("+init=epsg:3452")	

spatialize_incidents <- function(d) {
  d %>% 
    summarise(sum(is.na(Latitude) | is.na(Longtitude))) %>%
    paste("observations with missing Latitude or Longitude removed") %>%
    print()
  
  d %>%
    filter(!(is.na(Latitude) | is.na(Longtitude)))%>% 
    mutate(Latitude = as.numeric(Latitude),
           Longtitude = as.numeric(Longtitude)) %>%
    toSpatialPoints2(  "Longtitude","Latitude", p4string = nola_proj())
}


###Helper function.  The 2 at the end is an artifact of prior scripts when this function was rewritten for performance.  Sort of like ggplot2?
toSpatialPoints2 <- function(df, X, Y, remove.outliers = FALSE, nola.only = TRUE, 
														 ..., p4string) {
	# Converts a data.frame into a SpatialPointsDataFrame given X, Y lat long
	cnames <- c(X,Y)
	latlon.proj <- CRS("+init=epsg:4326")
	spatial.points <- SpatialPointsDataFrame(coords = df[,cnames], df, proj4string = latlon.proj)
	return(spatial.points)
}
