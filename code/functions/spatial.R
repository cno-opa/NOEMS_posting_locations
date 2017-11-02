###Turn EMS incidents into a spatialpointsdataframe 
nola_proj <- function() CRS("+init=epsg:3452")	

countWithin <- function(within, around, col.name = as.character(bquote(within)),
												return.type = "vec", ..., range) {
	# Answers a common spatial analysis question: How many of one object are within
	# another?  To adapt this function to answer similar questions, see help(over)
	# within: points, the object to be counted around: polygons, boundaries in which
	# to evaluate, or points to search near return.type: one of c('vec', 'poly',
	# 'points'). If vec, returns a vector of the counts of within in around.
	# Otherwise returns around with this vector appended to it, either as a
	# SpatialPolygons or SpatialPoints object.  Can only use 'points' if return.type
	# is a SpatialPoints object optional - range: if around is a SpatialPoints
	# object, the area (in feet) to search around around
	dots <- eval(substitute(alist(...)))
	if (!is.null(dots$range)) {
		range <- dots$range
	}
	
	within.crs <- within@proj4string
	around.crs <- around@proj4string
	within <- spTransform(within, NOLA.proj)
	around <- spTransform(around, NOLA.proj)
	
	if (is(around, "SpatialPoints") | is(around, "SpatialPointsDataFrame")) {
		around.poly <- gBuffer(around, width = range, byid = TRUE)
		count.within <- sapply(over(around.poly, geometry(within), returnList = TRUE),
													 length)
	} else {
		count.within <- sapply(over(around, geometry(within), returnList = TRUE),
													 length)
		around.poly <- around
	}
	
	# getName <- function(v1){deparse(substitute(v1))} getName <-
	# function(v1){as.character(bquote(v1))} final.name <- paste0('count.of.',
	# col.name)
	
	if (return.type == "points") {
		around$poly.name <- count.within
		names(around)[which(names(around) == "poly.name")] <- col.name
		around <- spTransform(around, around.crs)
		return(around)
	} else if (return.type == "poly") {
		around.poly$poly.name <- count.within
		names(around.poly)[which(names(around.poly) == "poly.name")] <- col.name
		around.poly <- spTransform(around.poly, around.crs)
		return(around.poly)
	} else if (return.type == "vec") {
		return(count.within)
	}
}

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
