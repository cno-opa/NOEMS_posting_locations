NOLA.proj <- CRS("+init=epsg:3452")

solve.simultaneous <- function(model.calls, model.polys, m.size) {
  ptm <- proc.time()
  
  m.calls <- spTransform(model.calls, NOLA.proj)
  m.polys <- spTransform(model.polys , NOLA.proj)
  
  
  coverage <- function(polys) {
    rev <- over(m.calls, SpatialPolygons(polys@polygons, proj4string = polys@proj4string ))
    sum(!is.na(rev))
  }
  
  
  combs <- list()
  
  combs$indexes <- combn(nrow(m.polys),m.size, simplify = FALSE)
  
  combs$counts <- lapply(combs$indexes, function(indexes) coverage(m.polys[indexes,]))
  
  output.df <- data.frame(coverage = as.numeric(combs$counts))
  
  
  for (i in seq_len(nrow(output.df))) {
    
    for (n in 1:length(combs$indexes[[i]])) {
      index <- combs$indexes[[i]][[n]]
      
      output.df[i,n+1] <- m.polys@data$NewName[index]
    }
    
  }
  
  
  output <- output.df[order( output.df$coverage, decreasing = TRUE),]
  
  print(proc.time() - ptm)
  
  # df <- output
  # df$NewName <- df$V2
  # df <- join( df, model.polys@data, by = "NewName")
  # df$V2 <- df$NewName
  # df <- df[,1:3]
  # 
  # df$NewName <- df$V3
  # df <- join( df, model.polys@data, by = "NewName")
  # df$V3 <- df$NewName
  # df <- df[,1:3]
  # 
  return(output)
}




add.poisson.test  <- function(solution.set) {
  top.coverage <- solution.set$coverage[1]
  
  poisson_test_p_value <- function(test.coverage,t = 1,r = top.coverage,alternative = "less") {
    test <- poisson.test(test.coverage , t,  r, alternative)
    round( test$p.value, digits = 16)
  }
  
  tested.solution <- solution.set
  
  tested.solution$p.value <- sapply(tested.solution$coverage,poisson_test_p_value)
  
  return(tested.solution)
}








getNames <- function(data) data[, grepl("V",names( data))]

calculate_coverage <- function(p, c) {
  
  over(c, p, returnList = TRUE) %>% 
    map_int(nrow) %>%
    tbl_df() %>%
    summarise(triple_coverage = sum(value > 2),
              double_coverage = sum(value > 1))
}





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

order.by.coverage <- function(spatial.points, polygons) {
	#The following function chooses polygon of greatest impact, given call points and a set of candidate polygons
	ordered.polygons <- polygons
	ordered.polygons$CallCount <- countWithin(spatial.points, ordered.polygons)
	
	ordered.polygons <- ordered.polygons[order(ordered.polygons$CallCount, decreasing = TRUE),]
	
	return(ordered.polygons)
}

rank.polygon.impact <- function(impact.points, polygons, rank.to = 2, seed.polygons = NULL) {
	#Sequentially finds set of N polygons with most coverage.
	
	initial.polygons <- if (is.null(seed.polygons)) polygons else seed.polygons
	
	ordered.set <- order.by.coverage(impact.points, initial.polygons)
	
	solution.set <-  if (is.null(seed.polygons)) {
		ordered.set[1,]
	} else 
		rank.polygon.impact(impact.points, 
												seed.polygons, 
												rank.to = nrow(seed.polygons@data) 
		)
	
	remaining.polygons <- remove.by.NewName(solution.set$NewName, polygons)
	if (rank.to > 1){
		for (i in 2:rank.to) {
			uncovered.calls <- uncovered.points(impact.points, solution.set)
			ordered.set <- order.by.coverage(uncovered.calls, remaining.polygons )
			next.greatest.coverage.polygon <- ordered.set[1,]
			solution.set <- rbind(solution.set, next.greatest.coverage.polygon)
			remaining.polygons <- remove.by.NewName(next.greatest.coverage.polygon$NewName, remaining.polygons)
		}
	}
	
	
	
	return(solution.set)
}


uncovered.points <- function(points, coverage.area) {
	#The set of calls that lie outside of the set of polygons that is passed as an argument
	within <- spTransform(points, NOLA.proj)
	around <- spTransform(coverage.area, NOLA.proj)
	calls.contained <- over(within, around, returnList = FALSE)
	# return(calls.contained)
	return(points[is.na(calls.contained[,1]) ,])
}
