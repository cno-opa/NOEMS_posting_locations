calculate_call_coverage <- function(polys, call_points) {
  rev <- over(call_points, SpatialPolygons(polys@polygons, proj4string = polys@proj4string ))
  sum(!is.na(rev))
}

solve_simultaneous <- function(model_calls, model_polys, model_size) {
  ptm <- proc.time()
  
  model_calls <- spTransform(model_calls, NOLA.proj)
  model_polys <- spTransform(model_polys , NOLA.proj)
  
  
  
  combs <- list()
  
  combs$indexes <- combn(nrow(model_polys),model_size, simplify = FALSE)
  
  combs$counts <- map_int(combs$indexes, ~calculate_call_coverage(model_polys[.x,], 
  																																model_calls))
  
  output_df <- data.frame(coverage = as.numeric(combs$counts))
  
  
  for (i in seq_len(nrow(output_df))) {
    for (n in 1:length(combs$indexes[[i]])) {
      index <- combs$indexes[[i]][[n]]
      
      output_df[i,n+1] <- model_polys@data$NewName[index]
    }
    
  }
  
  output <- output_df %>% 
  	arrange(desc(coverage))
  
  print(proc.time() - ptm)

  return(output)
}




add_poisson_test  <- function(solution_set) {
  top_coverage <- solution_set$coverage[1]
  
  tested_solution <- solution_set
  
  tested_solution$p.value <- map_dbl(tested_solution$coverage, 
  															 ~poisson_test_p_value(.x, comparison_rate = top_coverage))
  
  return(tested_solution)
}




poisson_test_p_value <- function(test_rate,comparison_rate) {
	test <- poisson.test(test_rate , T = 1, r = comparison_rate, alternative = 'less')
	round( test$p.value, digits = 16)
}

calculate_multi_coverage <- function(p, c) {
  over(c, p, returnList = TRUE) %>%
    map_int(nrow) %>%
    tbl_df() %>%
    summarise(triple_coverage = sum(value > 2),
              double_coverage = sum(value > 1))
}



order_by_coverage <- function(spatial_points, polygons) {
	#The following function chooses polygon of greatest impact, given call points and a set of candidate polygons
	ordered_polygons <- polygons
	ordered_polygons$CallCount <- countWithin(spatial_points, ordered_polygons)

	ordered_polygons <- ordered_polygons[order(ordered_polygons$CallCount, decreasing = TRUE),]

	return(ordered_polygons)
}
# 
# rank.polygon.impact <- function(impact.points, polygons, rank.to = 2, seed.polygons = NULL) {
# 	#Sequentially finds set of N polygons with most coverage.
# 
# 	initial.polygons <- if (is.null(seed.polygons)) polygons else seed.polygons
# 
# 	ordered.set <- order.by.coverage(impact.points, initial.polygons)
# 
# 	solution.set <-  if (is.null(seed.polygons)) {
# 		ordered.set[1,]
# 	} else
# 		rank.polygon.impact(impact.points,
# 												seed.polygons,
# 												rank.to = nrow(seed.polygons@data)
# 		)
# 
# 	remaining.polygons <- remove.by.NewName(solution.set$NewName, polygons)
# 	if (rank.to > 1){
# 		for (i in 2:rank.to) {
# 			uncovered.calls <- uncovered.points(impact.points, solution.set)
# 			ordered.set <- order.by.coverage(uncovered.calls, remaining.polygons )
# 			next.greatest.coverage.polygon <- ordered.set[1,]
# 			solution.set <- rbind(solution.set, next.greatest.coverage.polygon)
# 			remaining.polygons <- remove.by.NewName(next.greatest.coverage.polygon$NewName, remaining.polygons)
# 		}
# 	}
# # 	
# 	
# 	
# 	return(solution.set)
# }
# 
# 
# uncovered.points <- function(points, coverage.area) {
# 	#The set of calls that lie outside of the set of polygons that is passed as an argument
# 	within <- spTransform(points, NOLA.proj)
# 	around <- spTransform(coverage.area, NOLA.proj)
# 	calls.contained <- over(within, around, returnList = FALSE)
# 	# return(calls.contained)
# 	return(points[is.na(calls.contained[,1]) ,])
# }
