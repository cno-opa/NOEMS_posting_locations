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



