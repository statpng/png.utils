#' @export png.grid2df
png.grid2df <- function(out.list, grid){
  
  # grid <- as.data.frame(matrix(1:10,2,5))
  # out.list <- list(x=c(1,2),y=3:4)
  
  library(dplyr)
  
  grid.columns <- colnames(grid)
  
  cbind(grid, do.call("rbind", out.list)) %>% 
    tidyr::gather(measure, value, -grid.columns)
  
}
