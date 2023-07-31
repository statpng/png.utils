#' @export png.grid2df
png.grid2df <- function(out.list, grid){
  
  # grid <- as.data.frame(matrix(1:10,2,5))
  # out.list <- list(x=c(1,2),y=3:4)
  
  library(dplyr)
  
  grid.columns <- colnames(grid)
  
  cbind(grid, do.call("rbind", out.list)) %>% 
    tidyr::gather(x, value, -grid.columns)
  
}


#' @export png.mat2vec
png.mat2vec <- function(mat){
  n=nrow(mat); p=ncol(mat)
  new.names <- paste0(rep(colnames(mat), each=n), "//", rep(rownames(mat), p))
  out <- as.vector(mat)
  names(out) <- new.names
  out
}


#' @export png.combine.matlist2grid
png.combine.matlist2grid <- function(out.list, grid, wh.removed=NULL){
  if(is.null(wh.removed)){
    wh.removed <- which( !sapply(out.list, function(x) nrow(x)>2) )
  }
  
  mat <- out.list[[1]]
  
  dim <- append( apply(grid,2,function(x) length(unique(x))), c(x=nrow(mat), y=ncol(mat)) )
  dimnames <- append( apply(grid,2,function(x) unique(x)), list(x=rownames(mat), y=colnames(mat)) )
  
  out.array <- array(NA, dim=dim, dimnames=dimnames)
  for( i in 1:nrow(grid) ){
    if(i == wh.removed) next
    wh <- purrr::map2(dimnames[1:ncol(grid)], grid[i,], function(x,y) which(x %in% y))
    out.array[wh[[1]],wh[[2]],wh[[3]],wh[[4]],,] <- out.list[[i]]
  }
  
  out.df <- plyr::adply(out.array, 1:length(dim(out.array)))
  colnames(out.df)[ncol(out.df)] <- "value"
  
  out.df
}



#' @export png.unlist
png.unlist <- function(LIST, level=1){
  f <- function(x){
    if(is.atomic(x)){
      list(x)
    }else{
      x
    }
  }
  
  count <- 0
  
  count=count+1
  out <- unlist(lapply(LIST, f), recursive=FALSE)
  
  while(  count<level  ){
    count <- count+1
    out <- unlist(lapply(out, f), recursive=FALSE)
  }
  out
}