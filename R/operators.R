#' @export `%_%`
`%_%` <- function(x, y) {
  paste(x, y, sep="_")
}

#' @export `%+%`
`%+%` <- function(x, y) {
  paste0(x, y)
}

#' @export `%/%`
`%/%` <- function(x, y) {
  paste0(x, "/", y)
}

#' @export png.mat.pairs
png.mat.pairs <- function(LIST, f=function(x,y){ norm( x-y, "F" ) }){
  N=length(LIST)
  
  # f <- function(x,y){ norm( x-y, "F" ) }
  Combinations <- combn(1:N, 2)
  
  out <- matrix(NA,N,N)
  for( j in 1:ncol(Combinations) ){
    row <- Combinations[1,j]
    col <- Combinations[2,j]
    out[row,col] <- Reduce(f, LIST[Combinations[,j]])
    out[col,row] <- out[row,col]
  }
  out
}