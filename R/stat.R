#' @export 
png.angle <- function(true, est){
  # true: n x p; est: n x p
  
  # The largest principal angle
  qt <- qr.Q(qr(true))
  qe <- qr.Q(qr(est))
  fit.svd <- svd( crossprod(qe, qt) )
  theta <- acos(fit.svd$d |> round(12))
  
  # theta[1] * 180 / pi (in degree)
  list( max = theta[1] * 180 / pi, Grassmannian = norm( theta, "2" ) * 180 / pi )
}


#' @export 
png.CompareSubspace <- function(x, y){
  # x: n x p; y: n x p
  X <- x %>% svd() %>% { .$u[,(.$d>1e-10)] }
  Y <- y %>% svd() %>% { .$u[,(.$d>1e-10)] }
  
  c( angle = png.angle(X, Y)$max,
     cor = cancor(X, Y)$cor[1] )
}



#' @export
png.list.pairs <- function(LIST, f){
  if(FALSE){
    LIST = list(jlist.jive[[1]], jlist.ajive[[1]], jlist.slide[[1]])
  }
  
  n <- length(LIST)
  
  out <- list()
  for( i in 1:n ){
    for( j in 1:n ){
      if( i <= j ) next
      out <- append(out, list(f(LIST[[i]], LIST[[j]])))
    }
  }
  
  df <- expand.grid(i=1:n, j=1:n) %>% filter(i>j)
  df <- cbind.data.frame(df, dplyr::bind_rows(out))
  df$i <- names(LIST)[df$i]
  df$j <- names(LIST)[df$j]
  
  df
}
