#' @export png.rmse
png.rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}
