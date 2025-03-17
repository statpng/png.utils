#' @export png.array.combine
png.unload.packges <- function(){
  
  loaded_pkgs <- sessionInfo()$otherPkgs
  if (!is.null(loaded_pkgs)) {
    pkgs <- names(loaded_pkgs)
    # 각 패키지 언로드
    for (pkg in pkgs) {
      detach(paste("package", pkg, sep = ":"), character.only = TRUE, unload = TRUE)
    }
  }
  
}