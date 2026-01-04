#' @exportPattern "^[a-zA-Z\\.]"

function(){
  
  
  lapply(c("dplyr","tidyr","ggplot2","purrr","gtools"), function(x){
    usethis::use_package(x, type = "Imports")
  })
  lapply(c("broom","cobalt"), function(x){
    usethis::use_package(x, type = "Imports")
  })
  lapply(c("ggeffects","MatchIt"), function(x){
    usethis::use_package(x, type = "Imports")
  })
  lapply(c("tableone","gtsummary","cardx"), function(x){
    usethis::use_package(x, type = "Imports")
  })
  
  lapply(c("plotly"), function(x){
    usethis::use_package(x, type = "Imports")
  })
  
}



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




#' @title 리스트를 list(name=name, ...) 호출 객체로 변환
#' @param input_list 명명된(named) 리스트
#' @param output_type 반환 타입. "call" (기본값) 또는 "string"
#' @return "call"인 경우 unevaluated call, "string"인 경우 character
#' @export png.list2call
png.list2call <- function(input_list, output_type = "call") {
  # 입력값이 명명된 리스트인지 확인
  if (!is.list(input_list) || is.null(names(input_list))) {
    stop("입력값은 반드시 명명된 리스트(a named list)여야 합니다.")
  }
  
  # 리스트의 이름들을 추출
  list_names <- names(input_list)
  
  if (output_type == "call") {
    # rlang 패키지가 없으면 설치 안내
    if (!requireNamespace("rlang", quietly = TRUE)) {
      stop("rlang 패d키지를 설치해주세요: install.packages('rlang')")
    }
    
    # "이름 = 이름" 형태의 인자 리스트 생성
    args <- rlang::set_names(rlang::syms(list_names), list_names)
    
    # list() 호출(call) 객체 반환
    return(rlang::call2("list", !!!args))
    
  } else if (output_type == "string") {
    # "이름 = 이름" 형태의 문자열 생성
    body_string <- paste(list_names, list_names, sep = " = ", collapse = ", ")
    
    # 최종 문자열 반환
    return(paste0("list(", body_string, ")"))
    
  } else {
    stop("output_type은 'call' 또는 'string'이어야 합니다.")
  }
}