#' @export parse_model_string_to_list
parse_model_string_to_list <- function(model_string) {
  
'
CommunicationM_Facilitating_factor = CommunicationM1 + CommunicationM2 + CommunicationM5 + CommunicationM6 + CommunicationM9
CommunicationM_Open_factor = CommunicationM3 + CommunicationM4 + CommunicationM7 + CommunicationM8 + CommunicationM10
CommunicationM_OneSided_factor = CommunicationM11 + CommunicationM12 + CommunicationM13 + CommunicationM14 + CommunicationM15
'
  
  # 0. 입력 문자열의 앞뒤 공백 제거
  model_string <- trimws(gsub("~", "", model_string))
  
  # 1. 문자열을 줄 단위로 분리
  lines <- strsplit(model_string, "\n")[[1]]
  
  # 2. 결과를 저장할 빈 리스트 생성
  model_list <- list()
  
  # 3. 각 줄을 처리
  for (line in lines) {
    line <- trimws(line) # 각 줄의 앞뒤 공백 제거
    
    # 빈 줄이거나 '=' 문자가 없으면 다음 줄로 넘어감
    if (line == "" || !grepl("=", line)) {
      next
    }
    
    # '=' 문자를 기준으로 요인 이름과 구성 변수 문자열 분리
    parts <- strsplit(line, "\\s*=\\s*")[[1]]
    
    if (length(parts) == 2) {
      factor_name <- trimws(parts[1])
      components_string <- trimws(parts[2])
      
      # '+' 문자를 기준으로 구성 변수들 분리
      components <- strsplit(components_string, "\\s*\\+\\s*")[[1]]
      components <- trimws(components) # 각 구성 변수의 앞뒤 공백 제거
      
      # 결과 리스트에 추가
      model_list[[factor_name]] <- components
    } else {
      warning(paste("다음 줄은 올바른 형식이 아닙니다:", line))
    }
  }
  
  return(model_list)
}




# R 리스트를 lavaan 구문과 유사한 문자열로 변환하는 함수
#' @export convert_list_to_lavaan_syntax
convert_list_to_lavaan_syntax <- function(model_list) {
  # 입력값이 명명된 리스트인지 확인
  if (!is.list(model_list) || is.null(names(model_list))) {
    stop("입력값은 반드시 명명된 리스트여야 합니다 (Input must be a named list).")
  }
  
  # 리스트가 비어있으면 빈 문자열 반환
  if (length(model_list) == 0) {
    return("")
  }
  
  # 각 요인에 대한 문자열 라인을 생성
  # sapply를 사용하여 각 리스트 요소에 대해 함수를 적용
  model_lines <- sapply(names(model_list), function(factor_name) {
    components <- model_list[[factor_name]]
    
    # 구성 변수들이 문자형 벡터이고 비어있지 않은지 확인
    if (!is.character(components) || length(components) == 0) {
      warning(paste0("요인 '", factor_name, "'은(는) 유효한 구성 변수들이 없으므로 건너뜁니다 (Skipping factor '", factor_name, "' due to invalid or empty components)."))
      return(NA_character_) # 나중에 필터링하기 위해 NA 반환
    }
    
    # 구성 변수들을 " + "로 연결
    components_string <- paste(components, collapse = " + ")
    
    # "요인_이름 = 변수1 + 변수2 + ..." 형식의 문자열 생성
    paste(factor_name, components_string, sep = " =~ ")
  }, USE.NAMES = FALSE) # 결과 벡터의 이름을 사용하지 않음
  
  # NA로 표시된 (건너뛴) 라인들 제거
  model_lines <- model_lines[!is.na(model_lines)]
  
  # 모든 라인을 줄바꿈 문자("\n")로 연결하여 최종 문자열 생성
  final_syntax_string <- paste(model_lines, collapse = "\n")
  
  return(final_syntax_string)
}
