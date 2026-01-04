#' @import DT
#' @export glimpse_datatable
glimpse_datatable <- function(data, caption_text = NULL) {
  # 데이터의 각 열에 대한 요약 정보 생성
  summary_df <- tibble(
    `Column Name` = names(data),
    `Data Type`   = sapply(data, function(x) paste(class(x), collapse = ", ")),
    `Preview`     = sapply(data, function(x) {
      x <- if( is.numeric(x) ) round(x, 3)
      preview_vals <- as.character( head(unique(x), 5)  )
      if( sum(is.na(preview_vals)) > 0 ){
        preview_vals[is.na(preview_vals)] <- "NA" # NA 값도 문자열로 표시
      }
      paste(preview_vals, collapse = ", ")
    })
  )
  
  # 동적 캡션 생성
  if (is.null(caption_text)) {
    caption_text <- paste('Interactive Glimpse of', deparse(substitute(data)))
  }
  
  # datatable 객체 반환
  datatable(
    summary_df,
    options = list(
      scrollY = '400px',
      paging = FALSE,
      scrollCollapse = TRUE,
      scrollX = TRUE
    ),
    filter = 'top',
    class = 'cell-border stripe',
    caption = caption_text
  )
}