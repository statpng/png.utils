#' Remove Outliers in Grouped Data using IQR
#'
#' 이 함수는 이미 group_by로 그룹화된 데이터프레임에서,
#' 지정한 변수(value_var)의 1사분위, 3사분위, IQR을 각 그룹별로 계산한 후,
#' IQR의 multiplier배를 벗어나는 값을 이상치로 간주하여 제거합니다.
#'
#' @param data 이미 그룹화된 데이터프레임.
#' @param value_var 이상치 검출에 사용할 변수 (unquoted). 예: value
#' @param multiplier IQR에 곱할 값 (기본값 5).
#'
#' @return 이상치가 제거된 데이터프레임(그룹 정보는 유지됨).
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   group1 = rep(letters[1:3], each = 100),
#'   group2 = rep(LETTERS[1:2], length.out = 300)
#' )
#' df$value <- NA
#' cond <- df$group1 == "a" & df$group2 == "A"
#' df$value[cond] <- rt(sum(cond), df = 1)
#' df$value[!cond] <- rnorm(sum(!cond))
#' 
#' df %>% 
#'   ggplot() + geom_histogram(aes(value)) + facet_grid(group1~group2, scales="free")
#' 
#'   df %>% 
#'     dplyr::group_by(group1, group2) %>%
#'     remove_outliers_grouped(value, multiplier = 5) %>%
#'     ggplot() + geom_histogram(aes(value)) + facet_grid(group1~group2)
#' }
#'
#' @import dplyr rlang
#' @export png.remove_outliers
png.remove_outliers <- function(data, value_var, multiplier = 5) {
  value_var <- rlang::enquo(value_var)
  
  data %>%
    dplyr::filter(
      (!!value_var) >= stats::quantile(!!value_var, 0.25) - multiplier * IQR(!!value_var),
      (!!value_var) <= stats::quantile(!!value_var, 0.75) + multiplier * IQR(!!value_var)
    )
}
