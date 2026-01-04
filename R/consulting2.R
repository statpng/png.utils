
report <- function(){
  
  # install.packages("gtsummary")
  library(gtsummary)
  
  # 예시 데이터 (mtcars)
  data(mtcars)
  mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
  
  # 단 한 줄의 코드로 Table 1 생성!
  table1 <- mtcars %>%
    select(mpg, hp, wt, am) %>%
    tbl_summary(
      by = am, # am (변속기) 그룹별로 요약
      statistic = all_continuous() ~ "{mean} ({sd})", # 연속형 변수는 평균(표준편차)로
      missing_text = "(Missing)"
    ) %>%
    add_p() %>% # 그룹 간 p-value 추가
    add_overall() %>% # 전체 열 추가
    bold_labels()
  
  # table1 변수를 Quarto 문서에서 호출하면 표가 자동으로 나타남
  table1
  
}




#' @title 결측치 처리 기능이 포함된 통합 Cox 분석 함수
#' @description 단변량, 다변량, 단계적 선택 Cox 분석을 수행하고 결과를 병합합니다.
#'              각 분석 단계에 맞춰 N수를 최대한 보존하도록 결측치를 처리합니다.
#' @param data 사용할 데이터프레임.
#' @param time 생존 시간 변수명 (문자열).
#' @param status 이벤트 상태 변수명 (문자열).
#' @param predictors 분석에 포함할 예측 변수들의 벡터 (문자열).
#' 
#' @export analyze_coxph_all
analyze_coxph_all <- function(data, time, status, predictors, pv_tol=0.001) {
  
  # 1. 필수 패키지 설치 및 로드
  # install.packages(c("survival", "dplyr", "broom", "purrr", "scales"))
  library(survival)
  library(dplyr)
  library(broom)
  library(purrr)
  library(scales)
  
  numeric_predictors <- predictors[sapply(data[predictors], is.numeric)]
  
  
  # --- 1. 단변량(Univariate) 분석 ---
  # 각 변수별로 필요한 열에만 na.omit()을 적용하여 N수 최대화
  uni_results <- map_df(predictors, ~{
    
    # 해당 모델에 필요한 데이터만 선택 후 결측치 제거
    data_uni <- data %>%
      select(all_of(c(time, status, .x))) %>%
      na.omit()
    
    if (.x %in% numeric_predictors) {
      data_uni <- data_uni %>% mutate(across(all_of(.x), ~scale(.)))
    }
    
    formula_uni <- as.formula(paste("Surv(", time, ",", status, ") ~", .x))
    model_uni <- coxph(formula_uni, data = data_uni)
    
    tidy(model_uni, exponentiate = TRUE, conf.int = TRUE)
  }) %>% 
    mutate(
      HR_CI = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high),
      P_value = pvalue(p.value, accuracy =  pv_tol)
    ) %>%
    select(Variable = term, Univariate_HR_CI = HR_CI, Univariate_P = P_value)
  
  # --- 2. & 3. 다변량 및 단계적 분석 ---
  # 분석에 사용할 모든 변수를 포함하여 complete.cases 데이터셋 생성
  data_multi <- data %>%
    select(all_of(c(time, status, predictors))) %>%
    na.omit()
  
  if (length(numeric_predictors) > 0) {
    data_multi <- data_multi %>%
      mutate(across(all_of(numeric_predictors), ~scale(.)))
  }
  
  # 다변량(Multivariate) 분석
  formula_multi <- as.formula(paste("Surv(", time, ",", status, ") ~", paste(predictors, collapse = " + ")))
  model_multi <- coxph(formula_multi, data = data_multi)
  
  multi_results <- tidy(model_multi, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      HR_CI = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high),
      P_value = pvalue(p.value, accuracy = pv_tol)
    ) %>%
    select(Variable = term, Multivariate_HR_CI = HR_CI, Multivariate_P = P_value)
  
  # 단계적 선택(Stepwise) 다변량 분석
  model_step <- step(model_multi, direction = "backward", trace = 0)
  
  step_results <- tidy(model_step, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      HR_CI = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high),
      P_value = pvalue(p.value, accuracy = pv_tol)
    ) %>%
    select(Variable = term, Stepwise_HR_CI = HR_CI, Stepwise_P = P_value)
  
  # --- 4. 모든 결과 병합 ---
  final_table <- uni_results %>%
    full_join(multi_results, by = "Variable") %>%
    full_join(step_results, by = "Variable") %>%
    mutate(across(everything(), ~replace_na(.x, ""))) # NA는 빈칸으로 표시
  
  return(list(final_table=final_table, data_multi=data_multi))
}











# 필요한 패키지를 먼저 로드합니다.
library(survival)
library(survminer)

#' @title 카플란-마이어 플롯 생성 함수
#' @description 지정된 데이터와 변수를 사용하여 카플란-마이어 생존 곡선을 그립니다.
#' @param data 사용할 데이터프레임
#' @param time_var 생존 기간을 나타내는 변수명 (문자열)
#' @param status_var 사건 발생 상태를 나타내는 변수명 (문자열)
#' @param group_var 그룹을 나눌 변수명 (문자열). 전체 그룹을 보려면 NULL.
#' @param pval p-value 표시 여부 (기본값: TRUE)
#' @param conf.int 신뢰구간 표시 여부 (기본값: TRUE)
#' @param risk.table 위험표(risk table) 표시 여부 (기본값: TRUE)
#' @param ... ggsurvplot에 전달할 추가 인자들 (예: title, xlab, palette 등)
#' @export create_km_plot
create_km_plot <- function(formula, data,
                            pval = TRUE, conf.int = TRUE, risk.table = TRUE, ...) {
  
  # 2. 카플란-마이어 모델 적합
  fit <- survfit(formula, data = data)
  fit$call$formula <- formula
  
  # 3. ggsurvplot으로 시각화 (이 부분은 변경 없음)
  ggsurvplot(
    fit,
    data = data,
    pval = pval,
    conf.int = conf.int,
    risk.table = risk.table,
    ggtheme = theme_minimal(),
    ... 
  )
}
