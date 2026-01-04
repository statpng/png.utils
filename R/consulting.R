# -----------------------------------------------------
# 0. 필요 패키지 설치 및 로드
# -----------------------------------------------------
# PSM의 핵심 패키지 MatchIt, 균형 평가를 위한 cobalt, 모델 결과 정리를 위한 broom
# 패키지가 설치되어 있지 않으면 자동으로 설치합니다.

#' @import broom
#' @import cobalt
#' @import MatchIt
#' @import ggplot2
#' 
#' @title 성향점수매칭(PSM) 및 결과 회귀분석 수행 함수
#' @description 주어진 데이터에 대해 PSM을 수행하여 그룹 간 균형을 맞추고, 
#' 매칭된 데이터를 사용하여 치료/노출 변수가 결과 변수에 미치는 영향을 회귀분석으로 추정합니다.
#'
#' @param data 분석할 데이터 프레임.
#' @param treatment_var 치료/노출 변수명(문자열). 반드시 0과 1로 코딩되어야 함.
#' @param outcome_var 결과 변수명(문자열).
#' @param covariates 성향점수 계산에 사용할 공변량(교란변수)들의 변수명 벡터.
#' @param outcome_family 결과 회귀분석 모델 종류. 'gaussian'(선형회귀) 또는 'binomial'(로지스틱회귀) 등.
#' @param caliper 매칭에 사용할 캘리퍼 값. 성향점수 표준편차의 0.2배를 주로 사용. NULL이면 사용 안 함.
#'
#' @return 결과들을 담은 리스트. 포함된 내용:
#'         - match_object: MatchIt의 매칭 결과 객체.
#'         - balance_summary: 매칭 전후의 균형 평가표.
#'         - balance_plot: Love plot (균형 시각화 그래프).
#'         - matched_data: 매칭된 후의 데이터 프레임.
#'         - outcome_model: 최종 결과 회귀 모델 객체.
#'         - model_summary: 회귀 모델 결과 요약표.
#' @export perform_psm_regression
perform_psm_regression <- function(data, treatment_var, outcome_var, covariates, 
                                   outcome_family = "gaussian", caliper = 0.2) {
  
  if(FALSE){
    if (!require("MatchIt")) install.packages("MatchIt")
    if (!require("cobalt")) install.packages("cobalt")
    if (!require("broom")) install.packages("broom")
    
    library(MatchIt)
    library(cobalt)
    library(broom)
    library(ggplot2)
    
    # 재현성을 위해 시드 설정 (언제 실행해도 같은 결과가 나옴)
    set.seed(2025)
    
    # 데이터 샘플 크기 지정
    n <- 1000
    
    # 1. 교란변수(Covariates) 생성
    # C1: 나이 (age) - 평균 50, 표준편차 10의 정규분포
    age <- rnorm(n, 50, 10)
    
    # C2: 성별 (sex_male) - 남성(1)일 확률 50%
    sex_male <- rbinom(n, 1, 0.5)
    
    # 2. 치료/노출 변수(Treatment) 생성
    # 나이가 많고 남성일수록 새로운 치료(treatment=1)를 받을 확률이 높도록 설계
    # 이는 현실에서 특정 특성을 가진 환자들이 특정 치료를 더 선호하는 상황을 모방
    prob_treatment <- plogis(-2 + 0.03 * age + 0.5 * sex_male)
    treatment <- rbinom(n, 1, prob_treatment)
    
    # 3. 결과 변수(Outcome) 생성
    # 결과(outcome)는 치료(treatment), 나이(age), 성별(sex_male) 모두의 영향을 받음
    # 여기서 treatment의 실제 인과 효과(true causal effect)는 '+10'으로 설정
    outcome <- 100 + 10 * treatment + 5 * age + 10 * sex_male + rnorm(n, 0, 20)
    
    # 4. 모든 변수를 하나의 데이터 프레임으로 결합
    sim_data <- data.frame(
      outcome = outcome,
      treatment = treatment,
      age = age,
      sex_male = sex_male
    )
    
    # 5. 생성된 데이터의 상위 6개 행을 출력하여 확인
    print("### 생성된 예시 데이터 (sim_data) ###")
    head(sim_data)
    
    
    {
      data=sim_data
      treatment_var="treatment"
      outcome_var="outcome"
      covariates=c("age", "sex_male")
      outcome_family = "gaussian"
      caliper = 0.2
    }
  }
  
  
  # --- 1단계: 성향점수 모델 공식 생성 및 매칭 수행 ---
  
  # 성향점수 모델 포뮬러 생성 (예: treatment ~ cov1 + cov2 + ...)
  ps_formula <- as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
  
  cat("### 1. 성향점수매칭을 시작합니다...\n")
  cat("   - 치료변수(X):", treatment_var, "\n")
  cat("   - 결과변수(Y):", outcome_var, "\n")
  cat("   - 교란변수(C):", paste(covariates, collapse=", "), "\n")
  
  # MatchIt 패키지를 이용한 매칭
  match_obj <- matchit(
    ps_formula,
    data = data,
    method = "nearest", # 1:1 최근접 이웃 매칭
    distance = "logit", # 로짓 성향점수 사용
    caliper = caliper   # 캘리퍼 지정 (지나치게 다른 샘플 매칭 방지)
  )
  
  cat("\n### 2. 매칭 후 그룹 간 균형을 평가합니다...\n")
  
  # --- 2단계: 균형 평가 ---
  
  # cobalt 패키지의 bal.tab() 함수로 매칭 전후 균형 요약
  # Std. Mean Diff. (표준화된 평균차)가 0.1 이하이면 보통 균형이 잘 맞았다고 판단
  balance_summary <- bal.tab(match_obj, un = TRUE) # un=TRUE: 매칭 전 결과도 함께 표시
  
  # Love plot 시각화: 매칭 후 모든 점들이 0.1 근처의 점선 안으로 들어오면 좋음
  balance_plot <- love.plot(
    match_obj,
    binary = "std",
    thresholds = c(m = .1), # 표준화된 평균차 0.1 기준선 추가
    abs = TRUE, # 절대값으로 표시
    title = "공변량 균형 평가 (Covariate Balance - Love Plot)"
  )
  # if(plot) balance_plot
  
  
  # --- 3단계: 매칭된 데이터 추출 ---
  
  cat("### 3. 매칭된 데이터를 추출합니다...\n")
  matched_data <- match.data(match_obj)
  
  
  # --- 4단계: 결과 회귀분석 ---
  
  # Y ~ X 형태의 단순 회귀분석. 
  # 이미 매칭을 통해 교란변수들의 영향이 통제(균형)되었기 때문에,
  # 결과 모델에는 교란변수를 다시 넣을 필요가 없음.
  outcome_formula <- as.formula(paste(outcome_var, "~", treatment_var))
  
  cat("### 4. 매칭된 데이터로 최종 회귀분석을 수행합니다...\n")
  
  outcome_model <- glm(outcome_formula, data = matched_data, family = outcome_family)
  
  # broom::tidy()로 결과 요약
  model_summary <- tidy(outcome_model, conf.int = TRUE)
  
  # --- 5단계: 모든 결과물을 리스트로 묶어 반환 ---
  
  cat("### 분석이 완료되었습니다.\n")
  
  results <- list(
    match_object = match_obj,
    balance_summary = balance_summary,
    balance_plot = balance_plot,
    matched_data = matched_data,
    outcome_model = outcome_model,
    model_summary = model_summary
  )
  
  return(results)
}

# 
# # -----------------------------------------------------
# # 함수 사용 예제
# # -----------------------------------------------------
# # 1. 시뮬레이션 데이터 생성 (교란이 존재하는 상황)
# set.seed(2025)
# n <- 1000
# # 교란변수 생성 (나이, 성별)
# age <- rnorm(n, 50, 10)
# sex_male <- rbinom(n, 1, 0.5)
# 
# # 치료(X) 선택에 교란변수가 영향을 줌
# # 나이가 많고 남성일수록 새로운 치료(X=1)를 받을 확률이 높음
# prob_treatment <- plogis(-2 + 0.03 * age + 0.5 * sex_male)
# treatment <- rbinom(n, 1, prob_treatment)
# 
# # 결과(Y)에 치료(X)와 교란변수 모두가 영향을 줌
# # 실제 치료 효과(X의 계수)는 10
# outcome <- 100 + 10 * treatment + 5 * age + 10 * sex_male + rnorm(n, 0, 20)
# 
# # 데이터 프레임 생성
# sim_data <- data.frame(outcome, treatment, age, sex_male)
# 
# # 2. PSM 전, 보정 안 된 회귀분석 (참고용)
# # 교란 효과 때문에 treatment의 효과(10)가 왜곡되어 나타남
# crude_model <- lm(outcome ~ treatment, data = sim_data)
# cat("\n--- PSM 전, 보정 안 된 회귀분석 결과 ---\n")
# print(tidy(crude_model)) # treatment의 계수가 10보다 훨씬 크게 나옴
# 
# 
# # 3. 생성한 함수 호출
# psm_results <- perform_psm_regression(
#   data = sim_data,
#   treatment_var = "treatment",
#   outcome_var = "outcome",
#   covariates = c("age", "sex_male"),
#   outcome_family = "gaussian" # 결과 변수가 연속형이므로
# )
# 
# # 4. 결과 확인
# # (1) 매칭 전후 균형 통계표 확인
# print(psm_results$balance_summary)
# 
# # (2) 균형 시각화 (Love Plot) 확인
# print(psm_results$balance_plot)
# 
# # (3) PSM 후, 최종 회귀분석 결과 확인
# cat("\n--- PSM 후, 매칭된 데이터 기반 회귀분석 결과 ---\n")
# print(psm_results$model_summary)
# # treatment의 계수가 실제 효과인 10에 매우 가깝게 추정된 것을 볼 수 있음



















if(FALSE){
  
  
  # 1. 필요한 패키지 설치 및 로드
  # install.packages("drtmle")
  # install.packages("SuperLearner")
  # install.packages("dplyr")
  # install.packages("tidyr")
  # install.packages("ggplot2")
  # install.packages("np") # SL.npreg 사용 시 필요
  
  
  
  
  # 시뮬레이션 데이터 생성
  sim_data_aipw <- simulate_aipw_data(n = 1000)
  
  # --- 함수 실행 및 결과 해석 ---
  # SuperLearner 라이브러리를 지정할 때, SuperLearner 패키지가 설치되어 있어야 합니다.
  # SL.npreg는 np 패키지가 필요합니다.
  drtmle_analysis_results <- analyze_drtmle_dose_response(
    data = sim_data_aipw,
    treat_var_name = "time_to_treat",
    outcome_var_name = "death",
    confounder_var_names = "L1",
    n_bins = 8, # 8개의 이산 구간으로 나눔
    sl_library_g = c("SL.glm", "SL.npreg")[1], # PS 모델에 사용할 알고리즘
    sl_library_Q = c("SL.glm", "SL.npreg")[1]  # OR 모델에 사용할 알고리즘
  )
  
  # 결과 데이터프레임 확인
  print("Estimated Dose-Response Results (TMLE):")
  print(drtmle_analysis_results$results_df)
  
  # 용량-반응 곡선 플롯 확인
  print("Dose-Response Curve Plot (TMLE):")
  print(drtmle_analysis_results$dose_response_plot)
  
  
}








# # SL.npreg wrapper function (drtmle 내부에도 정의되어 있지만 명시적으로 로드)
# # drtmle:::SL.npreg 함수를 직접 사용해도 됩니다.
# SL.npreg <- function(Y, X, newX, family = gaussian(), obsWeights = rep(1, length(Y)), ...) {
#   options(np.messages = FALSE)
#   if (abs(diff(range(Y))) <= 1e-07) {
#     thisMod <- glm(Y ~ 1, data = X)
#   } else {
#     bw <- np::npregbw(stats::as.formula(paste("Y ~", paste(names(X), collapse = "+"))),
#                       data = X, ftol = 0.01, tol = 0.01, remin = FALSE)
#     thisMod <- np::npreg(bw)
#   }
#   pred <- stats::predict(thisMod, newdata = newX)
#   fit <- list(object = thisMod)
#   class(fit) <- "SL.npreg"
#   out <- list(pred = pred, fit = fit)
#   return(out)
# }
# 
# # SL.npreg에 대한 predict 메서드 (drtmle 내부에도 정의되어 있지만 명시적으로 로드)
# # drtmle:::predict.SL.npreg 함수를 직접 사용해도 됩니다.
# predict.SL.npreg <- function(object, newdata, ...) {
#   pred <- stats::predict(object$object, newdata = newdata)
#   return(pred)
# }


#' Analyze Simulation Data and Plot Dose-Response Curve using drtmle (TMLE)
#'
#' This function performs a doubly robust analysis (TMLE) on simulation data
#' with a continuous treatment variable to estimate and visualize a dose-response curve.
#' The continuous treatment is discretized into intervals for drtmle analysis.
#'
#' @param data A data frame with L1 (confounder), time_to_treat (continuous treatment), and death (binary outcome).
#' @param treat_var_name A string for the name of the continuous treatment variable (e.g., "time_to_treat").
#' @param outcome_var_name A string for the name of the binary outcome variable (e.g., "death").
#' @param confounder_var_names A character vector for the names of confounder variables (e.g., "L1").
#' @param n_bins Integer, number of bins to discretize the continuous treatment variable.
#' @param sl_library_g A character vector specifying the SuperLearner library for the PS model.
#' @param sl_library_Q A character vector specifying the SuperLearner library for the OR model.
#' @return A list containing:
#'   - results_df: A tibble with estimated mean outcomes for each discretized treatment level.
#'   - dose_response_plot: A ggplot object visualizing the dose-response curve.
#' @export analyze_drtmle_dose_response
analyze_drtmle_dose_response <- function(data,
                                         treat_var_name = "time_to_treat",
                                         outcome_var_name = "death",
                                         confounder_var_names = "L1",
                                         n_bins = 8, # Number of bins for discretizing treatment
                                         sl_library_g = c("SL.glm", "SL.mean", "SL.npreg"),
                                         sl_library_Q = c("SL.glm", "SL.mean", "SL.npreg")) {
  
  
  library(tmle)
  library(drtmle)
  library(SuperLearner)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(np) # For SL.npreg, if used in SuperLearner library
  
  
  
  if(FALSE){
    
    data = df_dtr
    treat_var_name = "time_to_antibiotics"
    outcome_var_name = "frailty_phenotype"
    confounder_var_names = c("AGE", "SEX", "BMI", "Elig02", "SOFA_Initial", "Hb", "CRP", "Lactate", "Plt", "Cr", "Bilirubin", "WBC")
    n_bins = 3 # 8개의 이산 구간으로 나눔
    sl_library_g = c("SL.glm", "SL.npreg")[1] # PS 모델에 사용할 알고리즘
    sl_library_Q = c("SL.glm", "SL.npreg")[1]  # OR 모델에 사용할 알고리즘
    
  }
  
  
  if(FALSE){
    
    sim_data_aipw <- simulate_aipw_data(n = 1000)
    
    drtmle_analysis_results <- analyze_drtmle_dose_response(
      data = sim_data_aipw,
      treat_var_name = "time_to_treat",
      outcome_var_name = "death",
      confounder_var_names = "L1",
      n_bins = 8, # 8개의 이산 구간으로 나눔
      sl_library_g = c("SL.glm", "SL.npreg"), # PS 모델에 사용할 알고리즘
      sl_library_Q = c("SL.glm", "SL.npreg")  # OR 모델에 사용할 알고리즘
    )
    
    data = sim_data_aipw
    treat_var_name = "time_to_treat"
    outcome_var_name = "death"
    confounder_var_names = "L1"
    n_bins = 3
    sl_library_g = c("SL.glm", "SL.npreg")[1]
    sl_library_Q = c("SL.glm", "SL.npreg")[1]
    
  }
  
  
  # Ensure correct variable types
  data[[outcome_var_name]] <- as.numeric(data[[outcome_var_name]])
  data[[treat_var_name]] <- as.numeric(data[[treat_var_name]])
  for(conf_var in confounder_var_names) {
    data[[conf_var]] <- as.numeric(data[[conf_var]])
  }
  
  # 1. 치료 변수 이산화 (Discretize continuous treatment)
  # cut 함수를 사용하여 'time_to_treat'를 n_bins개의 구간으로 나눕니다.
  # labels = FALSE를 사용하여 각 구간을 1부터 n_bins까지의 정수로 표현합니다.
  if(FALSE){
    data$discretized_treat_level <- as.integer(cut(data[[treat_var_name]],
                                                   breaks = n_bins,
                                                   include.lowest = TRUE,
                                                   labels = FALSE))
  }
  
  data$discretized_treat_level <- data[[treat_var_name]]
  
  
  # 각 구간의 대표값 (예: 중간값)을 계산합니다.
  # 이는 용량-반응 곡선의 x축 값이 됩니다.
  bin_midpoints <- data %>%
    group_by(discretized_treat_level) %>%
    summarise(midpoint = mean(.data[[treat_var_name]]), .groups = "drop") %>%
    arrange(discretized_treat_level)
  
  # `drtmle` 함수에 사용할 W, A, Y 변수를 준비합니다.
  W_data <- data %>% dplyr::select(all_of(confounder_var_names))
  A_data <- data$discretized_treat_level # 이산화된 치료 변수를 A로 사용
  Y_data <- data[[outcome_var_name]]
  
  # `a_0` 인자를 위해 이산화된 치료 레벨을 설정합니다.
  # `drtmle`는 `a_0`에 주어진 레벨에 대한 marginal mean을 추정합니다.
  a_0_levels <- sort(unique(A_data))
  
  # 2. drtmle 분석 수행 (Perform drtmle analysis)
  # `drtmle`는 이진 결과(`family = binomial()`)에 적합합니다.
  # `stratify = FALSE`는 OR 모델을 A=1과 A=0으로 나누지 않고 통합적으로 모델링함을 의미합니다.
  # `SL_g`, `SL_Q`, `SL_gr`, `SL_Qr`에 SuperLearner 라이브러리를 지정합니다.
  # `SL.npreg`는 비모수적 회귀로, 연속형 공변량에 유연하게 대응합니다.
  drtmle_fit <- drtmle(
    W = W_data,
    A = A_data,
    Y = Y_data,
    family = binomial(),
    SL_g = sl_library_g, # Propensity Score (PS) model
    SL_Q = sl_library_Q, # Outcome Regression (OR) model
    SL_gr = sl_library_g, # Reduced-dimension PS (often same as SL_g)
    SL_Qr = sl_library_Q, # Reduced-dimension OR (often same as SL_Q)
    stratify = FALSE, # Fit a single OR for all A levels if FALSE, otherwise stratified
    a_0 = a_0_levels # Estimate marginal means for all discretized treatment levels
  )
  
  
  
  # 3. 결과 추출 및 정리 (Extract and organize results)
  # drtmle_fit$est는 각 A 레벨에 대한 추정된 주변 평균을 포함합니다.
  results_df <- as_tibble((drtmle_fit$aiptw$est), rownames = "discretized_treat_level") %>%
    mutate(discretized_treat_level = as.integer(discretized_treat_level)) %>%
    rename(estimated_outcome = value) # V1은 drtmle_fit$est의 기본 이름
  
  # 이산화된 치료 레벨을 실제 time_to_treat의 대표값과 연결합니다.
  results_df <- results_df %>%
    left_join(bin_midpoints, by = "discretized_treat_level") %>%
    rename(dose = midpoint) %>%
    dplyr::select(dose, estimated_outcome, discretized_treat_level) %>%
    arrange(dose)
  
  # 4. 용량-반응 곡선 시각화 (Visualize dose-response curve)
  dose_response_plot <- ggplot(results_df, aes(x = dose, y = estimated_outcome)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 3) +
    labs(title = "Dose-Response Curve using drtmle",
         x = paste0("Hypothetical ", treat_var_name, " (Discretized Mean)"),
         y = paste0("Estimated Expected ", outcome_var_name, " Probability")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(fit=drtmle_fit, results_df = results_df, dose_response_plot = dose_response_plot))
}



# --- 시뮬레이션 데이터 생성 (이전과 동일) ---
simulate_aipw_data <- function(n = 1000) {
  L1 <- rnorm(n, mean = 0, sd = 1) # continuous confounder
  
  # treat_var: time_to_treat (연속형 치료, 예: 약물 투여까지의 시간)
  # L1에 따라 time_to_treat가 달라진다고 가정
  time_to_treat <- rnorm(n, mean = 5 + 2 * L1, sd = 2) # continuous treatment
  
  # outcome_var: death (이진 결과)
  # death는 L1, time_to_treat에 따라 달라진다고 가정
  death_prob <- 1 / (1 + exp(-(-1.5 + 0.5 * L1 - 0.2 * time_to_treat + 0.05 * time_to_treat^2)))
  death <- rbinom(n, 1, death_prob)
  
  return(data.frame(L1, time_to_treat, death))
}











#' @title GLM 분석 파이프라인 함수
#' @description 지정된 조건에 따라 데이터를 필터링하고, 기술 통계 테이블, 
#'              개별/다중 GLM 분석 결과 테이블 및 신뢰구간 그래프를 생성합니다.
#'
#' @param data 분석할 데이터프레임.
#' @param Y 종속변수 이름 (문자열).
#' @param X 주요 독립변수 이름 벡터 (문자열 벡터).
#' @param Z 공변량(covariates) 이름 벡터 (문자열 벡터, optional). 기본값은 NULL.
#' @param V 데이터를 필터링할 조건 (문자열). 예: "AGE > 50 & SEX == 'Male'". 기본값은 NULL (필터링 없음).
#' @param family GLM에 사용할 family. "auto" (기본값)로 설정 시 Y 변수 타입을 감지하여 'binomial' 또는 'gaussian'을 자동 선택.
#' @param exponentiate 로지스틱 회귀 결과에서 오즈비를 계산할지 여부. family가 "binomial"일 때 기본값은 TRUE.
#'
#'
#' @import tidyverse
#' @import gtsummary
#' @import tableone
#' @import rlang
#' 
#' @return 모든 결과물을 포함하는 리스트. 리스트 구성:
#'         - `filtered_data`: 필터링된 데이터
#'         - `summary_table`: tableone 요약 테이블
#'         - `glm_models`: 모든 GLM 모델 객체 (개별 + 다중)
#'         - `glm_results_table`: gtsummary로 생성된 통합 결과 테이블
#'         - `ci_plot`: 신뢰구간 ggplot 그래프
#'
#'
#' @export analyze_glm_pipeline
analyze_glm_pipeline <- function(data, Y, X, Z = NULL, V = NULL, family = "auto", exponentiate = NULL, xlim=NULL) {

  # --- 1, 2, 3 단계는 이전과 동일 ---
  # --- 1. 입력값 검증 및 데이터 준비 ---
  if (!is.data.frame(data)) stop("`data`는 반드시 데이터프레임이어야 합니다.")
  all_vars <- c(Y, X, Z)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다: ",
         paste(all_vars[!all_vars %in% names(data)], collapse = ", "))
  }
  if (!is.null(V)) {
    cat(paste("--- 데이터 필터링 적용:", V, "---\n\n"))
    filtered_data <- data %>%
      filter(!!rlang::parse_expr(V))
    if(nrow(filtered_data) == 0) stop("필터링 조건에 맞는 데이터가 없습니다.")
  } else {
    filtered_data <- data
  }
  
  filtered_data <- filtered_data %>% select( all_vars ) %>% drop_na()
  
  # --- 2. 데이터 요약 테이블 (Table 1) 생성 ---
  cat("--- 1. 데이터 요약 테이블 (Table 1) ---\n")
  cat(paste("종속변수 '", Y, "'에 따라 그룹화된 요약입니다.\n\n", sep=""))

  is_binomial <- FALSE
  if (n_distinct(filtered_data[[Y]]) == 2) {
    is_binomial <- TRUE
    y_levels <- sort(unique(filtered_data[[Y]]))
    filtered_data <- filtered_data %>%
      mutate(!!Y := factor(!!sym(Y), levels = y_levels))
  }

  vars_to_summarize <- c(X, Z)
  summary_table <- tableone::CreateTableOne(
    vars = vars_to_summarize,
    strata = Y,
    data = filtered_data,
    test = TRUE,
    smd = TRUE
  )
  print(summary_table, smd = TRUE, varLabels = TRUE)
  cat("\n\n")

  # --- 3. GLM 분석 수행 ---
  if (family == "auto") {
    model_type <- if (is_binomial) "binomial" else "gaussian"
  } else {
    model_type <- family
  }

  if (is.null(exponentiate)) {
    exponentiate <- (model_type == "binomial")
  }

  cat(paste("--- 2. GLM 결과 (Family:", model_type, ") ---\n"))

  target_level <- NULL
  if (model_type == "binomial") {
    target_level <- levels(filtered_data[[Y]])[2]
    cat(paste0("Info: 결과변수(Y) '", Y, "'의 '", target_level, "' 수준에 대한 확률을 모델링합니다.\n\n"))
  } else {
    cat("\n")
  }

  models <- list()
  tbl_regressions <- list()

  for (x_var in X) {
    formula_uni_str <- paste(Y, "~", paste(c(x_var, Z), collapse = " + "))
    model_uni <- glm(as.formula(formula_uni_str), data = filtered_data, family = model_type)
    models[[paste0("uni_", x_var)]] <- model_uni

    tbl_regressions[[x_var]] <- model_uni %>%
      tbl_regression(exponentiate = exponentiate) %>%
      modify_header(label = paste0("**", x_var, "**"))
  }

  formula_multi_str <- paste(Y, "~", paste(c(X, Z), collapse = " + "))
  model_multi <- glm(as.formula(formula_multi_str), data = filtered_data, family = model_type)
  models[["multi"]] <- model_multi

  tbl_multi_gtsum <- model_multi %>%
    tbl_regression(exponentiate = exponentiate)

  all_tbls <- c(tbl_regressions, list(multiple=tbl_multi_gtsum))
  tbl_names <- c(paste("개별:", X), "다중")

  table_caption <- if (!is.null(target_level)) {
    paste0("**표. 로지스틱 회귀분석 결과.** 결과: P(", Y, " = ", target_level, ")에 대한 오즈비(Odds Ratios)")
  } else {
    "**표. 일반화 선형 모형(GLM) 결과.**"
  }


  glm_results_table <- all_tbls
  # glm_results_table <- tbl_merge(
  #   tbls = all_tbls,
  #   tab_spanner = paste0("**", tbl_names, " 모형**")
  # ) %>%
  #   modify_spanning_header(everything() ~ NA) %>%
  #   modify_caption(table_caption)

  # print(glm_results_table)
  # cat("\n\n")

  # --- 4. 신뢰구간 그래프 생성 (수정된 부분) ---

  cat("--- 3. 주요 변수(X) 신뢰구간(CI) 그래프 (Univariate/Multivariate 2-Panel) ---\n\n")

  tidy_results <- list()
  for(model_name in names(models)){
    model_label <- if (model_name == "multi") "다중 모형" else paste("개별:", str_remove(model_name, "uni_"))

    tidy_results[[model_name]] <- broom::tidy(models[[model_name]], conf.int = TRUE, exponentiate = exponentiate) %>%
      mutate(model = model_label)
  }

  x_pattern <- paste0("^(", paste(X, collapse = "|"), ")")

  # <<< 수정된 부분 시작 >>>
  plot_data <- bind_rows(tidy_results) %>%
    filter(str_detect(term, x_pattern)) %>%
    # 1. 패널 구분을 위한 새 그룹 변수 'panel_group' 생성
    mutate(
      panel_group = if_else(str_starts(model, "개별:"), "개별 분석 (Univariate)", "다중 분석 (Multivariate)"),
      panel_group = factor(panel_group, levels = c("개별 분석 (Univariate)", "다중 분석 (Multivariate)"))
    ) %>%
    mutate(term = fct_inorder(term),
           model = factor(model, levels = c(paste("개별:", X), "다중 모형")))

  plot_subtitle <- if (!is.null(target_level)) {
    paste0("결과: P(", Y, " = ", target_level, ") | 모든 모형은 공변량으로 보정됨")
  } else {
    "모든 모형은 공변량으로 보정됨"
  }

  # 2. 새 그룹 변수로 패널을 나누고, 패널 내에서는 색상/위치로 모델 구분
  ci_plot <- ggplot(plot_data, aes(x = estimate, y = fct_rev(term), xmin = conf.low, xmax = conf.high, color = model)) +
    geom_point(position = position_dodge(width = 0.7), size = 2.5) +
    geom_errorbarh(height = 0.2, position = position_dodge(width = 0.7), size = 0.8) +
    facet_wrap(~ panel_group, scales = "free_y") + # 'panel_group'으로 패널 분리
    {if (exponentiate) geom_vline(xintercept = 1, linetype = "dashed", color = "grey50")} +
    labs(
      title = "주요 변수(X)의 회귀 계수 및 95% 신뢰구간",
      subtitle = plot_subtitle,
      x = if (exponentiate) "오즈비 (Odds Ratio) 또는 추정치" else "회귀 계수 (Estimate)",
      y = "주요 변수 (Term)",
      color = "모형 종류" # 범례 제목
    ) +
    # scale_x_continuous(limits=xlim) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = c("bottom","none")[2],
      strip.text = element_text(face = "bold", size = 11)
    )
  # <<< 수정된 부분 끝 >>>

  print(ci_plot)

  # --- 5. 결과물 반환 ---
  invisible(list(
    parameters = list(
      Y = Y, X = X, Z = Z, V = V, model_type = model_type,
      exponentiate = exponentiate
    ),
    filtered_data = filtered_data,
    summary_table = summary_table,
    glm_models = models,
    glm_results_table = glm_results_table,
    ci_plot = ci_plot
  ))

}





#' @title 그룹별 GLM 분석 및 통합 시각화 파이프라인
#' @description 지정된 그룹 변수의 각 수준에 대해 GLM 분석을 개별적으로 수행하고,
#'              CI Plot에서 모든 그룹의 결과를 통합하여 시각화합니다.
#'
#' @param data 분석할 데이터프레임.
#' @param group 그룹을 나눌 변수 이름 (문자열).
#' @param Y 종속변수 이름 (문자열).
#' @param X 주요 독립변수 이름 벡터 (문자열 벡터).
#' @param Z 공변량 이름 벡터 (문자열 벡터, optional).
#' @param ... analyze_glm_pipeline의 다른 인자들 (family, exponentiate 등)
#' @export analyze_glm_by_group
analyze_glm_by_group <- function(data, Y, X, Z = NULL, group = NULL, family = "auto", exponentiate = TRUE, xlim = NULL) {
  
  
  # 필요한 패키지
  library(tidyverse)
  library(gtsummary)
  library(tableone)
  library(rlang)
  library(stringr)
  library(nnet) # 다항 분석을 위해 포함
  
  
  
  #--- 1. 입력값 검증 및 전체 데이터 필터링 ---
  if (!is.data.frame(data)) stop("`data`는 반드시 데이터프레임이어야 합니다.")
  all_vars <- c(Y, X, Z, group)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  data[[group]] <- factor(data[[group]])
  group_levels <- levels(data[[group]])
  
  #--- 2. 그룹별로 분석을 반복 수행 (purrr::map 사용) ---
  
  all_group_results <- map(group_levels, function(current_group) {
    
    cat(paste0("\n\n========================================================\n"))
    cat(paste0(">>> 분석 시작: ", group, " = ", current_group, "\n"))
    cat(paste0("========================================================\n"))
    
    # 현재 그룹의 데이터만 필터링
    group_data <- data %>% 
      filter(!!sym(group) == current_group) %>%
      select(all_of(c(Y, X, Z))) %>%
      drop_na()
    
    if (nrow(group_data) < 20) {
      message("데이터가 너무 적어(", nrow(group_data),"개) 분석을 건너뜁니다.")
      return(NULL)
    }
    
    # <<< 이하 로직은 기존 analyze_glm_pipeline과 거의 동일 >>>
    # 단, filtered_data 대신 group_data를 사용
    
    # 모델 타입 결정
    y_var <- group_data[[Y]]
    y_levels_group <- NULL
    if (family == "auto") {
      # ... (이전과 동일한 모델 타입 감지 로직) ...
      if (is.numeric(y_var) && n_distinct(y_var) > 5) model_type <- "gaussian"
      else {
        group_data[[Y]] <- factor(y_var); y_levels_group <- levels(group_data[[Y]])
        if (length(y_levels_group) == 2) model_type <- "binomial"
        else if (length(y_levels_group) > 2) model_type <- "multinomial"
        else stop("Y 변수 수준이 1개 이하")
      }
    } else { model_type <- family }
    if (is.null(exponentiate)) exponentiate <- (model_type %in% c("binomial", "multinomial"))
    
    # 모델 적합
    models <- list()
    for (x_var in X) {
      cat("\n\n==================\n")
      print(x_var)
      cat("\n\n==================n")
      formula_uni_str <- paste(Y, "~", paste(c(x_var, Z), collapse = " + "))
      if (model_type == "multinomial") models[[paste0("uni_", x_var)]] <- nnet::multinom(as.formula(formula_uni_str), data = group_data, trace = FALSE)
      else models[[paste0("uni_", x_var)]] <- glm(as.formula(formula_uni_str), data = group_data, family = model_type)
    }
    formula_multi_str <- paste(Y, "~", paste(c(X, Z), collapse = " + "))
    if (model_type == "multinomial") model_multi <- nnet::multinom(as.formula(formula_multi_str), data = group_data, trace = FALSE)
    else model_multi <- glm(as.formula(formula_multi_str), data = group_data, family = model_type)
    models[["multi"]] <- model_multi
    
    # broom::tidy로 결과 정리 (플롯을 위해)
    tidy_results <- list()
    for (model_name in names(models)) {
      model_label <- if (model_name == "multi") "다중 분석 (Multivariate)" else "개별 분석 (Univariate)"
      tidy_df <- broom::tidy(models[[model_name]], conf.int = TRUE, exponentiate = exponentiate)
      # ... (y.level 처리 로직) ...
      tidy_results[[model_name]] <- tidy_df %>% mutate(model = model_label, !!group := current_group) # <<< 그룹 정보 추가
    }
    
    # 각 그룹의 결과를 리스트로 반환
    return(list(
      models = models,
      tidy_results_for_plotting = bind_rows(tidy_results) # <<< 시각화를 위해 tidy 결과 반환
    ))
  })
  
  # 리스트 이름 설정
  names(all_group_results) <- group_levels
  
  #--- 3. 모든 그룹의 결과를 통합하여 CI Plot 생성 ---
  
  cat("\n\n========================================================\n")
  cat(">>> 최종 통합 신뢰구간(CI) 그래프 생성\n")
  cat("========================================================\n")
  
  # 각 그룹에서 생성된 tidy 결과를 하나로 합침
  plot_data <- map(all_group_results, "tidy_results_for_plotting") %>%
    bind_rows() %>%
    filter(!is.null(!!sym(group))) # 분석 실패한 그룹 제외
  
  if (nrow(plot_data) == 0) {
    message("시각화할 분석 결과가 없습니다.")
    return(invisible(all_group_results))
  }
  
  # 플롯 데이터 가공
  x_pattern <- paste0("^(", paste(X, collapse = "|"), ")")
  plot_data_final <- plot_data %>%
    filter(str_detect(term, x_pattern)) %>%
    mutate(
      panel_group = factor(model, levels = c("개별 분석 (Univariate)", "다중 분석 (Multivariate)")),
      term = fct_inorder(term)
    )
  
  # <<< 최종 CI Plot 생성 (요청사항 반영) >>>
  ci_plot <- ggplot(plot_data_final, 
                    aes(x = estimate, y = fct_rev(term), 
                        xmin = conf.low, xmax = conf.high, 
                        color = !!sym(group))) + # <<< 그룹별로 색상 지정
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbarh(height = 0.2, position = position_dodge(width = 0.6), size = 0.8) +
    # <<< 그룹을 위아래(행)로, 분석 종류를 좌우(열)로 배치 >>>
    facet_grid( ~panel_group, scales = "free_y", switch = "y") +
    {if (exponentiate) geom_vline(xintercept = 1, linetype = "dashed", color = "grey50")} +
    labs(
      title = "그룹별 회귀분석 결과 비교",
      subtitle = "각 그룹의 결과를 색상 및 패널로 구분",
      x = if (exponentiate) "오즈비 (Odds Ratio) 또는 추정치" else "회귀 계수 (Estimate)",
      y = NULL,
      color = group
    ) +
    # coord_cartesian(xlim = xlim) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none", # 색상과 패널이 중복되므로 범례는 제거
      strip.text.y.left = element_text(face = "bold", angle = 0),
      strip.text.x = element_text(face = "bold", size = 11),
      axis.title.y = element_blank()
    )
  
  print(ci_plot)
  
  #--- 4. 결과물 반환 ---
  all_group_results$final_ci_plot <- ci_plot
  return(invisible(all_group_results))
}







#' @title GLM 결과 정제 및 변수 선택 함수
#' @description analyze_glm_pipeline의 결과 객체를 받아, 다중 회귀 모델에 대해
#'              변수 선택을 수행하고 최종 모델과 비교 결과를 반환합니다.
#'
#' @param analysis_results `analyze_glm_pipeline` 함수로부터 반환된 리스트 객체.
#' @param selection_method 변수 선택 방법. "backward"(후진 제거법), "stepwise"(단계적 선택법) 중 선택. 기본값은 "stepwise".
#' @param selection_alpha 후진 제거법에 사용할 p-value 기준(alpha). 기본값은 0.1.
#' @param keep_vars 변수 선택 과정에서 항상 포함시킬 변수 이름 벡터. 
#'                  기본값은 NULL이며, 이 경우 원본 분석의 주요 변수(X)가 자동으로 유지됩니다.
#'
#' @export refine_glm_model
refine_glm_model <- function(analysis_results,
                             selection_method = "stepwise",
                             selection_alpha = 0.1,
                             keep_vars = NULL) {
  
  library(tidyverse)
  library(gtsummary)
  library(rlang)
  library(broom) # broom 패키지 명시적 로드
  
  #--- 1. 입력값 검증 ---
  if (!is.list(analysis_results) || is.null(analysis_results$glm_models$multi)) {
    stop("`analyze_glm_pipeline` 함수로부터 생성된 유효한 결과 객체를 입력해주세요.")
  }
  
  model_multi <- analysis_results$glm_models$multi
  params <- analysis_results$parameters
  
  # 다항 로지스틱 회귀분석은 현재 지원하지 않음
  if (params$model_type == "multinomial") {
    message("알림: 다항 로지스틱 회귀분석에 대한 자동 변수 선택은 현재 지원되지 않습니다.")
    return(invisible(NULL))
  }
  
  #--- 2. 변수 선택 수행 ---
  cat(paste0("--- 다중 회귀 모델 정제 시작 (방법: ", selection_method, ") ---\n\n"))
  
  model_final <- NULL
  
  if (selection_method == "fdr") {
    # FDR 보정 p-value를 사용한 변수 선택
    
    # 1. 전체 모델의 계수와 p-value 추출 (절편 제외)
    model_summary <- broom::tidy(model_multi) %>%
      filter(term != "(Intercept)")
    
    if (nrow(model_summary) == 0) {
      message("모델에 변수가 없어 FDR 선택을 진행할 수 없습니다.")
      model_final <- model_multi # 원본 모델 그대로 반환
    } else {
      # 2. FDR 보정 (Benjamini-Hochberg)
      adjusted_summary <- model_summary %>%
        mutate(p.adj = p.adjust(p.value, method = "fdr"))
      
      # 3. 보정된 p-value가 selection_alpha보다 작은 변수들의 'term'을 선택
      significant_terms <- adjusted_summary %>%
        filter(p.adj < selection_alpha) %>%
        pull(term)
      
      # 4. 모델의 원래 변수명 가져오기 (factor 변수 처리 위함)
      original_predictors <- attr(model_multi$terms, "term.labels")
      
      # 5. 유의미한 term에 해당하는 원래 변수명 찾기
      # 예: "SexMale" term이 유의하면 "Sex" 변수를 선택
      selected_predictors <- original_predictors[sapply(original_predictors, function(p) {
        any(str_starts(significant_terms, p))
      })]
      
      # 6. `keep_vars`와 합쳐 최종 변수 목록 생성
      final_predictors <- union(selected_predictors, keep_vars)
      
      # 7. 최종 모델 생성
      if (length(final_predictors) == 0) {
        # 유의한 변수가 하나도 없으면 절편만 있는 모델 생성
        formula_new_str <- paste(params$Y, "~ 1")
        message("FDR 기준을 만족하는 변수가 없어 절편(intercept) 모델을 반환합니다.")
      } else {
        formula_new_str <- paste(params$Y, "~", paste(final_predictors, collapse = " + "))
      }
      
      model_final <- glm(as.formula(formula_new_str),
                         data = analysis_results$filtered_data,
                         family = params$model_type)
    }
    
  } else if (selection_method == "stepwise") {
    # AIC를 사용한 단계적 선택법 (후진)
    scope_formula <- if (!is.null(keep_vars)) {
      as.formula(paste("~", paste(keep_vars, collapse = " + ")))
    } else {
      # keep_vars가 없으면 최소 모델은 절편 모델.
      as.formula("~ 1")
    }
    
    model_multi$call$data <- analysis_results$filtered_data
    model_multi$call$family <- params$model_type
    
    model_final <- step(model_multi,
                        direction = "backward",
                        scope = list(lower = scope_formula, upper = . ~ .),
                        trace = 0) # trace=0으로 중간 과정 출력 생략
    
  } else if (selection_method == "backward") {
    # p-value 기반 후진 제거법 (기존 로직 유지)
    current_model <- model_multi
    
    while(TRUE) {
      model_summary <- broom::tidy(current_model)
      
      all_model_vars <- attr(current_model$terms, "term.labels")
      removable_vars <- setdiff(all_model_vars, keep_vars)
      
      if (length(removable_vars) == 0) break
      
      # 제거 가능한 변수들의 p-value 추출
      p_values_to_check <- model_summary %>%
        filter(term %in% unlist(sapply(removable_vars, function(v) model_summary$term[str_starts(model_summary$term, v)])))
      
      if (nrow(p_values_to_check) == 0) break
      
      max_p_row <- p_values_to_check %>% slice_max(p.value, n = 1)
      
      if (max_p_row$p.value > selection_alpha) {
        var_to_remove_term <- max_p_row$term
        
        # term으로부터 원래 변수명 찾기
        base_var_to_remove <- removable_vars[sapply(removable_vars, function(p) str_starts(var_to_remove_term, p))]
        
        new_predictors <- setdiff(all_model_vars, base_var_to_remove)
        
        formula_new_str <- if(length(new_predictors) > 0) {
          paste(params$Y, "~", paste(new_predictors, collapse = " + "))
        } else {
          paste(params$Y, "~ 1")
        }
        
        current_model <- glm(as.formula(formula_new_str),
                             data = analysis_results$filtered_data,
                             family = params$model_type)
      } else {
        break
      }
    }
    model_final <- current_model
    
  } else {
    stop("지원되지 않는 변수 선택 방법입니다. 'fdr', 'backward' 또는 'stepwise'를 사용해주세요.")
  }
  
  #--- 3. 결과 요약 및 비교 테이블 생성 ---
  cat("--- 변수 선택 결과 ---\n")
  
  initial_variable <- names(model_multi$coefficients)
  final_variable <- names(model_final$coefficients)
  
  # 최종 선택 모델 요약 테이블
  tbl_final <- tbl_regression(model_final, exponentiate = params$exponentiate)
  
  # 기존 다중 모델과 최종 모델 비교 테이블
  comparison_table <- tbl_merge(
    tbls = list(
      tbl_regression(model_multi, exponentiate = params$exponentiate),
      tbl_final
    ),
    tab_spanner = c("**전체 다중 모델**", "**최종 선택 모델**")
  )
  
  print(comparison_table)
  
  #--- 4. 결과물 반환 ---
  invisible(list(
    final_model = model_final,
    comparison_table = comparison_table,
    final_variable = final_variable,
    diff_variable = setdiff(initial_variable, final_variable)
  ))
}











#' @title 동적 Alluvial Plot 생성 함수
#' @description 두 범주형 변수 간의 흐름을 시각화하는 Alluvial plot을 생성합니다.
#'
#' @param data 분석할 데이터프레임.
#' @param axis1_var 첫 번째 축으로 사용할 변수 이름 (문자열).
#' @param axis2_var 두 번째 축으로 사용할 변수 이름 (문자열).
#' @param fill_var 흐름(alluvium)의 색상을 채우는 데 사용할 변수 이름 (문자열). 
#'                 기본값은 NULL이며, 이 경우 `axis1_var`를 사용합니다.
#' @param title 그래프의 주 제목.
#' @param axis1_lab 그래프에 표시될 첫 번째 축의 레이블. 기본값은 `axis1_var`와 동일.
#' @param axis2_lab 그래프에 표시될 두 번째 축의 레이블. 기본값은 `axis2_var`와 동일.
#'
#' @export create_alluvial_plot
create_alluvial_plot <- function(data, 
                                 axis1_var, 
                                 axis2_var, 
                                 fill_var = NULL,
                                 title = "Alluvial Plot",
                                 axis1_lab = NULL,
                                 axis2_lab = NULL,
                                 proportion = TRUE) {
  
  
  library(ggalluvial)
  
  
  
  #--- 1. 입력값 검증 ---
  required_vars <- c(axis1_var, axis2_var)
  if (!all(required_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  #--- 2. 동적 변수 설정 ---
  # fill_var가 지정되지 않으면 axis1_var를 사용
  if (is.null(fill_var)) {
    fill_var <- axis1_var
  }
  # 축 레이블이 지정되지 않으면 변수 이름을 사용
  if (is.null(axis1_lab)) {
    axis1_lab <- axis1_var
  }
  if (is.null(axis2_lab)) {
    axis2_lab <- axis2_var
  }
  
  #--- 3. 플롯을 위한 데이터 전처리 ---
  # .data[[var]] 문법을 사용하여 문자열로 된 변수 이름을 동적으로 처리
  df_alluvial <- data %>%
    filter(!is.na(.data[[axis1_var]]) & !is.na(.data[[axis2_var]])) %>%
    group_by(.data[[axis1_var]], .data[[axis2_var]]) %>%
    summarise(freq = n(), .groups = 'drop')
  
  
  if(proportion){
    
    df_alluvial <- df_alluvial %>%
      # 각 x_var 그룹 내에서 ipw_freq의 합으로 나누어 비율(proportion)을 계산
      group_by(.data[[axis1_var]]) %>%
      mutate(
        proportion = freq / sum(freq),
        # 비율에 100을 곱해 정규화된 빈도(%)를 만듦
        freq = proportion * 100
      ) %>%
      ungroup()
    
  }
  
  
  
  #--- 4. 플롯 생성 ---
  if (nrow(df_alluvial) == 0) {
    message("Alluvial plot을 생성하기 위한 데이터가 부족합니다.")
    return(invisible(NULL)) # 빈 플롯 대신 NULL 반환
  }
  
  plot <- ggplot(data = df_alluvial,
                 # aes() 내부에서도 .data[[var]] 사용
                 aes(axis1 = .data[[axis1_var]], 
                     axis2 = .data[[axis2_var]], 
                     y = freq)) +
    geom_alluvium(aes(fill = .data[[fill_var]]), alpha = 0.7) + # 흐름
    geom_stratum(width = 1/3) + # 축
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) + # 축 레이블
    scale_x_discrete(limits = c(axis1_lab, axis2_lab), expand = c(0.15, 0.05)) +
    labs(
      title = title,
      y = "빈도 (Frequency)",
      fill = fill_var # 범례 제목
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  return(plot)
}




#' @title K-level Alluvial Plot 생성 함수
#' @description 지정된 여러 축(k-level)에 대한 Alluvial plot (충적 다이어그램)을 생성합니다.
#' @param data 데이터프레임.
#' @param axis_vars character vector. 플롯의 축으로 사용할 변수 이름들의 벡터. (예: `c("변수1", "변수2", "변수3")`)
#' @param title character. 플롯의 제목.
#' @param axis_labs character vector. 각 축에 표시될 레이블. 지정하지 않으면 `axis_vars`의 변수 이름이 사용됩니다.
#' @param proportion logical. TRUE이면 첫 번째 축의 그룹을 기준으로 각 흐름의 비율(%)을 계산하여 y축으로 사용합니다. FALSE이면 절대 빈도를 사용합니다.
#' @param fill_by_first_axis logical. TRUE이면 모든 흐름(alluvium)의 색상을 첫 번째 축의 값에 따라 결정합니다. FALSE이면 각 축의 값(stratum)에 따라 색상이 바뀝니다.
#' @return ggplot 객체.
#' 
#' 
#' @export create_alluvial_plot_k
create_alluvial_plot_k <- function(data, 
                                   axis_vars,
                                   title = "Alluvial Plot",
                                   axis_labs = NULL,
                                   proportion = TRUE,
                                   fill_by_first_axis = TRUE) {
  
  # --- 0. 필수 패키지 로드 ---
  # install.packages(c("ggplot2", "ggalluvial", "dplyr", "tidyr"))
  library(ggplot2)
  library(ggalluvial)
  library(dplyr)
  library(tidyr)
  
  # --- 1. 입력값 검증 ---
  if (!all(axis_vars %in% names(data))) {
    stop("`axis_vars`에 지정된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  if (is.null(axis_labs)) {
    axis_labs <- axis_vars
  } else if (length(axis_vars) != length(axis_labs)) {
    stop("`axis_vars`와 `axis_labs`의 길이가 일치해야 합니다.")
  }
  
  # --- 2. 플롯을 위한 데이터 전처리 ---
  
  # 2.1. 각 흐름(flow)의 빈도 계산
  df_counts <- data %>%
    # NA 값을 가진 행은 분석에서 제외
    filter(if_all(all_of(axis_vars), ~ !is.na(.))) %>%
    # 지정된 모든 축 변수를 기준으로 그룹화하여 빈도 계산
    group_by(across(all_of(axis_vars))) %>%
    summarise(freq = n(), .groups = 'drop') %>%
    filter(freq > 0)
  
  # 2.2. (선택) 비율(%) 계산
  if (proportion) {
    # 첫 번째 축 변수를 기준으로 그룹 내 비율 계산
    first_axis_var <- sym(axis_vars[1])
    df_counts <- df_counts %>%
      group_by(!!first_axis_var) %>%
      # mutate(freq = freq / sum(freq) * 100) %>% # 백분율로 표시
      mutate(freq = freq / sum(freq)) %>% # 0-1 사이 비율로 표시
      ungroup()
  }
  
  # 2.3. ggalluvial을 위한 'long' 형식으로 데이터 변환
  df_long <- df_counts %>%
    mutate(alluvium_id = row_number()) %>% # 각 흐름에 고유 ID 부여
    pivot_longer(
      cols = all_of(axis_vars),
      names_to = "x",      # 축의 순서를 나타낼 컬럼
      values_to = "stratum" # 각 축의 값을 나타낼 컬럼
    ) %>%
    # 축의 순서를 factor level로 지정
    mutate(x = factor(x, levels = axis_vars))
  
  # 2.4. (선택) 첫 번째 축 기준으로 색상 채우기
  if (fill_by_first_axis) {
    # 첫 번째 축의 값을 가져와서 df_long에 합침
    fill_df <- df_counts %>% 
      mutate(alluvium_id = row_number()) %>%
      select(alluvium_id, fill_col = all_of(axis_vars[1]))
    
    df_long <- df_long %>% left_join(fill_df, by = "alluvium_id")
    fill_aes <- aes(fill = fill_col)
    fill_lab <- axis_vars[1]
  } else {
    fill_aes <- aes(fill = stratum)
    fill_lab <- "Value"
  }
  
  # --- 3. 플롯 생성 ---
  if (nrow(df_long) == 0) {
    message("Alluvial plot을 생성하기 위한 데이터가 없습니다.")
    return(invisible(NULL))
  }
  
  y_lab <- if (proportion) "비율 (Proportion)" else "빈도 (Frequency)"
  
  plot <- ggplot(data = df_long,
                 aes(x = x, stratum = stratum, alluvium = alluvium_id,
                     y = freq, label = stratum)) +
    geom_flow(fill_aes, alpha = 0.7) + # 흐름 (Alluvium)
    geom_stratum(width = 1/3) +      # 축 (Stratum)
    geom_text(stat = "stratum", size = 3.5) +
    scale_x_discrete(labels = axis_labs, expand = c(0.15, 0.05)) +
    labs(
      title = title,
      y = y_lab,
      fill = fill_lab
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_blank() # x축 제목 제거
    )
  
  return(plot)
}






#' @title 모델 기반 조정된 Alluvial Plot 생성 함수
#' @description 공변량의 효과를 통계적으로 조정한 후, 두 범주형 변수 간의 
#'              기대 흐름(expected flow)을 시각화하는 Alluvial plot을 생성합니다.
#'
#' @param data 분석할 데이터프레임.
#' @param y_var 종속변수 이름 (문자열). 이 변수의 각 수준이 축2가 됩니다.
#' @param x_var 주요 독립변수 이름 (문자열). 이 변수의 각 수준이 축1이 됩니다.
#' @param covariates 조정할 공변량들의 이름 (문자열 벡터).
#' @param title 그래프의 주 제목.
#' @param subtitle 그래프의 부제.
#'
#' @export create_adjusted_alluvial
create_adjusted_alluvial <- function(data, 
                                y_var, 
                                x_var, 
                                covariates,
                                stabilized = TRUE,
                                proportion = TRUE,
                                title = "IPW Adjusted Alluvial Plot",
                                subtitle = NULL) {
  
  #--- 1. 입력값 검증 및 데이터 타입 설정 ---
  required_vars <- c(y_var, x_var, covariates)
  if (!all(required_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  data <- data %>% dplyr::select(x_var, y_var, covariates) %>% drop_na()
  
  data[[y_var]] <- factor(data[[y_var]])
  data[[x_var]] <- factor(data[[x_var]])
  
  #--- 2. 성향 점수 모델 적합 (Propensity Score Model) ---
  # x_var를 종속변수로, 공변량을 독립변수로 모델링
  formula_ps <- paste(x_var, "~", paste(covariates, collapse = " + "))
  
  message("다음 공식으로 성향 점수 모델을 적합합니다:\n", formula_ps)
  
  fit_ps <- nnet::multinom(as.formula(formula_ps), data = data, trace = FALSE)
  
  #--- 3. 각 개인의 성향 점수(Propensity Score) 계산 ---
  # 모든 개인에 대해 각 x_var 수준에 속할 확률을 예측
  ps_matrix <- predict(fit_ps, newdata = data, type = "probs")
  
  # 각 개인이 실제로 속한 x_var 수준에 해당하는 성향 점수만 추출
  # (이 부분이 다소 복잡할 수 있습니다)
  actual_x_levels <- data[[x_var]]
  
  # 각 행(개인)에 대해, 실제 속한 그룹의 예측 확률을 가져옴
  if (is.vector(ps_matrix)) { # x_var 수준이 2개인 경우
    # glm과 달리 nnet은 모든 수준의 확률을 반환하지 않을 수 있음.
    # 이 경우, 현재 열이 두 번째 수준에 대한 확률이라고 가정.
    # 좀 더 강건한 방법은 matrix로 변환하는 것.
    ps_matrix_full <- as.data.frame(matrix(ps_matrix, ncol=1))
    colnames(ps_matrix_full) <- levels(actual_x_levels)[2]
    ps_matrix_full[[levels(actual_x_levels)[1]]] <- 1 - ps_matrix_full[,1]
    ps_matrix <- as.matrix(ps_matrix_full[,levels(actual_x_levels)])
  }
  
  propensity_scores <- ps_matrix[cbind(1:nrow(data), as.numeric(actual_x_levels))]
  
  #--- 4. 가중치(Weights) 계산 ---
  data$weight <- 1 / propensity_scores
  
  if (stabilized) {
    message("안정화된 가중치(Stabilized Weights)를 계산합니다.")
    # x_var의 한계 확률 계산 (가중치 분자)
    marginal_probs <- prop.table(table(data[[x_var]]))
    
    # 각 개인의 실제 x_var 수준에 해당하는 한계 확률을 가져옴
    p_marginal <- marginal_probs[data[[x_var]]]
    
    # 안정화된 가중치 = (한계 확률) / (성향 점수)
    data$weight <- p_marginal / propensity_scores
  }
  
  #--- 5. 가중치를 적용하여 조정된 빈도 계산 ---
  ipw_freq_df <- data %>%
    group_by(.data[[x_var]], .data[[y_var]]) %>%
    summarise(ipw_freq = sum(weight), .groups = 'drop')
  
  #--- 6. 플롯 생성 ---
  if(proportion){
    
    ipw_freq_df <- ipw_freq_df %>%
      # 각 x_var 그룹 내에서 ipw_freq의 합으로 나누어 비율(proportion)을 계산
      group_by(.data[[x_var]]) %>%
      mutate(
        proportion = ipw_freq / sum(ipw_freq),
        # 비율에 100을 곱해 정규화된 빈도(%)를 만듦
        ipw_freq = proportion * 100
      ) %>%
      ungroup()
    
  }
  
  
  plot <- ggplot(data = ipw_freq_df,
                 aes(axis1 = .data[[x_var]], 
                     axis2 = .data[[y_var]], 
                     y = ipw_freq)) +
    geom_alluvium(aes(fill = .data[[x_var]]), alpha = 0.7, width = 1/3) +
    geom_stratum(width = 1/3) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
    scale_x_discrete(limits = c(x_var, y_var), expand = c(0.15, 0.05)) +
    labs(
      title = title,
      subtitle = subtitle,
      y = "IPW 조정 빈도 (IPW Adjusted Frequency)",
      fill = x_var
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face="bold", hjust=0.5))
  
  
  
  return(plot)
}












#' @title 동적 치료 결정 규칙(DTR) 분석 함수
#' @description drtmle 패키지를 사용하여 하나 이상의 치료 변수에 대한 
#'              용량-반응 관계(dose-response relationship)를 추정합니다.
#'
#' @param data 분석할 데이터프레임.
#' @param outcome_var 결과 변수 이름 (문자열).
#' @param treatment_vars 분석할 치료(노출) 변수들의 이름 (문자열 벡터).
#' @param confounders 조정할 공변량들의 이름 (문자열 벡터).
#' @param show_plot 분석 결과를 그래프로 바로 출력할지 여부 (논리값). 기본값은 TRUE.
#' @param ... drtmle() 함수에 전달할 추가 인자들. 
#'            예: n_bins=3, sl_library_g=c("SL.glm"), sl_library_Q=c("SL.glm"), family=binomial() 등
#'
#' @export analyze_dtr
analyze_dtr <- function(data, 
                        outcome_var, 
                        treatment_vars, 
                        confounders, 
                        family,
                        show_plot = TRUE,
                        sl_library_g = c("SL.glm", "SL.mean", "SL.npreg"),
                        sl_library_Q = c("SL.glm", "SL.mean", "SL.npreg"),
                        ...) {
  
  # family = binomial(),  # binomial(), gaussian(), poisson()
  # SL_g = sl_library_g,  # Propensity Score (PS) model
  # SL_Q = sl_library_Q,  # Outcome Regression (OR) model
  # SL_gr = sl_library_g,  # Reduced-dimension PS (often same as SL_g)
  # SL_Qr = sl_library_Q,  # Reduced-dimension OR (often same as SL_Q)
  # stratify = FALSE,  # Fit a single OR for all A levels if FALSE, otherwise stratified
  # a_0 = a_0_levels  # Estimate marginal means for all discretized treatment levels
  
  
  # install.packages(c("drtmle", "SuperLearner"))
  library(drtmle)
  library(tidyverse)
  
  #--- 1. 입력값 검증 ---
  required_vars <- c(outcome_var, treatment_vars, confounders)
  if (!all(required_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  #--- 2. 각 치료 변수에 대해 분석을 반복 수행 ---
  all_results <- lapply(treatment_vars, function(treat_var) {
    
    cat(paste0("\n--- '", treat_var, "'에 대한 DTR 분석 시작 ---\n"))
    
    #--- 3. 현재 분석에 필요한 데이터 준비 ---
    analysis_cols <- c(outcome_var, treat_var, confounders)
    analysis_data <- data %>%
      select(all_of(analysis_cols)) %>%
      drop_na() # 분석에 필요한 변수에 결측치가 있는 행 제거
    
    if (nrow(analysis_data) == 0) {
      warning(paste("'", treat_var, "' 분석에 사용할 데이터가 없습니다 (결측치 제거 후). 다음 변수로 넘어갑니다."))
      return(NULL)
    }
    
    #--- 4. drtmle 함수 호출 ---
    # tryCatch를 사용하여 분석 중 오류가 발생해도 전체가 멈추지 않도록 함
    fit <- tryCatch({
      drtmle(
        W = analysis_data[, confounders, drop = FALSE], # 공변량
        A = analysis_data[[treat_var]],                 # 치료 변수
        Y = analysis_data[[outcome_var]],               # 결과 변수
        family = binomial(),
        SL_g = sl_library_g, # Propensity Score (PS) model
        SL_Q = sl_library_Q, # Outcome Regression (OR) model
        SL_gr = sl_library_g, # Reduced-dimension PS (often same as SL_g)
        SL_Qr = sl_library_Q, # Reduced-dimension OR (often same as SL_Q)
        stratify = FALSE, # Fit a single OR for all A levels if FALSE, otherwise stratified
        a_0 = a_0_levels, # Estimate marginal means for all discretized treatment levels
        ...                                             # 추가 인자 전달
      )
    }, error = function(e) {
      warning(paste("'", treat_var, "' 분석 중 오류 발생:", e$message))
      return(NULL)
    })
    
    if (is.null(fit)) return(NULL)
    
    #--- 5. 결과 정리 및 시각화 ---
    if (show_plot) {
      plot(fit, main = paste("Dose-Response Curve for", treat_var))
    }
    
    cat(paste0("--- '", treat_var, "' 분석 결과 요약 ---\n"))
    results_df <- as.data.frame(fit$results)
    print(results_df)
    
    return(
      list(
        treatment_variable = treat_var,
        drtmle_fit = fit,
        summary = results_df,
        ci = ci(fit)
      )
    )
  })
  
  # 리스트 이름 설정
  names(all_results) <- treatment_vars
  
  return(all_results)
}

























#' @title 1단계: 기초 통계 및 개별 회귀분석
#' @description Table 1과 각 주요 변수(X)에 대한 개별 GLM을 수행합니다.
#' 
#' 
#' @export analyze_univariate_glm
analyze_univariate_glm <- function(data, Y, X, Z = NULL, V = NULL, family = "auto", exponentiate = NULL) {
  
  # ... (이전과 동일한 코드) ...
  library(tidyverse)
  library(gtsummary)
  library(tableone)
  library(rlang)
  library(broom)
  
  # ... (입력값 검증 및 데이터 준비 코드) ...
  if (!is.data.frame(data)) stop("`data`는 반드시 데이터프레임이어야 합니다.")
  all_vars <- c(Y, X, Z)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다: ",
         paste(all_vars[!all_vars %in% names(data)], collapse = ", "))
  }
  
  if (!is.null(V)) {
    cat(paste("--- 데이터 필터링 적용:", V, "---\n\n"))
    filtered_data <- data %>%
      filter(!!rlang::parse_expr(V))
    if(nrow(filtered_data) == 0) stop("필터링 조건에 맞는 데이터가 없습니다.")
  } else {
    filtered_data <- data
  }
  
  filtered_data <- filtered_data %>% select(all_of(all_vars)) %>% drop_na()
  
  cat("--- 1. 데이터 요약 테이블 (Table 1) ---\n")
  cat(paste("종속변수 '", Y, "'에 따라 그룹화된 요약입니다.\n\n", sep=""))
  
  is_binomial <- FALSE
  if (n_distinct(filtered_data[[Y]]) == 2) {
    is_binomial <- TRUE
    y_levels <- sort(unique(filtered_data[[Y]]))
    filtered_data <- filtered_data %>%
      mutate(!!Y := factor(!!sym(Y), levels = y_levels))
  }
  
  vars_to_summarize <- c(X, Z)
  summary_table <- tableone::CreateTableOne(
    vars = vars_to_summarize,
    strata = Y,
    data = filtered_data,
    test = TRUE,
    smd = TRUE
  )
  print(summary_table, smd = TRUE, varLabels = TRUE)
  cat("\n\n")
  
  if (family == "auto") {
    model_type <- if (is_binomial) "binomial" else "gaussian"
  } else {
    model_type <- family
  }
  
  if (is.null(exponentiate)) {
    exponentiate <- (model_type == "binomial")
  }
  
  cat(paste("--- 2. 개별(Univariate) GLM 결과 (Family:", model_type, ") ---\n"))
  
  target_level <- NULL
  if (model_type == "binomial") {
    target_level <- levels(filtered_data[[Y]])[2]
    cat(paste0("Info: 결과변수(Y) '", Y, "'의 '", target_level, "' 수준에 대한 확률을 모델링합니다.\n\n"))
  } else {
    cat("\n")
  }
  
  uni_models <- list()
  tbl_regressions <- list()
  
  for (x_var in X) {
    formula_uni_str <- paste(Y, "~", paste(c(x_var, Z), collapse = " + "))
    model_uni <- glm(as.formula(formula_uni_str), data = filtered_data, family = model_type)
    uni_models[[x_var]] <- model_uni
    
    # tbl_regression 생성 시, 해당 변수(x_var)만 포함하도록 수정
    tbl_regressions[[x_var]] <- model_uni %>%
      tbl_regression(exponentiate = exponentiate, include = all_of(x_var))
  }
  
  # 개별 분석 결과들을 하나의 테이블로 병합하여 출력
  uni_results_table <- tbl_stack(
    tbls = tbl_regressions,
    group_header = NULL # 각 변수를 그룹으로 묶지 않음
  )
  
  print(uni_results_table)
  cat("\n\n")
  
  # --- 결과물 반환 ---
  invisible(list(
    parameters = list(
      Y = Y, X = X, Z = Z, V = V, model_type = model_type,
      exponentiate = exponentiate, target_level = target_level
    ),
    filtered_data = filtered_data,
    summary_table = summary_table,
    univariate_models = uni_models,
    univariate_tbl_list = tbl_regressions # <<< 이 부분 추가됨
  ))
}






#' @title 2단계: 다중 회귀 모델 분석 및 정제 (Univariate 결과 통합 기능 추가)
#' @description 데이터를 받아 다중 모델을 생성 및 정제하고, 선택적으로
#'              Univariate 분석 결과를 통합하여 비교 테이블을 생성합니다.
#' @param univariate_results `analyze_univariate_glm`의 결과 객체 (optional).
#'                           제공되면 비교 테이블에 개별 분석 결과가 포함됩니다.
#'                           
#' @export analyze_multivariate_glm
analyze_multivariate_glm <- function(data, Y, X, Z = NULL, V = NULL,
                                     family = "auto", exponentiate = NULL,
                                     selection_method = "stepwise",
                                     selection_alpha = 0.1,
                                     keep_vars = NULL,
                                     univariate_results = NULL) { # <<< 매개변수 추가
  
  # ... (이전과 동일한 라이브러리 로드, 데이터 준비, 모델 생성, 변수 선택 코드) ...
  library(tidyverse)
  library(gtsummary)
  library(broom)
  library(rlang)
  
  if (!is.data.frame(data)) stop("`data`는 반드시 데이터프레임이어야 합니다.")
  all_vars <- c(Y, X, Z)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다: ",
         paste(all_vars[!all_vars %in% names(data)], collapse = ", "))
  }
  
  if (!is.null(V)) {
    filtered_data <- data %>% filter(!!rlang::parse_expr(V))
    if(nrow(filtered_data) == 0) stop("필터링 조건에 맞는 데이터가 없습니다.")
  } else {
    filtered_data <- data
  }
  
  filtered_data <- filtered_data %>% select(all_of(all_vars)) %>% drop_na()
  
  is_binomial <- FALSE
  if (n_distinct(filtered_data[[Y]]) == 2) {
    is_binomial <- TRUE
    y_levels <- sort(unique(filtered_data[[Y]]))
    filtered_data <- filtered_data %>%
      mutate(!!Y := factor(!!sym(Y), levels = y_levels))
  }
  
  if (family == "auto") model_type <- if (is_binomial) "binomial" else "gaussian"
  else model_type <- family
  
  if (is.null(exponentiate)) exponentiate <- (model_type == "binomial")
  
  formula_multi_str <- paste(Y, "~", paste(c(X, Z), collapse = " + "))
  model_multi <- glm(as.formula(formula_multi_str), data = filtered_data, family = model_type)
  
  # ... (변수 선택 로직: stepwise, backward, fdr - 이전과 동일) ...
  model_final <- NULL
  
  if (selection_method == "stepwise") {
    scope_formula <- if (!is.null(keep_vars) || !is.null(Z)) {
      as.formula(paste("~", paste(c(keep_vars, Z), collapse = " + ")))
    } else {
      as.formula("~ 1")
    }
    model_multi$call$data <- quote(filtered_data)
    model_final <- step(model_multi, direction = "backward", scope = list(lower = scope_formula, upper = . ~ .), trace = 0)
    
  } else if (selection_method == "backward") {
    current_model <- model_multi
    while(TRUE) {
      model_summary <- broom::tidy(current_model)
      all_model_vars <- attr(current_model$terms, "term.labels")
      removable_vars <- setdiff(all_model_vars, c(keep_vars, Z))
      if (length(removable_vars) == 0) break
      
      p_values_to_check <- model_summary %>%
        filter(term %in% unlist(sapply(removable_vars, function(v) model_summary$term[str_starts(model_summary$term, v)])))
      if (nrow(p_values_to_check) == 0) break
      
      max_p_row <- p_values_to_check %>% slice_max(p.value, n = 1, with_ties = FALSE)
      if (max_p_row$p.value > selection_alpha) {
        var_to_remove_term <- max_p_row$term
        base_var_to_remove <- removable_vars[sapply(removable_vars, function(p) str_starts(var_to_remove_term, p))]
        new_predictors <- setdiff(all_model_vars, base_var_to_remove)
        formula_new_str <- if(length(new_predictors) > 0) paste(Y, "~", paste(new_predictors, collapse = " + ")) else paste(Y, "~ 1")
        current_model <- glm(as.formula(formula_new_str), data = filtered_data, family = model_type)
      } else {
        break
      }
    }
    model_final <- current_model
    
  } else if (selection_method == "fdr") {
    # ... (fdr 로직) ...
  } else {
    stop("지원되지 않는 변수 선택 방법입니다.")
  }
  
  
  # --- 결과 요약 및 비교 테이블 생성 (수정된 부분) ---
  cat("--- 최종 모델과 전체 모델 (+개별 모델) 비교 ---\n")
  
  tbl_full_multi <- tbl_regression(model_multi, exponentiate = exponentiate)
  tbl_final <- tbl_regression(model_final, exponentiate = exponentiate)
  
  # univariate_results가 제공되었는지에 따라 테이블 구성을 다르게 함
  if (!is.null(univariate_results) && !is.null(univariate_results$univariate_tbl_list)) {
    # 1. 개별 분석 결과들을 하나의 테이블로 쌓음 (stack)
    tbl_uni_stacked <- tbl_stack(
      tbls = univariate_results$univariate_tbl_list,
      group_header = NULL
    )
    
    # 2. 세 종류의 테이블을 병합 (merge)
    comparison_table <- tbl_merge(
      tbls = list(tbl_uni_stacked, tbl_full_multi, tbl_final),
      tab_spanner = c("**개별 분석**", "**초기 다중 모델**", paste0("**최종 모델 (", selection_method, ")**"))
    )
  } else {
    # univariate_results가 없으면 기존 방식대로 두 테이블만 병합
    comparison_table <- tbl_merge(
      tbls = list(tbl_full_multi, tbl_final),
      tab_spanner = c("**초기 다중 모델**", paste0("**최종 모델 (", selection_method, ")**"))
    )
  }
  
  print(comparison_table)
  
  initial_vars <- all.vars(formula(model_multi))
  final_vars <- all.vars(formula(model_final))
  
  # --- 결과물 반환 ---
  invisible(list(
    initial_full_model = model_multi,
    final_model = model_final,
    comparison_table = comparison_table,
    final_variables = final_vars,
    removed_variables = setdiff(initial_vars, final_vars)
  ))
}












#' @title 상호작용 효과 시각화
#' @description 지정된 두 변수 간의 상호작용을 GLM 모델에 적합하고,
#'              'ggeffects' 패키지를 사용하여 결과를 시각화합니다.
#' @param data 데이터프레임
#' @param Y 종속변수 이름
#' @param x_var1 주 효과를 볼 변수 (x축에 해당)
#' @param x_var2 조건(그룹)이 될 변수 (선의 색상/종류로 구분)
#' @param Z 모델에 포함될 다른 공변량 벡터 (optional)
#' @param family GLM family. 기본값은 "auto".
#' @param x2_values x_var2가 연속형 변수일 때, 어느 지점에서 값을 계산할지 지정.
#'                  "auto" (기본값)는 평균 및 평균±1SD, "quartiles"는 사분위수를 사용합니다.
#'                  또는 직접 `c(10, 20, 30)`과 같이 값을 지정할 수 있습니다.
#' @export plot_interaction
plot_interaction <- function(data, Y, x_var1, x_var2, Z = NULL, family = "auto", x2_values = "auto") {
  if(FALSE){
    data=df_alive_nonNA
    Y="is_frail_discharge"
    x_var1="time_to_antibiotics"
    x_var2="is_frail_admission"
    Z=covariates
    family="binomial"
  }  
  
  # --- 1. 라이브러리 로드 ---
  library(tidyverse)
  library(ggeffects)
  library(rlang)
  
  # --- 2. 입력값 검증 및 데이터 준비 ---
  all_vars <- c(Y, x_var1, x_var2, Z)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  # 상호작용 분석을 위해 결측치 제거
  filtered_data <- data %>%
    select(all_of(all_vars)) %>%
    drop_na() %>% 
    filter(!is.infinite(get(x_var1))) %>% 
    filter(!is.infinite(get(x_var2)))
  
  
  is_binomial <- FALSE
  if (n_distinct(filtered_data[[Y]]) == 2) {
    is_binomial <- TRUE
    y_levels <- sort(unique(filtered_data[[Y]]))
    filtered_data <- filtered_data %>%
      mutate(!!Y := factor(!!sym(Y), levels = y_levels))
  }
  
  if (family == "auto") {
    model_type <- if (is_binomial) "binomial" else "gaussian"
  } else {
    model_type <- family
  }
  
  # --- 3. 상호작용 모델 생성 ---
  # 포뮬라 생성: Y ~ x_var1 * x_var2 + Z1 + Z2 + ...
  interaction_formula_str <- paste(
    Y, "~",
    paste(c(paste(x_var1, "*", x_var2), Z), collapse = " + ")
  )
  interaction_model <- glm(as.formula(interaction_formula_str), 
                           data = filtered_data, 
                           family = model_type)
  
  # --- 4. ggeffects를 이용한 예측값 계산 ---
  # x_var2의 종류에 따라 terms 설정
  terms <- if (is.numeric(filtered_data[[x_var2]]) && length(x2_values) > 0) {
    paste0(x_var1, " [all]", ", ", x_var2, " [", paste(x2_values, collapse = ","), "]")
  } else {
    c(x_var1, x_var2)
  }
  
  # ggpredict로 예측값 및 신뢰구간 계산
  pred_effects <- ggpredict(interaction_model, terms = terms)
  
  # --- 5. ggplot을 이용한 시각화 ---
  plot_title <- paste("Interaction Plot of", x_var1, "and", x_var2, "on", Y)
  y_lab <- if(is_binomial) paste("Predicted Pr(", Y, "=", y_levels[2], ")") else paste("Predicted", Y)
  
  interaction_plot <- plot(pred_effects) +
    labs(
      title = plot_title,
      y = y_lab,
      x = x_var1,
      colour = x_var2,
      fill = x_var2,
      linetype = x_var2
    ) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
  return(interaction_plot)
}













#' @title 상호작용 효과 탐색
#' @description 특정 주요 변수와 다른 후보 변수들 간의 상호작용 유의성을
#'              체계적으로 검정하고 결과를 요약합니다.
#' @param data 데이터프레임
#' @param Y 종속변수
#' @param primary_var 상호작용을 검정할 중심 변수
#' @param interaction_candidates `primary_var`와 상호작용을 검정할 후보 변수 목록
#' @param Z 모델에 항상 포함될 보정 변수 (optional)
#' @param family GLM family. 기본값은 "auto".
#' @param test_method 유의성 검정 방법. "LRT"(기본값) 또는 "p.value".
#' @param p_threshold 유의성 판단 기준 p-value.
#' @export explore_interactions
explore_interactions <- function(data, Y, primary_var, interaction_candidates, Z = NULL, 
                                 family = "auto", test_method = "LRT", p_threshold = 0.1) {
  
  # --- 1. 라이브러리 로드 ---
  library(tidyverse)
  library(broom)
  library(rlang)
  library(knitr) # for kable()
  
  # --- 2. 입력값 검증 및 데이터 준비 ---
  all_vars <- c(Y, primary_var, interaction_candidates, Z)
  if (!all(all_vars %in% names(data))) {
    stop("입력된 변수 중 일부가 데이터에 존재하지 않습니다.")
  }
  
  # primary_var는 후보 목록에서 제외
  interaction_candidates <- setdiff(interaction_candidates, primary_var)
  
  filtered_data <- data %>%
    select(all_of(all_vars)) %>%
    drop_na() %>% 
    filter(!is.infinite(get(primary_var))) %>% 
    filter(!is.infinite(get(x_var2)))
  
  is_binomial <- (n_distinct(filtered_data[[Y]]) == 2)
  if (is_binomial) {
    y_levels <- sort(unique(filtered_data[[Y]]))
    filtered_data <- filtered_data %>%
      mutate(!!Y := factor(!!sym(Y), levels = y_levels))
  }
  
  if (family == "auto") {
    model_type <- if (is_binomial) "binomial" else "gaussian"
  } else {
    model_type <- family
  }
  
  cat(paste0("--- '", primary_var, "' 변수와의 상호작용 탐색 시작 ---\n"))
  cat(paste("유의성 검정 방법:", test_method, "| 유의수준(p-threshold):", p_threshold, "\n\n"))
  
  # --- 3. 각 후보 변수와 상호작용 검정 (purrr::map_dfr 사용) ---
  results_df <- map_dfr(interaction_candidates, ~{
    candidate_var <- .x
    
    # 모델 포뮬라 생성
    base_terms <- paste(c(primary_var, candidate_var, Z), collapse = " + ")
    
    # LRT 방법
    if (test_method == "LRT") {
      model_small_formula <- as.formula(paste(Y, "~", base_terms))
      model_big_formula <- as.formula(paste(Y, "~", paste0(primary_var, "*", candidate_var), "+", paste(Z, collapse = " + ")))
      
      model_small <- glm(model_small_formula, data = filtered_data, family = model_type)
      model_big <- glm(model_big_formula, data = filtered_data, family = model_type)
      
      lrt_result <- anova(model_small, model_big, test = "LRT")
      p_val <- lrt_result$`Pr(>Chi)`[2]
      
      tibble(
        interaction_term = paste(primary_var, ":", candidate_var),
        p.value = p_val
      )
    } 
    # p.value (Wald Test) 방법
    else if (test_method == "p.value") {
      model_big_formula <- as.formula(paste(Y, "~", paste0(primary_var, "*", candidate_var), "+", paste(Z, collapse = " + ")))
      model_big <- glm(model_big_formula, data = filtered_data, family = model_type)
      
      # 상호작용 항의 p-value 추출 (여러 개일 경우 최소값 선택)
      p_val <- broom::tidy(model_big) %>%
        filter(str_detect(term, paste0("^", primary_var, ":", "|", ":", primary_var))) %>%
        pull(p.value) %>%
        min()
      
      tibble(
        interaction_term = paste(primary_var, ":", candidate_var),
        p.value = p_val
      )
    }
  })
  
  # --- 4. 결과 정리 및 출력 ---
  final_results <- results_df %>%
    arrange(p.value) %>%
    mutate(
      significant = ifelse(p.value < p_threshold, "Yes", "No"),
      p.value = scales::pvalue(p.value, accuracy = .001, add_p = TRUE)
    )
  
  cat("검정 결과 (p-value가 낮은 순서):\n")
  print(kable(final_results, format = "pipe", align = 'lrr'))
  
  invisible(final_results)
}





#' @title 하위그룹별 상호작용 분석 및 시각화
#' @description 특정 범주형 변수(strata_var)의 각 그룹(수준)별로 데이터를 나누어,
#'              지정된 상호작용(interaction_vars)의 유의성을 검정하고,
#'              유의한 경우에만 상호작용 플랏을 그립니다.
#' @param data 데이터프레임
#' @param Y 종속변수
#' @param interaction_vars 상호작용을 검정할 두 변수의 이름 벡터. 예: `c("age", "treatment")`
#' @param strata_var 데이터를 나눌 기준이 되는 범주형 변수 이름.
#' @param Z 모델에 항상 포함될 보정 변수 (optional)
#' @param family GLM family. 기본값은 "auto".
#' @param p_threshold 상호작용의 유의성 판단 기준 p-value. 기본값 0.1
#' @param min_subgroup_size 모델링을 위한 최소 하위그룹 크기. 기본값 30.
#' @return 각 하위그룹의 p-value와 (유의한 경우) ggplot 객체를 담은 리스트
#' @export plot_interactions_for_each_level
plot_interactions_for_each_level <- function(data, Y, interaction_vars, strata_var, Z = NULL, 
                                             family = "auto", p_threshold = 0.1, min_subgroup_size = 30) {
  
  
  data <- data %>%
    filter(if_all(all_of(interaction_vars), ~ !is.infinite(.)))
  
  
  
  # --- 1. 라이브러리 및 함수 의존성 확인 ---
  library(tidyverse)
  library(broom)
  library(rlang)
  if (!exists("plot_interaction")) {
    stop("이 함수를 실행하려면 'plot_interaction' 함수가 필요합니다.")
  }
  
  # --- 2. 입력값 검증 ---
  if(length(interaction_vars) != 2) stop("`interaction_vars`는 반드시 두 개의 변수 이름을 포함해야 합니다.")
  if(!is.factor(data[[strata_var]]) && !is.character(data[[strata_var]])) {
    stop("`strata_var`는 범주형 변수(factor or character)여야 합니다.")
  }
  
  strata_levels <- unique(na.omit(data[[strata_var]]))
  results_list <- list()
  
  cat(paste0("===== Subgroup Interaction Analysis based on '", strata_var, "' =====\n\n"))
  
  # --- 3. 각 그룹(수준)별로 반복 작업 ---
  for (level in strata_levels) {
    cat(paste0("--- Analyzing Subgroup: ", strata_var, " = ", level, " ---\n"))
    
    # 3.1. 하위그룹 데이터 생성
    subset_data <- data %>% filter(!!sym(strata_var) == level)
    
    if (nrow(subset_data) < min_subgroup_size) {
      cat(paste0("Warning: '", level, "' 그룹의 데이터가 ", nrow(subset_data),
                 "개로 너무 적어 분석을 건너뜁니다 (최소 ", min_subgroup_size, "개 필요).\n\n"))
      results_list[[as.character(level)]] <- list(p_value = NA, message = "Subgroup too small", plot = NULL)
      next
    }
    
    # 3.2. 하위그룹 내 상호작용 유의성 검정 (LRT)
    p_val <- NA
    tryCatch({
      var1 <- interaction_vars[1]
      var2 <- interaction_vars[2]
      
      # 모델 포뮬라
      model_small_formula <- as.formula(paste(Y, "~", var1, "+", var2, "+", paste(Z, collapse = " + ")))
      model_big_formula   <- as.formula(paste(Y, "~", var1, "*", var2, "+", paste(Z, collapse = " + ")))
      
      # 모델 적합
      model_type <- if (n_distinct(subset_data[[Y]]) == 2) "binomial" else "gaussian" # subset 기준으로 결정
      model_small <- glm(model_small_formula, data = subset_data, family = model_type)
      model_big   <- glm(model_big_formula, data = subset_data, family = model_type)
      
      # LRT 검정
      lrt_result <- anova(model_small, model_big, test = "LRT")
      p_val <- lrt_result$`Pr(>Chi)`[2]
      
      results_list[[as.character(level)]] <- list(p_value = p_val, plot = NULL)
      
    }, error = function(e) {
      cat(paste("Error during model fitting or LRT:", e$message, "\n\n"))
      results_list[[as.character(level)]] <- list(p_value = NA, message = e$message, plot = NULL)
    })
    
    if(is.na(p_val)) next
    
    # 3.3. 조건부 시각화
    if (p_val < p_threshold) {
      cat(paste0("Result: Interaction is SIGNIFICANT (p = ", round(p_val, 3), "). Generating plot...\n\n"))
      
      # plot_interaction 함수를 재사용하여 시각화
      interaction_plot <- plot_interaction(
        data = subset_data,
        Y = Y,
        x_var1 = interaction_vars[1],
        x_var2 = interaction_vars[2],
        Z = Z,
        family = family
      ) + 
        labs(subtitle = paste("Subgroup:", strata_var, "=", level)) # 부제 추가
      
      print(interaction_plot) # 플랏 출력
      results_list[[as.character(level)]]$plot <- interaction_plot # 결과 리스트에 저장
      
    } else {
      cat(paste0("Result: Interaction is NOT significant (p = ", round(p_val, 3), ").\n\n"))
    }
  }
  
  invisible(results_list)
}















#' @title Compositional Stacked Bar Plot 생성 함수
#' @param data 분석할 데이터프레임
#' @param time_var 시간 정보를 담은 연속형 변수 이름 (따옴표 없이)
#' @param comp_vars 조성 비율 정보를 담은 열 이름들의 벡터 (character vector)
#' @param n_bins 나눌 시간 구간의 수. 기본값은 3.
#' @param title 그래프 제목
#' @export create_compositional_barplot
create_compositional_barplot <- function(data, time_var, comp_vars, n_bins = 3, title = "Compositional Change by Time") {
  
  # 필요한 라이브러리 로드
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  
  
  plot_data <- data %>%
    # 1. 연속형 변수를 지정된 수의 구간으로 나눔
    mutate(time_bin = cut( get(time_var), breaks = n_bins, include.lowest = TRUE)) %>%
    # 2. Wide 포맷에서 Long 포맷으로 데이터 재구조화
    pivot_longer(
      cols = all_of(comp_vars),
      names_to = "composition_type",
      values_to = "proportion"
    ) %>%
    # 3. 각 시간 구간 및 조성 타입별 평균 비율 계산
    group_by(time_bin, composition_type) %>%
    summarise(mean_proportion = mean(proportion), .groups = 'drop')
  
  # 4. ggplot을 사용하여 그래프 생성
  p <- ggplot(plot_data, aes(x = time_bin, y = mean_proportion, fill = composition_type)) +
    geom_col(position = "fill") +
    labs(
      title = title,
      x = "Time Group",
      y = "Proportion",
      fill = "Composition Type"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  
  return(p)
}














#' @export auto_eda_2
auto_eda_2 <- function(data, var1, var2) {
  # Tidy evaluation을 위해 변수 이름을 quosure로 변환
  var1_enquo <- enquo(var1)
  var2_enquo <- enquo(var2)
  
  # 데이터프레임에서 해당 변수 추출
  df <- data %>% select(!!var1_enquo, !!var2_enquo)
  
  # 변수 타입 확인
  type1 <- class(df[[1]])
  type2 <- class(df[[2]])
  
  # Case 1: 수치형 vs 수치형
  if (is.numeric(df[[1]]) && is.numeric(df[[2]])) {
    cat("### 분석 타입: 수치형 vs 수치형 ###\n\n")
    
    # 통계 요약: 상관계수
    correlation <- cor(df[[1]], df[[2]], use = "complete.obs")
    cat(paste0("피어슨 상관계수 (Pearson Correlation): ", round(correlation, 4), "\n"))
    cat("상관계수는 두 변수 간의 선형 관계의 강도와 방향을 나타냅니다. (-1 ~ 1)\n\n")
    
    # 시각화: 산점도와 회귀선
    p <- ggplot(df, aes(x = !!var1_enquo, y = !!var2_enquo)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", col = "red", se = FALSE) +
      labs(title = paste("산점도:", quo_name(var1_enquo), "vs", quo_name(var2_enquo)),
           subtitle = paste("상관계수 (r) =", round(correlation, 2)),
           x = quo_name(var1_enquo),
           y = quo_name(var2_enquo)) +
      theme_minimal()
    print(p)
    
    # Case 2: 수치형 vs 범주형 (순서 무관)
  } else if ((is.numeric(df[[1]]) && (is.character(df[[2]]) || is.factor(df[[2]]))) ||
             ((is.character(df[[1]]) || is.factor(df[[1]])) && is.numeric(df[[2]]))) {
    
    cat("### 분석 타입: 수치형 vs 범주형 ###\n\n")
    
    # 변수 역할을 명확히 함 (num_var, cat_var)
    if(is.numeric(df[[1]])) {
      num_var_enquo <- var1_enquo
      cat_var_enquo <- var2_enquo
    } else {
      num_var_enquo <- var2_enquo
      cat_var_enquo <- var1_enquo
    }
    
    # 통계 요약: 그룹별 기술 통계
    summary_stats <- data %>%
      group_by(!!cat_var_enquo) %>%
      summarise(
        count = n(),
        mean = mean(!!num_var_enquo, na.rm = TRUE),
        sd = sd(!!num_var_enquo, na.rm = TRUE),
        median = median(!!num_var_enquo, na.rm = TRUE)
      )
    cat("범주별 수치형 변수 요약:\n")
    print(summary_stats)
    
    # 시각화: 박스플롯
    p <- ggplot(data, aes(x = !!cat_var_enquo, y = !!num_var_enquo, fill = !!cat_var_enquo)) +
      geom_boxplot() +
      labs(title = paste("박스플롯:", quo_name(num_var_enquo), "by", quo_name(cat_var_enquo)),
           x = quo_name(cat_var_enquo),
           y = quo_name(num_var_enquo)) +
      theme_minimal() +
      theme(legend.position = "none")
    print(p)
    
    # Case 3: 범주형 vs 범주형
  } else if ((is.character(df[[1]]) || is.factor(df[[1]])) && (is.character(df[[2]]) || is.factor(df[[2]]))) {
    cat("### 분석 타입: 범주형 vs 범주형 ###\n\n")
    
    # 통계 요약: 교차표 (Contingency Table)
    contingency_table <- table(df[[1]], df[[2]])
    cat("교차표:\n")
    print(contingency_table)
    
    # 카이제곱 검정
    chi_sq_test <- chisq.test(contingency_table)
    cat("\n카이제곱 검정 (Chi-squared Test):\n")
    cat("두 변수가 서로 독립적인지 검정합니다.\n")
    print(chi_sq_test)
    
    # 시각화: 누적 막대 그래프
    p <- ggplot(df, aes(x = !!var1_enquo, fill = !!var2_enquo)) +
      geom_bar(position = "fill") +
      labs(title = paste("비율 막대 그래프:", quo_name(var1_enquo), "vs", quo_name(var2_enquo)),
           x = quo_name(var1_enquo),
           y = "비율",
           fill = quo_name(var2_enquo)) +
      theme_minimal()
    print(p)
    
  } else {
    cat("지원되지 않는 변수 타입 조합입니다.\n")
  }
}


#' @export auto_eda_3
auto_eda_3 <- function(data, var1, var2, var3) {
  # 변수 이름들을 quosure 리스트로 캡처
  vars_enquo <- enquos(var1, var2, var3)
  
  # 데이터프레임에서 해당 변수들 선택
  df <- data %>% select(!!!vars_enquo)
  
  # 변수 타입 확인
  types <- sapply(df, class)
  num_count <- sum(sapply(df, is.numeric))
  cat_count <- sum(sapply(df, function(x) is.character(x) || is.factor(x)))
  
  # Case 1: 수치형 2개, 범주형 1개
  if (num_count == 2 && cat_count == 1) {
    cat("### 분석 타입: 수치형(2) vs 범주형(1) ###\n\n")
    
    # 변수 역할 할당
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_var  <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    cat(paste0("범주형 변수 '", cat_var, "'의 각 수준에 따라 두 수치형 변수('", 
               num_vars[1], "', '", num_vars[2], "')의 관계를 분석합니다.\n\n"))
    
    # 통계 요약: 그룹별 상관계수
    grouped_corr <- data %>%
      group_by(!!sym(cat_var)) %>%
      summarise(correlation = cor(!!sym(num_vars[1]), !!sym(num_vars[2]), use = "complete.obs"))
    
    cat("범주별 상관계수 요약:\n")
    print(grouped_corr)
    
    # 시각화: 색상으로 구분된 산점도
    p <- ggplot(data, aes(x = !!sym(num_vars[1]), y = !!sym(num_vars[2]), color = !!sym(cat_var))) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) + # 그룹별 회귀선 추가
      labs(title = paste(num_vars[1], "vs", num_vars[2], "by", cat_var),
           x = num_vars[1], y = num_vars[2], color = cat_var) +
      theme_minimal()
    print(p)
    
    # Case 2: 수치형 1개, 범주형 2개
  } else if (num_count == 1 && cat_count == 2) {
    cat("### 분석 타입: 수치형(1) vs 범주형(2) ###\n\n")
    
    # 변수 역할 할당
    num_var  <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    cat(paste0("두 범주형 변수('", cat_vars[1], "', '", cat_vars[2], 
               "')의 조합에 따른 수치형 변수('", num_var, "')의 분포를 분석합니다.\n\n"))
    
    # 통계 요약: 그룹별 기술 통계
    summary_stats <- data %>%
      group_by(!!sym(cat_vars[1]), !!sym(cat_vars[2])) %>%
      summarise(
        count = n(),
        mean = mean(!!sym(num_var), na.rm = TRUE),
        sd = sd(!!sym(num_var), na.rm = TRUE),
        .groups = "drop"
      )
    cat("그룹별 수치형 변수 요약:\n")
    print(summary_stats)
    
    # 시각화: Faceted Boxplot
    p <- ggplot(data, aes(x = !!sym(cat_vars[1]), y = !!sym(num_var), fill = !!sym(cat_vars[1]))) +
      geom_boxplot() +
      facet_wrap(vars(!!sym(cat_vars[2]))) +
      labs(title = paste(num_var, "분포 by", cat_vars[1], "and", cat_vars[2]),
           x = cat_vars[1], y = num_var) +
      theme_minimal() +
      theme(legend.position = "none")
    print(p)
    
    # Case 3: 수치형 3개
  } else if (num_count == 3) {
    cat("### 분석 타입: 수치형(3) ###\n\n")
    
    num_vars <- names(df)
    cat(paste0("세 수치형 변수('", num_vars[1], "', '", num_vars[2], "', '", num_vars[3],
               "') 간의 관계를 분석합니다.\n\n"))
    
    # 통계 요약: 상관 행렬
    cor_matrix <- cor(df, use = "complete.obs")
    cat("상관 행렬 (Correlation Matrix):\n")
    print(round(cor_matrix, 3))
    
    # 시각화: Pairs Plot (GGally 패키지 추천)
    cat("\n시각화를 위해 GGally::ggpairs() 사용을 강력히 추천합니다.\n")
    cat("여기서는 기본 산점도 행렬을 생성합니다 (한 변수를 크기로 표현).\n")
    
    p <- ggplot(data, aes(x=!!sym(num_vars[1]), y=!!sym(num_vars[2]), size=!!sym(num_vars[3]), color=!!sym(num_vars[3]))) +
      geom_point(alpha=0.6) +
      scale_color_viridis_c() +
      labs(title = paste(num_vars[1], "vs", num_vars[2], ", Size & Color by", num_vars[3]),
           x = num_vars[1], y = num_vars[2]) +
      theme_minimal()
    print(p)
    
  } else {
    cat("지원되지 않는 변수 타입 조합입니다.\n(수치형 2개/범주형 1개, 수치형 1개/범주형 2개, 수치형 3개 조합을 지원합니다.)\n")
  }
}