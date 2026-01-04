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