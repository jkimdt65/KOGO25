knitr::opts_chunk$set(echo = TRUE)
# 1) 2x2 분할표 데이터 생성
# 행: 치료군 (placebo, aspirin)
# 열: 심근경색 발생 여부 (yes, no)
mi <- matrix(c(189, 104,   # placebo 그룹: yes, no
               10845, 10933),  # aspirin 그룹: yes, no
             ncol = 2)

# 분할표 출력
mi

# 2) 행과 열 이름 설정
# 행 이름: treat 그룹
# 열 이름: 심근경색 발생 여부
dimnames(mi) <- list(
  treat = c("placebo", "aspirin"),
  "myocardial infarction" = c("yes", "no")
)

# 이름이 붙은 분할표 출력
mi

# 3) 행 방향으로 합계 계산
# placebo 그룹과 aspirin 그룹 각각의 총합 (yes + no)
margin.table(mi, 1)

# 4) 행별 비율 계산
# placebo 그룹과 aspirin 그룹 각각에서 yes/no 비율
prop.table(mi, 1)
# 1) 2x2 분할표 데이터 생성
# 행: 부모의 안전벨트 착용 여부
# 열: 자녀의 안전벨트 착용 여부
# 값: 관찰된 빈도수
ex1 <- matrix(c(56, 2,   # 부모 buckled: 자녀 buckled, unbuckled
                8, 16),  # 부모 unbuckled: 자녀 buckled, unbuckled
              nrow = 2, byrow = TRUE)  # byrow=TRUE는 한 행씩 채우는 옵션 (생략 가능)

# 2) 행과 열 이름 설정
dimnames(ex1) <- list(
  parent = c("buckled", "unbuckled"),
  child = c("buckled", "unbuckled")
)

# 3) 분할표 출력
ex1

# 4) 전체에 대한 비율 계산
# 표에 있는 모든 셀의 합을 기준으로 각 셀의 비율
prop.table(ex1)
# 1) aspirin 데이터 분할표에 대한 카이제곱 독립성 검정
chisq.test(mi)

# p-value만 추출
res <- chisq.test(mi)   # 검정 결과를 객체로 저장
res$p.value             # p-value만 출력

# 2) 부모-자녀 안전벨트 분할표에 대한 카이제곱 독립성 검정
chisq.test(ex1)

# p-value만 추출
res <- chisq.test(ex1)
res$p.value
# 1) 2x2 분할표 데이터 생성
# Guess: 실험참가자의 예측 (Milk 먼저 vs Tea 먼저)
# Truth: 실제 사실 (Milk 먼저 vs Tea 먼저)
ex2 <- matrix(c(3, 1, 1, 3), nrow = 2, byrow = TRUE)

# 행과 열 이름 설정
dimnames(ex2) <- list(
  Guess = c("Milk", "Tea"),
  Truth = c("Milk", "Tea")
)

# 분할표 확인
ex2

# 2) 피셔의 정확검정 수행 (기본: 양측검정)
fisher.test(ex2)

# 3) 피셔의 정확검정 - 단측검정 (대립가설: 두 변수가 양의 연관성을 가진다는 가설)
fisher.test(ex2, alternative = "greater")
# 1) 선택 가능한 성공 횟수의 범위 (0 ~ 36)
x <- 0:36

# 2) 하이퍼지오메트릭 분포의 확률 계산
# 모집단: 총 326명 중 관심집단 214명
# 표본: 36명 중 x명 성공
p <- choose(214, x) * choose(112, 36 - x) / choose(326, 36)

# 3) 분포 시각화
plot(x, p, col = "red", pch = 16)

# 4) 관측된 성공 횟수 = 30일 때 확률
observed.p <- p[x == 30]
observed.p

# 5) Fisher의 정확검정 p값 = 관측된 값보다 더 극단적인 값들의 누적합
# (동일하거나 더 작은 확률을 갖는 모든 케이스의 합)
exact.P <- sum(p[p <= observed.p])
exact.P

# 6) 동일한 데이터를 2x2 분할표로 만들어 피셔의 정확검정 수행
d <- matrix(c(30, 6, 184, 106), ncol = 2)
fisher.test(d)
# 1) 데이터 벡터: 맥주 마신 병 수
beers <- c(5, 2, 9, 8, 3, 7, 3, 5, 3, 5)

# 2) 혈중알코올농도(BAL)
BAL <- c(0.10, 0.03, 0.19, 0.12, 0.04, 0.095, 0.07, 0.06, 0.02, 0.05)

# 3) beers와 BAL 간의 피어슨 상관계수 검정
cor.test(beers, BAL)