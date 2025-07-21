knitr::opts_chunk$set(echo = TRUE)
library(UsingR)
# x축: -4 ~ 4 범위를 0.05 간격으로 생성
x <- seq(-4, 4, by = 0.05)

# 표준정규분포 밀도값 계산
Std.Normal <- dnorm(x)

# 자유도 2인 t분포 밀도값 계산
t2 <- dt(x, df = 2)

# 자유도 30인 t분포 밀도값 계산
t30 <- dt(x, df = 30)

# 표준정규분포 곡선을 그림 (실선)
plot(x, Std.Normal, type = "l",
     ylim = c(0, 0.5),   # y축 범위
     ylab = "density")   # y축 레이블

# 자유도 2인 t분포 곡선을 그림 (점선)
lines(x, t2, lty = 2)

# 자유도 30인 t분포 곡선을 그림 (점선)
lines(x, t30, lty = 3)

# 범례 추가
legend("topleft",
       lty = 1:3,
       c("Standard normal", "t(2)", "t(30)"))
# 1행 2열로 그래픽 화면을 나눔 (한 화면에 Q-Q plot 2개 배치)
par(mfrow = c(1, 2))

# 표준정규분포에서 난수 100개 생성
r.normal <- rnorm(100)

# 자유도 2인 t분포에서 난수 100개 생성
r.t <- rt(100, df = 2)

# 정규 Q-Q plot: 표준정규분포 난수의 Q-Q plot
qqnorm(r.normal)
qqline(r.normal)  # 기준선 추가

# 정규 Q-Q plot: 자유도 2인 t분포 난수의 Q-Q plot
qqnorm(r.t)
qqline(r.t)       # 기준선 추가
# 샘플 데이터: 무게(ozs) 벡터 생성
ozs <- c(1.95, 1.80, 2.10, 1.82, 1.75, 2.01, 1.83, 1.90)

# 90% 신뢰구간 계산을 위한 t-검정 수행
CI <- t.test(ozs, conf.level = 0.9)

# t.test()의 신뢰구간 출력 (confint()로도 동일)
confint(CI)

# 정규 Q-Q plot: 표본이 정규분포를 따르는지 확인
qqnorm(ozs)

# Q-Q plot에 기준선 추가
qqline(ozs)  # 점들이 직선에 가깝게 배열되면 정규성 가정이 적절함
# 원 데이터: CEO 보수(pay.00) 벡터 생성
pay.00 <- c(110, 12, 2.5, 98, 1017, 540, 54, 4.3, 150, 432)

# 로그 변환 후 Wilcoxon 부호순위검정 (중앙값에 대한 신뢰구간 포함)
ans <- wilcox.test(log(pay.00), conf.int = TRUE, conf.level = 0.9)

# 결과 출력
print(ans) #추가

# confint() 사용: 로그 스케일에서의 신뢰구간을 지수로 역변환(exp)해서 원 스케일로 변환
confint(ans, transform = exp)  # 로그의 역변환으로 해석 가능

# 원자료의 상자그림: CEO 보수 데이터 분포 확인
boxplot(pay.00, xlab = "CEO")

# 로그 변환한 상자그림: 왜도(skewness) 완화 후 분포 확인
boxplot(log(pay.00), xlab = "log.CEO")
# 연비 데이터: mpg (miles per gallon)
mpg <- c(11.4, 13.1, 14.7, 15.0, 15.5, 15.6, 15.9, 16.0, 16.8)

# 단일 표본 t-검정:
# 귀무가설 H0: 평균 = 17
# 대립가설 H1: 평균 < 17 (왼쪽 단측검정)
t.test(mpg, mu = 17, alt = "less")

# 그래픽 창을 1행 2열로 나누어 boxplot과 Q-Q plot을 한 화면에 표시
par(mfrow = c(1, 2))

# boxplot으로 데이터의 중심위치와 분포 확인
boxplot(mpg)

# 정규 Q-Q plot으로 정규성 가정 시각적으로 확인
qqnorm(mpg)

# Q-Q plot에 기준선 추가
qqline(mpg)
# 내장 데이터셋 salmon.rate 불러오기
data("salmon.rate")

# 데이터 요약 통계 확인
summary(salmon.rate)

# 로그 변환 후 Wilcoxon 부호순위검정:
# 귀무가설 H0: median(log(rate)) = log(0.005)
# 대립가설 H1: median(log(rate)) > log(0.005)
ans <- wilcox.test(log(salmon.rate), mu = log(0.005), alt = "greater")

# 검정 결과 전체 출력
print(ans)

# p-value만 출력
ans$p.value

# 로그 변환 전후 값 상자그림 비교
# 비율을 해석하기 쉽게 *100 하여 퍼센트 단위로 표현
boxplot(
  list(salmon.rate * 100, log(salmon.rate * 100)),
  names = c("rate", "log rate")
)
# 원 데이터: 10으로 나누어 평균 수준을 낮춤
ex4 <- c(104, 121, 147, 147, 140, 145, 146, 149, 160, 168) / 10

# 줄기-잎 그림으로 데이터 분포 확인
stem(ex4)

# 그래픽 창 1행 2열로 나누어 Box plot & Q-Q plot 한 번에 보기
par(mfrow = c(1, 2))

# 상자그림: 이상치와 중심값 시각화
boxplot(ex4, main = "Box plot")

# 정규 Q-Q plot: 정규성 가정 시각적 점검
qqnorm(ex4)
qqline(ex4)

# Shapiro-Wilk 검정으로 정규성 검정
shapiro.test(ex4)

# 그래픽 창 다시 1개로 초기화
par(mfrow = c(1, 1))

# MASS 패키지의 boxcox()로 최적 변환 파라미터 찾기
library(MASS)
bc <- boxcox(ex4 ~ 1, lambda = seq(-6, 6))  # 상수모형(~1)

# 정규성을 가장 잘 만족시키는 lambda 값 찾기
lambda <- bc$x[which.max(bc$y)]
lambda

# Box-Cox 변환: 최적 lambda 사용
bcex4 <- (ex4 ^ lambda - 1) / lambda

# 변환 후 Shapiro-Wilk 정규성 검정 다시 실행
shapiro.test(bcex4)
# prop.test() : 단일 비율(또는 두 비율 이상) 검정 및 신뢰구간 계산
# 466 successes out of 1013 trials, 신뢰수준 95%
ans <- prop.test(466, 1013, conf.level = 0.95)

# prop.test() 결과 객체(ans)에 어떤 정보가 들어있는지 이름 확인
names(ans)

# 신뢰구간(conf.int)만 출력
ans$conf.int
# binom.test() : 단일 비율에 대한 이항 정확검정 (정규근사가 아니라 정확)
# 성공 466건, 시도 1013건, 신뢰수준 95%
ans <- binom.test(466, 1013, conf.level = 0.95)

# binom.test 결과에서 신뢰구간만 바로 출력
ans$conf.int
# prop.test(): 단일 비율 검정 (정규근사)
# x = 5850 successes, n = 50000 trials
# 귀무가설 H0: p = 0.113
# 대립가설 H1: p > 0.113 (오른쪽 단측검정)
ans <- prop.test(x = 5850, n = 50000, p = 0.113, alt = "greater")

# 검정 결과 전체 출력
print(ans)

# p-value만 추출
ans$p.value
# x축 값: 0 ~ 30 구간을 0.05 간격으로 생성
x <- seq(0, 30, by = 0.05)

# 자유도(df) 1인 카이제곱분포의 밀도함수
chis1 <- dchisq(x, df = 1)

# 자유도(df) 5인 카이제곱분포의 밀도함수
chis5 <- dchisq(x, df = 5)

# 자유도(df) 20인 카이제곱분포의 밀도함수
chis20 <- dchisq(x, df = 20)

# 자유도 1인 카이제곱분포 곡선 그리기 (실선)
plot(x, chis1, type = "l",
     ylim = c(0, 1),   # y축 범위 설정
     ylab = "density") # y축 레이블

# 자유도 5인 카이제곱분포 곡선 추가 (점선)
lines(x, chis5, lty = 2)

# 자유도 20인 카이제곱분포 곡선 추가 (점선)
lines(x, chis20, lty = 3)

# 범례 추가
legend("top",
       lty = 1:3,
       c("Chisq(1)", "Chisq(5)", "Chisq(20)"))