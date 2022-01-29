# Fertility rate analysis project

# Analysis of factors affecting fertility rate(출산율에 영향을 미치는 요인 분석)


# 데이터 불러오기
getwd()
setwd("C:/ITWILL/Project/빅데이터 29기_1st project(with R)/data")

# 1993년~2020년 한국 출산율 통계 시각화
korea_fertility <- read.csv('korea_fertility_rate.csv')
plot(korea_fertility$Year, korea_fertility$Fertility_rate, ylim = c(0,2), col='red', type='o', lwd=1,
     main = '한국의 출산율 증감 추이(1993~2020)')

# 데이터셋 불러오기
fertility <- read.csv('fertility_rate.csv')
head(fertility,10)

# 종속변수/독립변수 추출 및 결측치 제거
fertility <- fertility[c(3:6)] # 종속변수: 출산율 / 독립변수: 도시화율, 여성 경제활동인구 비율, 1인당 국민소득 열만 추출
names(fertility) <- c('fertility_rate', 'urbanization_rate', 'female_worker_ratio', 'national_income') # 열 이름 영문으로 수정
fertility <- na.omit(fertility) # 결측치가 존재하는 행 제거
str(fertility) # 'data.frame':	205 obs. of  4 variables:

# 각 변수별 분포, 통계량 확인
library(moments)

# 1) fertility_rate(출산율)
summary(fertility$fertility_rate)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.110   1.680   2.090   2.491   2.900   6.000
hist(fertility$fertility_rate) # 히스토그램 확인 : 왼쪽으로 치우친 형태(주로 1.5~2.5)
skewness(fertility$fertility_rate) # 왜도 = 1.183909 > 0
kurtosis(fertility$fertility_rate) # 첨도 = 3.428276 -> 정규분포(3)보다 약간 뾰족한 형태

# 2) urbanization_rate(도시화율)
summary(fertility$urbanization_rate)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   53.80   67.20   65.51   81.30  100.00 
hist(fertility$urbanization_rate) # 히스토그램 확인 : 오른쪽으로 약간 치우친 형태
skewness(fertility$urbanization_rate) # 왜도 = -0.3967547 < 0
kurtosis(fertility$urbanization_rate) # 첨도 = 2.572506 -> 정규분포(3)보다 뭉툭한 형태

# 3) female_worker_ratio(여성 경제활동인구 비율)
summary(fertility$female_worker_ratio)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.111  38.536  44.119  40.545  47.125  50.465 
hist(fertility$female_worker_ratio) # 히스토그램 확인 : 오른쪽으로 많이 치우친 형태
skewness(fertility$female_worker_ratio) # 왜도 = -1.497028 < 0
kurtosis(fertility$female_worker_ratio) # 첨도 = 4.284562 -> 정규분포(3)보다 뾰족한 형태

# 4) national_income(1인당 국민소득)
summary(fertility$national_income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  480    3090    6970   15752   20140   92910 
hist(fertility$national_income) # 히스토그램 확인 : 왼쪽으로 많이 치우친 형태
skewness(fertility$national_income) # 왜도 = 1.723397 > 0
kurtosis(fertility$national_income) # 첨도 = 5.413992 -> 정규분포(3)보다 매우 뾰족한 형태


# 상관계수 확인
COR <- cor(fertility)
COR["fertility_rate",]

# fertility_rate   urbanization_rate female_worker_ratio     national_income 
#     1.00000000         -0.49020925         -0.07164783         -0.47659585 
# -> 여성 경제활동 인구 비율은 상관관계가 거의 없는 것으로 보임


# 상관계수 및 회귀선 시각화
library(psych)
pairs.panels(fertility, stars = TRUE, lm = TRUE, ci = TRUE)

# 독립변수간 다중공선성 확인
library(car)
vif(f_lm) > 10 # 분산팽창계수가 10을 초과하는지 확인

# urbanization_rate female_worker_ratio     national_income 
#              FALSE               FALSE               FALSE
# -> 독립변수간 다중공선성 문제 없음             

# 회귀모델 생성 및 확인
f_lm = lm(formula = fertility_rate ~., data = fertility)
summary(f_lm)

#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          4.349e+00  4.100e-01  10.608  < 2e-16 ***
# urbanization_rate   -1.954e-02  4.195e-03  -4.659 5.78e-06 ***
# female_worker_ratio -7.941e-03  7.069e-03  -1.123 0.262604    
# national_income     -1.623e-05  4.346e-06  -3.734 0.000246 ***
# 
# Residual standard error: 0.9551 on 201 degrees of freedom
# Multiple R-squared:  0.3026,	Adjusted R-squared:  0.2922 
# F-statistic: 29.07 on 3 and 201 DF,  p-value: 1.182e-15
# 
# # p-value: 1.182e-15 < 0.05이므로 통계적으로 유의하다
# # Adjusted R-squared:  0.2922 -> 모델의 설명력은 그다지 높지 않은 편
# # RMSE = 0.9551
# # MSE = 0.9551^2 = 0.912216














