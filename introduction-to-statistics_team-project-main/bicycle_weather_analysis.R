# 기상 조건에 따른 따릉이 이용 패턴 분석 프로젝트
# 목적: R을 활용한 기상 변수별 이용 의향 차이 검정(t-test) 및 서비스 개선 인사이트 도

# 1. 데이터 로드
data <- read.csv("C:/Users/yunzz/Downloads/introduction-to-statistics_team-project-main/sample_data.csv", header=TRUE, fileEncoding = "UTF-8")

# 2. 통계적 가설 검정 (t-test)

# [가설 1] 강수 강도에 따른 자전거 이용 의향 차이 검증

# 이슬비(약한 비)일 때
score_drizzle <- data$intent_light_rain
# 보통 비(우산 필요)일 때
score_rain <- data$intent_moderate_rain

mean(score_drizzle)
mean(score_rain)

# 대응표본 t-검정
t.test(score_drizzle, score_rain, mu=0, paired=TRUE, alternative="greater")

# [가설 2] 결빙 위험 인식 차이 검증

#눈이 올 때
score_snow <- data$intent_snow
#눈 온 후 빙판길
score_ice  <- data$intent_icy_road

mean(score_snow)
mean(score_ice)

# 대응표본 t-검정
t.test(score_snow, score_ice, mu=0, paired=TRUE, alternative="greater")

# [가설 3] 이용 목적에 따른 악천후 시(강한 비, 보통 비) 이용 의향 비교

# 통학 목적 그룹과 여가 목적 그룹으로 분류
target_purpose <- "통학"
group_commute <- data[grepl(target_purpose, data$usage_purpose), ]
group_leisure <- data[!grepl(target_purpose, data$usage_purpose), ]

mean(group_commute$intent_moderate_rain)
mean(group_leisure$intent_moderate_rain)

# 등분산 검정
var.test(group_commute$intent_moderate_rain, group_leisure$intent_moderate_rain)

# 독립표본 t-검정
t.test(group_commute$intent_moderate_rain, group_leisure$intent_moderate_rain, alternative="greater", var.equal=F)


# [가설 4] 추위 저항력 vs 더위 저항력 비교

limit_cold <- data$temp_limit_cold
limit_hot  <- data$temp_limit_hot

#쾌적온도
temp_data <- read.csv("C:/Users/yunzz/Downloads/introduction-to-statistics_team-project-main/ta_20251209174603.csv", fileEncoding="EUC-KR")
ideal_temp <- mean(temp_data$평균기온, na.rm = TRUE)

# 추위 저항력 
tol_cold <- abs(ideal_temp - limit_cold)

# 더위 저항력 
tol_hot <- abs(limit_hot - ideal_temp)

mean(tol_cold)
mean(tol_hot)

# 대응표본 t-검정
t.test(tol_cold, tol_hot, mu=0, paired=TRUE, alternative="greater")

