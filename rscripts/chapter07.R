# Chapter 07

# 실습: 실습용 데이터 가져오기 
getwd() #현재디렉토리
setwd("C:/RStudy/Rwork-2nd/Part-II")
dataset <- read.csv("dataset.csv", header = T)
dataset

# 실습: 전체 데잍 보기 
print(dataset)
View(dataset)

# 실습: 데이터의 앞부분과 뒷부분 보기 
head(dataset)
tail(dataset)

# 실습: 데이터 셋 구조 보기 
names(dataset)
attributes(dataset)
str(dataset) #obs = 관측값. 300 obs(observed). 테이블 레코드 개수가 300개.
#& 컬럼 이름. 컬럼이 7개구나. dataset 비교해가면서 공부.


# 다양한 방법으로 데이터 셋 조회하기 
# 단계 1: 데이터 셋에서 특정 변수 조회하기 
dataset$age
dataset$resident 
length(dataset$age) #데이터수 확인

#단계 2: 특정 변수으 조회 결과를 변수에 저장하기 
x <- dataset$gender
y <- dataset$price

x
y

# 단계 3: 산점도 그래프로 변수 조회
plot(dataset$price)

# 단계 4: 칼럼명을 사용하여 특정 변수 조회
dataset["gender"]
dataset["price"]


# 단계 5: 색인을 사용하여 특정 변수 조회
dataset[2] #컬럼 2번
dataset[6]
dataset[3, ] #슬라이싱. 3행.
dataset[ , 3]

# 단계 6: 2개 이상의 칼럼 조히 
dataset[c("job", "price")]
dataset[c(2, 6)] #변수 번호로 뽑아
dataset[c(1, 2, 3)]
dataset[c(2, 4:6, 3, 1)] #2번째 열, 4~6번째열, 3번째열, 1번째열


#단계 7: 특정행/열을 조회
dataset[ , c(2:4)] #열
dataset[c(2:4), ] #행
dataset[-c(1:100), ] #100개 빼고 101번부터.


# 실습: summary() 함수를 사용하여 결측치 확인하기
summary(dataset$price) #최소, 평균 등등 각종 요약 #201p
sum(dataset$price)#데이터에 결측치가 있으면(NA가 있으면) sum을 해보면

# 실습: sum() 함수의 속성을 이용하여 결측치 제거하기 
sum(dataset$price, na.rm = T)


#na 가 결측치. na.rm 은 결측치를 어떻게 처리할지 함수.

# 실습: 결측치 제거 함수를 이용하여 결측치 제거 
price2 <- na.omit(dataset$price)
sum(price2)
length(price2)


# 실습: 결측치를 0으로 대체하기 
x <- dataset$price
x[1:30]
dataset$price2 = ifelse(!is.na(x), x, 0)
dataset$price2[1:30]


# 실습: 결측치를 평균으로 대체하기 
x <- dataset$price
x[1:30]
dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2))
dataset$price3[1:30]
dataset[c('price', 'price2', 'price3')]


# 실습: 범주형 변수의 극단치(Outlier) 처리하기 (함수이름이 테이블)
table(dataset$gender) #빈도수. 젠더중 1이 173개, 2가 124개. 
pie(table(dataset$gender))


# 실습: subset() 함수를 사용하여 데이터 정제하기 
dataset <- subset(dataset, gender == 1 | gender == 2) #젠더 1,2만 빼고 제거
dataset
length(dataset$gender) #3명 빠졌네
pie(table(dataset$gender))
pie(table(dataset$gender), col = c("red", "blue"))
#R의 특징 - 그래프로

# 실습: 연속형 변수의 극단치 보기 
dataset <- read.csv("dataset.csv", header = T)
dataset$price
length(dataset$price)
plot(dataset$price)
summary(dataset$price)


# 실습: price 변수의 데이터 정제와 시각화 
dataset2 <- subset(dataset, price >= 2 & price <= 8)
View(dataset2)
length(dataset2$price)
stem(dataset2$price)


# 실습: age 변수의 데이터 정제와 시각화 
# 단계 1: age 변수에서 NA 발견
summary(dataset2$age)
length(dataset2$age)

# 단계 2: age 변수 정제(20 ~ 69)
dataset2 <- subset(dataset2, age >= 20 & age <= 69)
length(dataset2)

# 단계 3: box 플로팅으로 평균연령 분석
boxplot(dataset2$age)


# 실습: boxplot와 통계를 이용한 극단치 처리하기 
# 단계 1: boxplot로 price의 극단치 시각화
boxplot(dataset$price)

# 단계 2: 극단치 통계 확인
boxplot(dataset$price)$stats
#boxplot의 결과에 대해. ()객체 $ 변수
#[2,] 1사분위
#[3,] 2사분위
#[4,] 3사분위
#[5,] 4사분위 -> 그림이랑 같이봐야해. 207p 그린거 봐.

# 단계 3: 극단치를 제거한 서브 셋 만들기 
dataset_sub <- subset(dataset, price >= 2 & price <= 7.9)
summary(dataset_sub$price)

# 실습: 가독성을 위해 resident 갈럼을 대상으로 코딩 변경하기 
#레지던트2 라는 컬럼 만들겠다. 기본 테이블 레지던트 컬럼이 1이면 서울특별시로 바꿔라
#그걸 레지던트2 컬럼에 넣어라. (해석연습)
dataset2$resident2[dataset2$resident == 1] <- '1.서울특별시'
dataset2$resident2[dataset2$resident == 2] <- '2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <- '3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <- '4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <- '5.시구군'
#데이터 작업을 넣어주는 그걸 코딩이라고 한다 R에선. 작업한걸 추가해주네 보니까.

# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("resident", "resident2")]
head(dataset2)

# 실습: 가독성을 위해 job 칼럼을 대상으로 코딩 변경하기
dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'
# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("job", "job2")]

# 나이를 나타내는 age 칼럼을 대상으로 코딩 변경하기 
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"
head(dataset2)


# 실습: 만족도(survey)를 긍정순서로 역 코딩 1. 매우만족, 2.만족, 3.보통
# 5점이 매우만족이 되어야지. 209p
survey <- dataset2$survey
csurvey <- 6 - survey #6에서 뺀값을 저장.
csurvey

dataset2$survey <- csurvey 
head(dataset2) #보면 바뀌어있다. 1->5로.


# 실습: 범주형 vs 범주형 데이터 분포 시각화 
# 단계 1: 실습을 위한 데이터 가져오기 
setwd("C:/RStudy/Rwork-2nd/Part-II")
new_data <- read.csv("new_data.csv", header = TRUE, fileEncoding = "euc-kr" )
str(new_data)
View(new_data)
# 단계 2: 코딩 변경된 거주지역(resident) 칼럼과 성별(gender) 칼럼을
#         대상으로 빈도수 구하기 
resident_gender <- table(new_data$resident2, new_data$gender2)
resident_gender #서울에 있는 남자의 빈도수. (앞쪽이 x, 뒤쪽이 y. 컬럼명)
gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident #x에 남자 여자(행이름), y에 각 지역.열이름.


# 단계 3: 성별(gender)에 따른 거주지역(resident)의 분포 현황 시각화 
barplot(resident_gender, beside = T, horiz = T, #t,f 막 바꿔봐 
        col = rainbow(5), 
        legend = row.names(resident_gender), 
        main = '성별에 따른 거주지역 분포 현황')

# 단계 4: 거주지역(resident)에 따른 성별(gender)의 분포 현황 시각화 
barplot(gender_resident, beside = T, 
        col = rep(c(2, 4), 5), horiz = T, 
        legend = c("남자", "여자"),
        main = '거주지역별 성별 분포 현황')
#211p

# 실습: 연속형 vs 범주형 데이터의 시각화 
# 단계 1: lattice 패키지 설치와 메모리 로딩 및 데이터 준비
install.packages("lattice")
library(lattice)

# 단계 2: 직업 유형에 따른 나이 분포 현황
densityplot(~ age, data = new_data, 
            groups = job2, 
            # plot.points = T: 밀도, auto.key = T: 범례)
            plot.points = T, auto.key = T)


# 실습: 연속형 vs 범주형 vs 범주형 // factor, groups 뭐가 뭔지 구분.
# 단계 1: 성별에 따른 직급별 구매비용 분석
densityplot(~ price | factor(gender), #성별에 따른 가격의 분포 나타내는 밀도플롯
            data = new_data, #쓸 데이터 지정
            groups = position2, #서브그룹 지정
            plot.points = T, auto.key = T)



# 단계 2: 직급에 따른 성별 구매비용 분석
densityplot(~ price | factor(position2), 
            data = new_data, 
            groups = gender2, 
            plot.points = T, auto.key = T)


# 실습: 연속형(2개) vs 범주형(1개) 데이터 분포 시각화 
xyplot(price ~ age | factor(gender2), #나이가 얼마일때 얼마짜리를 샀냐
       data = new_data)


# 실습: 파생변수 생성하기 //중요!
# 단계 1: 데이터 파일 가져오기 
setwd("C:/RStudy/Rwork-2nd/Part-II")
user_data <- read.csv("user_data.csv", header = T, fileEncoding = "euc-kr")
head(user_data)
table(user_data$house_type)

# 단계 2: 파생변수 생성
house_type2 <- ifelse(user_data$house_type == 1 | ##하우스타입이 1이나 2면 0
                        #그게 아니면 다 1로. 파생변수 0,1로 만들라고.
                        user_data$house_type == 2, 0 , 1)
house_type2[1:10]
user_data$house_type[1:10]

# 단계 3: 파생변수 추가 
user_data$house_type2 <- house_type2
head(user_data)


# 실습: 1:N의 관계를 1:1 관계로 파생변수 생성하기 
# 단계 1: 데이터 파일 가져오기 
pay_data <- read.csv("pay_data.csv", header = T,fileEncoding = "euc-kr")
head(pay_data, 10)
table(pay_data$product_type)

#219페이지
# 단계 2: 고객별 상품 유형에 따른 구매금액과 합계를 나타내는 파생변수 생성
library(reshape2) ##dcast는 데이터의 재 구조화때 쓰는 함수. pay_data 데이터셋
#dcast의 공식 = dcast(데이터셋, 행~열, 함수) 즉 사용자 id별로 제품유형으로 재구조화 해라.
#함수: 이 부분엔은 새로 재구조하는 테이블에서 셀값을 계산하는 함수를 지정.
#user_id와 product_type을 가진 행들의 값을 합산.
#na.ra=T는 즉결치 제거
product_price <- dcast(pay_data, user_id ~ product_type, 
                       sum, na.rm = T)
head(product_price, 3) #3개만.
# 단계 3: 칼럼명 수정
names(product_price) <- c('user_id', '식표품(1)', '생필품(2)',
                          '의류(3)', '잡화(4)', '기타(5)')
head(product_price)


# 실습: 고객식별번호(user_id)에 대한 지불유형(pay_method)의 파생변수 생성하기 
# 단계 1: 고객별 지불유형에 따른 구매상품 개수를 나타내는 팡생변수 생성
pay_price <- dcast(pay_data, user_id ~ pay_method, length) #렝쓰는 개수세기
head(pay_price, 3)
#해석해보면 재 구조화를 할건데, pay_data 데이터셋(프레임)을 가져와서
#행에는 user_id, 열들은 pay_method로 쓸거고
#그 안에 함수는, 값들은 갯수(빈도)로 하겠다.


# 단계 2: 칼럼명 변경하기 
names(pay_price) <- c('user_id', '현금(1)', '직불카드(2)', 
                      '신용카드(3)', '상품권(4)')
head(pay_price, 3)

# 실습: 고객정보(user_data) 테이블에 파생변수 추가하기 
# 단계 1: 고객정보 테이블과 고객별 상품 유형에 따른
#         구매금액 합계 병합하기 
library(plyr)
user_pay_data <- join(user_data, product_price, by = 'user_id')
#그냥 끼워 넣어버리네.
head(user_pay_data, 10)

# 단계 2: [단계 1]의 병합 결과를 대상으로 고객별 지불유형에 따른
#         구매상품 개수 병합하기 
user_pay_data <- join(user_pay_data, pay_price, by = 'user_id')
user_pay_data[c(1:10), c(1, 7:15)]


# 실습: 사칙연산으로 총 구매금액 파생변수 생성하기 
# 단계 1: 고객별 구매금액의 합계(총 구매금액) 계산하기 
user_pay_data$총구매금액 <- user_pay_data$`식표품(1)` +
  user_pay_data$`생필품(2)` +
  user_pay_data$`의류(3)` +
  user_pay_data$`잡화(4)` +
  user_pay_data$`기타(5)`

# 단계 2: 고객별 상품 구매 총금액 칼럼 확인하기 
user_pay_data[c(1:10), c(1, 7:11, 16)]


# 실습: 정제된 데이터 저장하기 
print(user_pay_data)

setwd("C:/RStudy/Rwork-2nd/Part-II")
write.csv(user_pay_data, "cleanData.csv", quote = F, row.names = F)

data <- read.csv("cleanData.csv", header = TRUE)
data


# 실습: 표본 샘플링
# 단계 1: 표본 추출하기 
nrow(data)#데이터 400개 레코드에서 뽑을 인덱스(행의 번호만 뽑았다.)
choice1 <- sample(nrow(data), 30) #400개중 난수로 30개(인덱스를 뽑음).
choice1

# 50 ~ (data 길이) 사이에서 30개 행을 무작위 추출
choice2 <- sample(50:nrow(data), 30)#50~400 사이의 30개를 뽑았다. 인덱스를. 
#랜덤으로.
choice2

# 50~100 사이에서 30개 행을 무작위 추출 
choice3 <- sample(c(50:100), 30)
choice3

# 다양한 범위를 지정하여 무작위 샘플링
choice4 <- sample(c(10:50, 80:150, 160:190), 30)
choice4

# 단계 2: 샘플링 데이터로 표본추출
data[choice1, ]


# 실습: iris 데이터 셋을 대상으로 7:3 비율로 데이터 셋 생성하기 
# 단계 1: iris 데이터 셋의 관측치와 칼럼 수 확인
data("iris")
dim(iris) #dimesion 레코드개수,행(150) 컬럼개수,열(5)
head(iris)

# 단계 2: 학습 데이터*70%), 검정 데이터(30%) 비율로 데이터 셋 구성
idx <-sample(1:nrow(iris), nrow(iris) * 0.7)
idx #70%의 데이터
training <- iris[idx, ] #학습데이터
testing <- iris[-idx, ] #전체에서 70%를 뺀 나머지. 검증데이터.
dim(training)



# 실습: 데이터 셋을 대상으로 K겹 교차 검정 데이터 셋 생성하기 
# 단계 1: 데이터프레임 생성
name <- c('a', 'b','c', 'd', 'e', 'f')
score <- c(90, 85, 99, 75, 65, 88)
df <- data.frame(Name = name, Score = score)

# 단계 2: 교차 검정을 위한 패키지 설치 
install.packages("cvTools")
library(cvTools)

# 단계 3: K겹 교차 검정 데이터 셋 생성
cross <- cvFolds(n = 6, K = 3, R = 1, type = "random")
cross
#전체 6개. 3겹. 


# 단계 4: K겹 교차 검정 데이터 셋 구조 보기 
str(cross)
cross$which

# 단계 5: subsets 데이터 참조하기 
cross$subsets[cross$which == 1, 1]
cross$subsets[cross$which == 2, 1]
cross$subsets[cross$which == 3, 1]

# 단계 6: 데이터프레임의 관측치 적용하기 
r = 1
K = 1:3
for(i in K) {
  datas_idx <- cross$subsets[cross$which == i, r]
  cat('K = ', i, '검정데이터 \n')
  print(df[datas_idx, ])
  
  cat('K = ', i, '훈련데이터 \n')
  print(df[-datas_idx, ])
}


