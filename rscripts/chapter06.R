# Chapter 06

# 실습: iris 데이터 셋을 대상으로 '%>%' 기호를 이용하여 함수 적용하기 
# 파이프라인(세로파이프라고 생각)
## 책에 있는 예제 싹다 바꿔서 dplyr만 쓸거야.
#필터, 슬라이스, 셀렉트, 뮤테이트, 서머라이즈(이거 5개만 쓸거야)

install.packages("dplyr")
library(dplyr)
iris %>% head() #이건 되네. 눈에 안보이는데 맨앞에 dataset 들어간다고 가정.
#스트림 파이프라인 짜라고 하는거.
#1. 컬럼 먼저 조작하고, 이 컬럼 존재한다는 가정하에 특정행 갖고와서
#2. summarize로 가져와서 결과를 보는게 목표.

head() # x없다고 하는데... 이렇게 안되는데?

iris %>% head() %>% subset(Sepal.Length >= 5.0)


# 실습: dplyr 패키지와 hflight 데이터 셋 설치 
install.packages(c("dplyr", "hflights")) #설치할때 no! 눌릴것. 리스타트 하고 설치할거냐 뭍는거.
library(dplyr)
library(hflights)

str(hflights) #항공기 승객용 데이터. 데이터구조 확인

# 실습: tbl_df() 함수 사용하기 
?tbl_df#데이터프레임을 tbl로 바꾸는거. tribble로.
hflights_df <- tbl_df(hflights) 
hflights_df


# 실습: hflights_df를 대상으로 특정일의 데이터 추출하기 
filter(hflights_df, Month == 1 & DayofMonth == 2)  # 1월 2일 데이터 추출


# 실습: hflights_df를 대상으로 지정된 월의 데이터 추출하기 
filter(hflights_df, Month == 1 | Month == 2) # 1월 또는 2월 데이터 추출


# 실습: hflights_df를 대상으로 데이터 정렬하기 
arrange(hflights_df, Year, Month, DepTime, ArrTime)

# 실습: hflights_df를 대상으로 지정된 칼럼 데이터 검색하기 
select(hflights_df, Year, Month, DepTime, ArrTime)

# 실습: hflights_df를 대상으로 칼럼의 범위로 검색하기 
select(hflights_df, Year:ArrTime)

# 실습: hflights_df에서 출발 지연시간과 도착 지연시간의 차이를 계산한 칼럼 추가하기 
mutate(hflights_df, gain = ArrTime - DepTime, 
       gain_per_hour = gain / (AirTime / 60))#컬럼추가


# 실습: mutate() 함수에 의해 추가된 칼럼 뵉 #셀렉트 열. mutate = 열추가.
#둘다 열이네? 데이터셋.
#열추가 한거 셀렉트.  (데이터셋,셀렉트 선택자)
# mutate() , year, Month~~ 
# 여기서 mutate(hf~/60)) 이 부분이 데이터셋인걸 알겠어? 한눈에 들어와야해.
select(mutate(hflights_df, gain = ArrDelay - DepDelay, 
              gain_per_hour = gain / (AirTime / 60)),
       Year, Month, ArrDelay, DepDelay, gain, gain_per_hour)


# 실습: hflights_df에서 비행시간의 평균 구하기 
summarise(hflights_df, avgAirTime = mean(AirTime, na.rm = TRUE))
# hflights_df %>% summarise(avgAirTime = mean(AirTime, na.rm = TRUE))

# 실습: hflights_df의 관측치 길이 구하기 
summarise(hflights_df, cnt = n(), 
          delay = mean(AirTime, na.rm = TRUE))


# 실습: 도착시간(ArrTime)의 표준편차와 분산 계산하기 
summarise(hflights_df, arrTimeSd = sd(ArrTime, na.rm = TRUE),
          arrTimeVar = var(ArrTime, na.rm = T))


# 실습: 집단변수를 이용하여 그룹화하기
species <- group_by(iris, Species)
str(species)
species


# 실습: 공통변수를 이용하여 내부조인(inner_join)하기
# 단계 1: join 실습용 데이터프레임 생성
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 2:6, z = rnorm(5))

df1

df2

# 단계 2: inner_join 하기 
inner_join(df1, df2, by = 'x')

# 실습: 공통변수를 이용하여 왼쪽 조인(left_join)하기
left_join(df1, df2, by = 'x')

# 실습: 공통변수를 이용하여 오른쪽 조인(right_join)하기
right_join(df1, df2, by = 'x')


# 실습: 공통변수를 이용하여 전체 조인(full_join)하기
full_join(df1, df2, by = 'x')


# 실습: 두 개의 데이터프레임을 행 단위로 합치기 
# 단계 1: 실습을 위한 데이터프레임 생성
df1 <- data.frame(x = 1:5, y = rnorm(5))
df2 <- data.frame(x = 6:10, y = rnorm(5))

df1

df2

# 단계 2: 데이터프레임 합치기 
df_rows <- bind_rows(df1, df2)
df_rows


# 실습: 두 개의 데이터프레임을 열 단위로 합치기 
df_cols <- bind_cols(df1, df2)
df_cols


# 실습: 데이터프레임의 칼럼명 수정하기 
df_rename <-rename(df_cols, x2 = x1)
df_rename <- rename(df_rename, y2 = y1)
df_rename


# 실습: reshape2 패키지 설치와 데이터 가져오기 
install.packages("reshape2")
data <- read.csv("C:/Rwork/Part-II/data.csv")
data
library(reshape2)


# 실습: 넓은 형식(wide format)으로 변경하기 
wide <- dcast(data, Customer_ID ~ Date, sum)
wide

# 실습: 파일 저장 및 읽기
setwd("C:/Rwork/Part-II")
write.csv(wide, "wide.csv", row.names = FALSE)

wide <- read.csv("wide.csv")
colnames(wide) <- c('Customer_ID', 'day1', 'day2', 'day3',
                    'day4', 'day5', 'day6', 'day7')
wide

# 실습: 넓은 형식의 데이터를 긴 형식으로 변경하기 
# 단계 1: 데이터를 긴 형식으로 변경하기 
long <- melt(wide, id = "Customer_ID")
long

# 단계 2: 칼럼명 변경하기 
name <- c("Customer_ID", "Date","Buy")
colnames(long) <- name
head(long)
     
# 실습: smiths 데이터 셋 확인하기 
# 단계 1: smiths 데이터 셋 가져오기 
data("smiths")
smiths

# 단계 2: 넓은 형식의 smiths 데이터 셋을 긴 형식으로 변경 
long <- melt(id = 1:2, smiths)
long

# 단계 3: 긴 형식을 넓은형식으로 변경하기 
dcast(long, subject + time ~ ...)


# 실습: airquality 데이터 셋의 구조 변경하기

# 단계 1: airquality 데이터 셋 가져오기 
data('airquality')
str(airquality)
airquality


# 단계 2: 칼럼 제목을 대문자로 일괄 변경하기 
names(airquality) <- toupper(names(airquality))
head(airquality)

# 단계 3: melt() 함수를 이용하여 넓은 형식을 긴 형식으로 변경하기 
air_melt <- melt(airquality, id = c("MONTH", "DAY"), na.rm = TRUE)
head(air_melt)

# 단계 4: acast() 함수를 이용하여 3차원으로 구조 변경하기 
names(air_melt) <- tolower(names(air_melt))
acast <- acast(air_melt, day ~ month ~ variable)
acast
class(acast)

# 단계 5: 집합함수 적용하기 
acast(air_melt, month ~ variable, sum, margins = TRUE)
