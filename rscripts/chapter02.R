# Chapter 02


#판다스 데이터 프레임이 뭘 뜻하는거야?(시리즈의 모음) 그럼 시리즈는 뭐야?
#리스트에 리스트 하면 판다스에서는 시리즈라고 한다.
#시리즈랑 리스트랑 다른게 뭐야?
#리스트는 인덱스로.
#시리즈는 첨자가 없다 (인덱스 2개 loc, iloc)
#리스트는 행 중심
#시리즈는 열 중심(판다스) 기본적으로 R이 가진 세계관은 리스트가 없고 벡터가 있다.
#자료 중심이 컬럼 중심. 이 컬럼을 합쳐야지. 단일은 의미 없으니까.
#벡터 = 무조건 열벡터.판다스는 데이터를 열중심으로 본다.

# 실습: c() 함수를 이용한 벡터 객체 생성
c(1:20)
1:20 #1 포함, 20포함 범위. 파이썬이랑 다르다. 파이썬의 슬라이싱 닮았다.
c(1, 2, 3, 4, 5)

# 실습: seq() 함수를 이용한 벡터 객체 생성
seq(1, 10, 2)

# rep() 함수를 이용한 벡터 생성
rep(1:3, 3)
rep(1:3, each = 3)

# 실습: union(), setdiff() 그리고 intersect() 함수를 이용한 벡터 자료 처리 
x <- c(1, 3, 5, 7)
y <- c(3, 5)
union(x, y)
setdiff(x, y) #x기준의 1,7 이다.
intersect(x, y) #교집합


# 실습: 숫자형, 문자형 논리형 벡터 생성
v1 <- c(33, -5, 20:23, 12, -2:3)
v2 <- c("홍길동", "이순신", "유관순")
v3 <- c(T, TRUE, FALSE, T, TRUE, F, T)
v1; v2; v3

# 실습: 자료형이 혼합된 경우 #for 못돈다
v4 <- c(33, 05, 20:23, 12, "4") #자료형 다르면 지옥이다.
v4

# 실습: 한 줄에 ㅔ여러 개의 스크립트 명령문 사용 
v1; mode(v1); class(v1)
v2; mode(v2); class(v2)
v3; mode(v3); class(v3)
v4; mode(v4); class(v4) #숫자가 다 문자열로 바껴서간다. 자료형 혼합이면.

# 실습: 벡터 객체의 값에 칼럼명 지정
age <- c(30, 35, 40)
age
names(age) <- c("홍길동", "이순신", "강감찬")
age
age <- NULL

# 실습: 벡터 자료 참조하기 
a <- c(1:50)
a[10:45]
a[19: (length(a) - 5)]

# 실습: 잘못된 첨자를 사용하는 경우
a[1, 2]

# 실습: c() 함수에서 콤마 사용 예
v1 <- c(13, -5, 20:23, 12, -2:3)
v1[1]
v1[c(2, 4)]
v1[c(3:5)]
v1[c(4, 5:8, 7)]


# 실습: 음수 값으로 벡터 자료의 첨자를 사용하는 예 된다. 파이썬도 돼
v1[-1]; v1[-c(2, 4)]; v1[-c(2:5)]; v1[-c(2, 5:10, 1)]


# 실습: RSADBE 패키지 설치와 메모리 로딩
install.packages("RSADBE")
library(RSADBE)
data(Severity_Counts)
str(Severity_Counts)


# 실습: RSADBE 패키지에서 제공디는 데이터 셋 보기 
Severity_Counts

## R에 벡터 특징
## 컬럼 중심
## 혼합 자료형 => 문자열 (얘 말고는 파이썬과 같다)
## 슬라이싱 가능, 첨자 가능



# 실습: 벡터를 이용한 행렬 객체 생성 #시리즈는 데이터 프레임의 부분집합이다.
# 해당 데이터를 데이터 프레임으로 만들고, 동일 데이터 형태로 전처리 하고
# 각각 시리즈로 전처리(쪼개)
#노멀리제이션, 종송함수를 제거하는거까지. 내부의 부분집합을 빼서 중복되지 않게
#SQL 할때 1단계->2단계 가는 그 자료 나누던거 있잖아.
#열 우선으로.

m <- matrix(c(1:5))
m




# 실습: 벡터의 열 우선으로 행렬 객체 생성하기 
# 데이터 가장 빨리 분석하는건 데이터는 컬럼 단위로 구성해서 처리.
# 행으로 구성하면 안된다. 순회를 가로로 하지말고 세로로.
# 결과를 병렬로 처리하도록 해라.
m <- matrix(c(1:10), nrow = 2)
m


# 실습: 행과 열의 수가 일치하지 않는 경우 #때려넣네
m <- matrix(c(1:11), nrow = 2)
m


# 실습: 벡터의 행 우선으로 행렬 객체 생성하기 
m <- matrix(c(1:10), nrow = 2, byrow = T) #행우선
m

#판다스 볼때 세로로 보는것!! 시리즈 보기.

# 실습: 행 묶음으로 행렬 객체 생성하기 
x1 <- c(m, 40, 50:52)
x2 <- c(30, 5, 6:8)
mr <- rbind(x1, x2)
mr


# 실습: 열 묶음으로 행렬 객체 생성하기 
mc <- cbind(x1, x2)
mc

# 실습: 2행으로 행렬 객체 생성하기 
m3 <- matrix(10:19, 2)
m4 <- matrix(10:20, 2)
m3  
mode(m3); class(m3)  
  
# 실습: 첨자를 사용하여 행렬 객체에 접근하기 
m3[1, ]
m3[ , 5]
m3[2, 3]
m3[1, c(2:5)]

# 실습: 3행 3열의 행렬 객체 생성하기 
x <- matrix(c(1:9), nrow = 3, ncol = 3)
x


# 실습: 자료의 개수 보기 
length(x)
ncol(x)

# 실습: ;apply() 함수 적용하기 #판다스에서.
apply(x, 1, max)#x에다가 적용해라. 1을 적용. max를. 제일 큰값 갖고오는거네.
apply(x, 1, min)
apply(x, 2, mean)
help(apply)
#순서 numpy. 판다스 drop. 데이터베이스의 삭제는 drop이다. 아래로 떨어뜨리는거.
#한쪽 컬럼 전체를 떨어뜨릴수 있다.
#1은 컬럼, 2는 행이 기본이다. 판다스에서 0이 행, 1이 열로 드랍이 기본이다.
#

# 실습: 사용자 정의 함수 적용하기 
f <- function(x) {
  x * c(1, 2, 3)
}
result <- apply(x, 1, f) #for loop 돌면 안돼? apply가 빠르다.
result
#파이썬의 리스트 컴프리헨시브는 반드시 알아야한다
#기본식을 리스트 컴프리헨시브로. (람다식?) 속도차가 2배차이. 퇴근 하고 못하고.


#-------------------------------------

# 실습: 열 우선 순서로 사용자 정의 함수 적용하기 
result <- apply(x, 2, f)
result


# 실습: 행렬 객체에 칼럼명 지정하기 
colnames(x) <- c("one", "two", "three")
x

# 실습: 배열 객체 생성하기 
vec <- c(1:12)
arr <- array(vec, c(3, 2, 2))
arr


# 실습: 배열 객체의 자료 조회하기 
arr[ , , 1]
arr[ , , 2]
mode(arr); class(arr)


# 실습: 데이터 셋 가져오기 
library(RSADBE)
data("Bug_Metrics_Software")

# 실습: 데이터 셋 구조 보기 
str(Bug_Metrics_Software)


# 실습: 데이터 셋 자료 보기 
Bug_Metrics_Software


#-----------------------------------------------------
#벡터를 기준으로, 벡터에 이름을 붙여서 데이터 프레임을 만든다.
#데이터를 봤을때 어떻게 쓸건지 결정을 해야해.
#데이터 어떻게 놓을지. 세로로 뒤집어.

# 실습: 벡터를 이용한 데이터프레임 객체 생성하기
no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150, 250, 300)
vemp <- data.frame(No = no, Name = name, Pay = pay)
vemp


# 실습: matrix를 이용한 데이터프레임 객체 생성하기 
m <- matrix(
  c(1, "hong", 150,
    2, "lee", 250,
    3, "kim", 300), 3, by = T)
memp <- data.frame(m)
memp


# 실습: 텍스트 파일을 이용한 데이터프레임 객체 생성하기 
getwd()
txtemp <- read.table('data/emp.txt', header = 1, sep = "", fileEncoding = "euc-kr")
txtemp


# 실습: csv 파일을 이용한 데이터프레임 객체 생성하기 
csvtemp <- read.csv('data/emp.csv', header = T, fileEncoding = "euc-kr")
csvtemp
help(read.csv)
name <- c("사번", "이름", "급여")
read.csv('data/emp2.csv', header = F, col.names = name, fileEncoding = "euc-kr")

# 실습: 데이터프레임 만들기 
df <- data.frame(x = c(1:5), y = seq(2, 10, 2), z = c('a', 'b', 'c', 'd', 'e'))
df

# 실습: 데이터프레임의 칼럼명 참조하기 
df$x #df. 데이터프레임. x값 출력 - column 중심

# 실습: 데이터프레임의 자료구조, 열 수, 행 수, 칼럴명 보기
#어떻게든 데이터를 데이터프레임으로 바꾼다
#어떻게든 안쪽의 내용을 숫자로 다 바꾼다.
str(df)
ncol(df)
nrow(df)
names(df)
df[c(2:3), 1]

# 실습: 요약 통계량 보기 
summary(df)


# 실습: 데이터프레임 자료에 함수 적용하기 
apply(df[ , c(1, 2)], 2, sum) #이 부분이 중요하다. 
#내가 만든 커스텀 함수를 써야한다. 내장함수 같은건 극히 제한적이다.
#5분위, 7분위건


# 실습: 데이터프레임의 부분 객체 만들기 
x1 <- subset(df, x >= 3) #파이썬에선 필터. 여긴 서브셋. 부울식이 오는것.
#x가 3보다 크거나 같은애를 다 잡아오는거. boolean 이다. 원하는걸 부분집합으로
#뽑아낼때 #무신사 쿠폰. 누구 거르고 거르고
x1
y1 <- subset(df, y <= 8)
xyand <- subset(df, x >= 2 & y <= 6)
xyor <- subset(df, x >= 2 | y <= 6) 
xyand
xyor
y1


# 실습: student 데이터프레임 만들기 
sid = c("A", "B", "C", "D")
score = c(90, 80, 70, 60)
subject = c("컴퓨터", "국어국문", "소프트웨어", "유아교육")

student <- data.frame(sid, score, subject)
student


# 실습: 자료형과 자료구조 보기 
mode(student); class(student)
str(sid); str(score); str(subject)
str(student)


# 실습: 두 개 이상의 데이터프레임 병합하기 
# 단계 1: 병합할 데이터프레임 생성
height <- data.frame(id = c(1, 2), h = c(180, 175))
weight <- data.frame(id = c(1, 2), w = c(80, 75))

# 단계 2: 데이터프레임 병합하기 #x가 이거인 애랑 y가 이거인 애랑 합칠거야. merge
#나에게 데이터가 주어지면
#코테는 반만 가도 붙지만 1차 심층에서 데이터 주고 풀어라! 한다
#최우선 데이터프레임 1.생성, 
#2.부울필터(subset), 
#3.병합(새로운 형태로) 데이터를 .csv로 안주고 데이터베이스에서 가져와! 해버리면
#난이도 상승.
user <- merge(height, weight, by.x = "id", by.y = "id")
user


# 실습: galton 데이터 셋 가져오기 
install.packages("UsingR")
library(UsingR)
data(galton)

# 실습: galton 데이터 셋 구조 보기 
str(galton)
dim(galton)
head(galton, 15)


# 실습: key를 생략한 list 생성하기 #키가 있는 리스트가 있어??
#우린 이걸 딕셔너리라고 부르기로 했어.
list <- list("lee", "이순신", 95)
list


# 실습: 리스트를 벡터 구조로 변경하기 
unlist <- unlist(list)
unlist

# 실습: 1개 이상의 값을 갖는 리스트 객체 생성하기 
num <- list(c(1:5), c(6, 10))
num


# 실습: key와 value 형식으로 리스트 객체 생성하기 #키밸류인데.
#파이썬의 리스트$$ 딕셔너리, 리스트 컴프리헨시브 잘해야한다. 이걸 잘해야.
member <- list(name = c("홍길동", "유관순"), age = c(35, 25),
               address = c("한양", "충남"), gender = c("남자", "여자"),
               htype = c("아파트", "오피스텔"))
member

member$name
member$name[1]
member$name[2]

# 실습: key를 이용하여 value에 접근하기 #리스트 쓰지않을거다
# 데이터프레임으로 우린 뭔갈 해볼거다.
member$age[1] <- 45
member$id <- "hong"
member$pwd <- "1234"
member
member$age <- NULL
member
length(member)
mode(member); class(member)

# 실습: 리스트 객체에 함수 적용하기 
a <- list(c(1:5))
b <- list(c(6:10))
lapply(c(a, b), max)


# 실습: 리스트 형식을 벡터 형식으로 반환하기 
sapply(c(a, b), max) 


# 실습: 다차원 리스트 객체 생성하기 
multi_list <- list(c1 = list(1, 2, 3),
                   c2 = list(10, 20, 30), 
                   c3 = list(100, 200, 300))
multi_list$c1; multi_list$c2; multi_list$c3

# 실습: 다차원 리스트를 열 단위로 바인딩하기 
do.call(cbind, multi_list)
class(do.call(cbind, multi_list))


# 실습: 문자열 추출하기 
install.packages("stringr")
library(stringr)
str_extract("홍길동35이순신45유관순25", "[1-9]{2}") #정규식. 숫자 2개 전부다. 35,45,25
str_extract_all("홍길동35이순신45유관순25", "[1-9]{2}")
#정규식은 언제쓰나?이메일 인지 아닌지 인증 확인. 전화번호 비교할때 이때. 쓴다.


# 실습: 반복 수를 지정하여 영문자 추출하기 
string <- "hongkd105leess1002you25강감찬2005"
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[a-z]{3,}")
str_extract_all(string, "[a-z]{3,5}")


# 실습: 문자열에서 한글, 영문자, 숫자 추출하기 
str_extract_all(string, "hong")
str_extract_all(string, "25")
str_extract_all(string, "[가-힣]{3}") #한글 다 잡아오기.
str_extract_all(string, "[a-z]{3}")#소문자 가져오기
str_extract_all(string, "[0-9]{4}")#숫자. 4개인거 추출

# 실습: 문자열에서 한글, 영문자, 숫자를 제외한 나머지 추출하기 
str_extract_all(string, "[^a-z]")
str_extract_all(string, "[^a-z]{4}")
str_extract_all(string, "[^가-힣]{5}")
str_extract_all(string, "[^0-9]{3}")


# 실습: 주민등록번호 검사하기 #이거 외워라.
jumin <- "123456-1234567"
str_extract(jumin, "[0-9]{6}-[1234][0-9]{6}")
str_extract_all(jumin, "\\d{6}-[1234]\\d{6}")


# 실습: 지정된 길이의 단어 추출하기 
name <- "홍길동1234,이순신5678,강감찬1012"
str_extract_all(name, "\\w{7,}") #단어


# 실습: 문자열의 길이 구하기 
string <- "hongkd105leess1002you25강감찬2005"
len <- str_length(string)
len

# 실습: 문자열 내에서 특정 문자열의 위치(index) 구하기 
string <- "hongkd105leess1002you25강감찬2005"
str_locate(string, "강감찬")


# 실습: 부분 문자열 만들기 
string_sub <- str_sub(string, 1, len - 7)
string_sub
string_sub <- str_sub(string, 1, 23)
string_sub


# 실습: 대문자, 소문자 변경하기 
ustr <- str_to_upper(string_sub); ustr
str_to_lower(ustr)

# 실습: 문자열 교체하기 
string_sub
string_rep <- str_replace(string_sub, "hongkd105", "홍길동35,")
string_rep <- str_replace(string_rep, "leess1002", "이순신45,")
string_rep <- str_replace(string_rep, "you25", "유관순25,")
string_rep


# 실습: 문자열 결합하기 
string_rep
string_c <- str_c(string_rep, "강감찬55")
string_c

# 실습: 문자열 분리하기 
string_c
string_sp <- str_split(string_c, ",")
string_sp


# 실습: 문자열 합치기
# 단계 1: 문자열 벡터 만들기 
string_vec <- c("홍길동35", "이순신45", "유관순25", "강감찬55")
string_vec

# 단계 2: 콤마를 기준으로 문자열 벡터 합치기 
string_join <- paste(string_vec, collapse = ",")
string_join
