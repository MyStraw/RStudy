#DB는 파일이다
#DB를 만드는 가장 큰 요건? 저장할려고
#무결성있게 저장하려고.(제약조건을 위한 무결성)

#업데이트가 일어남과 동시에. 현재값? 혹은 변경된 값?
#read write update 연산이 동시에 일어날때
#가져가는 친구는 어떤걸 가져가야해?
#순서 
#reat - T0
#write - 뒤로
#update - 먼저
#이건 근데 케바케다

#예를들어
# 1.난 적당한 무결성만 원한다. 저장 잘되고 쿼리만 됐음 좋겠어
# 2.여러명 안써. 나혼자 쓸거야. 업데이트 그만해~!
#핸드폰에 데이터베이스 들어가있나?
#연락처
# 3. SQLIte 라는게 있다. 

#쌩컴퓨터 주고 포트폴리오 돌려봐! 하면 못돌려. 깃에있는거 바로 됨?
#MySQL로? 안됨. SQLite로 해야된다. 핸드폰에 이게 들어가있다.


library(tidyverse)
library(DBI)
library(RSQLite)
install.packages("RSQLite") #설치하고 위에 라이브러리 불러와

#연결해줘. 하는데 무슨db? 그냥 테스트 db에~
#포트폴리오는 배포가 안되면 말짱 꽝이다.
#누가 내가 만든 코드를 볼수가 있어야해.
#MySQL로 짜면 배포가 되나?
#DB는 SQLite부터 시작하라고 해. 
con<-dbConnect(RSQLite::SQLite(), "test.db")
dbListTables(con)
#플라토에서 데이터 가져오기


moving_data<-read_csv("./data/seoul_moving_202107_09_hr.csv")
reference <- readxl::read_excel("./data/reference.xlsx")
install.packages("readxl") #엑셀파일 읽는 라이브러리
#프론트엔드, 백엔드 DB만 바라보고있다. DB에 알잘딱깔쎈으로 넣어줘야.
#다 할줄알아야. csv 파일 읽고, xlsx 읽고. 두 파일 있다고 가정했을때.

#1. 먼저 뭐부터? 사이즈부터 확인.

glimpse(moving_data) #백만개? for 안돼
glimpse(reference)

#컬럼명부터 조져야지. 데이터셋에 괄호 써? 안써. 언더바!
names(moving_data) <- gsub(" ", "", names(moving_data))#공백없애
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합")
names(moving_data)

names(reference)<- c("시도코드", "시군구코드", "시군구이름", "전체이름")

copy_to(con, moving_data, "moving_data",
        temporary = FALSE,
        indexes = list("대상연월", "요일", "도착시간", 
                       "출발시군구코드", "도착시군구코드"),
        overwrite = TRUE
        )
#해석. 이 데이터를 카피 con.으로 가.
#이상한 테이블 만들어서 중간 임시값 가지마.
#복합 인덱스로. overwrite 중복있으면 덮어써.
#나혼자 쓸거니까 이래도 되지
#이렇게 하니까 test.db에 116메가 짜리가 생성.

copy_to(con, reference, "reference",
        temporary = FALSE,
        indexes = list("시군구코드"),
        overwrite = TRUE
)

dbListTables(con)
#외부라이브러리 없이 R에서 동작가능 -> SQLite
#다음시간엔 tidyverse로 바꿔
#벌크작업이라 한다 이걸. DB에 카피 밀어넣는거

#DB 컬럼추가가 개 빡세->null 허용이 된다. 새로 들어오는 컬럼
#차라리 테이블 새로 만들어!
#mutation은 열 추가

#1변수명 그럴싸하게 만들자
moving_db <-tbl(con,"moving_data") #자기가 쿼리 날려서
moving_db %>% head(6)#얘길 해준다.

reference_db <- tbl(con, "reference")
reference_db

#문제 하나 풀어보자
#평균 이동시간 기준으로 이동 거리르 중/단/장기로 구분

moving_db <- moving_db %>% #가져와
  mutate(평균이동시간_시 = 평균이동시간_분 / 60) #mutate 하면 컬럼이 생겼다
#해줄게 해줄게~ 하고 가상에만 넣고, 실제엔 안넣어
#중간과정을 다 반영하지 않아. 다 끝나고 나서 넣어라! 할때 넣어줄게.
#이걸 lazy(지연) 연산. 안그럼 계속 db 접근해야하니까.
#판다스건 자바건 다 마찬가지. DB에 들어갔는지 아닌지 확인필요
#DB로부터 tranjection OK받아야.
#원본은 가만 내비둬. 계산 빠르게 할라고 때려넣었다. db 반영할라고 넣은게 아냐.


###############
moving_db$평균이동시간_시 #잉 null이 나오네.? 되게 해야돼

glimpse(moving_db) #여기선 시간_시 가 보이는데. #읽을때야 비로소 보인다.
moving_db$평균이동시간_시 #왜 null 나와?
##############
moving_db <-moving_db %>%
  mutate(평균이동시간_시 = 평균이동시간_분 / 60) %>%
  mutate(이동타입 = case_when(
    between(평균이동시간_시, 0, 0.5) ~ "단기",
    between(평균이동시간_시, 0, 1) ~ "중기",
    평균이동시간_시 >=1 ~ "장기",
    TRUE ~ as.character(평균이동시간_시)
  )) %>%
  relocate(이동타입)

moving_db %>% colnames()

#데이터분석가로 올라면 domain 지식이 중요하다. 이걸 아는 애가 엑셀배워서 들어오는데
#지금은 통계학과 애들도 R을 많이 배운다. 사회 문화가 R로
#인턴은 어디나 있다. 

#서로다른 레퍼런스2개 핸들링을 해야하는데 데이터베이스에 밀어넣기만 하면
#타이디버드를 이용해 데이터분석을 쉽게쉽게 해야한다.
#이거 주면 백엔드 팀에서

#정형 - dataframe만 하면 되는데 안편리하다
#타이드버드가 R의 주도적이라 안쓸수가 없다.
#pandas 왜 써? 제일 많이써. 지원이 많아

#여기까지가 9-1 정형 데이터.
