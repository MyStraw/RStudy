#연습용

## 1. 파일 읽어야 됨
df <- read.csv("data/시군구_성_월별_출생_2021.csv", fileEncoding = "euc-kr")
df

# 데이터 가져와서 읽었으니, 이제 파악해야 하는데, 알아볼수가 없네.
#2.전처리.
colnames(df) # 이제 뭔가 망한 것 같아요... 컬럼 명을 어떻게???? X 없애는게 관건
#규칙성을 봐. 월 3개가 나오는데 
# 뭘 어떻게 하죠?

#얘를 잡아내려면 스트링 앞에꺼 잡아내면 될것같다.
#점을 기준으로 스필릿을.?
#맨 끝의 1이면 남자. 2 이면 여자. 이런걸 생각하고 설계하고 챗GPT 한테 물어봐

#반복문을 못쓰니 반복문 말고 apply를 쓰면 돼
#어플라이 잘 되는지 확인 해야.. .가짜 함수 만들어

f <- function(x){
  n <- strsplit(x,".")
  return(n)
}

apply(colnames(df), f) #아무일 안하는 항등함수 만드니 일 안하네. 에러발생
#인자값중에 FUN이 missing. FUN이 없다. 위치기반 함수. position argument 쓴다.
#f 차리에 f가 아니다.

#Error in apply(colnames(df), f) : 
#argument "FUN" is missing, with no default // 이런 에러

lapply(colnames(df), f) #f 실행을 하고 이걸 실행. 1.10에서 보고 lapply인지 확인


#Escaping 시켜봐.

f <- function(x){
  n <- strsplit(x,"\\.") #문자처리해! 탈출. 문자처리해서 탈출시켜라!!
  return(n)
}

lapply(colnames(df), f) 
#너무 띄어져있네? 결과값이. 
#[[900]] 이쪽보면 딕셔너리. 키밸류로 움직? 키값을 잡으면 우째.


#리스트를 풀어
#풀고 1개짜리 잡고, 2개짜리 잡고. for loop 하면 너무 걸린다.
#else if 적기전 하나 실행해보고, else if 해서 실행해보고
#if도 없이 해보고. 결과값에서 차이를 봐. 결과랑 많이 비교해보면서 해.

f <- function(x){
  n <- unlist(strsplit(x,"\\.")) 
  if (length(n) ==1){
    return(x)
  } else if (length(n)==2){
    return(x)
  }
  return(n)
}

lapply(colnames(df), f) 


#문자를 결합하는 문법. 뭐가있니?
#이항연산이 가능한 숫자가 아닌거 주지마. R은 안된다. 에러남.
"hello" + "world" 
#문자 더하는게 힘드네... 히밤

gsub("Hello", "world")#이것도 에러. 에러 보려면 물음표

?gsub #정규식 사촌동생인데??? 정규식 넣어. 이런 쓰는법이 나온다
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
#     fixed = FALSE, useBytes = FALSE)
  
#gsub("X","", paste(n[1], n[2], "전체"), sep=".")


f <- function(x){
  n <- unlist(strsplit(x,"\\.")) 
  if (length(n) ==1){
    return(x)
  } else if (length(n)==2){
    return (gsub("X","", paste(n[1], n[2], "전체", sep=".")))
  } else{
    if (n[3]=="1"){
      return (gsub("X","", paste(n[1], n[2], "남자", sep=".")))
    }else {
      return (gsub("X","", paste(n[1], n[2], "여자", sep=".")))
    }
  }
}

lapply(colnames(df), f) 

#3은 안했으니까 else로 하면 됨. if 구문도 잘 배워.
#3번째 오는게 1이면 남자. 2면 여자.

#코드 맞냐? [3] == 1 이러면 1은 숫자인데
#결과에 보이는건 ".1" 스트링으로 들어있다.
#타입이 안맞다.
#"[3] == "1" 이렇게 해야한다. 같은 객체를 가리키냐 하는걸로 될수가 있다.
# if (identical(n[3], "1")) 이렇게 해야한다. 
#참조끼리 compare 할때.
#equals나 === 같은걸로 해야한다.
#같은 객체를 가리키는걸로 비교해선 안된다. 이렇게 꼭 해.

f <- function(x){
  n <- unlist(strsplit(x,"\\.")) 
  if (length(n) ==1){
    return(x)
  } else if (length(n)==2){
    return (gsub("X","", paste(n[1], n[2], "전체", sep=".")))
  } else{
    if (identical(n[3], "1")){
      return (gsub("X","", paste(n[1], n[2], "남자", sep=".")))
    }else {
      return (gsub("X","", paste(n[1], n[2], "여자", sep=".")))
    }
  }
}

names(df) <- lapply(colnames(df), f) 
names(df) #네임즈를 컬럼으로.

head(df) #망했다. 컬럼이 가로로.
#데이터에 대한 개념이 전혀없다는걸 온몸으로 드러낸거.

#1. 옆으로 widely로 긴 데이터를 줘놓고 2. 니가 조작할수 있는걸 바꿔봐
#column은 변수라고 하고, 횡으로 하는건 observation(관측값)이라 한다.
#세로로 읽는건 단독값. 
#가로로 있는걸 세로로 꺾어야.
#엑셀로 피벗테이블을 하던가. for loop로 돌던가.

#통계청은 왜 wide 테이블을 줄까. 통계적으론 이게 맞아. 시계열로. 늘여놔야.
#보기가 편하다.
#우리는 세로로 하고 쿼리를 해야해. 분석을 해야하는 입장. 꺾어야해.
#라이브러리 쓸거야.

#1.파일 읽었고.
#2.전처리. 했고
##3. 데이터 분석이 용이하도록 구조 변경

library(reshape2) #데이터를 꺾는다 -> 녹인다. 아래로 붙일겨. 녹인다는 말로 한다.
#판다스에서 melt 라고 한다. 면접때 widely 데이터 준다.
#column 10000개짜리 준다 막.

melt_data <- melt(df, id = "시군구별")
melt_data #뭐가 이래
head(melt_data) #헤드부터 함 보자 . 시군구별 value가 (명). 데이터가 안맞아
#관측값이 숫자가 나와야 하는데.. for loop 돌아야?

melt_data[melt_data$시군구별 == "시군구별"] #$달러 표시는 컬럼 선택하는거
#이거 하니까 정의하지 않은 열들이 선택되었대.

melt_data[melt_data["시군구별"] == "시군구별"]  ## 4.4

#이렇게 써야. 쌍따옴표, 리스트 데이터테입. 
#해시가 딕셔너리고 배열이 리스트고 비슷한데, R은 좆같다. 
#R이 자유도가 너무 높아. 앞에서 했던 identy 못쓴다. 키값

#키값인지 아닌지 판단. 판단을 이 방식을 쓰니 이 방식을 쓰는게 좋겠지.
#둘다 되는거라면.

unique(melt_data$시군구별) #유니크값인데 시군구별 먹는다
#시군구별 없애고야 말겠다


#if~for로 풀수있지만 속도가 느리다. 
# 시군구 없는앨 갖고와서, 
df2<- melt_data[!(melt_data["시군구별"]=="시군구별"),]
head(df2)#df2에 시군구별 뺀거 다있다.




##4. 데이터 정리

#variable 이거 뭐야. 데이터는 원자단위로 쪼개야.
#variable에 1997.01.전체 이거 뭐야. 쪼개.
#또 나눠

f1 <- function(x){
  n <- unlist(strsplit(x,"\\."))
  return(n[1])
}


f2 <- function(x){
  n <- unlist(strsplit(x,"\\."))
  return(n[2])
}


f3 <- function(x){
  n <- unlist(strsplit(x,"\\."))
  return(n[3])
}



df2["연도"] <- apply(df2["variable"], 1, f1)
df2["월"] <- apply(df2["variable"], 1, f2)
df2["성별"] <- apply(df2["variable"], 1, f3)
head(df2)

colnames(df2)[3] <-"출생아수" #colnames(df2)의 3번째 컬럼이름을 출생아수 명명
head(df2)

##5. 데이터 선별. 전국의 전체만 있으면 돼. 결과 보면서 해

df_all = df2[(df2["시군구별"] == "전국") & (df2["성별"] == "전체"),]
df_all = df_all[, c("출생아수", "연도", "월")]
df_all

##6. 시각화로 확인해보자
sum_agg = aggregate(df_all["출생아수"]~df_all["연도"], FUN=sum) #연도 따라가~
#출생아수에 있는거. 뭐지 뭐가 잘못됐네
#Error in model.frame.default(formula = df_all["출생아수"] ~ df_all["연도"]) : 
#  변수 df_all["출생아수"]에 유효하지 않은 타입 (list)입니다
mode(df_all["출생아수"])


sum_agg = aggregate(as.integer(df_all["출생아수"])~as.integer(df_all["연도"]), FUN=sum)
#계속 df_all["출생아수"] 이게 리스트래. 이걸 리스트 아니게 해야
#이것도 안되네.

mode(df_all["출생아수"]) #어떤 타입인지 보자
mode(df_all$출생아수)
class(df_all["출생아수"])
class(df_all$출생아수) # 캐릭터타입이네. 달러는 안의 1타입을 뱉어. 안쪽에 있는걸 다 들고와서
#다 밸류값으로.. 괄호를 치면? 싹다 참조 타입. 타입이 있다.
#우린 지금 안에있느 값 하나한가 필요. [] 이 방법을 쑬수 없다. 달러방법이 필요
#value 원값을 그대로 넘겨야한다. 어그리게이트 값에 줄때.


sum_agg = aggregate(as.integer(df_all$출생아수)~as.integer(df_all$연도), FUN=sum)
sum_agg

colnames(sum_agg) #이게 헤더. 첫번째가 연도, 두번째가 출생아수. 헤더에 있는 내용을
#다른사람이 읽을수 있게 바꿔야한다..[0] 쓰면 안된다.
colnames(sum_agg)[0] <-"연도" #이러면 안된다.

colnames(sum_agg)[1] <-"연도"
colnames(sum_agg)[2] <-"출생아수"
colnames(sum_agg)
plot(sum_agg$연도, sum_agg$출생아수, type='b')
#낮아진 지점들이 왜 낮아졌는지. 평균부분을 보고 왜 내려갔는지. 이게 초급.