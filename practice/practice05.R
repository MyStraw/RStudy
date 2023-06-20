#Chapter 09

install.packages("rJava")
install.packages("stringr")
install.packages("hash")
install.packages("tau")
install.packages("Sejong")
install.packages("RSQLite")
install.packages("devtools")

#KoNLP.tar 쓰고싶은데 이거 쓸려니까 위에꺼 다 해야..
#이건 자바 사용자가 쓰는거.
#1. 자바 깔고 2.2019년도 stringr 들고오고, 3.ID값 들고오고
#4.이건뭐야. 5.세종(코퍼스) 세종 붙은건 다 중요.
#6. sqlite 필요. 7. 컴파일시 과정시 핸들링.

# 실습: 형태소 분석을 위한 KoNLP 패키지 설치
install.packages("KoNLP_0.80.2.tar", repos = NULL)

# 실습 : 한글 사전과 텍스트 마이닝 관련 패키지 설치
install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")

# 실습 : 패키지 로딩
# library(KoNLP)
install.packages("hash")
install.packages("tau")
install.packages("devtools")
install.packages("RSQLite")

library(KoNLP)
library(tm)
library(wordcloud)



# 실습 : 텍스트 자료 가져오기
facebook <- file("C:/Rwork/Part-II/facebook_bigdata.txt",)