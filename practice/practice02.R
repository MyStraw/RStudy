inistall.packages("tidyverse")
library("tidyverse")
data(mpg)
mpg

#tribble이 뭐야? data.frame과 유사한 자료구조를 제공(note00.pdf 봐봐.)

#1.2.1 현대 필터

filter(mpg, manufacturer=="hyundai")#현대 친구만 델꼬와

#조건식 여러개 쓸때 이때 필요하겄지
#필터쓸때 주의점. 
filter(mpg, manufacturer=="hyundai", cty >= 20) 

filter(mpg, model=="sonata" | cty >= 28) # 논리 연산자를 사용가능
#콤마 찍으면 안돼? 쭉쭉? 갑자기 이건 왜?
#or and

filter(mpg, model=="sonata" | cty >= 28, year==2008)
#이건 콤마? ~에서 ~이고 그중에서 뭐 인걸 갖고올때. 조건식 복잡할땐
#부울타입 올수있다. 이런 필터링은 pandas에서 제공 안하니까...
#판다스에서 쓸순있지만 번잡해진다.

#filter 맨앞에 뭐야?  데이터셋(mpg). 데이터셋이 괄호 바로뒤에 오는거 기억.!
#마이너스 조심. 제외하는거.
#지워져도 1,2,3,4가 그대로. 데이터가 바뀌어있어.

#slice_sample(hyundai_2008, prop=0.8) # 데이터에서 80% 행을 추출
#이것도 데이터셋이 온다. hyundai_2008이 데이터셋.


#현대 2008 갖고오고싶다.

hyundai_2008 <- filter(mpg,  manufacturer=="hyundai", year==2008)
hyundai_2008

slice(hyundai_2008, 1) #첫번째행만 보여줘. 행중심 가져오는건 문제가 없네
#집에서 책 보고 arrange 등 해봐

arrange(hyundai_2008, model, trans)

#필터, 어레인지 두 함수 모두 맨앞에 데이터셋이 온다. 이거 꼭 기억

select(hyundai_2008, model, year, cty, hwy)
#이거 하면 practice01에서 한거 단 몇줄로 가능하네. 통계청에서도 행렬 바로 가능했고.
#노가다로 일일이 한번 해봤다.

