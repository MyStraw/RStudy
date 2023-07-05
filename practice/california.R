library(tidyverse)
library(reshape2)

# 1. 데이터 불러오기
housing = read_csv("practice/housing.csv")
housing
# 앞/뒤를 확인해서 해당 데이터 확인
head(housing)
tail(housing)
summary(housing)

## 데이터 전체 구조를 확인.
str(housing)

## EDA를 위한 간단한 시각화



#ggplot(data = housing) +
#  geom_histogram(bins = 30) + #높이가 30개짜리인 히스토그램. x,y 넣으면 안돼.
# 못그리네... gg

#hist(housing$longitude)  이것도 gg

#aes 매핑. 밸류라고 하는 애를 x로 연결해줘 

#data로 들고와야 하는데 바로 못쓰니 녹여서. value로 녹인값들로.
#melt를 하면 값들이 value라는 컬럼에 들어온다. 이 value를 x축에...
#bins를 안정해주면 들쭉날쭉해진다. 척도가 안맞는다.
#감쌀거야. 녹인 변수 전부를 다 감싸. 대신에 scale을 x축을 니가 한걸로 맞춰서 감싸
#유사판다스

plot_histo <-ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins=30) +
  facet_wrap(~variable, scales = 'free_x')
plot_histo

#default 이름있는 매개변수 생략가능.(지금은 배우니까) 주니어는 이름을 다 단다.
#헷갈리니까. 통일해서 써. named를 다 달아.
ggplot(data = housing, 
       mapping = aes(x = longitude, y = latitude,
                     color = median_house_value)) +
  geom_point(aes(size=population), alpha=0.4)



# 2. 전처리

## 이상치 처리
bedroom_mean <- mean(housing$total_bedrooms, na.rm = T)
bedroom_median <- median(housing$total_bedrooms, na.rm = T)
bedroom_median
bedroom_mean


#이건 중위값이랑 평균이랑 어느정도 갭이 있는지 볼라고 하는거
ggplot(data = housing, mapping = aes(x = total_bedrooms)) +
  geom_histogram(bins=30, color = 'black', fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "red"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "yellow"), lwd = 1.5)

  
### 중위값으로 한다고 가정하자!

#이거 밀어넣는다. 1.결측치 처리해서 NA값 살릴생각.
# 어쩔수없이 중위값으로 때려넣을거야. 혹시 걔 비었어? 그러면 중위값 넣어
#NA로 나간값을 중위값으로 채웠음.
#밑에 이 값은 쓰지 않을건데... 어떻게 드랍?
#각 집에 방의 영향성은 갖고있는데,
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm=T)
#housing$total_bedrooms

#households- 가계. 거주자. NA를 중위값으로 채웠는데, bedrooms 갯수가 집값 영향있는거 알아
#방 갯수는 중요하다. households 기준으로 나눌거야. 왜??
#근데 이건 왜 mean? 이거 쓰면 1인당 쓰는 방인데.. 평균이잖아.
housing$mean_bedrooms <- housing$total_bedrooms / housing$households 
housing$mean_rooms <- housing$total_rooms / housing$households
#이 집에 몇명이 사는지 필요. 1인당 얼마정도 쓰는지 환산해서. bedrooms를 쓰려는데 못쓰니까
#대신 이걸 보존시키고 싶어.
head(housing)

#이제 지워야지. 타이디버드 안써도 돼. 컬럼이 2개면 괄호 쳐야돼.
#판다스도 마찬가지. 지워야되니.. 행렬 [행,렬]
#colomn 지울거야. drop 할때 행은 손댈 생각이 없으니, 앞부분 비워. 뒷부분 열만 지워야.
#슬라이싱에 -1이 안되는데 우찌할까? 판다스랑 마찬가지
#지우는걸 생각하지 말고, 걔 말고 다 살린다고 생각. 지우면 리턴이 지운값만 나온다.(1개 나와)
drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[,!(names(housing) %in% drops)]
housing


## 범주형 
categories <- unique(housing$ocean_proximity) #몇개있냐. 5개
#bay 안으로 들어간게 만, ocean은 바깥.
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)




  
##결측치 처리(NA는 그렇다 치고.)



# 3. 머신러닝

# 4. 결과확인