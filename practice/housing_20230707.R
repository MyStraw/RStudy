library(tidyverse)
library(reshape2)

# 1. 데이터 불러오기
housing = read_csv("data/housing.csv")

## 데이터 확인
head(housing)
tail(housing)
summary(housing)
str(housing)

## EDA를 위한 간단한 시각화
plot_histo <- ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins=30) +
  facet_wrap(~variable, scales = 'free_x')
plot_histo

ggplot(data = housing, 
       mapping = aes(x = longitude, y = latitude,
                     color = median_house_value)) +
  geom_point(aes(size = population), alpha=0.4)

# 2. 전처리

## 데이터 결측치
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm = T) 
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)
drops <- c('total_bedrooms', 'total_rooms')
housing <- housing[,!(names(housing) %in% drops)]
housing

## 범주형(오호.. 이상한데..)
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)


#파이썬의 for
for(cat in categories) { #cat_housing에 넣을거야 #얘는 객체 기준으로 for 돈다
  cat_housing[,cat] =  rep(0, times = nrow(cat_housing)) #df다. 콤마 기준 행렬. 컬럼을 하나 더 만들고싶으면? 
  #판다스는 보통 시리즈 만들고, 결합시키라 한다. merge로. 근데 못만들면?
  #내가 넣고싶은게 컬럼이니 일단 콤마 찍고봐. cat를 돌아서 넣을건데
  #반복할거에요 0을. 몇번? row 만큼. 어디 로우? cat_housing의 row 만큼 돌아서
}

#for문 이렇게 두개 합칠라면 안된다.

#초기화부터 먼저 해라. 속도가 한껏 빨라진다. 위에껄로.
for(i in 1:length(cat_housing$ocean_proximity)){ #얘는 인덱스로 돈다
  cat <- as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] <- 1
}

head(cat_housing)

cat_names <- names(cat_housing)
cat_names
keep_columns <- cat_names[cat_names !="ocean_proximity"]
cat_housing <- select(cat_housing, one_of(keep_columns))
tail(cat_housing)




## 결측치 처리

# 3. 머신러닝

# 4. 결과 확인
