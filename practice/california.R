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
### "가정과 결과"를 정의해서 해당 문제를 빠르게 해결

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


#쟤를 어떻게 해결할까(대부분이 무시)
##결측치 처리(NA는 그렇다 치고.)


## 범주형 (Onehot Encoding)
categories <- unique(housing$ocean_proximity) #몇개있냐. 5개
#bay 안으로 들어간게 만, ocean은 바깥.
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




##결측치 처리(수치형) (위에는 결측치 문자열 처리했공)
colnames(housing)

drops <- c("ocean_proximity", "median_house_value") #뒤는 답지라서 뺀다
housing_num <- housing[, !(names(housing) %in% drops)]
colnames(housing_num)#durlek ek Eofusjgdmfrjdi


scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)


## 결합
head(cat_housing)
head(scaled_housing_num)
head(housing$median_house_value) #이거 빼먹지마. 답안지야
#이거 3개 결합해야돼

#cbind 는 컬럼 기준으로 바인딩. 컬럼바인딩.
cleaned_housing <- cbind(cat_housing, scaled_housing_num,
                         median_house_value = housing$median_house_value)
head(cleaned_housing)


#데이터 전처리 -> 숫자로 다 만들고 이제 머신러닝



#-------------------------------------------------------#

######### 3. 머신러닝. 

#x랑 y에 뭘 할지부터. 집값은 확실히 y. 나머지 잡아서.
#싸이킥런은 일반적으로 뭐해? 다 써야지. 숫자로 다 바꿔놨으니 해야지.
#이제 셔플링 해서 나눠야지.
set.seed(42) #은하수를 여행하는 히치하이커. 시드번호는 암꺼나 해도 된다.
#랜덤 42로 고정. 결과확인시 시드 변경돼서 흔들리면 답이 안나오니까.
#실제 문제에선 시드값 빼야한다
#샘플 이용해서 샘플링



##### 데이터 분리 
#샘플링 과정에서 랜덤값이 고정되길 원한다. 실험을 여러번 반복해도
#같은 결과값이 나오는지 봐야지.
sample <- sample.int(n = nrow(cleaned_housing),
                     size = floor(.8*nrow(cleaned_housing)),
                     replace = F)

#트레인과 테스트를 나눠서.
train <- cleaned_housing[sample,] #뒤엔 다 갖고와야지
test <- cleaned_housing[-sample,] #샘플 제외하고 다 갖고오기

#맞는지 틀린지는 둘이 합산해서

nrow(train) + nrow(test) == nrow(cleaned_housing) #트루가 나와야한다.



##### 모델 설계
### 선형 모델(이게 젤 좋다) 일단 1차 결과부터 내고 시작해야한다.이게 기준선
#선형모델 glm 쓰자. 얘는 답안부터 써야한다.
colnames(cleaned_housing) 
glm_house<-glm(median_house_value ~ median_income+mean_rooms+population,
    data=cleaned_housing)#소득 중요하다 생각해, 방 개수도
glm_house
#이거 쓰면 돼? 안돼. 지맘대로다. 선긋기라. 처음 선긋기에 따라 너무 달라진다.

#cv 써야하니 라이브러리 깔자.
#install.packages("boot")
library(boot)

k_fold_cv_error<-cv.glm(cleaned_housing, glm_house, K=5) #k fold 해라. 여러번 돌려볼거다. 5번
k_fold_cv_error$delta
#원 데이터 줄게, glm하우스 가지고 5번 반복해. 반복하면 오차율이 나온다(cv_error) 그 오차의 delta(간극)이 얼마야?
#그게 [1] 6964563438 6958607203 이만큼 나온다. 해결이 잘 안돼

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse
#[1] 오차율 83453.96 이렇게 나온다. 8만불 정도면 집살수 있어~

glm_house$coefficients #계수를 뜻한다. 플러스인게 좋지
#현재 glm하우스 오차율 83453.96 이정도. 8천만정도 오차율이 있다. 
#집사러 가니까  1억 플마 사이야~ 하는거. 
#median_income이 전체값 좌우한다.( 82608.959)

#나쁘지 않은데 사람한테 설명을 해야하는데...


##################################################
##### 랜덤 포레스트 (진짜로 그러한지 보자)
install.packages("randomForest")
library(randomForest)


##데이터 분리
#주요값들이 수형도로, 좋은지 드러나게. 랜덤포레스트로
#나무 몇개? 500개. 중요도 출력 할거야? ㅇㅇ 이것땜에 하는거니까 T
#오래걸린다

###중요 변수값 확인
names(train)
train_y = train[,'median_house_value']
train_x = train[,names(train) != 'median_house_value']

rf_model = randomForest(train_x,
                        y=train_y,
                        ntree=500,
                        importance = T)

rf_model$importance

#cv는 cv따라, 랜덤포레스트는 또 x,y 둘다 라이브러리 달라. 쓸라면 이런거 잘 신경써서.



test_y = test[,'median_house_value']
test_x = test[,names(train) != 'median_house_value']
y_pred = predict(rf_model, test_x)
test_mse = mean((y_pred - test_y)^2)
test_mse
test_rmse = sqrt(test_mse)
test_rmse
#[1] 48403.98 이렇게 나왔네. 위에 glm_cv_rmse. 선형회귀일때보다 확 줄었다.



###### 학습
##### XGBoost #또 문법 다르다. 매번 맞춰줘야... cv랑 랜포도 그렇고
install.packages("xgboost")
library(xgboost)

dtrain = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest =  xgb.DMatrix(data = as.matrix(test_x), label = test_y)
watchlist = list(train=dtrain, test=dtest)
bst <- xgb.train(data = dtrain,
                 max.depth = 8,
                 eta = 0.3,
                 nthread = 2,
                 nround = 1000,
                 watchlist = watchlist,
                 objective = "reg:squarederror",
                 early_stopping_rounds = 50,
                 print_every_n = 500)
                 


# 4. 결과확인

#XGBoost를 활용한 RMSE 값이 가장 낮고(48301->47810), 주요 특징값은 'median_income'

## 1) ..
## 2) ..
## 3) ..