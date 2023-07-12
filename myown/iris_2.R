#아이리스 데이터를 분석해보자
#오목조목 이것저것 다 해보자



library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
#일단 데이터를 불러와서
iris
#어떤 데이터인지 한번 훑어보고
head(iris)
#열이 뭐가 있는지 보고
names(iris)
#구조도 보공
str(iris)
summary(iris)

df<-iris

names(df) #열 이름만 출력

#Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species
#이렇게 5개의 열이 있네.
names(df) <- c("sl", "sw", "pl", "pw", "sp") #열 이름을 짧게 바까보자
names(df)

names(df)[5] <-"s"
names(df)

#특정 데이터만 추출해보쟈 subset()을 이용하면 된다(필터링)
df_1 <-subset(df, s=='versicolor') #(어디데이터를 쓸건지, 조건은 뭔지)
df_1

df_2<-subset(df, sl>6 & s =='versicolor') #df에 있는 데이터 중에 S.Length이 6초과, 종이 versicolor인거
df_2
#종이 setosa인것중에 sl, sw, s열만 선택. 여러개 선택시엔 select 쓴당
df_3<- subset(df, s=='setosa', select=c(sl,sw,s))
df_3
df_4 <- subset(df,select = -c(s))
df_4

#벡터는 1차원, 행렬은 2차원, 데이터프레임도 2차원


#결측치를 찾아보자 : 대부분 제거를 하지만, 중요한 부분이라면? 아니면 데이터량이 너무 적다면?
#최대한 살려야지... 이럴땐 평균값 혹은 중앙값! 으로 대체할때도 있다
#is.na를 사용

table(is.na(iris)) #빈도 계산. False가 결측치 아닌거, True가 결측치인거. 
colSums(is.na(iris)) #컬럼별 결측치
#결측치가 없넹...


#Sepal.Length 컬럼으로 히스토그램 그려보자
summary(iris$Sepal.Length)
#히스토그램, 아이리스 데이터의 Sepal.Length를 이용할거고. x축의 라벨링 = xlab
#히스토그램의 색깔 정할땐 col. 제목은 main, xlim은 x축의 범위를 4.3~7.9로 지정.
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta", 
     main = "iris 꽃 받침 길이 Histogram", xlim = c(4.3, 7.9))

#Sepal.Width도 그려보자
summary(iris$Sepal.Width)
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

#품종별 꽃받침 너비 누가 클깡
#y~x => y는 x에 대하여
#꽃받침 너비를 품종에 대하여~ 나타내보장~ 이런거지. 종에다른 꽃받침 너비.
boxplot(data = iris, Sepal.Width ~ Species)



#상관분석을 해보자. 각 변수간의 연관된 정도를 파악.
#영향을 미치는 변수는 무엇이냐~
test_s <- subset(iris[1:4], iris$Species == 'setosa')
test_s
cor(test_s)
plot(test_s)
corrplot(cor(test_s))
#이건 꽃받침 빌이랑 너비가 관계있네

test_ver <- subset(iris[1:4], iris$Species == 'versicolor')
test_ver
cor(test_ver)
plot(test_ver)
corrplot(cor(test_ver))
#이건 꽃받침 길이랑 꽃잎길이 | 꽃잎길이랑 꽃잎너비

test_vi <- subset(iris[1:4], iris$Species == 'virginica')
test_vi
cor(test_vi)
plot(test_vi)
corrplot(cor(test_vi))
#꽃받침 길이랑 꽃잎 길이가 연관이 높다

#회귀분석 : 두 변수간의 관계. linear model = lm
testvi_lm <- lm(Sepal.Length ~ Petal.Length, data = test_vi)
testvi_lm
summary(testvi_lm) #회귀모델 결과 확인
#p값. 0.05보다 작다. 신뢰수준 95% 유의. 귀무가설x. 
#R값. 결정계수가 1에 가까울수록 회귀모델의 성능이 뛰어나다.
names(testvi_lm)
plot(test_vi$Sepal.Length, test$Petal.Length)

#의사결정트리


# rpart 함수를 써서 분류
rpart_model <- rpart(Species ~ ., data = iris)
rpart_model

# 시각화
rpart.plot(rpart_model)


#데이터셋 새로. 품종만 따로.
iris_cp <-as.data.frame(iris$Species)
names(iris_cp) <-c("act")
head(iris_cp)


set.seed(42) #우주의 숫자. 은하수를 여행하는 히치하이커를 위한 안내서
#훈련용 셋이랑 테스트셋을 나누기 8:2로.
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainSet <- iris[trainIndex,]
testSet <- iris[-trainIndex,]





model_rf <- randomForest(Species~., data=trainSet, type="class")

# 품종 텍스트를 숫자로 바꿔야한다 xgboost 쓸라믄.
iris$Species <- as.numeric(iris$Species) - 1 #이러면 0부터 시작


# 훈련 데이터와 테스트 데이터를 행렬로 변환
trainSet_x <- as.matrix(trainSet[, -5]) #5번째 열 제외하고 다 선택. 콤마는
#모든행 선택
trainSet_y <- trainSet[, 5]
testSet_x <- as.matrix(testSet[, -5])
testSet_y<- testSet[, 5]

# xgboost 파라미터 설정
params <- list("objective" = "multi:softprob",
               "eval_metric" = "mlogloss",
               "num_class" = 3)

# 모델 훈련
model <- xgboost(data = trainSet_x, label = trainSet_y, params = params, nrounds = 100)

# 예측
preds <- predict(model, ttestSet_x)
preds <- matrix(preds, ncol = 3, byrow = TRUE)
pred.labels <- max.col(preds) - 1

# 정확도 계산
accuracy <- sum(testSet_y == pred.labels) / length(testSet_y)
print(paste("Accuracy: ", accuracy))



















##############################################################
data(iris)

# 단계 2: 랜덤 포레스트 모데 ㄹ생성
model <- randomForest(Species ~ ., data = iris)
model

# 실습: 파라미터 조정 - 트리 개수 300개, 변수 개수 4개 지정  
model2 <- randomForest(Species ~ ., data = iris,
                       ntree = 300, mtry = 4, na.action = na.omit)
model2

# 실습: 중요 변수를 생성하여 랜덤 포레스트 모델 생성 
# 단계 1: 중요 변수로 랜덤 포레스트 모델 생성
model3 <- randomForest(Species ~ ., data = iris,
                       importance = T, na.action = na.omit)

# 단계 2: 중요 변수 보기 
importance(model3)

# 단계 3: 중요 변수 시각화
varImpPlot(model3)

### 엔트포리(Entropy): 불확실성
x1 <- 0.5; x2 <- 0.5 
e1 <- -x1 * log2(x1) - x2 * log2(x2)
e1

x1 <- 0.7; x2 <- 0.3               
e2 <- -x1 * log2(x1) - x2 * log2(x2)
e2

# 실습: ;최적의 파라미터(ntree, mtry) 찾기 
# 단계 1: 속성값 생성
ntree <- c(400, 500, 600)
mtry <- c(2:4)
param <- data.frame(n = ntree, m = mtry)
param

# 단계 2: 이중 for() 함수를 이용하여 모델 생성
for(i in param$n) {
  cat('ntree =', i, '\n')
  for(j in param$m) {
    cat('mtry =', j, '\n')
    model_iris <- randomForest(Species ~ ., data = iris,
                               ntree = i, mtry = j, na.action = na.omit)
    print(model_iris)
  }
}

# 실습: 다향 분류 xgboost 모델 생성
# 단계 1: 패키지 설치


# 단계 2: y 변수 생성
iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
table(iris_label)
iris$label <- iris_label

# 단계 3: 데이터 셋 생성
idx <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[idx, ] 
test <- iris[-idx, ]

# 단계 4: matrix 객체 변환
train_mat <- as.matrix(train[-c(5:6)])
dim(train_mat)

train_lab <- train$label
length(train_lab)

# 단계 5: xgb.DMatrix 객체 변환
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

# 단계 6: model 생성 - xgboost matrix 객체 이용
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2,
                     objective = "multi:softmax", 
                     num_class = 3,
                     verbose = 0)
xgb_model

# 단계 7: testset 생성
test_mat <- as.matrix(test[-c(5:6)])
dim(test_mat)
test_lab <- test$label
length(test_lab)

# 단계 8: model prediction
pred_iris <- predict(xgb_model, test_mat)
pred_iris

# 단계 9: confusion matrix
table(pred_iris, test_lab)

# 단계 10: 모델 성능평가1 - Accuracy
(19 + 13 + 12) / length(test_lab)

# 단계 11: model의 중요 변수(feature)와 영향력 보기 
importance_matrix <- xgb.importance(colnames(train_mat), 
                                    model = xgb_model)
importance_matrix

# 단계 12: 중요 변수 시각화 
xgb.plot.importance(importance_matrix)