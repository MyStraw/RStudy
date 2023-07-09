# 실습: iris 데이터 셋 가져오기 
data(iris)
iris


head(iris)
names(iris) #컬럼명 보기

str(iris) #데이터셋의 구조를 출력.

install.packages("tidyverse")
library(tidyverse)

table(is.na(iris)) #결측치 확인, 하나도 없다. 750개 다있네 5*150

summary(iris$Sepal.Length)

#히스토그램, iris의 데이터프레임의 Sepal.Length 변수를 사용해 히스토그램 그린다.
# xlab = x축 이름, col = 색깔, main = 히스토그램 제목. 
#xlim = x축의 범위를 4.3~7.9로.
#x limit, c는 벡터 생성 함수. c= concatenate. 여러개의 요소를 하나의 벡터로 연결


# 실습: iris 데이터 셋의 꽃받침 길이(Sepal.Length) 칼럼으로 히스토그램 시각화하기 
summary(iris$Sepal.Length)
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta", 
     main = "iris 꽃 받침 길이 Histogram", xlim = c(4.3, 7.9))

# 실습: iris 데이터 셋의 꽃받침 너비(Sepal.Width) 칼럼으로 히스토그램 시각화하기 
summary(iris$Sepal.Width)
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))




hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta",
     main = "iris 꽃받침 길이 Histogram", xlim = c(4.3, 7.9))

summary(iris$Sepal.Width)

hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose",
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 실습: 히스토그램에서 빈도와 밀도 표현하기 
# 단계 1: 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "green", 
     main = "iris 꽃받침 너비 Histogram: 빈도수", xlim = c(2.0, 4.5))

# 단계 2: 확률 밀도에 의해서 히스토그램 그리기 
hist(iris$Sepal.Width, xlab = "iris.$Sepal.Width", 
     col = "mistyrose", freq = F, 
     main = "iris 꽃받침 너비 Histogram: 확률 밀도", xlim = c(2.0, 4.5))
lines(density(iris$Sepal.Width), col = "red")


# 실습: 정규분포 추정 곡선 나타내기 
# 단계 1: 계급을 밀도로 표현한 히스토그램 시각화 
par(mfrow = c(1, 1))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     freq = F, main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 단계 2: 히스토그램에 밀도를 기준으로 분포곡선 추가 
lines(density(iris$Sepal.Width), col = "red")

# 단계 3: 히스토그램에 정규분포 추정 곡선 추가 
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
            sd = sd(iris$Sepal.Width)),
      col = "blue", add = T)


# 실습: iris 데이터 셋의 4개 변ㄴ수를 상호 비교
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])


# 실습: 3차원으로 산점도 시각화하기 
# 단계 1: 3차원 산점도를 위한 scatterplot3d 패키지 설치 및 로딩
install.packages("scatterplot3d")
library(scatterplot3d)

# 단계 2: 꽃의 종류별 분류
iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

# 단계 3: 3차원 틀 생성하기 
d3 <- scatterplot3d(iris$Petal.Length, 
                    iris$Sepal.Length,
                    iris$Sepal.Width, 
                    type = 'n')

# 단계 4: 3차원 산점도 시각화
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, 
            bg = 'orange', pch = 21)

d3$points3d(iris_versicolor$Petal.Length, 
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue', pch = 23)

d3$points3d(iris_virginica$Petal.Length, 
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, 
            bg = 'green', pch = 25)


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

iris %>% head() %>% subset(Sepal.Length >= 5.0)

# 실습: 집단변수를 이용하여 그룹화하기
species <- group_by(iris, Species)
str(species)
species


# 실습: iris 데이터 셋을 대상으로 7:3 비율로 데이터 셋 생성하기 
# 단계 1: iris 데이터 셋의 관측치와 칼럼 수 확인
data("iris")
dim(iris) #dimesion 레코드개수,행(150) 컬럼개수,열(5)
head(iris)

# 단계 2: 학습 데이터*70%), 검정 데이터(30%) 비율로 데이터 셋 구성
idx <-sample(1:nrow(iris), nrow(iris) * 0.7)
idx #70%의 데이터
training <- iris[idx, ] #학습데이터
testing <- iris[-idx, ] #전체에서 70%를 뺀 나머지. 검증데이터.
dim(training)



#--------------------------------------------------- 여기 칸 안할겨
# 실습: 다중 공선성 문제 확인#우린 이거 안한다
# 단계 1: 패키지 설치
install.packages("car")
# 실습: 다중 공선성 문제 확인
# 단계 1: 패키지 설치 및 데이터 로딩
library(car)
data(iris)

# 단계 2: iris 데이터 셋으로 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + 
              Petal.Length + Petal.Width, data = iris)
vif(model)
sqrt(vif(model)) > 2

# 단계 3: iris 변수 간의 상관계수 구하기 
cor(iris[ , -5])

# 실습: 데이터 셋 생성과 회귀모델 생성
# 단계 1: 학습데이터와 검저엗이터 표본 추출
x <-sample(1:nrow(iris), 0.7 * nrow(iris))
train <- iris[x, ]
test <- iris[-x, ]

# 단계 2: 변수 제거 및 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
model
summary(model)

# 실습: 회귀방정식 도출
# 단계 1: 회귀방정식을 위한 절편과 기울기 보기
model

# 단계 2: 회귀방정식 도출
head(train, 1)
# 다중 회귀방정식 적용
Y = 2.3826 +  0.5684 * 2.9 + 0.4576 * 4.6
Y
6.6 - Y

# 실습: 검정데이터의 독립변수를 이용한 예측치 생성
pred <- predict(model, test)
pred

# 실습: 상관계수를 이용한 회귀모델 평가
cor(pred, test$Sepal.Length)

# 실습: 회귀분석의 기본 가정 충족으로 회귀분석 수행
# 단계 1: 회귀모델 생성
# 단계 1-1: 벼수 모델링
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width

# 단계 1-2: 회귀모델 생성
model <- lm(formula = formula, data = iris)
model

# 단계 2: 잔차(오차) 부석
# 단계 2-1: 독립성 검정 - 더빈 왓슨 값으로 확인
install.packages("lmtest")
library(lmtest)
dwtest(model)

# 단계 2-2: 등분산성 검정 - 잔차와 적합값의 분포
plot(model, which = 1)

# 단계 2-3: 잔차의 정규성 검정
attributes(model)
res <- residuals(model)
shapiro.test(res)
par(mfrow = c(1, 2))
hist(res, freq = F)
qqnorm(res)

# 단계 3: 다중 공선성 검사
library(car)
sqrt(vif(model)) > 2

# 단계 4: 회귀모델 생성과 평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length
model <- lm(formula = formula, data = iris)
summary(model)


#---------------------------------------------------- 여기 아래부터 하자
install.packages("party")
library(party)
# 실습: 학습데이터와 검정데이터 샘플링으로 분류분석 수행
# 단계 1: 학습데이터와 검정데이터 샘플링
set.seed(42)
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ]
test <- iris[-idx, ]

# 단계 2: formula(공식) 생성
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 단계 3: 학습데이터 이용 분류모델 생성
iris_ctree <- ctree(formula, data = train)
iris_ctree

# 단계 4: 분류모델 플로팅
# 단계 4-1: 간단한 형식으로 시각화 
plot(iris_ctree, type = "simple")

# 단계 4-2: 의사결정 트리로 플로팅
plot(iris_ctree)

# 단계 5: 분류모델 평가
# 단계 5-1: 모델의 예측치 생성과 혼돈 매트릭스 생성
pred <- predict(iris_ctree, test)

table(pred, test$Species)

# 단계 5-2: 분류 정확도 - 96%
(14 + 16 + 13) / nrow(test)

# 실습: K겹 교차 검정 샘플링으로 분류 분석하기 
# 단계 1: K겹 교차 검정을 위한 샘플링 - 3겹, 2회 반복
install.packages("cvTools")
library(cvTools)
cross <- cvFolds(nrow(iris), K = 3, R = 2)

# 단계 2: K겹 교차 검정 데잍 보기 
str(cross)
cross
length(cross$which)
dim(cross$subsets)
table(cross$which)

# 단계 3: K겹 교차 검정 수행
R = 1:2
K = 1:3
CNT = 0
ACC <- numeric()

for(r in R) {
  cat('\n R = ', r, '\n')
  for(k in K) {
    
    datas_ids <- cross$subsets[cross$which == k, r]
    test <- iris[datas_ids, ]
    cat('test : ', nrow(test), '\n')
    
    formual <- Species ~ .
    train <- iris[-datas_ids, ]
    cat('train : ', nrow(train), '\n')
    
    model <- ctree(Species ~ ., data = train)
    pred <- predict(model, test)
    t <- table(pred, test$Species)
    print(t)
    
    CNT <- CNT + 1
    ACC[CNT] <- (t[1, 1] + t[2, 2] + t[3, 3]) / sum(t)
  }
  
}

CNT

# 단계 4: 교차 검정 모델 평가
ACC
length(ACC)

result_acc <- mean(ACC, na.rm = T)
result_acc


# 실습: rpart() 함수를 이용한 의사결정 트리 생성
# 단계 1: 패키지 설치 및 로딩
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# 단계 2: 데잍 로딩
data(iris)

# 단계 3: rpart() 함수를 이용한 분류분석
rpart_model <- rpart(Species ~ ., data = iris)
rpart_model

# 단계 4: 분류분석 시각화
rpart.plot(rpart_model)



# 실습: 랜덤 포레스트 기본 모델 생성
# 단계 1: 패키지 설치 및 데이터 셋 가져오기 
install.packages("randomForest")
library(randomForest)
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
install.packages("xgboost")
library(xgboost)

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


# 실습: iris 데이터 셋을이용한 인공신경망 모델 생성
install.packages("nnet")
library(nnet)
# 단계 1: 데이터 셋 생성
data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
nrow(training)
nrow(testing)

# 단계 2: 인공신경망 모델(은닉층 1개와 은닉층 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3

# 단계 3: 가중치 네트워크 보기 - 은닉층 1개 신경망 모델 
summary(model_net_iris1)

# 단계 4:가중치 네트워크 보기 - 은닉층 3개 신경망 모델 
summary(model_net_iris3)


# 단계 5: 분류모델 평가 
table(predict(model_net_iris1, testing, type = "class"), testing$Species)
table(predict(model_net_iris3, testing, type = "class"), testing$Species)

# 실습: neuralnet 패키지를 이용한 인공신경망 모델 생성
# 단계 1: 패키지 설치 
install.packages("neuralnet")
library(neuralnet)

# 단계 2: 데이터 셋 생성
data("iris")
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)

# 단계 3: 수치형으로 칼럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3

training_iris$Species <- NULL
head(training_iris)

testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3

testing_iris$Species <- NULL
head(testing_iris)

# 단계 4: 데이터 정규화
# 단계 4-1: 정규화 함수 정의 
normal <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 단계 4-2: 정규화 함수를 이용하여 학습데이터와/검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)

testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)
# 단계 5: 인공신경망 모델 생성 - 은닉 노드 1개
model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)

# 단계 6: 분류모델 성능 평가
# 단계 6-1: 모델의 예측치 생성 - compute() 함수 이용 
model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result

# 단계 6-2: 상관관계 분석 - 상관계수로 두 변수 간 선형관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 단계 7: 분류모델 성능 향상 - 은닉층 노드 2개 지정, backprop 속성 적용
# 단계 7-1: 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backprop", learningrate = 0.01)

# 단계 7-2: 분류모델 예측치 생성과 평가 
model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)

# 실습: iris 데이터 셋을 대상으로 군집 수 자르기 
# 단계 1: 유클리디안 거리 계산
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)

# 단계 2: 군집 수 자르기 
ghc <- cutree(hc, k = 3)
ghc

# 단계 3: iris 데이터 셋에 ghc 칼러 ㅁ추가 
iris$ghc <- ghc
table(iris$ghc)
head(iris)

# 단계 4: 요약 통계량 구하기 
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])
