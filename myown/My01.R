library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(caret)
library(ggplot2)
# 데이터 불러오기
data(iris)

table(is.na(iris)) #빈도 계산. False가 결측치 아닌거, True가 결측치인거. 
colSums(is.na(iris)) #컬럼별 결측치

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
# 품종별 꽃잎 길이 비교#평균
aggregate(Petal.Length ~ Species, data = iris, FUN = mean)


#상관분석을 해보자. 각 변수간의 연관된 정도를 파악.
#영향을 미치는 변수는 무엇이냐~
test_s <- subset(iris[,1:4], iris$Species == 'setosa')
test_s
cor(test_s)
plot(test_s)
corrplot(cor(test_s))
#이건 꽃받침 빌이랑 너비가 관계있네

test_ver <- subset(iris[,1:4], iris$Species == 'versicolor')
test_ver
cor(test_ver)
plot(test_ver)
corrplot(cor(test_ver))
#이건 꽃받침 길이랑 꽃잎길이 | 꽃잎길이랑 꽃잎너비

test_vi <- subset(iris[,1:4], iris$Species == 'virginica')
test_vi
cor(test_vi)
plot(test_vi)
corrplot(cor(test_vi))
#꽃받침 길이랑 꽃잎 길이가 연관이 높다

#한번에 다 보면 어떻게 될까
cor(iris[, 1:4])
corrplot(cor(iris[, 1:4]))
#어우.. 



#회귀분석 : 두 변수간의 관계. linear model = lm
testvi_lm <- lm(Sepal.Length ~ Petal.Length, data = test_vi)
testvi_lm
summary(testvi_lm) #회귀모델 결과 확인
#p값. 0.05보다 작다. 신뢰수준 95% 유의. 귀무가설x. 
#R값. 결정계수가 1에 가까울수록 회귀모델의 성능이 뛰어나다.
names(testvi_lm)
plot(test_vi$Sepal.Length, test_vi$Petal.Length)

#의사결정트리

# rpart 함수를 써서 분류
rpart_model <- rpart(Species ~ ., data = iris)
rpart_model

# 시각화
rpart.plot(rpart_model)


# Sepal.Length와 Sepal.Width의 산점도 그리기
plot(iris$Sepal.Length, iris$Sepal.Width, 
     xlab = "Sepal.Length", ylab = "Sepal.Width", 
     main = "Sepal.Length vs Sepal.Width Scatter Plot",
     col = iris$Species)

# Sepal.Length와 Petal.Length의 산점도 그리기
plot(iris$Sepal.Length, iris$Petal.Length, 
     xlab = "Sepal.Length", ylab = "Petal.Length", 
     main = "Sepal.Length vs Petal.Length Scatter Plot",
     col = iris$Species)

# Sepal.Length와 Petal.Width의 산점도 그리기
plot(iris$Sepal.Length, iris$Petal.Width, 
     xlab = "Sepal.Length", ylab = "Petal.Width", 
     main = "Sepal.Length vs Petal.Width Scatter Plot",
     col = iris$Species)

# Sepal.Width와 Petal.Length의 산점도 그리기
plot(iris$Sepal.Width, iris$Petal.Length, 
     xlab = "Sepal.Width", ylab = "Petal.Length", 
     main = "Sepal.Width vs Petal.Length Scatter Plot",
     col = iris$Species)

# Sepal.Width와 Petal.Width의 산점도 그리기
plot(iris$Sepal.Width, iris$Petal.Width, 
     xlab = "Sepal.Width", ylab = "Petal.Width", 
     main = "Sepal.Width vs Petal.Width Scatter Plot",
     col = iris$Species)

# Petal.Length와 Petal.Width의 산점도 그리기
plot(iris$Petal.Length, iris$Petal.Width, 
     xlab = "Petal.Length", ylab = "Petal.Width", 
     main = "Petal.Length vs Petal.Width Scatter Plot",
     col = iris$Species)


# Sepal.Length와 Sepal.Width의 산점도 그래프
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal.Length vs. Sepal.Width",
       x = "Sepal.Length",
       y = "Sepal.Width",
       color = "Species")

# Petal.Length와 Petal.Width의 산점도 그래프
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal.Length vs. Sepal.Width",
       x = "Sepal.Length",
       y = "Sepal.Width",
       color = "Species")



pairs(iris[, 1:4], col = iris$Species, pch = 19)








# 열 이름 너무 기니까 변경
names(iris) <- c("sl", "sw", "pl", "pw", "s")

# 훈련 데이터와 테스트 데이터 분리
set.seed(42)
trainIndex <- sample(1:nrow(iris), nrow(iris)*0.8)
trainSet <- iris[trainIndex, ]
testSet <- iris[-trainIndex, ]

# 랜덤 포레스트로 학습 및 예측
model_rf <- randomForest(s ~ ., data=trainSet, type="class")
rf.pred <- predict(model_rf, testSet)
rf.accuracy <- sum(rf.pred == testSet$s) / length(testSet$s)
print(paste("Random Forest Accuracy: ", rf.accuracy))

# Species를 수치형으로 변경하고 훈련 및 테스트 데이터를 행렬로 변환
trainSet$s <- as.numeric(trainSet$s) - 1
testSet$s <- as.numeric(testSet$s) - 1
trainSet_x <- as.matrix(trainSet[, -5])
trainSet_y <- trainSet[, 5]
testSet_x <- as.matrix(testSet[, -5])
testSet_y <- testSet[, 5]

# xgboost 파라미터 설정
params <- list("objective" = "multi:softprob",
               "eval_metric" = "mlogloss",
               "num_class" = 3)

# xgboost로 학습 및 예측
xgb.model <- xgboost(data = trainSet_x, label = trainSet_y, params = params, nrounds = 100)
xgb.pred <- predict(xgb.model, testSet_x)
xgb.pred <- matrix(xgb.pred, ncol = 3, byrow = TRUE)
xgb.pred.labels <- max.col(xgb.pred) - 1
xgb.accuracy <- sum(testSet_y == xgb.pred.labels) / length(testSet_y)
print(paste("XGBoost Accuracy: ", xgb.accuracy))

# 혼돈 행렬 출력
cm <- confusionMatrix(as.factor(xgb.pred.labels), as.factor(testSet_y))
print(cm)

# xgboost 변수 중요도 시각화
importance_matrix <- xgb.importance(model = xgb.model)
xgb.plot.importance(importance_matrix)


# 정확도 비교
print(paste("정확도 차이(랜덤포레스트-xgboost): ", abs(rf.accuracy - xgb.accuracy)))
