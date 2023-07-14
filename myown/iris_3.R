library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(caret)

# 데이터 불러오기
data(iris)

# 열 이름 변경
names(iris) <- c("sl", "sw", "pl", "pw", "s")

# 상관 분석
cor(iris[, 1:4])

# 꽃받침 너비에 따른 품종 분포 비교
boxplot(sw ~ s, data = iris)

# 품종별 꽃잎 길이 비교
aggregate(pl ~ s, data = iris, FUN = mean)

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

# 정확도 비교
print(paste("Accuracy Difference: ", abs(rf.accuracy - xgb.accuracy)))

# 혼동 행렬 출력
cm <- confusionMatrix(as.factor(xgb.pred.labels), as.factor(testSet_y))
print(cm)

# xgboost 변수 중요도 시각화
importance_matrix <- xgb.importance(model = xgb.model)
xgb.plot.importance(importance_matrix)
