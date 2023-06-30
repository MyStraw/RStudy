#필수 패키지 설치
install.packages("tidyverse")
install.packages("tidymodels")

install.packages("tictoc")
install.packages("doParallel")
install.packages("furrr")
install.packages("xgboost")

install.packages("ranger")
install.packages("glmnet")

install.packages("palmerpenguins") #데이터셋
install.packages("hrbrthemes") #테마

#패키지 불러오기
library(tidyverse)
library(tidymodels)
library(tictoc)
library(doParallel)
library(furrr)
library(xgboost)
library(ranger)
library(glmnet)
library(palmerpenguins)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
#이거 깔고 다시 테마 라이브러리 실행



##환경설정. CPU를 써야한다
#note02.pdf에 보면서 긁어와. cpu 4코어인데 8코어로 인식->병목. 코어 갯수 등록
#8코어
theme_set(hrbrthemes::theme_ipsum_rc())
plan(multicore, workers = availableCores())
cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(cores)
registerDoParallel(cores = cl)
set.seed(42)
#랜덤값으로 나오는 모든값을 seed(42)로. 슈도랜덤. 아무리 셔플링 해도 동일한 셔플링
#이런걸 왜해? (42가 중요한게 아니야. 아무숫자나)
#XG부스트로 양쪽의 대조군으로 같이 테스트 할거야
#랜덤 샘플링 할때마다 비교를 해야하니까.(셔플링이 돼도 같아야 비교가 되겠지)

#데이터 와글링(불러오기)

# 1. 데이터 불러오기 및 전처리

penguins_data <- penguins
penguins_data <- palmerpenguins::penguins #이건 똥컴에서 써라

penguins_data 

#일단 NA값부터 날려. EDA는 판다스에서 하는거.

glimpse(penguins_data)
t(map_df(penguins_data, ~sum(is.na(.))))

#df로 바꾸자
penguins_df <-
  penguins_data %>%
  filter(!is.na(sex)) %>% #녹음시작, 
  select(-year, -island)
head(penguins_df) #전처리 다 됐는지 확인

#컴퓨터 랜덤은 유사 랜덤이다
#반드시 기준을 놓고 섞어야.


penguins_split <-
  rsample::initial_split(
    penguins_df,
    prop = 0.7,
    strata = species #편향 가지면 종이 한쪽으로 치우칠수.(샘플링 잘못하면 특정 종이 빠진다)
  )
#타이타닉 데이터는 생존자가 아주 생각보다 치우쳐져있다. 아주 직관적
#생존자 1이 나오게 만들어야해. 내가 하면 머신러닝보다 잘하는데 왜 작게나와?
#샘플링 잘못하면 전부 죽은사람 가져와.

#---------여기까지가 불러오기 및 전처리

## 2.베이스라인. XG부스트 기준으로 베이스라인(기준선)
#성능이 좋아졌어요? 우리 가진 에셋이 있어. 우리가 뭔가 만든게 있어야 쟤보다 좋단 소리를 하지
#원본데이터 손대지 않고 간단히 XG돌려서 나오는 기준값을 가지고 이제 비교를 할거야
#그럼 여기서부터 전처리를 더 할까? 알고리즘을 더 할까? 하는 선택을 할수있다.
#업무스타일.

#틱을 적고 

install.packages("xgboost")
library(xgboost)

tic(" Baseline XGBoost training duration ")
xgboost_fit <-
  boost_tree() %>% #부스트 알고리즘 이용
  set_engine("xgboost") %>% #엔진은 xg부스트 쓸거야
  set_mode("classification") %>% #분류모델을 써라
  fit(species ~ ., data = training(penguins_split)) #나머지 다 써라. 
toc(log = TRUE)
 
preds <- predict(xgboost_fit, new_data = testing(penguins_split)) #테스팅. 피팅된 결과값 가지고. 파이썬은 4가지로(하나는 트레이닝, 테스트),
#텐서플로랑, 트레이닝 - 테스트, 테스트- 테스트, 트레이닝-트레이닝, 테스트-트레이닝 이거 4개 순서가 좀 다르다. 주의.
#지꺼 때려 넣었더니 문제는 다 맞췄다. 전형적인 오버피팅. 피팅을 줄여야. 모델을 고쳐야.
actual <- testing(penguins_split) %>% select(species)
yardstick::f_meas_vec(truth = actual$species, estimate = preds$.pred_class)
#이거 무조건 1 나와야. 학습한거 때려넣어서 1이 나와야지.
#0.98 정돈 나와야.


## 3. 모델 설정
#모델 3개를 한방에 만들 생각. 이거 왜? 그리드 검색
#앞에 애가 너무 잘맞으면 피팅을. 이 책이 하나도 안가르쳐줘. 걍 쓰심 됩니다!!
#그리드 검색을 시켜서 알고리즘을 보고 판단했을때 좋다 생각하는건 싹다 돌릴거야
#이 짓을 내가 할게 아니라 니가 해. 제일 좋은 파라미터값 갖고와~ 하는거
#머신러닝이 되면 딥러닝도 돼. 매개변수를 이해하기 위해 이걸.
#컴퓨터로 돌려놓기만 하면 최적값 찾아주는애가 있지 않을까?
#있다. 딥러닝. 근데 끝이 안나.
#그래서 회사들이 머신러닝 먼저 하고 이 결과값 들고 딥러닝으로.
#딥러닝은 돈, 시간이 무한대일때나...
#성능을 먼저 보고 딥러닝으로 간다


ranger_model <-
  parsnip::rand_forest(mtry = tune(), min_n = tune()) %>% #랜덤포레스트. 전형적.
  #기본적으로 학자들이 만든거. tune 이제부터 튜닝 하겠다 .니가 난쥬 값 너어줘.
  #min_n은 가지치기 최소 몇번 할건지 물어보는거.
  #밑에 트리 자식을 100개 합시다! 자식들 딥이 1개만 뭔말이야 ㅋㅋ
  set_engine("ranger") %>%
  set_mode("classification")

glm_model <-
  parsnip::multinom_reg(penalty = tune(), mixture = tune()) %>%
  #얘는 패널티가 있네. 알고리즘 따라 매개변수 들어가는게 달라
  #이거 이상 넘어가면 문제있는애야. 패널티 얼만큼? 적절하게.
  #이거 우리가 다 하기가 어렵네. tune 하는건 내가 할 생각이 없는거다
  #
  set_engine("glmnet") %>%
  set_mode("classification")

xgboost_model <-
  parsnip::boost_tree(mtry = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

hardhat::extract_parameter_dials(glm_model, "mixture")
#lm으로 가려고


#Grid가 뭐야? 격자 무늬로 되어있는걸 다 때려넣고있다 
#그리드검색 = 전역검색
#4. Grid 검색
ranger_grid <-
  hardhat::extract_parameter_set_dials(ranger_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_regular(levels = 4)
ranger_grid #표 실행해서 구조를 봐
#뭔가 엄청 때려넣어놨다.
#ggplot 써서 선점도로 함 보자

ranger_grid %>% ggplot(aes(mtry, min_n)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "Ranger: Regular grid for min_n & mtry combinations")
#이게 뭔 그래프여
#mrty(x) min_n(y) 1일때 5일때 40인 경우. 4인경우 다 뺐다(배제)
#나중에 이 4값은 배제하고.
#1,2,3,5중에 뭐가 제일 좋아? 몰라. 하지만 4는 날려
#

glm_grid <-
  parameters(glm_model) %>%
  grid_random(size = 20)
glm_grid

glm_grid %>% ggplot(aes(penalty, mixture)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "GLM: Random grid for penalty & mixture combinations")


xgboost_grid <-
  parameters(xgboost_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_max_entropy(size = 20)
xgboost_grid

xgboost_grid %>% ggplot(aes(mtry, learn_rate)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "XGBoost: Max Entropy grid for LR & mtry combinations")
#너무 아래쪽에 데이터가 몰려있네 이건. y 상한을 어디로 잡을거야? 0.15로 잡을확률이
#근데 난 딥러닝을 할줄몰라. R은 딥러닝이 안돼
#하이퍼 파라미터의 상한가 값은 알아낼수 있을거 같은데...
#이 값을 어떻게 봐야하는지?
#누구한테 상한 하한 보여줘야 할때 레시피
#판다스는 겁나 쉽게 나온다. 레시피를 뽑아낼수 있어야
#(이건 데이터 전처리)
#레시피 하면서 손을 댈거다. 앞에는 데이터 손댄적 없고, 전반적 경향을 보고
#Grid 해보니 이렇다~! 만 봤고, 뭐가 좋은지 몰라 아직
#(파이썬은 결과도 보여줄겨)
#데이터 전처리를 아무도 안했는데 어디서 하고싶어? 앞에 Grid 서치 했잖아
#그걸 반영해서 하면 좋겠거든. step_dummy 하면 자기가 알아서 다 한다.
#준비해서 juice 섞어서 summary 보여줘
#레시피 1까진 이래.


## 5. 2차 데이터 전처리
recipe_base <-
  recipe(species ~ ., data = training(penguins_split)) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) # Create dummy variables (which glmnet needs)

recipe_1 <-
  recipe_base %>%
  step_YeoJohnson(all_numeric())

recipe_1 %>%
  prep() %>%
  juice() %>%
  summary()

#레시피2 얘는 언제 쓰는거야. step_normalize. 데이터를 정규화. 실제 쓸수있는 데이터로
#떨어뜨린다. 이런거 없을때 사람이 개입하는거. 사람이 개입하면 우리가 얼마나 성능을
#올릴수 있는지 모른다. 기계한테 맡길수 있는건 다 맡기고, 어떤 부분만 보강할지.
#빠르게 전처리 하는 방법이다. normalize(R에서 제안하는 가장 현명한 전처리 법)

recipe_2 <-
  recipe_base %>%
  step_normalize(all_numeric())

recipe_2 %>%
  prep() %>%
  juice() %>%
  summary()
#면접때 데이터 전처리 언제 다 할겨. 이걸로 해.
# 3개를 한방에 다~
#쉬는시간

#
#yardstick - 긴 자







#1.8 Metrics

model_metrics <- yardstick::metric_set(f_meas, pr_auc)

#1.9 K-Fold CV () K겹 교차검증. 이래야 공신력 있는 데이터가 만들어져
#셔플을 5번 더 해보자.


data_penguins_3_cv_folds <-
  rsample::vfold_cv( #5겹. 매트릭 정해지고 fold도 정해짐. 3번 실행하면 돼
    v = 5,
    data = training(penguins_split),
    strata = species
  )

#매번 3번을 실행하는게 합리적? 일괄작업 작성
#일의 흐름을 만들어 줄게. add 추가해라. 모델을
#레시피를 넣어서. 원본모델 손안대고.
ranger_r1_workflow <-
  workflows::workflow() %>%
  add_model(ranger_model) %>%
  add_recipe(recipe_1)
glm_r2_workflow <-
  workflows::workflow() %>%
  add_model(glm_model) %>%
  add_recipe(recipe_2)
xgboost_r2_workflow <-
  workflows::workflow() %>%
  add_model(xgboost_model) %>%
  add_recipe(recipe_2)
#랜덤포레스트는 레시피1 쓰는데 grl, xgboost는 레시피2. 그냥 숫자만 바꿨다.
#그리드 서치 쓴다 이제


#1.10.2 튠을 돌린다. 앞까진 다 세팅
#이젠 알고리즘 돌릴때마다 workflow만 바꾸고
#레시피를 전부 변수로 바꿔놨다. 이 데이터 전처리가 더 좋지않나?
#그럼 레시피3. 좋은 레시피 나올대까지 전처리로 계속 레시피 만듦
#계속 돌린다. 회귀 할때까지... 일괄처리 해야함. 일일이 언제 다 해.
#시간을 아껴라
#싸이킥런 데이터 전처리 노멀레이제이션이라던지, one_hot 인코딩 등등
#일괄처리 정의 요건 #1.8 메트릭부터 다시봐
#1. 난 지금부터 이 메트릭으로 볼생각(기준점을 바꿀 생각이 없다)
#F와 
#일괄처리 안하면 엔터엔터 언제 쳐. 시간 줄이는!
#2. 측정값 바뀌면 모든게 바뀌니까 바뀌면 안된다.
#K-fold 값을 사전에 정의.
#1.10.1 이제 워크플로우 한방에.

#그리드 서치 시작
tic("Ranger tune grid training duration ")
ranger_tuned <-
  tune::tune_grid(
    object = ranger_r1_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = ranger_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)


#다 실행 해봐



## Ranger tune grid training duration : 13.44 sec elapsed
tic("GLM tune grid training duration ")
glm_tuned <-
  tune::tune_grid(
    object = glm_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = glm_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)
## GLM tune grid training duration : 1.96 sec elapsed
tic("XGBoost tune grid training duration ")
xgboost_tuned <-
  tune::tune_grid(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)
## XGBoost tune grid training duration : 1.89 sec elapsed

#파이썬보다 빠르다. 코어 다 땡겨써서.
#지금 현제 상태서 레시피 가장 좋은 값들은 다 들고왔다.
#회사에서 데이터 분석용 템플릿 만들어 놓는다. 1시간 정도면 끝
#분석결과 오고. 회사서 할거
#1. 원본 데이터 갖고올수 있는가? select 뭐리문은 할줄 알아야한다.
#2. 전처리가 생각보다 중요하다. NA가 많으면 지우는것도 벌벌 떨린다.
# 0으로 채우는 법도 있고 앞의것을 채우는 법도 있다. 누구는 다 지워라.
# 12월이면 앞에꺼 봐도 되지 않나? 평균해! 중앙값 해! min값 해!
# 이건 사람마다 달라서. 기본은 NA는 지운다.
# one hot, 범주형 등등 필요한데 지금은 쓰지 않는다.
# 레시피 형태로 우린 지정해놓고 쓴다. for loop 돌고 하는건 못쓸때 대비하는것.
# 라이브러리 없을때 할수 있어? ㅇㅇ, 그럼 라이브러리 쓰자.
# 라이브러리 없으면 힘드니까 쓰자! 가 돼야지. 라이브러리 배제해도 할수있어야.
# ont hot 다 갈켜줬어도 레시피 다 때려넣으면 끝..
#3. 워크플로우 짜고, 메트릭 짜고 넣고 돌리면. cpu가 다 땡겨쓰니까 빠르다.




### 결과 확인용
#1.10.3 학습결과확인. 파인튠 설치. 파인튠보다 좋은거 있음 그거 쓰셈.

install.packages("finetune")
library(finetune)


tic("Tune race training duration ")
ft_xgboost_tuned <-
  finetune::tune_race_anova(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = control_race(verbose_elim = TRUE) # 66
  )
toc(log = TRUE)




#그림으로 확인
## Tune race training duration : 2.92 sec elapsed
plot_race(ft_xgboost_tuned) + labs(title = "Parameters Race by Fold")
#지지플롯을 썼으니 더하기로.

#첫번째, 두번째, 세번째 이터레이션 돌때. 
#90이 넘어. 성공.
#3개를 다 봅시다. 깃허브에 올려놓는대.
#note03. 

#이왕 다 만들었으니
#정확도 0.994, 1, 989
#전처리 할때 레시피가 달랐다. Ranger이랑 XGboost랑.
#전처리 과정이 많이 늘어나면
#0.978 vs 0.968 1퍼센트 차이. 둘다 정확도 99% 
#그럼 전처리 과정이 좋은거. 비용 줄이고

#확률이 1로. 100이 나오면 의심한다. 안쓴다. GLM을 안골라.
#(노션)

bind_cols(
  tibble(model = c("Ranger", "GLM", "XGBoost")),
  bind_rows(
    ranger_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arrange(.metric) %>% pivot_wider(names_from = .metric, values_from = best_va),
    glm_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arrange(.metric) %>% pivot_wider(names_from = .metric, values_from = best_va),
    xgboost_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arrange(.metric) %>% pivot_wider(names_from = .metric, values_from = best_va)
  )
)

#전체 모델 확인
#F1 값이 뭔지 아나? F1 matrix 말이야. -> 노션


glm_tuned %>% collect_metrics() # 20 models and 2 metrics

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>% select_best(metric = "f_meas")

glm_tuned %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  select(mean, penalty, mixture) %>%
  pivot_longer(penalty:mixture,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "F1", title = "F1 MetricEvolution")


#1.13 개선하고 싶다
#f1값 제일 높은애 갖고와서~
best_f1 <-
  select_best(xgboost_tuned, metric = "f_meas")
final_model_op1 <-
  finalize_workflow(
    x = xgboost_r2_workflow,
    parameters = best_f1
  )
final_model_op1

#1.14 파인튜닝 한번 더. 최종적 옵티마이저 다 된걸
#원 데이터에 때려넣어
#1퍼센트 올리는데 2000만원 태운다.
#90% 넘어간 상태에서 1% 올리긴 진짜 어렵다.
#취업은 쉽고 취업후 3년은 어렵다. 버티는게 쉽지않다.
#자동화된 워크플로우를 짜놔야 한다. 파이썬에도 있다.
#코랩 돈주고 쓰는 이유가 24시간 돌려놔도 안끊겨

tic("Train final model Tune")
penguins_last_fit <-
  last_fit(final_model_op1,
           penguins_split,
           metrics = model_metrics
  )
toc(log = TRUE)

collect_metrics(penguins_last_fit) %>%
  arrange(.metric)

penguins_last_fit %>%
  collect_predictions() %>%
  conf_mat(truth = species, estimate = .pred_class)

penguins_last_fit %>%
  pull(.predictions) %>%
  as.data.frame() %>%
  filter(.pred_class != species)

install.packages("vip")
library(vip)
final_model_op1 %>%
  fit(data = penguins_df) %>%
  pull_workflow_fit() %>%
  vip(
    geom = "col",
    aesthetics = list(fill = "steelblue")
  ) +
  labs(title = "Feature Importance")

#딥러닝할때 제일 먼저 해야 좋을까요?
#파란 그래프. bill_length_mm을 봐야지요

tic.log() %>%
  unlist() %>%
  tibble()
