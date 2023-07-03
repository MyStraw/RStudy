# 미국 나스닥 데이터 분석

## 주제 : 확정해야 됨

## 0. 패키지 불러오기
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)


library(corrr)
library(rstatix)
library(prophet)
library(astsa)
library(forecast)

library(sysfonts)
library(showtext)

## 1. 데이터 프레임 작성
#파일 목록을 다 들고 와야됨
files<-list.files(path="data/nasdaq_stock/")
# 들고온 파일 목록을 다 읽어서, 데이터프레임. for loop 선호 안하는데 써야하나?

# 이러면 메모리 터짐. 20000개나 되는 데이터를 어떻게?
# 자기 데이터도 못만지는 애는 필요없으니 일부러 문제를 이렇게 냄
# 목적성이 df 만드는거니까..
#for i =? 안돼  read_csv()일단 써보자. for loop 돌면 안될거 같애. 
#루프 돌면 그럼 R 버려야돼.
stocks <- read_csv(paste0("data/nasdaq_stock/", files), id="name") %>%
  mutate(name = gsub("data/nasdaq_stock/", "", name),
         name = gsub("\\.csv", "", name)) %>%
  rename_with(tolower) #소문자로 바꿔볼려는데 왜 안바뀌지? 받아야한다. stocks에 받게 해보자
stocks

#데이터 프레임을 결합
df <-read_csv("data/nasdaq_stock_names.csv")
df

stocks <- stocks %>%
  inner_join(df, by = c("name" = "stock_symbol"))

stocks
## 2. 시계열 데이터 시각화

end_labels <- (stocks %>% #전체 주식 데이터 중에서 제일 큰애들. 
  group_by(company) %>%
  filter(date == max(date)) %>%
  arrange(-open) %>% #개장가 기준으로 다시 sorting
  select(open, company))[c(1:3, 12:14),] #open과 컴퍼니를 이용해서 갖고올겨
  #select(open, company)) #이렇게 하면 전체가 나옴

# 좀 더 해봐요
stocks %>% # 보내기
  ggplot(aes(date,open)) + #백그라운드. x축에 데이터 깔고
  geom_line(aes(color = company)) + #지오메트릭, company 수 알아서 칼라 정해주셈
  scale_y_continuous(sec.axis = sec_axis(~., breaks = end_labels$open,
                                         labels = end_labels$company)) +
  #y 연속범위에서
  #개별축을 다 집어넣고, 시작가에 있는 부분에 레이블값 넣어서 컴퍼니 6개의 높은애와
  #낮은애를 표시해줘
  scale_x_date(expand = c(0,0)) +
  labs(x="", y="Open", color="", title = "주요 회사의 시작가격")+
  theme(legend.position = "none")


(stocks %>%
    filter(company %in% end_labels$company[1:3]) %>%
    ggplot(aes(date, open)) +
    geom_line(aes(color = company)) +
    facet_wrap(~company) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Top 3", x = "")) /
  (stocks %>%
     filter(company %in% end_labels$company[-(1:3)]) %>%
     ggplot(aes(date, open)) +
     geom_line(aes(color = company)) +
     facet_wrap(~company) +
     theme_bw() +
     theme(legend.position = "none") +
     labs(title = "Bottom 3", x = ""))


#시계열

aapl <- stocks %>%
  filter(name == "AAPL") %>%
  select(ds = date, y = open) 
#시계열은 x가 고정이다. 시간. y는 시작가 정하고.
#y에 여러개 다 넣어도 되긴 하지만, 우린 아직 능력이..
#1개만 보자. 

(aapl %>% 
    mutate(diff = c(NA, diff(y))) %>% 
    ggplot(aes(ds, diff)) + 
    geom_point(color = "steelblue4", alpha = 0.7) + #점도표~
    labs(y = "Difference", x = "Date",
         title = "One Day Returns")
) /
  (aapl %>% 
     mutate(diff = c(NA, diff(y))) %>% 
     ggplot(aes(diff)) +
     geom_histogram(bins = 50, fill = "steelblue4", color = "black")
  )

m_aapl <- prophet(aapl)
forecast <- predict(m_aapl, make_future_dataframe(m_aapl, periods = 140))
plot(m_aapl, forecast)
prophet_plot_components(m_aapl, forecast)



## 3. 시계열 데이터 분리

## 4. 종가를 예측

##1번.
#df 작성해야 하는데... 난관. 파일을 합쳐야한다. 파이썬으로 할줄알아야해.
#어떤 폴더에 있는 데이터를 다 들고와서 df로 만들고 결합해서 통으로 만들기.

