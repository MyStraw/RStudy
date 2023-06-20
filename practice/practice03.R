#tidverse
#palmerpenguins

library("tidyverse")
library("palmerpenguins")
install.packages("palmerpenguins")


##1.데이터 확인

glimpse(penguins)
#NA -> 결측치. 일단 삭제 : 베이스 라인 구축한다.(입력하지 않았거나 고의로 딴거)
t(map_df(penguins, ~sum(is.na(.)))) #na가 "."인 애들. 이거 한줄 외워
#비어있는 애들 고르는거.
#354명중 2명은 모르겠네.
#11/354 이정도면 NA값 적네. 써도되겠다. 크리티컬 하지 않다.

####확인사항####
#데이터의 사이즈를 먼저 보고. 적네? for 돌아도 되겠네요
#NA 11개면 적네요
#다른 변수 하나 놓고, 플롯 데이터 왜 만드냐?
#중요할수도 있잖아. 원 데이터는 손대지 않는다
###############

plot_data<-penguins %>%
  drop_na()

t(map_df(plot_data, ~sum(is.na(.))))

##2. 데이터 구성(이미지 표현, 종의 분포현황)
#내가 가진거 이거니까 이거 써
count_data <- plot_data %>%
  group_by(species) %>%
  #count() #이것도 아래랑 같다.
  tally() #R의 tidverdy만 알면. count는 딴것도

count_data

#걍 ggplot만 하니까 허연그림만 나오네.
#data만 받은거.
#aes로 축 넣고.
#bar를 원해. 나머지 아래 애들은 색깔, 축이름 등등
#그래프의 3개가 기본. 데이터+축+어떤형태=>그래프는 나온다
#4장5장 수준의 그래프는 이정도로만 하고도 남는다.
#더하기는 레이어. 쌓아서 올라가는중. 노션에 그림 봐.

ggplot(count_data) + 
  aes(x = species, fill = species, weight = n) +
  geom_bar() + #여기까지 3줄은 할줄 알아야
  coord_flip() +#여기부턴 꾸미는거
  scale_fill_manual(
    values = c(Adelie = "#0D0887",
               Chinstrap = "#CA4778",
               Gentoo = "#F0F921")
  ) +
  labs(
    x = "펭귄 종류",
    y = "개체 수 (마리)",
    title = "팔머 펭귄 종별 개체 수",
    caption = "Esquisse 패키지를 통한 plotting",
    fill = "펭귄 종류"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")




