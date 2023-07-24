library(tidyverse)

# 데이터 읽기
data <- read.csv("C:/RStudy/subway_project/부산교통공사_부산도시철도 운행 정보_20230710.csv", fileEncoding = "CP949")

# 필요한 컬럼만 추출
filtered_data <- data %>% select(운행구간기점명, 운행구간종점명, 요일구분, 운행구간정거장, 정거장도착시각, 정거장출발시각)


check_rows <- function(row) {
  length(unlist(strsplit(row['운행구간정거장'], '\\+'))) == length(unlist(strsplit(row['정거장도착시각'], '\\+'))) && length(unlist(strsplit(row['운행구간정거장'], '\\+'))) == length(unlist(strsplit(row['정거장출발시각'], '\\+')))
}

# 일치하지 않는 행 확인
filtered_data[!apply(filtered_data, 1, check_rows), ]

# 일치하지 않는 행 제거
#filtered_data <- filtered_data[apply(filtered_data, 1, check_rows), ]

# 이후에 separate_rows() 함수 적용


# '+'로 연결된 문자열 분리 및 처리
new_data <- filtered_data %>% 
  separate_rows(운행구간정거장, 정거장도착시각, 정거장출발시각, sep = "\\+") %>%
  separate(운행구간정거장, into = c("station_num", "운행구간정거장"), sep = "-", convert = TRUE) %>%
  separate(정거장도착시각, into = c("arrival_num", "정거장도착시각"), sep = "-", convert = TRUE) %>%
  separate(정거장출발시각, into = c("departure_num", "정거장출발시각"), sep = "-", convert = TRUE) %>%
  select(-station_num, -arrival_num, -departure_num)

# 결과 확인
head(new_data)

write.csv(new_data, file = "drivinginfo_수정.csv", row.names = FALSE, fileEncoding = "CP949")

