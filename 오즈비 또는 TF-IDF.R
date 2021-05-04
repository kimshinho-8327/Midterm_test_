
# 오즈비 구하기

# 오즈비를 구하기 위해 "tidyr"이라는 라이브러리를 추가해준다.
# 존재하지 않을 시 install.packages("tidyr") 설치
library(tidyr)

# 오즈비에 사용할 영화별 단어 빈도를 저장할 movie_count 변수를 생성한다.
# 기준은 내림차순
movie_count <- movie %>%
  count(movie, word, sort = T)
movie_count
# 위에서 생성한 movie_count를 pivot_wider()를 사용해 long form의 형태에서 wideform 형태로 변환 시켜준다.
# names_from : 변수명으로 만들 값이 들어 있는 변수
# values_from : 변수에 채워 넣을 값이 들어 있는 변수
# values_fill : NA값을 0으로 만들어준다.
movie_wide <- movie_count %>%
  pivot_wider(names_from = movie,
              values_from = n,
              values_fill = list(n=0))
movie_wide

# wide_form 형태로 저장되어 있는 데이터 변수인 movie_Wide에 파생변수인 mutate() 함수를 사용해 각각의 오즈비를 계산하여 "ratio_remember"와 "ratio_minari" 변수에 대입한다.
# 오즈비 계산법 : (n+1) / (total+1)
movie_wide <- movie_wide %>%
  mutate(ratio_remember = (remember + 1) / (sum(remember + 1)),
         ratio_minari = (minari + 1) / (sum(minari + 1)))

# 텍스트 별 상대적인 단어의 비중을 구하기 위해 ratio_remember / ratio_minari로 나누어 준다.
# 텍스트에서 오즈비가 1보다 클 경우에는 remember의 상대적인 비중이 큰 경우이고 반대로 1보다 작을경우 minari의 상대적인 비중이 큰 경우이다.
# 1과 가까울 경우 비중이 비슷한 경우이다.
movie_wide <- movie_wide %>%
  mutate(odds_ratio = ratio_remember / ratio_minari)

# arrange() 함수에 (-) 붙여 내림차순으로 정렬한다.
movie_wide %>%
  arrange(-odds_ratio)

# arrange() 함수를 사용하여 오름차순으로 정렬한다.
movie_wide %>%
  arrange(odds_ratio)

# rank() 함수를 사용하여 오즈비가 가장 큰 값 10개와 가장 작은 값 10개를 추출하여 movie_wide_top10의 변수에 저장한다.
movie_wide_top10 <- movie_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

# movie_wide_top10 데이터가 모두 출력한다.
movie_wide_top10 %>%
  print(n=Inf)

# 비중이 큰 영화를 나타낸 변수를 추가하기 위해 파생 변수인 mutate()와 ifelse문을 사용해 1보다 클 경우 "remember" 그렇지 않으면 "minari"값을 가져온다.
movie_wide_top10 <- movie_wide_top10 %>%
  mutate(movie_category = ifelse(odds_ratio > 1, "remember", "minari"),
         n = ifelse(odds_ratio > 1, remember, minari))

# 위에서 저장한 각각의 오즈비 데이터를 시각화 하기 위해 ggplot() 과 geom_col() 함수를 사용해 막대그래프 형태로 출력해준다.
# reorder_within() 함수로 그래프별로 출력해주며 색깔은 영화에 따라 다르게 출력한다.
movie_wide_top10 %>%
  ggplot(aes(reorder_within(word, n, movie_category), n, fill = movie_category)) +
  geom_col() +
  # 시각화를 용이하게 하기위해 coord_flip()으로 x축과 y축의 위치를 변경해준다.
  coord_flip() +
  # 영화 별로 나누어 그래프를 출력해준다.
  facet_wrap(~movie_category, scales = "free") +
  # x에 표시된 항목을 제거해준다.  
  scale_x_reordered() +
  # 축에 설정된 글자를 제거한다.  
  labs(x = NULL, y = NULL) +
  # 막대 그래프 옆에 n값을 출력해준다.  
  geom_text(aes(label = n), hjust = -0.3) +
  # 그래프에 표시된 빈도 수가 짤려서 보이지 않도록 막대와 그래프 경계의 간격을 넓혀준다.  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.5)))


movie_wide %>%
  arrange(abs(1-odds_ratio)) %>%
  head(10)

movie_wide %>%
  filter(remember >= 20 & minari >= 20) %>%
  arrange(abs(1-odds_ratio)) %>%
  head(10)

# 위에서 생성해둔 movie_count 변수를 사용하여 tidytext 라이브러리에 내장함수인 bind_tf_idf()를 적용하여 TF-IDF를 구할 수 있다. 그리고 tf_idf의 값을 기준으로 내림차순 정렬한다.
# term : 단어
# document : 텍스트 구분 기준
# n : 단어 빈도
raw_frequency <- movie_count %>%
  bind_tf_idf(term = word,
              document = movie,
              n = n) %>%
  arrange(-tf_idf)

raw_frequency %>%
  arrange(tf_idf)

raw_frequency %>%
  filter(movie == "remember") %>%
  arrange(-tf_idf)


raw_frequency %>%
  filter(movie == "minari") %>%
  arrange(-tf_idf)
