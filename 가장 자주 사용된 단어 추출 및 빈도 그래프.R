# 데이터 전처리과정 - 필수라이브러리 부착
# tidyverse : tidyverse, ggplot2, readr, dplyr, forcats 라이브러리 포함
library(tidyverse)
#  'dplyr', 'ggplot2' 및 기타 정리 도구를 사용한 텍스트 마이닝rlsmd
library(tidytext)


# 미나리 댓글의 텍스트 파일을 불러와 "tibble" 형태로 저장한뒤 파생변수 생성 함수인 mutate()를
# 통해 새로운 변수인 movie를 생성한다.(값은 minari)
minari_review_raws <- readLines("C:/movie_data/2021-04-29-21-07-44-미나리.txt") %>%
  as_tibble() %>% 
  mutate(movie = "minari")

# 내일의 기억 댓글의 텍스트 파일을 불러와 "tibble" 형태로 저장한뒤 파생변수 생성 함수인 mutate()를
# 통해 새로운 변수인 movie를 생성한다.(값은 remember)
remember_review_raws <- readLines("C:/movie_data/2021-04-29-21-27-28-내일의 기억.txt") %>%
  as_tibble() %>%
  mutate(movie = "remember")

# bind_rows() 함수를 통해 열 형태로 두개의 데이터 합친뒤 select() 함수로 변수의 위치를 바꾸어준다.
movie_raws <- bind_rows(minari_review_raws, remember_review_raws) %>%
  select(movie, value)

# 위에서 합친 movie_raws데이터를 전처리 하여 movie라는 새로운 변수에 저장한다.
# str_replace_all(value, "[^가-힣], " ") : value안에 값에 존재하는 한글이 아닌 데이터를 모두 공백으로 변환시킨다.
# str_squish() : 연속된 공백을 제거한다.(한 번의 공백으로 만들어줌)
movie <- movie_raws %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
# movie 변수안에 있는 데이터를 unnest_tokens() 함수를 통해 띄어쓰기 기준으로 데이터를 분리한다.
# str_count() 함수를 사용해 단어 개수가 2개 이상인것만 추출한다.
# str_count() : 빈도 측정
# word : 띄어쓰기, sentences : 문장, characters : 글자
movie <- movie %>%
  unnest_tokens(input = value,
                output = word,
                token = "words") %>%
  filter(str_count(word) >= 2)

# movie 변수를 group_by() 함수로 영화를 기준으로 그룹화 하여 나눈뒤, 각 영화에 사용된 단어 빈도를 계산한다.(count(), sort = T 사용시 내림차순) 그리고 slice_max() 함수를 통해 영화별 단어 빈도수를 10개씩 각각 추출한다.
top10 <- movie %>%
  group_by(movie) %>%
  count(word, movie, sort = T) %>%
  slice_max(n, n = 10, with_ties = F)

top10 %>% print(n = Inf)


top10 %>%
  # ggplot() 함수를 통해 x축은 단어, y축은 빈도 수로 매핑을 하여 geom_col()을 사용해 막대 그래프 형태로 출력해준다.
  # reorder_within : 그래프별로 축 정렬하는 기능(x:축, by:정렬, within:그래프를 나누는 기준)
  # fill : 영화의 종류별로 막대 그래프의 색깔을 채운다.(2가지 색)
  ggplot(aes(reorder_within(word, n, movie), n, fill = movie)) +
  geom_col() +
  # facet_wrap()을 사용해 변수의 항목별로 그래프를 만듦(영화를 기준으로나눔) 그리고 추가 옵션으로 scales="free"로 x축과 y축의 크기를 그래프별로 정한다.
  facet_wrap(~movie, scales = "free") +
  # coord_flip()으로 글자가 겹치는 것을 방지하기 위해 x축과 y축의 위치를 반대로 출력한다.
  coord_flip() +
  # scale_x_reordered() 함수는 x에 표시된 항목을 제거시켜준다.
  scale_x_reordered() +
  # labs() 함수는 축의 이름이나 제목을 설정해주는 기능으로 x와 y의 이름은 NULL로 지정하여 글자를 없애준다.
  labs(x = NULL, y = NULL) +
  # geom_text() 함수는 그래프 옆에 빈도 수를 표시해준다.(hjust는 위치 설정)
  geom_text(aes(label = n), hjust = -0.3) +
  # 그래프에 표시된 빈도 수가 짤려서 보이지 않도록 막대와 그래프 경계의 간격을 넓히는 기능을 한다.
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.5)))
