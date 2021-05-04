# 감정사전 적용하기
library(dplyr)
library(readr)
library(textclean)

# 감정사전에 감정점수를 부여하기 위해 KNU한국어 감정사전을 불러온다.
dic <- read_csv("C:/csv/knu_sentiment_lexicon.csv")

# 감정사전을 적용시킬 미나리 영화와 내일의 기억 댓글 데이터 CSV파일을 불러와 raw_minari_comment 변수에 저장한다.
raw_minari_comment <- read_csv("C:/movie_data/2021-04-29-21-07-44-미나리.csv")
raw_remember_comment <- read_csv("C:/movie_data/2021-04-29-21-27-28-내일의 기억.csv")

# 미나리 영화와 내일의 기억 댓글 데이터를 전처리 시켜준다.
# 중복되는 공백을 제거 후 각 댓글마다 id를 부여한다.
# str_replace_all() 함수를 사용하지 않는 이유는 감정사전에는 특수문자도 포함되어있기때문에 공백 전처리만 해준다.
# replace_html() : html 태그 문자들을 공백으로 변환시켜줌
minari_comment <- raw_minari_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))

remember_comment <- raw_remember_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))

# 토큰화 함수를 사용해 댓글을 띄어쓰기 기준으로 각각 나누어 새로운 변수에 저장한다.
# drop = F 옵션을 사용할 경우 reply를 제거하지 않고 계속 유지해주는 기능을 한다.
word_minari <- minari_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

word_remember <- remember_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

# left_join() 함수를 사용해 댓글 별 감정점수를 부여한다.
# left_join() 함수는 bind_rows()와 반대로 행(옆)으로 합친다.
# 파생변수 mutate()를 통해 감정점수가 존재하지 않는 경우에는 NA로 표기가 되는데 그것을 방지하기 위해 ifelse문을 사용해 NA값을 0으로 만들어준다
word_minari <- word_minari %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_remember <- word_remember %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_minari
View(word_minari %>%
       filter(str_detect(reply, "어려운")))

View(word_remember %>%
       filter(str_detect(reply, "제작진")))
# remember와 minari를 id, reply 별로 구분하여 polarity를 합산해 감정점수를 구한다. 
# ungroup() : 그룹화를 해제하는 기능이며, 이후 분석작업을 그룹별로 처리하지 않게 하기 위해 그룹화를 해제 해주어야 한다.
# id : id로 나누는 이유는 같은 댓글이 있어도 서로다른 댓글로 구분하여 계산하기 위해서이다.
word_remember <- word_remember %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

word_minari <- word_minari %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

word_remember %>%
  select(score, reply) %>%
  arrange(score)

word_minari %>%
  select(score, reply) %>%
  arrange(score)







# score를 기준으로 파생변수인 sentiment 변수를 만들어 댓글의 감정점수가 1보다 크다면 "pos", -1보다 작다면 "neg" 그렇지 않은경우에는 모두 "neu"
remember_count <- word_remember %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

minari_count <- word_minari %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))


# 각각의 감정 값의 빈도 수를 측정하고 빈도율로 %화 시켜 새로운 변수에 저장한다.
remember_count <- remember_count %>%
  count(sentiment) %>%
  mutate(ratio = n / sum(n)*100) 

minari_count <- minari_count %>%
  count(sentiment) %>%
  mutate(ratio = n / sum(n)*100)

remember_count

minari_count

# ggplot() 함수와 geom_col()을 사용해 긍정 댓글와 부정 댓글의 갯수를 막대그래프로 나타낸다. 그래프의 색깔은 감정별로 구분되며, 각 막대위에 빈도 값을 나타내었다.
remember_count %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(y=NULL, title = "내일의 기억 감정분석 결과")

minari_count %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(y=NULL, title = "미나리 감정분석 결과")


# remember_count 변수에 더미 변수를 추가한다.
remember_count$dummy <- 0
# 기존와 동일하게 ggplot()함수와 geom_col()을 활용해 막대그래프를 생성한다. 색깔은 감정별로 다르게 설정한다.
# x축은 구성할 필요가 없으므로 ggplot()의 x축 자리에 더미변수를 넣어준다.
remember_count %>%
  ggplot(aes(dummy, ratio, fill = sentiment)) +
  geom_col() +
  # geom_text() 함수로 그래프 안에 백분율을 추가하고 round()함수로 소수점 둘째자리까지 출력되게한다.  
  geom_text(aes(label = paste0(round(ratio, 1), "%")), # paste0() 문자열을 붙임
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 제거
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) + # x축 눈금 삭제
  labs(title = "내일의 기억 감정별 비율")

minari_count$dummy <- 0
minari_count %>%
  ggplot(aes(dummy, ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "미나리 감정별 비율")
