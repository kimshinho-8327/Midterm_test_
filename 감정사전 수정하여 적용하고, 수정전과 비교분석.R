View(word_remember %>%
       filter(!score==0) %>%
       arrange(desc(score)))
word_remember %>%
  filter(!score == 0 & str_detect(reply, "슬프다")) 

dic %>%
  filter(word == "어려운")

# 감정 사전 수정하기
# mutate() 함수를 사용해 "어려운", "어려움", "어지럽게"의 감정점수를 2, 2, 1 점으로 변경한다.
new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("어려운", "어려움"), 2, ifelse(word == "어지럽게", 1, polarity)))
new_dic %>% filter(word %in% c("어려운", "어려움", "어지럽게"))

# 수정한 사전으로 감정점수 부여하기
# 댓글을 단어 기준으로 토큰화한 word_minari에서 앞에서 부여한 감정 점수 polarity를 제거한 다음 수정한 감정 사전을 이용해 감정 점수를 부여한뒤 NA를 0으로 변환한다.
word_minari <- word_minari %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 기존과 동일하게 댓글별 감정점수를 부여한다.
word_minari <- word_minari %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
# score가 1이상이면 "pos", -1이하면 "neg", 그렇지 않으면 "neu"를 부여한다.
minari_count <- word_minari %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
# 각 감정의 비율값도 추가한다.
minari_count <- minari_count %>%
  count(sentiment) %>%
  mutate(ratio = n / sum(n)*100)




# 감정 사전 수정하기
dic %>%
  filter(word == "힘든") %>%
  print(n = Inf)
# mutate() 함수를 사용해 "아픈", "힘든"의 감정점수를 2, 2점으로 변경한다.
new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("아픈", "힘든"), 2, polarity))
new_dic %>% filter(word %in% c("아픈", "힘든"))

dic %>% filter(word %in% c("아픈", "힘든"))

# 수정한 사전으로 감정점수 부여하기
# 댓글을 단어 기준으로 토큰화한 word_remember에서 앞에서 부여한 감정 점수 polarity를 제거한 다음 수정한 감정 사전을 이용해 감정 점수를 부여한뒤 NA를 0으로 변환한다.
word_remember <- word_remember %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 기존과 동일하게 댓글별 감정점수를 부여한다.
word_remember <- word_remember %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
# score가 1이상이면 "pos", -1이하면 "neg", 그렇지 않으면 "neu"를 부여한다.
remember_count <- word_remember %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
# 각 감정의 비율값도 추가한다.
remember_count <- remember_count %>%
  count(sentiment) %>%
  mutate(ratio = n / sum(n)*100)