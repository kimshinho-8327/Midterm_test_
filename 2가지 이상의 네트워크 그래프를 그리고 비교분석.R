
# 필수 라이브러리와 미나리 댓글과 내일의 기억 댓글을 각각 불러온다.
library(readr)
library(dplyr)
library(stringr)
raw_minari_comment <- read_csv("C:/movie_data/2021-04-29-21-07-44-미나리.csv")
raw_remember_comment <- read_csv("C:/movie_data/2021-04-29-21-27-28-내일의 기억.csv")

# 전처리 과정
minari_comment <- raw_minari_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())
remember_comment <- raw_remember_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())

library(tidytext)


library(KoNLP)
library(tidyr)
# 기존에는 extractNoun()을 통해 명사만 추출하였지만 여기서는 다른 품사도 추출해야 하기 때문에 SimplePos22()를 사용한다.
minari_comment <- minari_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)
remember_comment <- remember_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)

library(tidyr)

# separate_rows() 함수는 정규표현식에 따라 텍스트를 여러 행으로 나누는  기능을 한다. 품사별로 구분하기 위해 sep 옵션에 "[+]"를 주어 +가 등장할때마다 행을 나눈다.
minari_pos <- minari_comment %>%
  separate_rows(word, sep = "[+]")

remember_pos <- remember_comment %>%
  separate_rows(word, sep = "[+]")

# 명사추출

noun_minari <- minari_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun_remember <- remember_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

# 동사, 형용사 추출

pvpa_minari <- minari_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))

pvpa_remember <- remember_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%
  mutate(word = str_replace(word, "/.*$", "다"))

# 결합
comment_minari <- bind_rows(noun_minari, pvpa_minari) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment_remember <- bind_rows(noun_remember, pvpa_remember) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)


library(widyr)


pair_minari <- comment_minari %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

pair_remember <- comment_remember %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

library(tidygraph)


set.seed(1234)
graph_comment_minari <- pair_minari %>%
  filter(n >= 4) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1235)
graph_comment_remember <- pair_remember %>%
  filter(n >= 4) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_comment_minari
library(showtext)
library(ggraph)

font_add_google(name = "Black Han Sans", family = "bhs")
showtext_auto()

set.seed(1234)
ggraph(graph_comment_minari, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(5, 15)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "bhs") + # 폰트
  theme_graph() # 배경삭제 

set.seed(1235)
ggraph(graph_comment_remember, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(5, 15)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "bhs") + # 폰트
  theme_graph() # 배경삭제 





# 파이 계수 구하기
# 위에서 사용한 comment_minari / comment_remember를 이용하여 파이계수를 구한다. 
# 우선 add_count()로 단어 빈도를 추가한다음 4회 이상 사용된 단어를 추출한다. 그리고 widyr 패키지의 pairwise_cor()함수를 이용해 파이계수를 구한다.

minari_cors <- comment_minari %>%
  add_count(word) %>%
  filter(n >= 4) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)

remember_cors <- comment_remember %>%
  add_count(word) %>%
  filter(n >= 4) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
remember_cors


set.seed(1234)
graph_cors_minari <- minari_cors %>%
  filter(correlation >= 0.3) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

set.seed(1235)
graph_cors_remember <- remember_cors %>%
  filter(correlation >= 0.3) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_cors_minari
set.seed(1234)
ggraph(graph_cors_minari, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation, # 엣지 명암
                     edge_width = correlation), # 엣지 두께
                 show.legend = F) + # 범례 삭제
  scale_edge_width(range = c(1, 4)) + # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "bhs") +
  theme_graph()

set.seed(1235)
ggraph(graph_cors_remember, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = F) +
  scale_edge_width(range = c(1, 4)) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "bhs") +
  theme_graph()

?paste

# 엔그램 그래프
# 엔그램 그래프를 만들기 위해 우선 한 댓글이 하나의 행이 되도록 id별로 word를 결합한다.
line_minari_comment <- comment_minari %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

line_remember_comment <- comment_remember %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

bigram_minari <- line_minari_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

bigram_remember <- line_remember_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

# 바이그램 분리하기
bigram_separated_minari <- bigram_minari %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_separated_minari
bigram_separated_remember <- bigram_remember %>%
  separate(bigram, c("word1", "word2"), sep = " ")




# 단어쌍 빈도 구하기

pair_bigram_minari <- bigram_separated_minari %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram_minari

pair_bigram_remember <- bigram_separated_remember %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram_remember

# 네트워크 그래프 데이터 만들기
set.seed(1236)
graph_bigram_minari <- pair_bigram_minari %>%
  filter(n >= 2) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))


set.seed(1237)
graph_bigram_remember <- pair_bigram_remember %>%
  filter(n >= 2) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))




# 네트워크 그래프 만들기

set.seed(1236)
ggraph(graph_bigram_minari, layout = "fr") + # 레이아웃 
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 reple = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "bhs") + # 폰트
  theme_graph() # 배경 삭제


set.seed(1237)
ggraph(graph_bigram_remember, layout = "fr") + # 레이아웃 
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 reple = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "bhs") + # 폰트
  theme_graph() # 배경 삭제
