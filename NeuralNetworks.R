rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)

### Load Cleaned Data

load("SonaData.RData")

### Exploratory Data Analysis

words = tokenize_words(sona$speech)

## Plot the number of words against the year

edaPlotData = data.frame("Year" = sona$year,
                         "nWords" = sapply(words, length),
                         "President" = sona$president_13)

ggplot(edaPlotData, aes(x = Year, y = nWords, color = President, shape = President)) +
  geom_point(size = 7) +
  xlab("Year") + ylab("Number of Words") +
  scale_x_discrete(name = "Year", 
                   breaks = c("1994","1999","2004", "2009", 
                              "2014", "2019", "2023")) +
  scale_y_continuous(name = "Number of Words", 
                     breaks = c(3000, 6000, 9000)) +
  scale_shape_manual(values = rep(18, 6)) +
  theme_bw(base_size = 12)

## Plot the Average Sentence Length over time

avgSentenceLength = c()

for (j in 1:36){
  
  sentWords = sapply(tokenize_sentences(sona$speech[j]), tokenize_words)
  len = length(sentWords)
  sentL = c()
  
  for (k in 1:len){
    sentL[k] = sapply(sentWords[k], length)
  }
  
  avgSentenceLength[j] = mean(sentL)
}

edaPlotData1 = data.frame("Year" = sona$year,
                          "Length" = avgSentenceLength,
                          "President" = sona$president_13)

ggplot(edaPlotData1, aes(x = Year, y = Length, color = President, shape = President)) +
  geom_point(size = 7) +
  xlab("Year") + ylab("Average Sentence Length") +
  scale_x_discrete(name = "Year", 
                   breaks = c("1994","1999","2004", "2009", 
                              "2014", "2019", "2023")) +
  scale_y_continuous(breaks = c(20, 30, 40)) +
  scale_shape_manual(values = rep(18, 6)) +
  theme_bw(base_size = 12)

## Highlight the most commonly used words for each President

unnest_reg = "[^\\w_#@']"

speechWords = as.tibble(sona) %>%
  rename(president = president_13) %>%
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(president, year, word) 

# Mandela's most commonly used words

speechWords %>%
  filter(president == "Mandela") %>%
  count(word, sort = TRUE) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col(fill = "pink", col = "black") + coord_flip() + 
  xlab('') + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(n >= 115)

# Mbeki's most commonly used words

speechWords %>%
  filter(president == "Mbeki") %>%
  count(word, sort = TRUE) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col(fill = "pink", col = "black") + coord_flip() + 
  xlab('') + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(n >= 235)

# Zuma's most commonly used words

speechWords %>%
  filter(president == "Zuma") %>%
  count(word, sort = TRUE) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col(fill = "pink", col = "black") + coord_flip() + 
  xlab('') + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(n >= 169)

# Ramaphosa's most commonly used words

speechWords %>%
  filter(president == "Ramaphosa") %>%
  count(word, sort = TRUE) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) + geom_col(fill = "pink", col = "black") + coord_flip() + 
  xlab('') + ylab("Times Used in Speeches") +
  theme_bw(base_size = 12) +
  gghighlight(n >= 150)


### Models

# Separate speeches into sentences

speechSentences = as.tibble(sona) %>%
  rename(president = president_13) %>%
  unnest_tokens(sentences, speech, token = "sentences") %>%
  select(president, year, sentences) %>%
  mutate(sentID = row_number())

wordsWithSentID = speechSentences %>% 
  unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(sentID, president, year, word) 

## Bag-Of-Words Model

bagWords = wordsWithSentID %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(200, wt = n) %>%
  select(-n) 

speechTDF = speechSentences %>%
  inner_join(wordsWithSentID) %>%
  group_by(sentID, word) %>%
  count() %>%  
  group_by(sentID) %>%
  mutate(total = sum(n)) %>%
  ungroup()

# left_join got confused just using president as a variable 
# due to there being other president variables in speechTDF
# change to presidentName

bagWords = speechTDF %>%
  select(sentID, word, n) %>% 
  pivot_wider(names_from = word, values_from = n, values_fill = 0) %>%
  left_join(speechSentences %>% 
              rename(presidentName = president) %>% 
              select(sentID, presidentName), by = "sentID") %>%
  select(sentID, presidentName, everything())

table(bagWords$presidentName)

# remove deKlerk and Motlanthe
# use 1600 sentences from each of the four presidents

sampledBOW = bagWords %>% 
  filter(presidentName == "Mandela" | presidentName == "Mbeki" | 
           presidentName == "Ramaphosa" | presidentName == "Zuma") %>%
  group_by(presidentName) %>% 
  slice_sample(n = 1500) %>% 
  ungroup()

table(sampledBOW$presidentName)

set.seed(2023)

trainingIDS = sampledBOW %>% 
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

trainingSentences = sampledBOW %>%
  right_join(trainingIDS, by = "sentID") %>%
  select(-sentID)

tAndVSentences = sampledBOW %>%
  anti_join(trainingIDS, by = "sentID")

validationIDS = tAndVSentences %>%
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

validationSentences = tAndVSentences %>%
  right_join(validationIDS, by = "sentID") %>%
  select(-sentID)

testSentences = tAndVSentences %>%
  anti_join(validationIDS, by = "sentID") %>%
  select(-sentID)

 ## TF-IDF Model

speechWords %>%
  count(president, word, sort = TRUE) %>%
  bind_tf_idf(word, president, n) %>%
  arrange(desc(tf_idf))

## Neural Networks


  
