
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(rpart)
library(randomForest)
library(tictoc)

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

speechWords = as_tibble(sona) %>%
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

speechSentences = as_tibble(sona) %>%
  rename(president = president_13) %>%
  unnest_tokens(sentences, speech, token = "sentences") %>%
  select(president, year, sentences) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "’", "'")) %>%
  mutate(sentences, sentences = str_replace_all(sentences, "'", "")) %>%
  mutate(sentences, sentences = str_remove_all(sentences, "[0-9]")) %>%
  mutate(sentID = row_number())

wordsWithSentID = speechSentences %>% 
  unnest_tokens(word, sentences, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>%
  filter(!word %in% stop_words$word) %>%
  select(sentID, president, year, word) 

#### Bag-Of-Words Model

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
# use 1500 sentences from each of the four presidents

sampledBOW = bagWords %>% 
  filter(presidentName == "Mandela" | presidentName == "Mbeki" | 
           presidentName == "Ramaphosa" | presidentName == "Zuma") %>%
  group_by(presidentName) %>% 
  slice_sample(n = 1500) %>% 
  ungroup()

sampledBOW

table(sampledBOW$presidentName)

set.seed(2023)

trainingIDSBOW = sampledBOW %>% 
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

trainingSentencesBOW = sampledBOW %>%
  right_join(trainingIDSBOW, by = "sentID") %>%
  select(-sentID)

# if else repeat while function for in next break

trainingSentencesBOW = trainingSentencesBOW[, -c(495, 776, 1651)]

tAndVSentencesBOW = sampledBOW %>%
  anti_join(trainingIDSBOW, by = "sentID")

validationIDSBOW = tAndVSentencesBOW %>%
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

validationSentencesBOW = tAndVSentencesBOW %>%
  right_join(validationIDSBOW, by = "sentID") %>%
  select(-sentID)

validationSentencesBOW = validationSentencesBOW[, -c(495, 776, 1651)]

testSentencesBOW = tAndVSentencesBOW %>%
  anti_join(validationIDSBOW, by = "sentID") %>%
  select(-sentID)

testSentencesBOW = testSentencesBOW[, -c(495, 776, 1651)]

#### TF-IDF Model

tf.idf = speechTDF %>%
  bind_tf_idf(word, sentID, n) %>% 
  select(sentID, word, tf_idf) %>%
  pivot_wider(names_from = word, 
              values_from = tf_idf, 
              values_fill = 0) %>%  
  left_join(speechSentences %>% 
              rename(presidentName = president) %>% 
              select(sentID, presidentName), by = "sentID")

# remove deKlerk and Motlanthe
# use 1500 sentences from each of the four presidents

sampled.TF.IDF = tf.idf %>% 
  filter(presidentName == "Mandela" | presidentName == "Mbeki" | 
           presidentName == "Ramaphosa" | presidentName == "Zuma") %>%
  group_by(presidentName) %>% 
  slice_sample(n = 1500) %>% 
  ungroup()

table(sampled.TF.IDF$presidentName)

set.seed(2023)

trainingIDS.TF.IDF = sampled.TF.IDF %>% 
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

trainingSentences.TF.IDF = sampled.TF.IDF %>%
  right_join(trainingIDS.TF.IDF, by = "sentID") %>%
  select(-sentID)

# if else repeat while function for in next break
trainingSentences.TF.IDF = trainingSentences.TF.IDF[, -c(494, 775, 1650)]

tAndVSentences.TF.IDF = sampled.TF.IDF %>%
  anti_join(trainingIDS.TF.IDF, by = "sentID")

validationIDS.TF.IDF = tAndVSentences.TF.IDF %>%
  group_by(presidentName) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() %>%
  select(sentID)

validationSentences.TF.IDF = tAndVSentences.TF.IDF %>%
  right_join(validationIDS.TF.IDF, by = "sentID") %>%
  select(-sentID)

validationSentences.TF.IDF = validationSentences.TF.IDF[, -c(494, 775, 1650)]

testSentences.TF.IDF = tAndVSentences.TF.IDF %>%
  anti_join(validationIDS.TF.IDF, by = "sentID") %>%
  select(-sentID)

testSentences.TF.IDF = testSentences.TF.IDF[, -c(494, 775, 1650)]

save(speechSentences, bagWords, speechTDF,
     sampledBOW, sampled.TF.IDF,
     trainingSentencesBOW, trainingSentences.TF.IDF,
     validationSentencesBOW, validationSentences.TF.IDF,
     testSentencesBOW, testSentences.TF.IDF,
     file = "SetSentenceData.RData")

save(trainingSentencesBOW, trainingSentences.TF.IDF,
     validationSentencesBOW, validationSentences.TF.IDF,
     testSentencesBOW, testSentences.TF.IDF,
     file = "dataForNN.RData")
