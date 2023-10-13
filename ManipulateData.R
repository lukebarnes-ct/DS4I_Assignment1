
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
