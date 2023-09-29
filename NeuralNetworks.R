rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)

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


### Neural Networks

