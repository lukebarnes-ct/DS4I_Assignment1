rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)
library(keras)
library(reticulate)
library(tensorflow)

### Load Different Datasets

load("dataForNN.RData")
set.seed(2023)

## Neural Networks

# Pre-processing the Target Variable

presTarget.train.BOW = as.integer(factor(trainingSentencesBOW$presidentName)) - 1
presTarget.train.TF.IDF = as.integer(factor(trainingSentences.TF.IDF$presidentName)) - 1

presTarget.val.BOW = as.integer(factor(validationSentencesBOW$presidentName)) - 1
presTarget.val.TF.IDF = as.integer(factor(validationSentences.TF.IDF$presidentName)) - 1

presTarget.test.BOW = as.integer(factor(testSentencesBOW$presidentName)) - 1
presTarget.test.TF.IDF = as.integer(factor(testSentences.TF.IDF$presidentName)) - 1

# Removing President Variable from Data sets and scale Bag-Of-Words Data

presData.train.BOW = trainingSentencesBOW %>% 
  select(-presidentName) %>% 
  scale()

presData.train.TF.IDF = trainingSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()

presData.val.BOW = validationSentencesBOW %>% 
  select(-presidentName) %>%
  scale(center = attr(presData.train.BOW, "scaled:center"), 
        scale = attr(presData.train.BOW, "scaled:scale"))

presData.val.TF.IDF = validationSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()

presData.test.BOW = testSentencesBOW %>% 
  select(-presidentName) %>%
  scale(center = attr(presData.train.BOW, "scaled:center"), 
        scale = attr(presData.train.BOW, "scaled:scale"))

presData.test.TF.IDF = testSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()

# Remove columns with NAs

presData.train.BOW.NEW = presData.train.BOW %>%
  replace(is.na(.), 0)

presData.val.BOW = presData.val.BOW %>%
  replace(is.na(.), 0)

presData.test.BOW = presData.test.BOW %>%
  replace(is.na(.), 0)
  
# One-Hot Encoding President Name variable

presTarget.train.BOW.OH = to_categorical(presTarget.train.BOW)
presTarget.train.TF.IDF.OH = to_categorical(presTarget.train.TF.IDF)

presTarget.val.BOW.OH = to_categorical(presTarget.val.BOW)
presTarget.val.TF.IDF.OH = to_categorical(presTarget.val.TF.IDF)

presTarget.test.BOW.OH = to_categorical(presTarget.test.BOW)
presTarget.test.TF.IDF.OH = to_categorical(presTarget.test.TF.IDF)

# Creating Feed-Forward Neural Network Model

dimensions = dim(presData.train.BOW.NEW)
presNum = dim(presTarget.train.BOW.OH)[2]

## Bag-Of-Words

bowFit.FF.NN1 = keras_model_sequential()

bowFit.FF.NN1 %>% 
  layer_dense(units = dimensions[1]/10, 
              activation = "relu", 
              input_shape = dimensions[2]) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = presNum, activation = "softmax")

summary(bowFit.FF.NN1)

bowFit.FF.NN1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.01),
  metrics = c('accuracy'),
)

bowFit.FF.NN1.History = bowFit.FF.NN1 %>% fit(
  presData.train.BOW.NEW, presTarget.train.BOW.OH, 
  epochs = 20, batch_size = 100,
  verbose = 1, shuffle = TRUE
)

bowFit.FF.NN1.History = bowFit.FF.NN1 %>% fit(
  presData.train.BOW.NEW, presTarget.train.BOW.OH, 
  epochs = 20, batch_size = 100,
  validation_data = list(presData.val.BOW, presTarget.val.BOW.OH),
  verbose = 1, shuffle = TRUE
)

plot(bowFit.FF.NN1.History)

## TF-IDF

dimensions.tf.idf = dim(presData.train.TF.IDF)

tf.idf.Fit.FF.NN1 = keras_model_sequential()

tf.idf.Fit.FF.NN1 %>% 
  layer_dense(units = dimensions.tf.idf[1]/10, 
              activation = "relu", 
              input_shape = dimensions[2]) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = presNum, activation = "softmax")

summary(tf.idf.Fit.FF.NN1)

tf.idf.Fit.FF.NN1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.01),
  metrics = c('accuracy'),
)

tf.idf.Fit.FF.NN1.History = tf.idf.Fit.FF.NN1 %>% fit(
  presData.train.TF.IDF, presTarget.train.TF.IDF.OH, 
  epochs = 20, batch_size = 100,
  validation_data = list(presData.val.TF.IDF, presTarget.val.TF.IDF.OH),
  verbose = 1, shuffle = TRUE
)

plot(tf.idf.Fit.FF.NN1.History)



##### Use on Test Data [ONLY ONCE]

bowFit.FF.NN1.testPredictions = bowFit.FF.NN1 %>% 
  predict(presData.test.BOW) %>% 
  k_argmax() %>% 
  as.numeric()

bowFit.FF.NN1.tab = table(presTarget.test.BOW, 
                          bowFit.FF.NN1.testPredictions)
bowFit.FF.NN1.ClassError = (1 - round(sum(diag(bowFit.FF.NN1.tab))/sum(bowFit.FF.NN1.tab), 3))

tf.idf.Fit.FF.NN1.testPredictions = tf.idf.Fit.FF.NN1 %>% 
  predict(presData.test.TF.IDF) %>% 
  k_argmax() %>% 
  as.numeric()

tf.idf.Fit.FF.NN1.tab = table(presTarget.test.TF.IDF, 
                              tf.idf.Fit.FF.NN1.testPredictions)

tf.idf.Fit.FF.NN1.ClassError = (1 - round(sum(diag(tf.idf.Fit.FF.NN1.tab))/sum(tf.idf.Fit.FF.NN1.tab), 3))


