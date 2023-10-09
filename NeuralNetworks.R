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

## Imp Variables

dimensions = dim(presData.train.BOW.NEW)
presNum = dim(presTarget.train.BOW.OH)[2]

## Model Tuning

##### Keras Function for Tuning Parameters

actFunc = c("relu", "selu", "tanh", "sigmoid")
hiddenLayer = 32 * (2^seq(0, 5))
dropOut = seq(0.1, 0.85, by = 0.15)
bSize = 16 * (2^seq(0, 5))

kerasHPTuning = function(actF, hidLayer, dpRate, b, X, Y, XVal, YVal){
  
  dim = dim(X)
  nn = keras_model_sequential()
  
  nn %>% 
    layer_dense(units = hidLayer, 
                activation = actF, 
                input_shape = dim[2]) %>%
    layer_dropout(rate = dpRate) %>%
    layer_dense(units = presNum, activation = "softmax")
  
  summary(nn)
  
  nn %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(learning_rate = 0.01),
    metrics = c('accuracy'),
  )
  
  nn.History = nn %>% fit(
    X, Y, 
    epochs = 10, batch_size = b,
    validation_data = list(XVal, YVal),
    verbose = 1, shuffle = TRUE
  )
  
  return(nn.History$metrics$val_accuracy[10])
}

reluArray = array(0, dim = c(length(hiddenLayer), 
                             length(dropOut),
                             length(bSize)))

seluArray = array(0, dim = c(length(hiddenLayer), 
                             length(dropOut),
                             length(bSize)))

tanhArray = array(0, dim = c(length(hiddenLayer), 
                             length(dropOut),
                             length(bSize)))

sigmArray = array(0, dim = c(length(hiddenLayer), 
                             length(dropOut),
                             length(bSize)))

for (i in 1:length(hiddenLayer)){
  
  for (j in 1:length(dropOut)){
    
    for (k in 1:length(bSize)){
      
      reluArray[i, j, k] =  kerasHPTuning(actFunc[1],
                                          hiddenLayer[i],
                                          dropOut[j],
                                          bSize[k],
                                          X = presData.train.TF.IDF,
                                          Y = presTarget.train.TF.IDF.OH,
                                          XVal = presData.val.TF.IDF,
                                          YVal = presTarget.val.TF.IDF.OH)
      
      seluArray[i, j, k] = kerasHPTuning(actFunc[2],
                                         hiddenLayer[i],
                                         dropOut[j],
                                         bSize[k],
                                         X = presData.train.TF.IDF,
                                         Y = presTarget.train.TF.IDF.OH,
                                         XVal = presData.val.TF.IDF,
                                         YVal = presTarget.val.TF.IDF.OH)
      
      tanhArray[i, j, k] = kerasHPTuning(actFunc[3],
                                         hiddenLayer[i],
                                         dropOut[j],
                                         bSize[k],
                                         X = presData.train.TF.IDF,
                                         Y = presTarget.train.TF.IDF.OH,
                                         XVal = presData.val.TF.IDF,
                                         YVal = presTarget.val.TF.IDF.OH)
      
      sigmArray[i, j, k] = kerasHPTuning(actFunc[4],
                                         hiddenLayer[i],
                                         dropOut[j],
                                         bSize[k],
                                         X = presData.train.TF.IDF,
                                         Y = presTarget.train.TF.IDF.OH,
                                         XVal = presData.val.TF.IDF,
                                         YVal = presTarget.val.TF.IDF.OH)
    }
  }
}

maxValues = c(max(reluArray),
              max(seluArray),
              max(tanhArray),
              max(sigmArray))

which.max(reluArray)

bestActF = actFunc[which.max(maxValues)]
bestH = hiddenLayer[6]
bestDP = dropOut[1]
bestB = bSize[1]

## Bag-Of-Words Keras Tuning

reluBOWArray = array(0, dim = c(length(hiddenLayer), 
                                length(dropOut),
                                length(bSize)))

for (i in 1:length(hiddenLayer)){
  
  for (j in 1:length(dropOut)){
    
    for (k in 1:length(bSize)){
      
      reluBOWArray[i, j, k] =  kerasHPTuning(actFunc[1],
                                             hiddenLayer[i],
                                             dropOut[j],
                                             bSize[k],
                                             X = presData.train.BOW.NEW,
                                             Y = presTarget.train.BOW.OH,
                                             XVal = presData.val.BOW,
                                             YVal = presTarget.val.BOW.OH)
    }
  }
}

bestBOWActF = actFunc[1]
bestBOWH = hiddenLayer[3]
bestBOWDP = dropOut[4]
bestBOWB = bSize[6]

##### Keras Function for Tuning Number of Layers

kerasLayerTuning = function(nLayer, X, Y, XVal, YVal){
  
  dim = dim(X)
  nn = keras_model_sequential()
  
  if (nLayer == 1){
    
    nn %>% 
      layer_dense(units = bestH, 
                  activation = bestActF, 
                  input_shape = dim[2]) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = presNum, activation = "softmax")
    
    summary(nn)
    
    nn %>% compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(learning_rate = 0.01),
      metrics = c('accuracy'),
    )
    
    nn.History = nn %>% fit(
      X, Y, 
      epochs = 10, batch_size = bestB,
      validation_data = list(XVal, YVal),
      verbose = 1, shuffle = TRUE
    )
  }
  
  if (nLayer == 2){
    
    nn %>% 
      layer_dense(units = bestH, 
                  activation = bestActF, 
                  input_shape = dim[2]) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/2, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = presNum, activation = "softmax")
    
    summary(nn)
    
    nn %>% compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(learning_rate = 0.01),
      metrics = c('accuracy'),
    )
    
    nn.History = nn %>% fit(
      X, Y, 
      epochs = 10, batch_size = bestB,
      validation_data = list(XVal, YVal),
      verbose = 1, shuffle = TRUE
    )
  }
  
  if (nLayer == 3){
    
    nn %>% 
      layer_dense(units = bestH, 
                  activation = bestActF, 
                  input_shape = dim[2]) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/2, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/4, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = presNum, activation = "softmax")
    
    summary(nn)
    
    nn %>% compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(learning_rate = 0.01),
      metrics = c('accuracy'),
    )
    
    nn.History = nn %>% fit(
      X, Y, 
      epochs = 10, batch_size = bestB,
      validation_data = list(XVal, YVal),
      verbose = 1, shuffle = TRUE
    )
  }
  
  if (nLayer == 4){
    
    nn %>% 
      layer_dense(units = bestH, 
                  activation = bestActF, 
                  input_shape = dim[2]) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/2, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/4, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = bestH/8, 
                  activation = bestActF) %>%
      layer_dropout(rate = bestDP) %>%
      layer_dense(units = presNum, activation = "softmax")
    
    summary(nn)
    
    nn %>% compile(
      loss = "categorical_crossentropy",
      optimizer = optimizer_rmsprop(learning_rate = 0.01),
      metrics = c('accuracy'),
    )
    
    nn.History = nn %>% fit(
      X, Y, 
      epochs = 10, batch_size = bestB,
      validation_data = list(XVal, YVal),
      verbose = 1, shuffle = TRUE
    )
  }
  
  return(mean(nn.History$metrics$val_accuracy[5:10]))
}

layers = 1:4
layError = c()
for (l in 1:length(layers)){
  
  layError[l] = kerasLayerTuning(layers[l],
                                 X = presData.train.TF.IDF,
                                 Y = presTarget.train.TF.IDF.OH,
                                 XVal = presData.val.TF.IDF,
                                 YVal = presTarget.val.TF.IDF.OH)
  }


# Creating Feed-Forward Neural Network Model

## Bag-Of-Words

bowFit.FF.NN = keras_model_sequential()

bowFit.FF.NN %>% 
  layer_dense(units = bestBOWH, 
              activation = bestBOWActF, 
              input_shape = dimensions[2]) %>%
  layer_dropout(rate = bestBOWDP) %>%
  layer_dense(units = presNum, activation = "softmax")

summary(bowFit.FF.NN)

bowFit.FF.NN %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.01),
  metrics = c('accuracy'),
)

bowFit.FF.NN.History = bowFit.FF.NN %>% fit(
  presData.train.BOW.NEW, presTarget.train.BOW.OH, 
  epochs = 40, batch_size = bestBOWB,
  validation_data = list(presData.val.BOW, presTarget.val.BOW.OH),
  verbose = 1, shuffle = TRUE
)

plot(bowFit.FF.NN.History)

## TF-IDF

dimensions.tf.idf = dim(presData.train.TF.IDF)

tf.idf.Fit.FF.NN = keras_model_sequential()

tf.idf.Fit.FF.NN %>% 
  layer_dense(units = bestH, 
              activation = bestActF, 
              input_shape = dimensions.tf.idf[2]) %>%
  layer_dropout(rate = bestBOWDP) %>%
  layer_dense(units = presNum, activation = "softmax")

summary(tf.idf.Fit.FF.NN)

tf.idf.Fit.FF.NN %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(learning_rate = 0.01),
  metrics = c('accuracy'),
)

tf.idf.Fit.FF.NN.History = tf.idf.Fit.FF.NN %>% fit(
  presData.train.TF.IDF, presTarget.train.TF.IDF.OH, 
  epochs = 20, batch_size = bestB,
  validation_data = list(presData.val.TF.IDF, presTarget.val.TF.IDF.OH),
  verbose = 1, shuffle = TRUE
)

plot(tf.idf.Fit.FF.NN.History)

##### Use on Test Data [ONLY ONCE]

bowFit.FF.NN.testPredictions = bowFit.FF.NN %>% 
  predict(presData.test.BOW) %>% 
  k_argmax() %>% 
  as.numeric()

bowFit.FF.NN.tab = table(presTarget.test.BOW, 
                          bowFit.FF.NN.testPredictions)
bowFit.FF.NN.ClassError = (1 - round(sum(diag(bowFit.FF.NN.tab))/sum(bowFit.FF.NN.tab), 3))

tf.idf.Fit.FF.NN.testPredictions = tf.idf.Fit.FF.NN %>% 
  predict(presData.test.TF.IDF) %>% 
  k_argmax() %>% 
  as.numeric()

tf.idf.Fit.FF.NN.tab = table(presTarget.test.TF.IDF, 
                              tf.idf.Fit.FF.NN.testPredictions)

tf.idf.Fit.FF.NN.ClassError = (1 - round(sum(diag(tf.idf.Fit.FF.NN.tab))/sum(tf.idf.Fit.FF.NN.tab), 3))


## Save Output for use later

save(reluArray, seluArray,
     tanhArray, sigmArray,
     reluBOWArray,
     layError,
     bowFit.FF.NN.History, tf.idf.Fit.FF.NN.History,
     bowFit.FF.NN.testPredictions, bowFit.FF.NN.tab, bowFit.FF.NN.ClassError,
     tf.idf.Fit.FF.NN.testPredictions, tf.idf.Fit.FF.NN.tab, tf.idf.Fit.FF.NN.ClassError,
     file = "FeedForwardNN_Data.RData")
