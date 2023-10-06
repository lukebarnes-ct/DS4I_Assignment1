library(ranger)
## Classification Tree

bowFit = rpart(presidentName ~., 
               data = trainingSentences,
               method = "class")

# plot(bowFit, main = 'Full Classification Tree')
# text(bowFit, use.n = TRUE, all = TRUE, cex=.8)

fittedtrain = predict(bowFit, type = 'class')
predtrain = table(trainingSentences$presidentName, fittedtrain)
predtrain

round(sum(diag(predtrain))/sum(predtrain), 3) # training accuracy

#### Random Forests

rfNames = colnames(trainingSentences) %>% 
  as_tibble() %>%
  mutate(newNames = str_remove_all(value, "[0-9]"))

rfTrainData = trainingSentences %>%
  mutate(presidentName = as.factor(presidentName))

colnames(rfTrainData) = rfNames

bowRF = ranger(presidentName ~ ., 
               data = rfTrainData,
               mtry = 5,
               num.trees = 500)

#### TF-IDF Model
speechWords %>%
  count(president, word, sort = TRUE) %>%
  bind_tf_idf(word, president, n) %>%
  arrange(desc(tf_idf))


sum(str_count(speechSentences$sentences, "[0-9]"))

str_subset(speechSentences$sentences, "[0-9]")

xxx = speechSentences$sentences
xxx = str_remove_all(xxx, "[0-9]")
xxx = str_remove_all(xxx, "\\s*(?<!\\S)(?!\\p{L}+(?!\\S))\\S+")
str_subset(xxx, "[0-9]")

trainingSentencesBOW$`function`

# ranger appears to be much faster than randomForests
tic()
bowFit.Ranger = ranger(presidentName ~ ., 
                       data = NEWtrainingSentencesBOW,
                       mtry = 20,
                       num.trees = 500)
toc()

trainFittedBOW.Ranger = bowFit.Ranger$predictions
trainPredBOW.Ranger = table(trainingSentencesBOW$presidentName, trainFittedBOW.Ranger)
round(sum(diag(trainPredBOW.Ranger))/sum(trainPredBOW.Ranger), 3)

trainFittedBOW.RF = predict(bowFit.RF, type = 'class')
trainPredBOW.RF = table(trainingSentencesBOW$presidentName, trainFittedBOW.RF)
round(sum(diag(trainPredBOW.RF))/sum(trainPredBOW.RF), 3)

bowWob = speechTDF %>%
  select(sentID, word, n)

tic()
bowFit.RF = randomForest(presidentName ~ ., 
                         data = NEWtrainingSentencesBOW,
                         importance = TRUE,
                         ntree = 20)
toc()

NEWtrainingSentencesBOW = trainingSentencesBOW[, -c(495, 776, 1651)]
NEWtrainingSentencesBOW$presidentName = factor(NEWtrainingSentencesBOW$presidentName)

match("break", names(trainingSentences.TF.IDF))

classErrorBOW.RF = (1 - round(sum(diag(trainPredBOW.Ranger))/sum(trainPredBOW.Ranger), 3))
classError.TF.IDF.RF = (1 - round(sum(diag(trainPred.TF.IDF.Ranger))/sum(trainPred.TF.IDF.Ranger), 3))

minTrees.TF.IDF.RF = seq[which.min(valErr)]

plot(seq, error, type = 'l', col = 'blue', lwd = 3, ylim = c(0.475, 0.525))
plot(seq, valErr, type = 'l', col = 'blue', lwd = 3)
lines(seq, valErr, col = 'red', lwd = 3)

grid.RF = expand.grid(mtry = seq(10, 50, by = 10),
                      splitrule = 'gini',
                      min.node.size = 1)

ctrl.RF = trainControl(method = 'oob', verboseIter = T)

tic()
gridsearch.TF.IDF.RF = train(presidentName ~ ., 
                             data = trainingSentences.TF.IDF,
                             method = 'ranger',
                             num.trees = minTrees.TF.IDF.RF,
                             verbose = T,
                             trControl = ctrl.RF,
                             tuneGrid = grid.RF)
toc()

res.TF.IDF.RF = gridsearch.TF.IDF.RF$results
gridsearch.TF.IDF.RF$finalModel

tic()
gridsearchBOW.RF = train(presidentName ~ ., 
                         data = trainingSentencesBOW,
                         method = 'ranger',
                         num.trees = minTreesBOW,
                         verbose = T,
                         trControl = ctrl.RF,
                         tuneGrid = grid.RF)
toc()

resBOW.RF = gridsearchBOW.RF$results
gridsearchBOW.RF$finalModel