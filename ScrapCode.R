
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