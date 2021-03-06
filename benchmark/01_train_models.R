library(OpenML)
library(dplyr)
library(ranger)

tasks = listOMLTasks(tag="openml100", number.of.missing.values = 0, number.of.classes = 2)

# gbm does not currently handle categorical variables with more than 1024 levels. Variable 1: RESOURCE has 7085 levels.
tasks <- tasks %>% filter(task.id != 34539)
# 300 features
tasks <- tasks %>% filter(task.id != 3485)
# 971 features
tasks <- tasks %>% filter(task.id != 3891)
# 101 features
tasks <- tasks %>% filter(task.id != 9970)
# 501 features
tasks <- tasks %>% filter(task.id != 9976)
# 109 features
tasks <- tasks %>% filter(task.id != 9977)
# 1777 features
tasks <- tasks %>% filter(task.id != 14966)

auc_df <- data.frame(task_id = character(), model = character(), auc = numeric())



# saving ranger and gbm models for all datasets
for(i in 1:nrow(tasks)){
  # get info from predefined task 
  task_id <- tasks[i, "task.id"]
  print(paste("Task:", task_id))
  task <- getOMLTask(task_id)
  dataset <- task[["input"]][["data.set"]][["data"]]
  splits <- task[["input"]][["estimation.procedure"]][["data.splits"]]
  train_id <- splits %>% filter(fold == 1, type == "TRAIN") %>% pull(rowid)
  test_id <- splits %>% filter(fold == 1, type == "TEST") %>% pull(rowid)
  target_var <- task[["input"]][["target.features"]]
  
  task_train <- makeClassifTask(id = as.character(task_id), data = dataset[train_id, ], target = target_var)
  positive = getTaskDesc(task_train)$positive
  negative = getTaskDesc(task_train)$negative
  
  # # ranger part
  learner_ranger <- makeLearner("classif.ranger", predict.type = "prob")
  model_ranger <- train(learner_ranger, task_train)
  pred_ranger <- predict(model_ranger, newdata = dataset[test_id, ], type = "probability")
  probs_ranger <- getPredictionProbabilities(pred_ranger)
  auc <- measureAUC(probs_ranger, dataset[test_id, target_var], positive = positive, negative = negative)
  filename <- paste0("./benchmark/models/task_", task_id, "_ranger.rda")
  auc_df <- rbind(auc_df, data.frame(task_id = task_id, model = "ranger", auc = auc))
  save(model_ranger, file = filename)
  print(paste("ranger: ", auc))

  # # gbm part
  learner_gbm <- makeLearner("classif.gbm", predict.type = "prob")
  model_gbm <- train(learner_gbm, task_train)
  pred_gbm <- predict(model_gbm, newdata = dataset[test_id, ], type = "probability")
  probs_gbm <- getPredictionProbabilities(pred_gbm)
  auc <- measureAUC(probs_gbm, dataset[test_id, target_var], positive = positive, negative = negative)
  filename <- paste0("./benchmark/models/task_", task_id, "_gbm.rda")
  auc_df <- rbind(auc_df, data.frame(task_id = task_id, model =  "gbm", auc =  auc))
  save(model_gbm, file = filename)
  print(paste("gbm: ", auc))

  # # gbm with interaction depth part
  learner_gbm_id2 <- makeLearner("classif.gbm", predict.type = "prob", par.vals = list(interaction.depth = 2))
  model_gbm_id2 <- train(learner_gbm_id2, task_train)
  pred_gbm_id2 <- predict(model_gbm_id2, newdata = dataset[test_id, ], type = "probability")
  probs_gbm_id2 <- getPredictionProbabilities(pred_gbm_id2)
  auc <- measureAUC(probs_gbm_id2, dataset[test_id, target_var], positive = positive, negative = negative)
  filename <- paste0("./benchmark/models/task_", task_id, "_gbm_id2.rda")
  auc_df <- rbind(auc_df, data.frame(task_id = task_id, model =  "gbm_id2", auc =  auc))
  save(model_gbm_id2, file = filename)
  print(paste("gbm_id2: ", auc))
  
  # gbm with 3 interaction depth   
  learner_gbm_id3 <- makeLearner("classif.gbm", predict.type = "prob", par.vals = list(interaction.depth = 3))
  model_gbm_id3 <- train(learner_gbm_id3, task_train)
  pred_gbm_id3 <- predict(model_gbm_id3, newdata = dataset[test_id, ], type = "probability")
  probs_gbm_id3 <- getPredictionProbabilities(pred_gbm_id3)
  auc <- measureAUC(probs_gbm_id3, dataset[test_id, target_var], positive = positive, negative = negative)
  filename <- paste0("./benchmark/models/task_", task_id, "_gbm_id3.rda")
  auc_df <- rbind(auc_df, data.frame(task_id = task_id, model =  "gbm_id3", auc =  auc))
  save(model_gbm_id3, file = filename)
  print(paste("gbm_id3: ", auc))
}

write.csv(auc_df, file = "./benchmark/auc_results.csv")



