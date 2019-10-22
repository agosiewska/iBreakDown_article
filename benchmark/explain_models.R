library(OpenML)
library(dplyr)
library(ranger)
library(DALEX)
library(DALEXtra)
library(iBreakDown)

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



# remove calculated tasks
files <- list.files("./benchmark/explanations/")
calculated_tasks <- gsub("task_","", files)
calculated_tasks <- gsub("_gbm.rda","", calculated_tasks)
tasks <- tasks %>% filter(!(task.id %in% calculated_tasks))

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
  y <- as.numeric(dataset[,target_var] == positive)
  
  # load corresponding models
  filename_ranger <- paste0("./benchmark/models/task_", task_id, "_ranger.rda")
  load(filename_ranger)
  exp_ranger <- explain_mlr(model_ranger,
                            data = dataset,
                            y = y,
                            label = paste("ranger_", task_id))
  
  filename_gbm <- paste0("./benchmark/models/task_", task_id, "_gbm.rda")
  load(filename_gbm)
  exp_gbm <- explain_mlr(model_gbm,
                            data = dataset,
                            y = y,
                            label = paste("gbm_", task_id))
  ibd_ranger <- list()
  ibd_gbm <- list()
  for(j in 1:50){
    print(paste("Observation:", j))  
    new_observations <- dataset[test_id[j], ]  
    # ibd_ranger[[j]] <- local_interactions(exp_ranger, new_observation = new_observations)
    # paste("ranger calculated")
    ibd_gbm[[j]] <- local_interactions(exp_gbm, new_observation = new_observations)
    paste("gbm calculated")
  }
  # filename <- paste0("./benchmark/explanations/task_", task_id, "_ranger.rda")
  # save(ibd_ranger, file = filename)
  filename <- paste0("./benchmark/explanations/task_", task_id, "_gbm.rda")
  save(ibd_gbm, file = filename)
}



