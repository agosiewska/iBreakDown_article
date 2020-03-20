library(ggplot2)
library(dplyr)
library(OpenML)

files <- list.files("./benchmark/explanations/")

results <- data.frame(task=character(), model=character(), observation=numeric(), interactions = numeric())

for(file in files){
  load(paste0("./benchmark/explanations/", file))
  for(i in 1:50){
    task <- file
    if(grepl("ranger.rda", task)) model <- "ranger"
    if(grepl("gbm.rda", task)) model <- "gbm"
    if(grepl("gbm_id2.rda", task)) model <- "gbm_id2"
    if(grepl("gbm_id3.rda", task)) model <- "gbm_id3"

    task <- gsub("task_","", file)
    ibd <- get(paste0("ibd_",model))[[i]]
    vars <- ibd$variable
    n_interactions <- sum(grepl(":", vars))
    task <- gsub(paste0("_", model, ".rda"), "", task)
    results <- rbind(results, data.frame(task = task, model = model, observation = i, interactions = n_interactions))   
    }
}


# write.csv(results, "benchmark/interactions_results.csv")
results <- read.csv("benchmark/interactions_results.csv")


results %>%
  filter(model =="gbm") %>%
  select(interactions)%>%
  table()
# 
# results %>%
#   filter(model =="gbm_id2") %>%
#   select(interactions)%>%
#   table()
# 
# results %>%
#   filter(model =="gbm_id3") %>%
#   select(interactions)%>%
#   table()
# 
# results %>%
#   filter(model =="ranger") %>%
#   select(interactions)%>%
#   table()
# 
# table(results$model, results$interactions) %>%
#   xtable::xtable()

####################
# Summarize by task
results2 <- results
results2$interactions <- ifelse(results2$interactions >=4, "4+", results2$interactions)

interaction_table <- data.frame("task" = character(),
                                "model" = character(),
                                "0"=numeric(), 
                                "1"=numeric(), 
                                "2"=numeric(), 
                                "3"=numeric(), 
                                "4+"=numeric())

tasks <- results$task %>% unique()
models <- results$model %>% unique()

for(mdl in models){
  for(tsk in tasks){
    results_template <- c("task"=tsk, "model" = mdl, "0"=0, "1"=0, "2"=0, "3"=0, "4+"=0)
    tmp <- results2 %>%
      filter(model == mdl & task == tsk) 
    results_template[names(table(tmp$interactions))] <- table(tmp$interactions) 
    print(results_template)
    interaction_table <- rbind(interaction_table, as.data.frame(t(results_template)))
  }
}
  

# add tasks datasets
tasks_oml100 <- listOMLTasks(tag="openml100", number.of.classes = 2, number.of.missing.values = 0)
tasks_oml100[["task.id"]] <- as.factor(tasks_oml100[["task.id"]])
interaction_table <- interaction_table %>%
  left_join(tasks_oml100[,c("task.id", "name")], by = c("task"="task.id"))
interaction_table[["task_name"]]  <- paste0(interaction_table[["task"]], " (", interaction_table[["name"]], ")")
interaction_table <- interaction_table %>%
  select(-name)

it_ranger <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "ranger") 
it_gbm_id1 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm") 

table1 <- cbind(it_ranger[,-c(1,2)], it_gbm_id1[,-c(1,2)])

rownames(table1) <- it_gbm_id1$task_name
colnames(table1)
table1 <- table1[, -c(6, 12)]
xtable::xtable(table1)



it_gbm_id2 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm_id2") 
it_gbm_id3 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm_id3") 

table2 <- cbind(it_gbm_id2[,-c(1,2)], it_gbm_id3[,-c(1,2)])

rownames(table2) <- it_gbm_id2$task_name
table2 <- table2[, -c(6, 12)]
xtable::xtable(table2)



# Sum of interactions
n_interactions <- sum(sapply(table1[,-1], function(x) as.numeric(as.character(x)))) + sum(sapply(table2[,-1], function(x) as.numeric(as.character(x)))) 
n_explanations <- sum(sapply(table1, function(x) as.numeric(as.character(x)))) + sum(sapply(table2, function(x) as.numeric(as.character(x)))) 
n_interactions / n_explanations
  
  
  
  
  
  
  
  