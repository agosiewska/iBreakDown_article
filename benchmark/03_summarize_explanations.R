library(ggplot2)
library(dplyr)

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


write.csv(results, "benchmark/interactions_results.csv")
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
  

it_ranger <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "ranger") 
it_gbm_id1 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm") 

table1 <- cbind(it_ranger[,-c(1,2)], it_gbm_id1[,-c(1,2)])

rownames(table1) <- it_gbm_id1$task
xtable::xtable(table1)



it_gbm_id2 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm_id2") 
it_gbm_id3 <-interaction_table %>%
  arrange(as.numeric(as.character(task))) %>%
  filter(model == "gbm_id3") 

table2 <- cbind(it_gbm_id2[,-c(1,2)], it_gbm_id3[,-c(1,2)])

rownames(table2) <- it_gbm$task
xtable::xtable(table2, caption = model_name, label = paste0("fig:benchmark_", model_name))
