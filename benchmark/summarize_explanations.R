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

results %>%
  filter(model =="gbm") %>%
  select(interactions)%>%
  table()

results %>%
  filter(model =="gbm_id2") %>%
  select(interactions)%>%
  table()

results %>%
  filter(model =="gbm_id3") %>%
  select(interactions)%>%
  table()

results %>%
  filter(model =="ranger") %>%
  select(interactions)%>%
  table()

table(results$model, results$interactions) %>%
  xtable::xtable()
