library(ggplot2)
library(dplyr)
library(iBreakDown)
library(patchwork)

auc_df <- read.csv("./benchmark/auc_results.csv")

auc_plot_data <- auc_df
num_models <- auc_df$model %>% unique() %>% length()
auc_plot_data$auc_sort <- rep(auc_plot_data$auc[seq(1,nrow(auc_plot_data), by=num_models)], each = num_models)


p <- ggplot(auc_plot_data, aes(x = reorder(factor(task_id), auc_sort), y = auc, color = model)) + 
  geom_point(size = 2) +
  ylim(0,1) +
  theme_bw() +
  xlab("task id") +
  theme(axis.text.x = element_text(angle = 90))
p
ggsave("./benchmark/fig/models_performance.pdf", width = 7, height = 3)



obs <- 13
load("./benchmark/explanations/task_3493_gbm.rda")
load("./benchmark/explanations/task_3493_gbm_id2.rda")
load("./benchmark/explanations/task_3493_gbm_id3.rda")
load("./benchmark/explanations/task_3493_ranger.rda")
i_gbm <- ibd_gbm[[obs]][which(ibd_gbm[[obs]]$variable_name != "class"),] 
i_gbm[1:(nrow(i_gbm)-1), "position"] <- as.numeric(as.character(i_gbm[1:(nrow(i_gbm)-1), "position"])) - 1
i_gbm_id2 <- ibd_gbm_id2[[obs]][which(ibd_gbm_id2[[obs]]$variable_name != "class"),] 
i_gbm_id2[1:(nrow(i_gbm_id2)-1), "position"] <- as.numeric(as.character(i_gbm_id2[1:(nrow(i_gbm_id2)-1), "position"])) - 1
i_gbm_id3 <- ibd_gbm_id3[[obs]][which(ibd_gbm_id3[[obs]]$variable_name != "class"),] 
i_gbm_id3[1:(nrow(i_gbm_id3)-1), "position"] <- as.numeric(as.character(i_gbm_id3[1:(nrow(i_gbm_id3)-1), "position"])) - 1
i_ranger <- ibd_ranger[[obs]][which(ibd_ranger[[obs]]$variable_name != "class"),] 
i_ranger[1:(nrow(i_ranger)-1), "position"] <- as.numeric(as.character(i_ranger[1:(nrow(i_ranger)-1), "position"])) - 1

i_gbm[,"label"] <- "GBM 1"
i_gbm_id2[,"label"] <- "GBM 2"
i_gbm_id3[,"label"] <- "GBM 3"
i_gbm_id4[,"label"] <- "GBM 4"
i_ranger[,"label"] <- "random forest"

plot(i_gbm) +
  plot(i_gbm_id2) + 
  plot(i_gbm_id3) + 
  plot(i_ranger)

ggsave("./benchmark/fig/break_down_benchmark.pdf", width = 9, height = 7, scale = 0.85)

