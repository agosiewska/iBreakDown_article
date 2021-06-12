library(ggplot2)
library(dplyr)
library(iBreakDown)
library(patchwork)

auc_df <- read.csv("./benchmark/auc_results.csv")

auc_plot_data <- auc_df
num_models <- auc_df$model %>% unique() %>% length()
auc_plot_data$auc_sort <- rep(auc_plot_data$auc[seq(1,nrow(auc_plot_data), by=num_models)], each = num_models)

auc_plot_data$model <- ifelse(auc_plot_data$model == "gbm", "GBM 1", auc_plot_data$model)
auc_plot_data$model <- ifelse(auc_plot_data$model == "gbm_id2", "GBM 2", auc_plot_data$model)
auc_plot_data$model <- ifelse(auc_plot_data$model == "gbm_id3", "GBM 3", auc_plot_data$model)

task_id_order <-  c(9967L,3954L, 9946L, 10093L, 3L, 34537L, 43L, 3899L, 3494L, 3902L, 9978L,
                    9957L, 3492L, 14965L, 9952L, 37L, 219L, 49L, 3913L, 9983L, 31L,
                    3917L, 3903L, 3918L, 3493L, 10101L, 9971L, 9980L)
auc_plot_data$task_id <- factor(auc_plot_data$task_id, levels = rev(task_id_order))

p <- ggplot(auc_plot_data, aes(x = task_id, y = auc, color = model)) + 
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0.5, 1), expand=c(0,0)) +
  theme_bw() +
  xlab("task id") +
  theme() +
  coord_flip()
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

