library(ggplot2)
library(dplyr)
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
