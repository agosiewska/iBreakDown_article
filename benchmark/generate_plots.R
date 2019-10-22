library(ggplot2)
auc_df <- read.csv("./benchmark/auc_results.csv")

auc_plot_data <- auc_df
auc_plot_data$auc_sort <- rep(auc_plot_data$auc[seq(1,nrow(auc_plot_data), by=2)], each = 2)

ggplot(auc_plot_data, aes(x = reorder(factor(task_id), auc_sort), y = auc, color = model)) + 
  geom_point(size = 2) +
  ylim(0,1) +
  theme_bw() +
  xlab("task id")

